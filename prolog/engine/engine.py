"""Prolog Engine with Explicit Stacks (Stage 0) - Fixed Implementation."""

import json
import os
from typing import Dict, List, Optional, Any, Tuple
from prolog.ast.terms import (
    Term,
    Atom,
    Int,
    Float,
    Var,
    Struct,
    List as PrologList,
    PrologDict,
)
from prolog.ast.clauses import Program, Clause, ClauseCursor
from prolog.engine.rename import VarRenamer
from prolog.unify.store import Store, Cell
from prolog.unify.unify import unify
from prolog.engine.indexed_program import IndexedProgram

# Import new runtime types
from prolog.engine.runtime import (
    GoalType,
    ChoicepointKind,
    Goal,
    Frame,
    Choicepoint,
    GoalStack,
    Trail,
)
from prolog.engine.trail_adapter import TrailAdapter
from prolog.engine.errors import PrologThrow, UndefinedPredicateError
from prolog.engine.cursors import StreamingClauseCursor

# JSON support imports
from prolog.engine.json_convert import (
    json_to_prolog,
    prolog_to_json,
    CLASSIC_MODE,
    DICT_MODE,
)

# Utility function imports
from prolog.engine.utils.terms import reify_var, reify_term
from prolog.engine.utils.copy import (
    copy_term_with_fresh_vars,
    copy_term_recursive,
    build_prolog_list,
)
from prolog.engine.utils.arithmetic import eval_int

# Builtin registration system
from prolog.engine.builtins import register_all
from prolog.engine.builtins.types import (
    builtin_var,
    builtin_nonvar,
    builtin_atom,
    builtin_integer,
    builtin_float,
    builtin_number,
    builtin_atomic,
    builtin_compound,
    builtin_callable,
    builtin_ground,
    is_ground,
)

# Debug imports (conditional imports moved here)
from prolog.debug.tracer import PortsTracer
from prolog.debug.metrics import EngineMetrics

# CLP(FD) imports (conditional imports moved here)
from prolog.engine.builtins_clpfd import (
    _builtin_in,
    _builtin_fd_eq,
    _builtin_fd_neq,
    _builtin_fd_lt,
    _builtin_fd_le,
    _builtin_fd_gt,
    _builtin_fd_ge,
    _builtin_fd_var,
    _builtin_fd_inf,
    _builtin_fd_sup,
    _builtin_fd_dom,
    _builtin_all_different,
    _builtin_fd_reif_equiv,
    _builtin_fd_reif_implies,
    _builtin_fd_reif_implied,
    _builtin_fd_disj,
    _builtin_element_3,
    _builtin_global_cardinality,
    _builtin_nvalue,
    _builtin_lex_chain,
    _builtin_table,
)
from prolog.clpfd.label import _builtin_label, _builtin_labeling, push_labeling_choices

# Parser imports (conditional imports moved here)
from prolog.parser.parser import parse_program, parse_query
from prolog.ast.clauses import Program as ProgramClass

# Debug imports
from prolog.debug.filters import TraceFilters


class Engine:
    """Prolog inference engine with explicit stacks (no Python recursion)."""

    def __init__(
        self,
        program: Program,
        occurs_check: bool = False,
        max_solutions: Optional[int] = None,
        trace: bool = False,
        max_steps: Optional[int] = None,
        use_indexing: bool = False,
        debug: bool = False,
        use_streaming: bool = False,
        metrics: bool = False,
        mode: str = "dev",
    ):
        """Initialize engine with a program.

        Args:
            program: The Prolog program to execute.
            occurs_check: Whether to use occurs check in unification.
            max_solutions: Maximum number of solutions to find.
            trace: Whether to enable tracing.
            max_steps: Maximum number of steps to execute (for debugging infinite loops).
            use_indexing: Whether to use first-argument indexing for clause selection.
            debug: Whether to enable debug instrumentation (e.g., candidate counting).
            use_streaming: Whether to use streaming clause selection (requires indexing).
            metrics: Whether to enable metrics collection.
            mode: Engine mode - "dev" for development (default) or "iso" for ISO compliance.
        """
        # Convert to IndexedProgram if indexing is enabled
        if use_indexing:
            self.program = (
                IndexedProgram.from_program(program)
                if not isinstance(program, IndexedProgram)
                else program
            )
        else:
            self.program = program

        self.use_indexing = use_indexing
        self.debug = debug
        self.occurs_check = occurs_check

        # Determine if streaming should be enabled
        # Check environment variable override
        env_streaming = os.environ.get("PYLOG_STREAM_SELECTION")
        if env_streaming == "1":
            # Force streaming on (if indexing enabled)
            self.use_streaming = use_indexing
        elif env_streaming == "0":
            # Force streaming off
            self.use_streaming = False
        else:
            # Use parameter, but only if conditions are met:
            # - Indexing must be enabled
            # - Debug/trace should be off (or we gate it later)
            # - Metrics should be off
            self.use_streaming = use_streaming and use_indexing

        # Core state - using new runtime types
        self.store = Store()
        self.trail = Trail()
        # Set engine reference on trail so attribute hooks can be dispatched
        self.trail.engine = self
        self.goal_stack = GoalStack()
        self.frame_stack: List[Frame] = []
        self.cp_stack: List[Choicepoint] = []

        # Note: cp_stack is accessed via choicepoints() method for snapshot module

        # Query tracking with fast lookups
        self._query_vars: List[Tuple[int, str]] = []  # [(varid, name), ...] for order
        self._qid_by_name: Dict[str, int] = {}  # name -> varid for fast allocation
        self._qname_by_id: Dict[int, str] = {}  # varid -> name for fast reification
        self._initial_var_cutoff = 0  # Vars below this are query vars

        # Solution collection
        self.solutions: List[Dict[str, Any]] = []
        self.max_solutions = max_solutions

        # Configuration
        self.trace = trace
        self._trace_log: List[str] = []  # For debugging
        self.max_steps = max_steps  # Step budget for infinite loop detection
        self._steps_taken = 0  # Counter for steps executed
        self.mode = mode  # Engine mode: "dev" or "iso"

        # Add write_stamp for tracer
        self.write_stamp = 0

        # Builtin registry: maps (name, arity) -> callable
        self._builtins = {}
        self._register_builtins()

        # Attribute hook registry: maps module_name -> hook_function
        self._attr_hooks = {}

        # Debug counters and frame tracking
        self._debug_frame_pops = 0
        self._debug_trail_writes = 0
        self._next_frame_id = 0  # Monotonic frame ID counter
        self._cut_barrier = None  # Legacy field for tests
        self._last_exit_info: Optional[Tuple[str, Any, int]] = None

        # Exception handling is done via try/except PrologThrow

        # Debug ports for tracing
        self._ports: List[str] = []
        self._goal_call_depths: Dict[int, int] = {}
        self._goal_call_flags: Dict[int, bool] = {}

        # Variable renaming for clause isolation
        self._renamer = VarRenamer(self.store)

        # Initialize debug counter if debug mode is enabled
        if self.debug:
            self._candidates_considered = 0

        # Initialize tracer if trace=True
        if trace:
            self.tracer = PortsTracer(self)
        else:
            self.tracer = None

        # Optional override for tracer frame depth (used during backtracking)
        self._frame_depth_override: Optional[int] = None

        # Initialize metrics if metrics=True
        if metrics:
            self.metrics = EngineMetrics()
        else:
            self.metrics = None

        # Feature flag for variable selection caching optimization
        self._uses_variable_selection_caching = True

        # Dynamic predicate registry (name, arity)
        self._dynamic_preds: set[tuple[str, int]] = set()

    # Public accessor methods for snapshot module
    def store_size(self) -> int:
        """Get the current store size."""
        return len(self.store.cells)

    def trail_length(self) -> int:
        """Get the current trail length."""
        return len(self.trail)

    def trail_top_value(self) -> int:
        """Get the current trail position."""
        return (
            self.trail.position()
            if hasattr(self.trail, "position")
            else len(self.trail)
        )

    def goal_height(self) -> int:
        """Get the current goal stack height."""
        return len(self.goal_stack) if hasattr(self, "goal_stack") else 0

    def goal_top_value(self) -> int:
        """Get the current goal top (same as height for list-based stack)."""
        return len(self.goal_stack) if hasattr(self, "goal_stack") else 0

    def frame_height(self) -> int:
        """Get the current frame stack height."""
        return len(self.frame_stack) if hasattr(self, "frame_stack") else 0

    def frame_top_value(self) -> int:
        """Get the current frame top (same as height for list-based stack)."""
        return len(self.frame_stack) if hasattr(self, "frame_stack") else 0

    def choicepoint_height(self) -> int:
        """Get the current choicepoint stack height."""
        return len(self.cp_stack) if hasattr(self, "cp_stack") else 0

    def choicepoint_top(self) -> int:
        """Get the current choicepoint top (same as height for list-based stack)."""
        return len(self.cp_stack) if hasattr(self, "cp_stack") else 0

    def write_stamp_value(self) -> int:
        """Get the current write stamp."""
        return self.write_stamp if hasattr(self, "write_stamp") else 0

    def choicepoints(self) -> list:
        """Get list of current choicepoints (for snapshot)."""
        return self.cp_stack if hasattr(self, "cp_stack") else []

    def frames(self) -> list:
        """Get list of current frames (for snapshot)."""
        return self.frame_stack if hasattr(self, "frame_stack") else []

    def reset(self):
        """Reset engine state for reuse."""
        self.store = Store()
        self.trail = Trail()
        # Set engine reference on trail so attribute hooks can be dispatched
        self.trail.engine = self
        self._renamer = VarRenamer(self.store)  # New renamer with new store
        self.goal_stack = GoalStack()
        self.frame_stack = []
        self.cp_stack = []
        self._query_vars = []
        self._qid_by_name = {}
        self._qname_by_id = {}
        self._initial_var_cutoff = 0
        self.solutions = []
        self._trace_log = []
        self._debug_frame_pops = 0
        self._debug_trail_writes = 0
        self._steps_taken = 0
        self._next_frame_id = 0
        self._cut_barrier = None
        self._last_exit_info = None
        self._frame_depth_override = None
        self._goal_call_depths = {}
        self._goal_call_flags = {}
        # Reset debug counters if in debug mode
        if self.debug:
            self._candidates_considered = 0
            # Also reset metrics when debug=True
            if self.metrics:
                self.metrics.reset()
        # Exception handling is done via try/except PrologThrow
        # Don't reset ports - they accumulate across runs

    def _trace_port(
        self, port: str, term: Optional[Term], depth_override: Optional[int] = None
    ) -> None:
        """Emit a trace event with optional frame depth override."""
        if not self.tracer or term is None:
            return

        previous = self._frame_depth_override
        try:
            if depth_override is not None:
                self._frame_depth_override = depth_override

            # Collect current variable bindings for the goal
            bindings = self._collect_goal_bindings(term)

            # Use emit_event_with_bindings if we have bindings, otherwise use regular emit_event
            if bindings:
                self.tracer.emit_event_with_bindings(port, term, bindings)
            else:
                self.tracer.emit_event(port, term)
        finally:
            if depth_override is not None:
                self._frame_depth_override = previous

    def set_trace_filters(self, filters: TraceFilters) -> None:
        """Set TraceFilters instance on the PortsTracer.

        Args:
            filters: TraceFilters instance for event filtering
        """
        if self.tracer is not None:
            self.tracer.set_filters(filters)

    def _normalize_pred_id(self, pred) -> str:
        """Normalize predicate identifier for consistent metrics/tracing.

        Args:
            pred: Either a string like "foo/2" or tuple like ("catch", 3)

        Returns:
            Normalized string identifier
        """
        if isinstance(pred, tuple) and len(pred) == 2:
            name, arity = pred
            return f"{name}/{arity}"
        return str(pred)

    def _collect_goal_bindings(self, term: Term) -> Dict[str, Any]:
        """Collect current variable bindings for all variables in the goal term.

        Args:
            term: The goal term to collect bindings for

        Returns:
            Dictionary mapping variable names to their current bindings
        """
        bindings = {}

        def collect_vars(t: Term) -> None:
            """Recursively collect variables from a term."""
            if isinstance(t, Var):
                # Get variable name hint for display
                var_name = t.hint if t.hint else f"_{t.id}"

                # Check if variable ID exists in store before dereferencing
                if t.id < len(self.store.cells):
                    # Get current binding from store
                    deref_result = self.store.deref(t.id)
                    if deref_result[0] == "BOUND":
                        # Variable is bound to a value
                        bound_term = deref_result[2]
                        bindings[var_name] = str(bound_term)
                    else:
                        # Variable is unbound - show its canonical form
                        bindings[var_name] = var_name
                else:
                    # Variable ID doesn't exist in store - likely test data
                    bindings[var_name] = var_name

            elif isinstance(t, Struct):
                # Recursively process structure arguments
                for arg in t.args:
                    collect_vars(arg)

            elif isinstance(t, PrologList):
                # Process list items and tail
                for item in t.items:
                    collect_vars(item)
                if t.tail is not None:
                    collect_vars(t.tail)

            # Atoms and Ints don't contain variables

        collect_vars(term)
        return bindings

    def _register_goal_depth(self, goal: Goal, depth: Optional[int] = None) -> None:
        """Track the logical call depth for a goal."""
        if goal is None:
            return
        if depth is None:
            depth = len(self.frame_stack)
        self._goal_call_depths[id(goal)] = depth

    def _push_goal(
        self, goal: Goal, depth: Optional[int] = None, call_emitted: bool = False
    ) -> None:
        """Push a goal and record its logical metadata."""
        self.goal_stack.push(goal)
        self._register_goal_depth(goal, depth)
        self._goal_call_flags[id(goal)] = call_emitted

    def _push_goals_with_metadata(
        self,
        goals: tuple,
        depths: tuple,
        call_flags: tuple,
    ) -> None:
        """Push multiple goals with depth and call metadata."""
        for goal, depth, call_emitted in zip(goals, depths, call_flags):
            self._push_goal(goal, depth, call_emitted)

    def _pop_goal_metadata(
        self, goal: Goal, default_depth: Optional[int] = None
    ) -> tuple[int, bool]:
        """Remove and return metadata for a goal being dispatched."""
        if default_depth is None:
            default_depth = len(self.frame_stack)
        depth = self._goal_call_depths.pop(id(goal), default_depth)
        call_emitted = self._goal_call_flags.pop(id(goal), False)
        return depth, call_emitted

    def _shrink_goal_stack_to(self, height: int) -> None:
        """Shrink goal stack while clearing metadata."""
        assert height >= 0, f"shrink_to({height}) - height must be non-negative"
        while self.goal_stack.height() > height:
            removed = self.goal_stack.pop()
            if removed is not None:
                rid = id(removed)
                self._goal_call_depths.pop(rid, None)
                self._goal_call_flags.pop(rid, None)

    def _register_builtins(self):
        """Register all builtin predicates.

        All builtins use uniform signature: fn(engine, args_tuple) -> bool
        This simplifies dispatch and makes nondeterministic builtins easier.
        """
        # Register type-checking and other organized builtins via registry system
        register_all(self._builtins)

        # Core control flow
        self._builtins[("true", 0)] = lambda eng, args: True
        self._builtins[("fail", 0)] = lambda eng, args: False
        self._builtins[("!", 0)] = lambda eng, args: eng._builtin_cut(args)
        self._builtins[("call", 1)] = lambda eng, args: eng._builtin_call(args)
        self._builtins[("=", 2)] = lambda eng, args: eng._builtin_unify(args)
        self._builtins[("\\=", 2)] = lambda eng, args: eng._builtin_not_unify(args)
        # All-solutions predicates - now registered via builtins.solutions module
        # Dynamic database predicates
        self._builtins[("dynamic", 1)] = lambda eng, args: eng._builtin_dynamic(args)
        self._builtins[("assertz", 1)] = lambda eng, args: eng._builtin_assert(
            args, append=True
        )
        self._builtins[("asserta", 1)] = lambda eng, args: eng._builtin_assert(
            args, append=False
        )
        self._builtins[("retract", 1)] = lambda eng, args: eng._builtin_retract(args)
        self._builtins[("retractall", 1)] = lambda eng, args: eng._builtin_retractall(
            args
        )
        self._builtins[("abolish", 1)] = lambda eng, args: eng._builtin_abolish(args)
        # Extended call predicates
        self._builtins[("call", 2)] = lambda eng, args: eng._builtin_call_n(args)
        self._builtins[("call", 3)] = lambda eng, args: eng._builtin_call_n(args)
        self._builtins[("call", 4)] = lambda eng, args: eng._builtin_call_n(args)
        self._builtins[("call", 5)] = lambda eng, args: eng._builtin_call_n(args)
        self._builtins[("call", 6)] = lambda eng, args: eng._builtin_call_n(args)
        self._builtins[("call", 7)] = lambda eng, args: eng._builtin_call_n(args)
        self._builtins[("call", 8)] = lambda eng, args: eng._builtin_call_n(args)
        # Negation as failure
        self._builtins[("\\+", 1)] = lambda eng, args: eng._builtin_not_provable(args)
        # Additional meta-predicates
        self._builtins[("copy_term", 2)] = lambda eng, args: eng._builtin_copy_term(
            args
        )
        self._builtins[("unify_with_occurs_check", 2)] = (
            lambda eng, args: eng._builtin_unify_with_occurs_check(args)
        )
        self._builtins[("arg", 3)] = lambda eng, args: eng._builtin_arg(args)
        self._builtins[("once", 1)] = lambda eng, args: eng._builtin_once(args)
        self._builtins[("throw", 1)] = lambda eng, args: eng._builtin_throw(args)
        self._builtins[("catch", 3)] = lambda eng, args: eng._builtin_catch(args)
        # Attributed variable builtins
        self._builtins[("put_attr", 3)] = lambda eng, args: eng._builtin_put_attr(args)
        self._builtins[("get_attr", 3)] = lambda eng, args: eng._builtin_get_attr(args)
        self._builtins[("del_attr", 2)] = lambda eng, args: eng._builtin_del_attr(args)

        # Dict builtins
        self._builtins[("dict_create", 3)] = lambda eng, args: eng._builtin_dict_create(
            args
        )
        self._builtins[("get_dict", 3)] = lambda eng, args: eng._builtin_get_dict(args)
        self._builtins[("put_dict", 3)] = lambda eng, args: eng._builtin_put_dict(args)

        # JSON support predicates
        self._builtins[("json_read", 3)] = lambda eng, args: eng._builtin_json_read(
            args
        )
        self._builtins[("json_write", 3)] = lambda eng, args: eng._builtin_json_write(
            args
        )
        self._builtins[("json_read_dict", 3)] = (
            lambda eng, args: eng._builtin_json_read_dict(args)
        )
        self._builtins[("json_write_dict", 3)] = (
            lambda eng, args: eng._builtin_json_write_dict(args)
        )
        self._builtins[("atom_json_term", 3)] = (
            lambda eng, args: eng._builtin_atom_json_term(args)
        )

        # CLP(FD) builtins

        self._builtins[("in", 2)] = lambda eng, args: _builtin_in(eng, *args)
        self._builtins[("#=", 2)] = lambda eng, args: _builtin_fd_eq(eng, *args)
        # FD inequality

        self._builtins[("#\\=", 2)] = lambda eng, args: _builtin_fd_neq(eng, *args)
        self._builtins[("#<", 2)] = lambda eng, args: _builtin_fd_lt(eng, *args)
        self._builtins[("#=<", 2)] = lambda eng, args: _builtin_fd_le(eng, *args)
        self._builtins[("#>", 2)] = lambda eng, args: _builtin_fd_gt(eng, *args)
        self._builtins[("#>=", 2)] = lambda eng, args: _builtin_fd_ge(eng, *args)

        # Reification builtins

        self._builtins[("#<=>", 2)] = lambda eng, args: _builtin_fd_reif_equiv(
            eng, *args
        )
        self._builtins[("#==>", 2)] = lambda eng, args: _builtin_fd_reif_implies(
            eng, *args
        )
        self._builtins[("#<==", 2)] = lambda eng, args: _builtin_fd_reif_implied(
            eng, *args
        )
        self._builtins[("fd_var", 1)] = lambda eng, args: _builtin_fd_var(eng, *args)
        self._builtins[("fd_inf", 2)] = lambda eng, args: _builtin_fd_inf(eng, *args)
        self._builtins[("fd_sup", 2)] = lambda eng, args: _builtin_fd_sup(eng, *args)
        self._builtins[("fd_dom", 2)] = lambda eng, args: _builtin_fd_dom(eng, *args)
        self._builtins[("label", 1)] = lambda eng, args: _builtin_label(eng, *args)
        self._builtins[("labeling", 2)] = lambda eng, args: _builtin_labeling(
            eng, *args
        )
        self._builtins[("all_different", 1)] = lambda eng, args: _builtin_all_different(
            eng, *args
        )
        self._builtins[("element", 3)] = lambda eng, args: _builtin_element_3(
            eng, *args
        )
        self._builtins[("global_cardinality", 2)] = (
            lambda eng, args: _builtin_global_cardinality(eng, *args)
        )
        self._builtins[("nvalue", 2)] = lambda eng, args: _builtin_nvalue(eng, *args)
        self._builtins[("lex_chain", 1)] = lambda eng, args: _builtin_lex_chain(
            eng, *args
        )
        self._builtins[("table", 2)] = lambda eng, args: _builtin_table(eng, *args)
        self._builtins[("#\\/", 2)] = lambda eng, args: _builtin_fd_disj(eng, *args)

    def solve(
        self, goal: Term, max_solutions: Optional[int] = None
    ) -> List[Dict[str, Any]]:
        """Solve a single goal (convenience method for tests).

        Args:
            goal: Single goal term to solve.
            max_solutions: Maximum number of solutions to find.

        Returns:
            List of solution dictionaries mapping variable names to values.
        """
        # Reset debug counter at start of each solve
        if self.debug:
            self._candidates_considered = 0

        return self.run([goal], max_solutions)

    def run(
        self, goals: List[Term], max_solutions: Optional[int] = None
    ) -> List[Dict[str, Any]]:
        """Run a query with given goals using single iterative loop.

        NOTE: This method guarantees a clean state only when _query_vars is empty.
        The query reuse path (when _query_vars is already set) is intended for
        testing only and may leak state between runs. For production use, always
        ensure the engine is reset or create a new engine instance.

        Args:
            goals: List of goal terms to prove.
            max_solutions: Maximum number of solutions to find.

        Returns:
            List of solution dictionaries mapping variable names to values.
        """
        # If query_vars is already set up (by test), don't reset
        if not self._query_vars:
            # Reset state for new query
            self.reset()
            if max_solutions is not None:
                self.max_solutions = max_solutions

            # Allocate variables for query goals and track them
            renamed_goals = []
            for goal in goals:
                renamed_goal = self._allocate_query_vars(goal)
                renamed_goals.append(renamed_goal)
        else:
            # Query vars pre-allocated, just clear solutions
            self.solutions = []
            self.max_solutions = max_solutions or self.max_solutions
            renamed_goals = goals

        # Set cutoff after allocating all query variables
        self._initial_var_cutoff = len(self.store.cells)

        # Check if max_solutions is 0 - no point in running
        if self.max_solutions == 0:
            # Clean up state before returning
            self.trail.unwind_to(0, self.store)
            self._shrink_goal_stack_to(0)
            self.frame_stack.clear()
            self.cp_stack.clear()
            return self.solutions

        # Reset step counter for new query
        self._steps_taken = 0

        # Reset tracer for new query run
        if self.tracer:
            self.tracer._reset_for_new_run()

        # Push initial goals (in reverse for left-to-right execution)
        for goal in reversed(renamed_goals):
            g = Goal.from_term(goal)
            self._push_goal(g, depth=0)

        # Main single iterative loop
        while True:
            # Check step budget
            if self.max_steps is not None:
                self._steps_taken += 1
                if self._steps_taken > self.max_steps:
                    # Step budget exceeded - stop execution
                    break

            try:
                # Pop next goal
                goal = self.goal_stack.pop()

                if goal is None:
                    # No more goals - found a solution
                    self._record_solution()

                    # Check if we've found enough solutions
                    if self.max_solutions and len(self.solutions) >= self.max_solutions:
                        break

                    # Backtrack to find more solutions
                    if not self._backtrack():
                        break
                    continue

                call_depth, call_emitted = self._pop_goal_metadata(goal)

                # Trace if enabled
                if self.trace:
                    self._trace_log.append(f"Goal: {goal.term}")

                # Dispatch based on goal type
                if goal.type == GoalType.PREDICATE:
                    # Check if it's actually a builtin (PREDICATE goals always have terms)
                    if goal.term and self._is_builtin(goal.term):
                        if not self._dispatch_builtin(goal, call_depth, call_emitted):
                            if not self._backtrack():
                                break
                    else:
                        if not self._dispatch_predicate(goal, call_depth, call_emitted):
                            if not self._backtrack():
                                break
                elif goal.type == GoalType.BUILTIN:
                    # Explicit builtin goal (used by some internal schedulers)
                    if not self._dispatch_builtin(goal, call_depth, call_emitted):
                        if not self._backtrack():
                            break
                elif goal.type == GoalType.CONJUNCTION:
                    self._dispatch_conjunction(goal)
                elif goal.type == GoalType.DISJUNCTION:
                    self._dispatch_disjunction(goal)
                elif goal.type == GoalType.IF_THEN_ELSE:
                    if not self._dispatch_if_then_else(goal):
                        if not self._backtrack():
                            break
                elif goal.type == GoalType.CUT:
                    self._dispatch_cut()
                elif goal.type == GoalType.POP_FRAME:
                    self._dispatch_pop_frame(goal)
                elif goal.type == GoalType.CONTROL:
                    self._dispatch_control(goal)
                else:
                    # Unknown goal type - should not happen
                    if not self._backtrack():
                        break

            except PrologThrow as exc:
                # Handle thrown exception
                handled = self._handle_throw(exc.ball)
                if handled:
                    # Exception was caught
                    if self.metrics:
                        self.metrics.record_exception_caught()
                    continue

                # No handler found - clean up and re-raise
                self.trail.unwind_to(0, self.store)
                self._shrink_goal_stack_to(0)
                self.frame_stack.clear()
                self.cp_stack.clear()
                raise

        # Clean up for next query
        # Unbind query variables to restore clean state
        for var_id, _ in self._query_vars:
            if var_id < len(self.store.cells):
                cell = self.store.cells[var_id]
                # Only unbind if it's still bound
                if cell.tag == "bound":
                    self.store.cells[var_id] = Cell(
                        tag="unbound", ref=var_id, term=None, rank=cell.rank
                    )

        # Clear query vars tracking so the next run() will reset properly
        self._query_vars = []
        self._qid_by_name = {}
        self._qname_by_id = {}
        self._initial_var_cutoff = 0

        # Persist CLP(FD) domains for solution variables across cleanup so
        # tests can inspect final domains after query execution.
        # Collect domains for all variables present in solutions.
        persist_domains: Dict[int, Any] = {}
        if self.solutions:
            for sol in self.solutions:
                for v in sol.values():
                    if isinstance(v, Var):
                        # Check if variable ID is valid before dereferencing
                        if v.id < len(self.store.cells):
                            # Deref to current root
                            r = self.store.deref(v.id)
                            if r[0] == "UNBOUND":
                                root = r[1]
                                # Capture clpfd domain if present
                                if root in getattr(self.store, "attrs", {}):
                                    mod_attrs = self.store.attrs[root]
                                    if "clpfd" in mod_attrs:
                                        fd = mod_attrs["clpfd"]
                                        # Only process if fd is a dict (proper CLP(FD) attribute)
                                        if isinstance(fd, dict):
                                            dom = fd.get("domain")
                                            if dom is not None:
                                                persist_domains[root] = dom

        # Clean up all state before returning
        self.trail.unwind_to(0, self.store)
        self._shrink_goal_stack_to(0)
        self.frame_stack.clear()
        self.cp_stack.clear()

        # Re-apply persisted domains without trailing (post-cleanup state)
        if persist_domains:
            if not hasattr(self.store, "attrs"):
                self.store.attrs = {}
            for vid, dom in persist_domains.items():
                if vid not in self.store.attrs:
                    self.store.attrs[vid] = {}
                # Recreate clpfd attribute with only domain preserved
                self.store.attrs[vid]["clpfd"] = {
                    "domain": dom,
                }

        return self.solutions

    def _unify(self, a: Term, b: Term) -> bool:
        """Helper for unification with current engine settings.

        Args:
            a: First term to unify
            b: Second term to unify

        Returns:
            True if unification succeeds, False otherwise
        """
        if self.metrics:
            self.metrics.record_unification_attempt()

        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
        result = unify(a, b, self.store, trail_adapter, occurs_check=self.occurs_check)

        if self.metrics and result:
            self.metrics.record_unification_success()

        return result

    def unify(self, a: Term, b: Term) -> bool:
        """Public unification method for testing CLP(FD) integration.

        This is a convenience method for tests that need to perform
        unification with FD variables and check domain consistency.

        Args:
            a: First term to unify
            b: Second term to unify

        Returns:
            True if unification succeeds, False otherwise
        """
        return self._unify(a, b)

    def _match_only(self, term1: Term, term2: Term) -> bool:
        """Check if two terms unify WITHOUT modifying the store.

        This is used for trial matching in catch/throw to avoid polluting
        the store with bindings that might need to be undone.

        Args:
            term1: First term
            term2: Second term

        Returns:
            True if the terms would unify, False otherwise
        """
        # Save current trail position and stamp
        saved_trail_pos = self.trail.position()
        saved_stamp = self.trail.current_stamp

        # Create a temporary stamp for the trial
        self.trail.next_stamp()

        # Do trial unification
        result = self._unify(term1, term2)

        # Restore everything - undo any changes made
        self.trail.unwind_to(saved_trail_pos, self.store)
        self.trail.set_current_stamp(saved_stamp)

        return result

    def _handle_throw(self, ball: Term) -> bool:
        """Handle a thrown exception by searching for a matching catch.

        Args:
            ball: The thrown exception term

        Returns:
            True if exception was caught and handled, False otherwise
        """

        # Search from inner to outer
        i = len(self.cp_stack) - 1
        while i >= 0:
            cp = self.cp_stack[i]
            i -= 1

            if cp.kind != ChoicepointKind.CATCH:
                continue
            if cp.payload.get("phase") != "GOAL":
                continue

            # Scope guard: only catch if cp window encloses current point
            cp_height = cp.payload.get("cp_height", 0)
            if cp_height > len(self.cp_stack) - 1:
                continue

            # --- Two-phase unification: check match without side effects ---
            if not self._match_only(ball, cp.payload.get("catcher")):
                continue

            # --- We have a matching catcher: restore to catch baselines ---
            if self.trace:
                self._trace_log.append(
                    f"[CATCH] Restoring: cp_height={cp_height}, current cp_stack len={len(self.cp_stack)}"
                )

            self.trail.unwind_to(cp.trail_top, self.store)
            self._shrink_goal_stack_to(cp.goal_stack_height)
            while len(self.frame_stack) > cp.frame_stack_height:
                self.frame_stack.pop()
            while len(self.cp_stack) > cp_height:
                removed = self.cp_stack.pop()
                if self.trace:
                    self._trace_log.append(f"[CATCH] Removing CP: {removed.kind.name}")

            # Switch to the catch window stamp
            self.trail.set_current_stamp(cp.stamp)

            # Restore streaming cursors (conservative approach)
            cursor_snapshots = cp.payload.get("cursor_snapshots", {})
            for other_cp in self.cp_stack[:cp_height]:
                if other_cp.kind == ChoicepointKind.PREDICATE:
                    cursor = other_cp.payload.get("cursor")
                    if hasattr(cursor, "clone") and callable(cursor.clone):
                        snapshot = cursor_snapshots.get(id(cursor))
                        if snapshot:
                            # Replace with snapshot directly (already cloned at catch time)
                            other_cp.payload["cursor"] = snapshot

            # Re-unify to make catcher bindings visible to Recovery
            ok2 = self._unify(ball, cp.payload.get("catcher"))
            assert ok2, "ball unified in trial but failed after restore (bug)"

            # Validate frame stack consistency for recursive predicates
            expected_depth = cp.payload.get("recursion_depth", 0)
            if expected_depth > 0 and len(self.frame_stack) > 0:
                current_pred = self.frame_stack[-1].pred
                actual_depth = sum(
                    1 for f in self.frame_stack if f.pred == current_pred
                )

                if actual_depth != expected_depth:
                    # Log the inconsistency but don't fail - this is defensive
                    if self.trace:
                        self._trace_log.append(
                            f"[CATCH] Frame depth mismatch: expected {expected_depth}, actual {actual_depth}"
                        )

            # Debug assertions
            assert self.goal_stack.height() == cp.goal_stack_height
            assert len(self.cp_stack) == cp_height
            assert len(self.frame_stack) >= cp.frame_stack_height

            # Emit catch_switch event when we catch an exception
            if self.tracer:
                self.tracer.emit_internal_event(
                    "catch_switch",
                    {
                        "exception": str(ball),
                        "handler": str(cp.payload.get("recovery")),
                    },
                )

            # Push only the recovery goal - DO NOT re-install the CATCH choicepoint
            # If recovery fails, we backtrack transparently to outer choicepoints
            self._push_goal(Goal.from_term(cp.payload.get("recovery")))
            return True

        return False  # No catcher matched

    def _allocate_query_vars(self, term: Term) -> Term:
        """Allocate fresh variables for query terms and track them.

        Args:
            term: A query goal term.

        Returns:
            Term with variables replaced by fresh allocated vars.
        """
        # Iterative implementation to handle deep structures without stack overflow
        # We'll process the term tree with explicit stack
        processed = {}  # Maps id(original_term) -> new_term

        # First pass: identify all terms in post-order (children before parents)
        all_terms = []
        visited = set()
        temp_stack = [(term, False)]  # (term, is_processed)

        while temp_stack:
            current, is_processed = temp_stack.pop()

            if id(current) in visited:
                continue

            if is_processed:
                # Second visit - add to result
                visited.add(id(current))
                all_terms.append(current)
            else:
                # First visit - push back with processed flag, then children
                temp_stack.append((current, True))

                if isinstance(current, Struct):
                    # Add args in reverse order for depth-first
                    for arg in reversed(current.args):
                        if id(arg) not in visited:
                            temp_stack.append((arg, False))
                elif isinstance(current, PrologList):
                    if id(current.tail) not in visited:
                        temp_stack.append((current.tail, False))
                    for item in reversed(current.items):
                        if id(item) not in visited:
                            temp_stack.append((item, False))
                elif isinstance(current, PrologDict):
                    # Process all values in the dict pairs
                    for key, value in reversed(current.pairs):
                        if id(value) not in visited:
                            temp_stack.append((value, False))
                        if id(key) not in visited:
                            temp_stack.append((key, False))

        # Second pass: process terms bottom-up (children before parents)
        for current in all_terms:
            if isinstance(current, Var):
                # Check if we've seen this variable name
                var_name = current.hint or f"_{current.id}"
                # Fast lookup for existing allocation
                if var_name in self._qid_by_name:
                    varid = self._qid_by_name[var_name]
                    result = Var(varid, var_name)
                else:
                    # Allocate new variable
                    varid = self.store.new_var(hint=var_name)
                    self._query_vars.append((varid, var_name))
                    self._qid_by_name[var_name] = varid
                    self._qname_by_id[varid] = var_name
                    result = Var(varid, var_name)
                processed[id(current)] = result

            elif isinstance(current, (Atom, Int, Float)):
                processed[id(current)] = current

            elif isinstance(current, Struct):
                # Process args - handle case where same term appears multiple times
                new_args = []
                for arg in current.args:
                    # Get processed version if available, otherwise use original
                    new_args.append(processed.get(id(arg), arg))
                processed[id(current)] = Struct(current.functor, tuple(new_args))

            elif isinstance(current, PrologList):
                # Process items and tail - handle shared references
                new_items = []
                for item in current.items:
                    new_items.append(processed.get(id(item), item))
                new_tail = processed.get(id(current.tail), current.tail)
                processed[id(current)] = PrologList(tuple(new_items), tail=new_tail)

            elif isinstance(current, PrologDict):
                # Process keys and values in dict pairs
                new_pairs = []
                for key, value in current.pairs:
                    new_key = processed.get(id(key), key)
                    new_value = processed.get(id(value), value)
                    new_pairs.append((new_key, new_value))
                processed[id(current)] = PrologDict(tuple(new_pairs), tag=current.tag)
            else:
                raise TypeError(f"Unknown term type: {type(current)}")

        return processed[id(term)]

    def _dispatch_predicate(
        self, goal: Goal, call_depth: int, call_emitted: bool
    ) -> bool:
        """Dispatch a predicate goal.

        Args:
            goal: The predicate goal to dispatch.

        Returns:
            True if successful, False if failed.
        """
        # Get matching clauses
        if isinstance(goal.term, Atom):
            functor = goal.term.name
            arity = 0
        elif isinstance(goal.term, Struct):
            functor = goal.term.functor
            arity = len(goal.term.args)
        else:
            # Can't match a variable or other term
            return False

        # Emit CALL port before processing (only once per logical call)
        if not call_emitted:
            self._port("CALL", f"{functor}/{arity}")
        # Also emit to tracer if enabled (always emit for tracer consumers)
        self._trace_port("call", goal.term, depth_override=call_depth)

        # Record metrics for CALL
        pred_id = f"{functor}/{arity}"
        if self.metrics:
            self.metrics.record_call(pred_id)

        # Get matching clauses - use indexing if available
        if self.use_indexing and hasattr(self.program, "select"):
            # Use indexed selection
            pred_key = (functor, arity)

            # Determine if we should use streaming
            # Streaming is disabled when debug or metrics are enabled to preserve candidate counting
            should_stream = (
                self.use_streaming
                and not self.debug
                and not self.metrics
                and not self.tracer  # Also disable with tracer to preserve debug behavior
            )

            if should_stream:
                # Use streaming cursor for memory efficiency
                clause_iterator = self.program.select(pred_key, goal.term, self.store)
                cursor = StreamingClauseCursor(
                    functor=functor, arity=arity, it=clause_iterator
                )
            else:
                # Materialize list for debug/metrics compatibility
                matches = list(self.program.select(pred_key, goal.term, self.store))
                cursor = ClauseCursor(matches=matches, functor=functor, arity=arity)

                # Track candidates in debug mode (only when materialized)
                if self.debug:
                    self._candidates_considered += len(matches)
                    if self.metrics:
                        # Count how many are actually yielded (have potential to match)
                        yielded = len([m for m in matches if m is not None])
                        self.metrics.record_candidates(len(matches), yielded)

                    # Log detailed info if trace is enabled too
                    if self.trace:
                        # Optimize total clause count for IndexedProgram
                        if isinstance(self.program, IndexedProgram):
                            pred_idx = self.program._index.preds.get((functor, arity))
                            total_clauses = len(pred_idx.order) if pred_idx else 0
                        else:
                            total_clauses = len(
                                self.program.clauses_for(functor, arity)
                            )

                        if total_clauses > 0:
                            self._trace_log.append(
                                f"pred {functor}/{arity}: considered {len(matches)} of {total_clauses} clauses"
                            )
        else:
            # Fall back to standard clause selection
            matches = self.program.clauses_for(functor, arity)
            cursor = ClauseCursor(matches=matches, functor=functor, arity=arity)

            # Track all candidates in debug mode (no filtering)
            if self.debug:
                self._candidates_considered += len(matches)
                if self.metrics:
                    # All clauses are yielded (no filtering without indexing)
                    self.metrics.record_candidates(len(matches), len(matches))

        if not cursor.has_more():
            # No matching clauses
            if self.mode == "iso":
                # In ISO mode, throw an error for undefined predicates
                raise UndefinedPredicateError(predicate=functor, arity=arity)
            else:
                # In dev mode, just fail - emit FAIL port
                self._port("FAIL", f"{functor}/{arity}")
                if self.metrics:
                    self.metrics.record_fail(pred_id)
                return False

        # Take first clause
        clause_idx = cursor.take()
        if clause_idx is None:
            return False
        clause = self.program.clauses[clause_idx]

        # Capture cut barrier BEFORE creating the alternatives choicepoint
        # This ensures cut will prune alternative clauses (ISO semantics)
        cut_barrier = len(self.cp_stack)

        # If there are more clauses, create a choicepoint
        if cursor.has_more():
            # Normal choicepoint with more alternatives
            self.trail.next_stamp()

            # Save the continuation (goals below the call) as an immutable snapshot
            continuation = self.goal_stack.snapshot()
            continuation_depths = tuple(
                self._goal_call_depths.get(id(g), len(self.frame_stack))
                for g in continuation
            )
            continuation_calls = tuple(
                self._goal_call_flags.get(id(g), False) for g in continuation
            )

            # Save store size for allocation cleanup
            store_top = self.store.size()

            # Debug: log what we're saving
            if self.trace:
                self._trace_log.append(
                    f"Creating PREDICATE CP for {functor}/{arity}, saving {len(continuation)} continuation goals"
                )
                for i, g in enumerate(continuation):
                    self._trace_log.append(f"  Continuation[{i}]: {g}")

            cp = Choicepoint(
                kind=ChoicepointKind.PREDICATE,
                trail_top=self.trail.position(),
                goal_stack_height=len(
                    continuation
                ),  # Height = number of continuation goals
                frame_stack_height=0,  # PREDICATE CPs don't manage frames (frame lifecycle is independent)
                payload={
                    "goal": goal,
                    "cursor": cursor,
                    "pred_ref": f"{functor}/{arity}",
                    "continuation": continuation,  # Frozen snapshot of continuation goals
                    "continuation_depths": continuation_depths,
                    "continuation_calls": continuation_calls,
                    "store_top": store_top,  # Store size for allocation cleanup
                    "call_depth": call_depth,
                    "has_succeeded": False,
                },
            )
            self.cp_stack.append(cp)

            # Emit internal event for CP push
            if self.tracer:
                # Compute alternatives count based on cursor type
                alternatives = 0
                if cursor:
                    if hasattr(cursor, "matches") and hasattr(cursor, "pos"):
                        alternatives = len(cursor.matches) - cursor.pos
                    elif hasattr(cursor, "has_more"):
                        alternatives = 1 if cursor.has_more() else 0
                self.tracer.emit_internal_event(
                    "cp_push",
                    {
                        "pred_id": f"{functor}/{arity}",
                        "alternatives": alternatives,
                        "trail_top": cp.trail_top,
                    },
                )

        # Rename clause with fresh variables
        renamed_clause = self._renamer.rename_clause(clause)

        # Try to unify with clause head
        if self._unify(renamed_clause.head, goal.term):
            # Unification succeeded - now push frame for body execution
            # Use the cut_barrier saved before creating choicepoint
            frame_id = self._next_frame_id
            self._next_frame_id += 1
            frame = Frame(
                frame_id=frame_id,
                cut_barrier=cut_barrier,
                goal_height=self.goal_stack.height(),
                pred=f"{functor}/{arity}",
                goal_term=goal.term,  # Store for EXIT port emission
                call_depth=call_depth,
            )
            self.frame_stack.append(frame)

            # Emit internal event for frame push
            if self.tracer:
                self.tracer.emit_internal_event(
                    "frame_push",
                    {"pred_id": f"{functor}/{arity}", "frame_id": frame_id},
                )

            # Don't call next_stamp() here - only when creating CPs

            # Push POP_FRAME sentinel first, then body goals
            self._push_goal(
                Goal(
                    GoalType.POP_FRAME,
                    None,  # Internal goals don't need terms
                    payload={"op": "POP_FRAME", "frame_id": frame_id},
                )
            )

            # Push body goals in reverse order
            for body_term in reversed(renamed_clause.body):
                body_goal = Goal.from_term(body_term)
                self._push_goal(body_goal)
            return True
        else:
            # Unification failed - no frame was created
            return False

    def _dispatch_builtin(
        self, goal: Goal, call_depth: int, call_emitted: bool
    ) -> bool:
        """Dispatch a builtin goal.

        Args:
            goal: The builtin goal to dispatch.

        Returns:
            True if successful, False if failed.
        """
        if isinstance(goal.term, Atom):
            key = (goal.term.name, 0)
            args = ()
            pred_id = f"{goal.term.name}/0"
        elif isinstance(goal.term, Struct):
            key = (goal.term.functor, len(goal.term.args))
            args = goal.term.args
            pred_id = f"{goal.term.functor}/{len(goal.term.args)}"
        else:
            return False

        builtin_fn = self._builtins.get(key)
        if builtin_fn is None:
            # Not a recognized builtin
            return False

        # Emit CALL port before executing builtin (only once per logical call)
        if not call_emitted:
            self._port("CALL", pred_id)

        # Also emit to tracer if enabled
        term = goal.term if goal.term else None
        self._trace_port("call", term, depth_override=call_depth)

        # Execute the builtin with uniform signature
        try:
            result = builtin_fn(self, args)

            # Emit EXIT or FAIL port based on result
            if result:
                self._port("EXIT", pred_id)
                self._trace_port("exit", term, depth_override=call_depth)
                return True

            self._emit_fail_port(pred_id, term, depth_override=call_depth)
            return False
        except (ValueError, TypeError):
            # Expected failures from arithmetic or type errors
            self._emit_fail_port(pred_id, term, depth_override=call_depth)
            return False

    def _dispatch_conjunction(self, goal: Goal):
        """Dispatch a conjunction goal.

        Args:
            goal: The conjunction goal to dispatch.
        """
        # (A, B) - push B then A for left-to-right execution
        conj = goal.term
        if isinstance(conj, Struct) and conj.functor == "," and len(conj.args) == 2:
            left, right = conj.args
            # Push in reverse order
            right_goal = Goal.from_term(right)
            left_goal = Goal.from_term(left)
            self._push_goal(right_goal)
            self._push_goal(left_goal)

    def _dispatch_disjunction(self, goal: Goal):
        """Dispatch a disjunction goal.

        Args:
            goal: The disjunction goal to dispatch.
        """
        # (A ; B) - try A first, create choicepoint for B
        disj = goal.term
        if isinstance(disj, Struct) and disj.functor == ";" and len(disj.args) == 2:
            left, right = disj.args

            # Save the continuation (goals after disjunction) as an immutable snapshot
            continuation = self.goal_stack.snapshot()
            continuation_depths = tuple(
                self._goal_call_depths.get(id(g), len(self.frame_stack))
                for g in continuation
            )
            continuation_calls = tuple(
                self._goal_call_flags.get(id(g), False) for g in continuation
            )

            # Create choicepoint for right alternative
            stamp = self.trail.next_stamp()
            cp = Choicepoint(
                kind=ChoicepointKind.DISJUNCTION,
                trail_top=self.trail.position(),
                goal_stack_height=len(
                    continuation
                ),  # Height = number of continuation goals
                frame_stack_height=len(self.frame_stack),
                payload={
                    "alternative": Goal.from_term(right),
                    "alternative_depth": len(self.frame_stack),
                    "continuation": continuation,  # Frozen snapshot of continuation goals
                    "continuation_depths": continuation_depths,
                    "continuation_calls": continuation_calls,
                },
                stamp=stamp,
            )
            self.cp_stack.append(cp)

            # Emit internal event for CP push
            if self.tracer:
                self.tracer.emit_internal_event(
                    "cp_push", {"pred_id": "disjunction", "trail_top": cp.trail_top}
                )

            # Try left first (continuation remains on stack)
            left_goal = Goal.from_term(left)
            self._push_goal(left_goal)

    def _dispatch_if_then_else(self, goal: Goal) -> bool:
        """Dispatch an if-then-else goal with proper commit semantics.

        Args:
            goal: The if-then-else goal.

        Returns:
            True if dispatched successfully, False otherwise.
        """
        # (A -> B) ; C - if A succeeds commit to B, else try C
        ite = goal.term
        if not (isinstance(ite, Struct) and ite.functor == ";" and len(ite.args) == 2):
            return False

        left, else_term = ite.args
        if not (
            isinstance(left, Struct) and left.functor == "->" and len(left.args) == 2
        ):
            return False

        cond, then_term = left.args

        # Capture current choicepoint stack height
        tmp_barrier = len(self.cp_stack)

        # Create choicepoint that runs Else if Cond fails exhaustively
        self.trail.next_stamp()
        cp = Choicepoint(
            kind=ChoicepointKind.IF_THEN_ELSE,
            trail_top=self.trail.position(),
            goal_stack_height=self.goal_stack.height(),
            frame_stack_height=len(self.frame_stack),
            payload={
                "else_goal": Goal.from_term(else_term),
                "else_goal_depth": len(self.frame_stack),
                "tmp_barrier": tmp_barrier,
            },
        )
        self.cp_stack.append(cp)

        # Emit internal event for CP push
        if self.tracer:
            self.tracer.emit_internal_event(
                "cp_push", {"pred_id": "if_then_else", "trail_top": cp.trail_top}
            )

        # Push control goal that will commit and run Then on first success
        then_goal = Goal.from_term(then_term)
        self._push_goal(
            Goal(
                GoalType.CONTROL,
                None,  # Internal control goal
                payload={
                    "op": "ITE_THEN",
                    "tmp_barrier": tmp_barrier,
                    "then_goal": then_goal,
                    "then_goal_depth": len(self.frame_stack),
                },
            )
        )

        # Push condition to evaluate
        self._push_goal(Goal.from_term(cond))

        return True

    def _dispatch_cut(self):
        """Execute cut (!) - remove choicepoints up to cut barrier."""
        if self.frame_stack:
            # Normal cut within a predicate
            current_frame = self.frame_stack[-1]
            cut_barrier = current_frame.cut_barrier

            # Find the highest CATCH CP below our target to act as a barrier
            catch_barrier = None
            for i in range(len(self.cp_stack) - 1, cut_barrier - 1, -1):
                if self.cp_stack[i].kind == ChoicepointKind.CATCH:
                    catch_barrier = i + 1  # Don't prune the CATCH itself
                    break

            # Apply the more restrictive barrier
            if catch_barrier is not None:
                cut_barrier = max(cut_barrier, catch_barrier)

            # Count alternatives being pruned
            alternatives_pruned = 0

            # Remove all choicepoints above the barrier
            while len(self.cp_stack) > cut_barrier:
                removed_cp = self.cp_stack.pop()

                # Count actual alternatives pruned
                if removed_cp.kind == ChoicepointKind.PREDICATE:
                    # For predicate CPs, count remaining clauses
                    cursor = removed_cp.payload.get("cursor")
                    if cursor and hasattr(cursor, "matches") and hasattr(cursor, "pos"):
                        # Count remaining alternatives in cursor (materialized)
                        alternatives_pruned += len(cursor.matches) - cursor.pos
                    elif cursor and hasattr(cursor, "has_more"):
                        # For streaming cursors, just count as 1 (can't count ahead)
                        alternatives_pruned += 1 if cursor.has_more() else 0
                    else:
                        alternatives_pruned += 1
                else:
                    # For other CP types, count as 1 alternative
                    alternatives_pruned += 1

                # Emit internal event for CP pop due to cut
                if self.tracer:
                    pred_id = (
                        removed_cp.payload.get("pred_ref", removed_cp.kind.name.lower())
                        if removed_cp.kind == ChoicepointKind.PREDICATE
                        else removed_cp.kind.name.lower()
                    )
                    self.tracer.emit_internal_event("cp_pop", {"pred_id": pred_id})

            # Emit cut_commit event after all CP pops (even if no alternatives pruned)
            if self.tracer:
                self.tracer.emit_internal_event(
                    "cut_commit",
                    {
                        "alternatives_pruned": alternatives_pruned,
                        "barrier": cut_barrier,
                    },
                )

            # Record metrics
            if self.metrics and alternatives_pruned > 0:
                self.metrics.record_cut()
                self.metrics.record_alternatives_pruned(alternatives_pruned)
        else:
            # Top-level cut: prune everything (commit to current solution path)
            # This commits to the current solution and prevents backtracking.
            # Some Prolog systems treat top-level cut as a no-op, but we choose
            # to make it commit to prevent finding additional solutions.
            # Test case: ?- (a ; b), !. should return only first success
            self.cp_stack.clear()

        # Cut always succeeds
        return True

    def _dispatch_pop_frame(self, goal: Goal):
        """Pop frame sentinel - executed when predicate completes.

        Only pops the frame if no choicepoints still reference it.
        This ensures frames stay alive for backtracking.

        Args:
            goal: The POP_FRAME goal with frame_id payload.
        """
        frame_id = goal.payload.get("frame_id")
        # Sanity check: should be popping the frame with matching ID
        if self.frame_stack and self.frame_stack[-1].frame_id == frame_id:
            # Check if any choicepoint still needs this frame
            # A CP needs this frame if its frame_stack_height is >= our current frame height
            current_frame_height = len(self.frame_stack)
            frame_still_needed = False

            for cp in self.cp_stack:
                # Skip PREDICATE CPs as they use frame_stack_height=0 as sentinel
                if (
                    cp.kind != ChoicepointKind.PREDICATE
                    and cp.frame_stack_height >= current_frame_height
                ):
                    frame_still_needed = True
                    break

            if not frame_still_needed:
                # Safe to pop the frame - no CPs reference it
                frame = self.frame_stack.pop()
                self._debug_frame_pops += 1

                self._update_catch_frames_after_pop()

                # Emit internal event for frame pop
                if self.tracer:
                    self.tracer.emit_internal_event(
                        "frame_pop",
                        {
                            "pred_id": self._normalize_pred_id(frame.pred),
                            "frame_id": frame.frame_id,
                        },
                    )

                # Emit EXIT port when frame is popped
                pred_ref = frame.pred or "unknown"
                self._port("EXIT", pred_ref)
                if frame.goal_term:
                    # Use the stored goal term for proper EXIT port emission
                    self._trace_port(
                        "exit", frame.goal_term, depth_override=frame.call_depth
                    )
                if self.metrics and frame.pred:
                    self.metrics.record_exit(frame.pred)

                # Mark predicate choicepoint as having produced a solution
                for cp_entry in reversed(self.cp_stack):
                    if (
                        cp_entry.kind == ChoicepointKind.PREDICATE
                        and cp_entry.payload.get("pred_ref") == pred_ref
                    ):
                        cp_entry.payload["has_succeeded"] = True
                        break

                # Remember last exit info for potential final FAIL
                self._last_exit_info = (pred_ref, frame.goal_term, frame.call_depth)
            # else: Leave frame in place for backtracking
        # If frame ID doesn't match, it's likely a stale POP_FRAME from backtracking
        # Just ignore it - the frame was already popped or never created

    def _dispatch_control(self, goal: Goal):
        """Handle internal control goals.

        Args:
            goal: The control goal.
        """
        op = goal.payload.get("op") if goal.payload else None

        if op == "CALL_META":
            # Push the inner goal transparently for meta-call
            # Note: meta ports only; no frame or choicepoint created here
            # to maintain call/1 transparency
            inner_goal = goal.payload["goal"]
            depth = goal.payload.get("goal_depth", len(self.frame_stack))
            self._push_goal(inner_goal, depth=depth)
            return
        elif op == "ITE_THEN":
            # Commit to Then branch in if-then-else
            tmp_barrier = goal.payload["tmp_barrier"]
            # Prune all choicepoints above tmp_barrier (removes Else CP)
            while len(self.cp_stack) > tmp_barrier:
                self.cp_stack.pop()
            # Debug assertion: confirm we pruned the ITE CP
            if __debug__:
                assert (
                    len(self.cp_stack) == tmp_barrier
                ), f"ITE commit failed: {len(self.cp_stack)} != {tmp_barrier}"
            # Schedule Then goal
            then_goal = goal.payload["then_goal"]
            then_depth = goal.payload.get("then_goal_depth", len(self.frame_stack))
            self._push_goal(then_goal, depth=then_depth)
        elif op == "LABEL_CONTINUE":
            # Continue labeling after binding a variable
            vars = goal.payload["vars"]
            var_select = goal.payload["var_select"]
            val_select = goal.payload["val_select"]
            if self.trace:
                self._trace_log.append("[LABEL_CONTINUE] dispatching continuation")
            push_labeling_choices(self, vars, var_select, val_select)
        elif op == "LABEL_BRANCH_STAMP":
            # Start a fresh trail stamp for the upcoming labeling branch
            # This ensures domain/attr changes in each branch are isolated
            # and properly undone on backtracking between alternatives.
            self.trail.next_stamp()
            return True
        # CATCH_BEGIN and CATCH_END are no longer used - catch/3 uses CATCH choicepoints

    def _backtrack(self) -> bool:
        """Backtrack to the most recent choicepoint.

        Returns:
            True if backtracking succeeded, False if no more choicepoints.
        """
        if self.metrics:
            self.metrics.record_backtrack()

        while self.cp_stack:
            cp = self.cp_stack.pop()

            # Emit internal event for CP pop
            if self.tracer:
                pred_id = (
                    cp.payload.get("pred_ref", cp.kind.name.lower())
                    if cp.kind == ChoicepointKind.PREDICATE
                    else cp.kind.name.lower()
                )
                self.tracer.emit_internal_event("cp_pop", {"pred_id": pred_id})

            if self.trace:
                self._trace_log.append(
                    f"[BACKTRACK] Popped CP: {cp.kind.name}, phase={cp.payload.get('phase', 'N/A')}"
                )

            # Restore state - unwind first, then restore heights
            # This order is safer if future features attach watchers to frames
            self.trail.unwind_to(cp.trail_top, self.store)

            # Note: CLP(FD) propagation queue is not trailed; we deliberately
            # do not clear it here to preserve scheduled propagation semantics.

            # Restore goal stack to checkpoint height
            # PREDICATE and DISJUNCTION CPs handle restoration in their handlers
            if cp.kind in [ChoicepointKind.PREDICATE, ChoicepointKind.DISJUNCTION]:
                # We'll handle goal stack restoration in the CP-specific handler below
                if self.trace:
                    self._trace_log.append(
                        f"Deferring goal stack restoration for {cp.kind} CP"
                    )
            elif cp.kind == ChoicepointKind.CATCH and cp.payload.get("has_pop_frame"):
                # Special handling for CATCH CPs that had a POP_FRAME sentinel
                # Simply restore to the target height - the POP_FRAME handling is already correct
                # Note: We don't assert goal stack height for CATCH CPs because POP_FRAME
                # sentinels and catch window semantics can cause legitimate height variations
                target_height = cp.goal_stack_height
                self._shrink_goal_stack_to(target_height)
            else:
                # Standard restoration for other CP kinds
                current_height = self.goal_stack.height()
                target_height = cp.goal_stack_height
                if self.trace:
                    self._trace_log.append(
                        f"Restoring goal stack from {current_height} towards {target_height} (kind={cp.kind.name})"
                    )

                # Only shrink if we've grown beyond the target; if we're below, leave as is.
                # For DISJUNCTION CPs we no longer snapshot and replay continuation; leaving
                # the stack below target is valid and the alternative branch will be pushed explicitly.
                if current_height > target_height:
                    self._shrink_goal_stack_to(target_height)

                # Assert invariant: goal stack restored correctly for kinds that require exact height
                # Skip assertion for CATCH and DISJUNCTION CPs due to their specific restoration semantics.
                if cp.kind not in (ChoicepointKind.CATCH, ChoicepointKind.DISJUNCTION):
                    assert (
                        self.goal_stack.height() == target_height
                    ), f"Goal stack height mismatch after restore: {self.goal_stack.height()} != {target_height}"

            # Remove catch frames that are now out of scope
            # When we backtrack past a catch's window, remove its frame
            # A catch frame is out of scope if we've backtracked BELOW its goal height
            # Catch frames are now managed via CATCH choicepoints

            # Pop frames above checkpoint
            # PREDICATE CPs don't manage frames (they use frame_stack_height=0 as a sentinel)
            if cp.kind != ChoicepointKind.PREDICATE:
                while len(self.frame_stack) > cp.frame_stack_height:
                    frame = self.frame_stack.pop()
                    self._debug_frame_pops += 1

                    # Emit internal event for frame pop
                    if self.tracer:
                        self.tracer.emit_internal_event(
                            "frame_pop",
                            {
                                "pred_id": self._normalize_pred_id(frame.pred),
                                "frame_id": frame.frame_id,
                            },
                        )

                # Debug assertion: verify frame restoration
                assert (
                    len(self.frame_stack) == cp.frame_stack_height
                ), f"Frame stack height mismatch: {len(self.frame_stack)} != {cp.frame_stack_height}"

            # Resume based on choicepoint kind
            if cp.kind == ChoicepointKind.PREDICATE:
                pred_ref = cp.payload["pred_ref"]
                call_depth = cp.payload.get("call_depth", len(self.frame_stack))
                has_succeeded = cp.payload.get("has_succeeded", False)

                # If the previous clause left an active frame (i.e., failed before exit),
                # emit FAIL for that frame before proceeding.
                if self.frame_stack and self.frame_stack[-1].pred == pred_ref:
                    self._fail_current_frame()

                # Emit REDO port before resuming normal predicate (only after success)
                goal = cp.payload.get("goal")
                if has_succeeded:
                    self._port("REDO", pred_ref)
                    if self.metrics:
                        self.metrics.record_redo(pred_ref)
                    if goal and goal.term:
                        self._trace_port("redo", goal.term, depth_override=call_depth)

                # Try next clause
                goal = goal or cp.payload["goal"]
                cursor = cp.payload["cursor"]

                # Restore goal stack to the continuation
                # The continuation is the snapshot of goals that should run after the predicate
                # Clear the goal stack and restore the continuation
                self._shrink_goal_stack_to(0)

                if "continuation" in cp.payload:
                    continuation = cp.payload["continuation"]
                    continuation_depths = cp.payload.get(
                        "continuation_depths",
                        tuple(len(self.frame_stack) for _ in continuation),
                    )
                    continuation_calls = cp.payload.get(
                        "continuation_calls", tuple(False for _ in continuation)
                    )
                    # Restore the continuation - these are the goals after the predicate
                    self._push_goals_with_metadata(
                        continuation, continuation_depths, continuation_calls
                    )

                    if self.trace:
                        self._trace_log.append(
                            f"PREDICATE CP: restored {len(continuation)} continuation goals"
                        )

                has_more = cursor.has_more()
                if self.trace:
                    self._trace_log.append(
                        f"PREDICATE CP: cursor.has_more()={has_more}"
                    )

                if has_more:
                    clause_idx = cursor.take()
                    clause = self.program.clauses[clause_idx]

                    if self.trace:
                        self._trace_log.append(
                            f"PREDICATE CP: Trying next clause #{clause_idx}"
                        )

                    # Shrink store to checkpoint size for allocation cleanup
                    if "store_top" in cp.payload:
                        self.store.shrink_to(cp.payload["store_top"])

                    # Enter new choice region when resuming
                    # This ensures proper trailing for the new clause attempt
                    self.trail.next_stamp()

                    # Compute cut barrier BEFORE pushing next alternatives CP
                    # This ensures cut will prune remaining alternatives (ISO semantics)
                    cut_barrier = len(self.cp_stack)

                    # If still more clauses after this one, re-push choicepoint
                    if cursor.has_more():
                        # Preserve the continuation from the original CP
                        continuation = cp.payload.get("continuation", ())
                        continuation_depths = cp.payload.get(
                            "continuation_depths",
                            tuple(len(self.frame_stack) for _ in continuation),
                        )
                        continuation_calls = cp.payload.get(
                            "continuation_calls", tuple(False for _ in continuation)
                        )

                        new_cp = Choicepoint(
                            kind=ChoicepointKind.PREDICATE,
                            trail_top=self.trail.position(),
                            goal_stack_height=len(continuation),  # Same as original
                            frame_stack_height=0,  # PREDICATE CPs don't manage frames
                            payload={
                                "goal": goal,
                                "cursor": cursor,
                                "pred_ref": cp.payload["pred_ref"],
                                "continuation": continuation,  # Preserve original continuation
                                "store_top": cp.payload.get(
                                    "store_top", self.store.size()
                                ),
                                "call_depth": cp.payload.get(
                                    "call_depth", len(self.frame_stack)
                                ),
                                "continuation_depths": continuation_depths,
                                "continuation_calls": continuation_calls,
                                "has_succeeded": cp.payload.get("has_succeeded", False),
                            },
                        )
                        self.cp_stack.append(new_cp)

                        # Emit internal event for CP push during backtracking
                        if self.tracer:
                            # Compute alternatives count based on cursor type
                            alternatives = 0
                            if cursor:
                                if hasattr(cursor, "matches") and hasattr(
                                    cursor, "pos"
                                ):
                                    alternatives = len(cursor.matches) - cursor.pos
                                elif hasattr(cursor, "has_more"):
                                    alternatives = 1 if cursor.has_more() else 0
                            self.tracer.emit_internal_event(
                                "cp_push",
                                {
                                    "pred_id": pred_ref,
                                    "alternatives": alternatives,
                                    "trail_top": new_cp.trail_top,
                                },
                            )

                    # Rename clause with fresh variables
                    renamed_clause = self._renamer.rename_clause(clause)

                    # Try to unify with clause head
                    unify_result = self._unify(renamed_clause.head, goal.term)

                    if self.trace:
                        self._trace_log.append(
                            f"PREDICATE CP: Unify result = {unify_result}"
                        )

                    if unify_result:
                        # Emit CALL port for resumed clause execution
                        if goal and goal.term:
                            self._trace_port(
                                "call", goal.term, depth_override=call_depth
                            )

                        # Unification succeeded - create frame for body execution
                        # cut_barrier was already computed above before pushing CP
                        frame_id = self._next_frame_id
                        self._next_frame_id += 1
                        frame = Frame(
                            frame_id=frame_id,
                            cut_barrier=cut_barrier,
                            goal_height=self.goal_stack.height(),
                            pred=pred_ref,
                            goal_term=goal.term,  # Store for EXIT port emission
                            call_depth=call_depth,
                        )
                        self.frame_stack.append(frame)

                        # Don't call next_stamp() here - only when creating CPs

                        # Push POP_FRAME sentinel and body goals
                        self._push_goal(
                            Goal(
                                GoalType.POP_FRAME,
                                None,  # Internal goals don't need terms
                                payload={"op": "POP_FRAME", "frame_id": frame_id},
                            )
                        )
                        for body_term in reversed(renamed_clause.body):
                            body_goal = Goal.from_term(body_term)
                            self._push_goal(body_goal)

                        if self.trace:
                            self._trace_log.append(
                                f"PREDICATE CP: Resuming with {self.goal_stack.height()} goals on stack"
                            )

                        return True
                    else:
                        # Unification failed, continue backtracking
                        # No frame was created
                        continue
                else:
                    # No more clauses to try - emit FAIL port
                    if self.frame_stack and self.frame_stack[-1].pred == pred_ref:
                        self._fail_current_frame()
                    else:
                        self._emit_fail_port(
                            pred_ref,
                            cp.payload.get("goal"),
                            depth_override=call_depth,
                        )
                    # Continue backtracking to find earlier choicepoints
                    continue

            elif cp.kind == ChoicepointKind.DISJUNCTION:
                # Clear goal stack and restore continuation
                self._shrink_goal_stack_to(0)

                # Restore the continuation (goals after disjunction)
                continuation = cp.payload.get("continuation", ())
                continuation_depths = cp.payload.get(
                    "continuation_depths",
                    tuple(len(self.frame_stack) for _ in continuation),
                )
                continuation_calls = cp.payload.get(
                    "continuation_calls", tuple(False for _ in continuation)
                )
                # Restore the continuation - these are the goals after the disjunction
                self._push_goals_with_metadata(
                    continuation, continuation_depths, continuation_calls
                )

                # Debug: log restoration details for disjunction CPs
                if self.trace:
                    self._trace_log.append(
                        f"[DISJ RESTORE] Restored {len(continuation)} continuation goals"
                    )

                # Restore the stamp window captured for this disjunction
                self.trail.set_current_stamp(cp.stamp)

                # Clear the CLP(FD) propagation queue to isolate branch state
                if hasattr(self, "clpfd_queue") and self.clpfd_queue is not None:
                    self.clpfd_queue.clear()

                # Try alternative branch
                alternative = cp.payload["alternative"]
                alt_depth = cp.payload.get("alternative_depth", len(self.frame_stack))
                self._push_goal(alternative, depth=alt_depth)

                return True

            elif cp.kind == ChoicepointKind.CATCH:
                # Check if this is a RECOVERY phase CATCH
                if cp.payload.get("phase") == "RECOVERY":
                    # Recovery failed - just continue backtracking
                    # The continuation was already on the goal stack
                    if self.trace:
                        self._trace_log.append(
                            "CATCH CP (RECOVERY) - continuing backtrack"
                        )
                    continue
                else:
                    # GOAL phase CATCH choicepoints are never resumed
                    if self.trace:
                        self._trace_log.append("CATCH CP (GOAL) - continuing backtrack")
                    continue

            elif cp.kind == ChoicepointKind.IF_THEN_ELSE:
                # Try else branch (only reached if condition failed exhaustively)
                else_branch = cp.payload["else_goal"]
                else_depth = cp.payload.get("else_goal_depth", len(self.frame_stack))
                self._push_goal(else_branch, depth=else_depth)
                return True

        # No more choicepoints
        # Emit FAIL for any remaining frames (propagate failure)
        while self.frame_stack:
            self._fail_current_frame()

        # Emit FAIL for the most recent predicate if appropriate
        if self._last_exit_info:
            pred_ref, term, depth = self._last_exit_info
            self._emit_fail_port(pred_ref, term, depth_override=depth)
            self._last_exit_info = None

        return False

    def _emit_fail_port(
        self, pred_ref: str, goal: Optional[Any], depth_override: Optional[int] = None
    ) -> None:
        """Emit FAIL port (and tracer event) for a predicate."""
        pred = pred_ref or "unknown"
        self._port("FAIL", pred)
        if self.metrics:
            self.metrics.record_fail(pred)

        if not self.tracer:
            return

        term = None
        if goal is not None:
            if isinstance(goal, Goal):
                term = goal.term
            else:
                term = goal

        if term:
            self._trace_port("fail", term, depth_override=depth_override)

        # FAIL invalidates any pending EXIT info
        self._last_exit_info = None

    def _fail_current_frame(self) -> None:
        """Emit FAIL for the current frame and pop it."""
        if not self.frame_stack:
            return

        frame = self.frame_stack[-1]
        self._emit_fail_port(
            frame.pred, frame.goal_term, depth_override=frame.call_depth
        )
        self._last_exit_info = None

        # Pop the frame after emitting FAIL (depth is correct during emission)
        self.frame_stack.pop()
        self._debug_frame_pops += 1

        if self.tracer:
            self.tracer.emit_internal_event(
                "frame_pop",
                {
                    "pred_id": self._normalize_pred_id(frame.pred),
                    "frame_id": frame.frame_id,
                },
            )

        self._update_catch_frames_after_pop()

    def _update_catch_frames_after_pop(self) -> None:
        """Adjust CATCH CP goal and frame heights after a frame pop."""
        current_goal_height = self.goal_stack.height()
        current_frame_height = len(self.frame_stack)
        for cp in self.cp_stack:
            if cp.kind == ChoicepointKind.CATCH:
                # Update goal height if it's now too high
                if cp.goal_stack_height > current_goal_height:
                    cp.goal_stack_height = current_goal_height
                # Update frame height if it's now too high
                if cp.frame_stack_height > current_frame_height:
                    cp.frame_stack_height = current_frame_height

    def _record_solution(self):
        """Record the current solution (bindings of query variables)."""
        # Ensure all pending CLP(FD) propagation is completed before capturing values
        try:
            if hasattr(self, "clpfd_queue") and self.clpfd_queue is not None:
                if not self.clpfd_queue.is_empty():
                    self.clpfd_queue.run_to_fixpoint(self.store, self.trail, self)
        except Exception:
            pass

        solution = {}

        for varid, var_name in self._query_vars:
            # Reify the variable to get its value
            value = self._reify_var(varid)
            solution[var_name] = value

        self.solutions.append(solution)
        if self.trace:
            try:
                xv = solution.get("X")
                yv = solution.get("Y")
                self._trace_log.append(
                    f"[SOLUTION] X={getattr(xv,'value', xv)}, Y={getattr(yv,'value', yv)}"
                )
            except Exception:
                pass

    def _reify_var(self, varid: int) -> Any:
        """Follow bindings to get the value of a variable."""
        return reify_var(self.store, self._qname_by_id, varid)

    def _reify_term(self, term: Term) -> Any:
        """Reify a term, following variable bindings."""
        return reify_term(self.store, self._qname_by_id, term)

    # Compatibility wrapper methods for legacy tests
    # These delegate to the new builtin system while preserving the old API
    def _builtin_var(self, args: tuple) -> bool:
        """Legacy compatibility wrapper for var/1."""
        return builtin_var(self, args)

    def _builtin_nonvar(self, args: tuple) -> bool:
        """Legacy compatibility wrapper for nonvar/1."""

        return builtin_nonvar(self, args)

    def _builtin_atom(self, args: tuple) -> bool:
        """Legacy compatibility wrapper for atom/1."""

        return builtin_atom(self, args)

    def _builtin_integer(self, args: tuple) -> bool:
        """Legacy compatibility wrapper for integer/1."""

        return builtin_integer(self, args)

    def _builtin_float(self, args: tuple) -> bool:
        """Legacy compatibility wrapper for float/1."""

        return builtin_float(self, args)

    def _builtin_number(self, args: tuple) -> bool:
        """Legacy compatibility wrapper for number/1."""

        return builtin_number(self, args)

    def _builtin_atomic(self, args: tuple) -> bool:
        """Legacy compatibility wrapper for atomic/1."""

        return builtin_atomic(self, args)

    def _builtin_compound(self, args: tuple) -> bool:
        """Legacy compatibility wrapper for compound/1."""

        return builtin_compound(self, args)

    def _builtin_callable(self, args: tuple) -> bool:
        """Legacy compatibility wrapper for callable/1."""

        return builtin_callable(self, args)

    def _builtin_ground(self, args: tuple) -> bool:
        """Legacy compatibility wrapper for ground/1."""

        return builtin_ground(self, args)

    def _is_ground(self, term: Term) -> bool:
        """Legacy compatibility wrapper for is_ground helper."""

        return is_ground(self, term)

    def _execute_builtin(self, term: Term) -> bool:
        """Execute a builtin directly (for testing).

        Args:
            term: The builtin term to execute.

        Returns:
            True if successful, False if failed.
        """
        if isinstance(term, Atom):
            key = (term.name, 0)
            args = ()
        elif isinstance(term, Struct):
            key = (term.functor, len(term.args))
            args = term.args
        else:
            return False

        builtin_fn = self._builtins.get(key)
        if builtin_fn is None:
            return False

        try:
            return builtin_fn(self, args)
        except (ValueError, TypeError):
            return False

    def _is_builtin(self, term: Term) -> bool:
        """Check if a term is a builtin predicate.

        Args:
            term: The term to check.

        Returns:
            True if it's a builtin, False otherwise.
        """
        if isinstance(term, Atom):
            key = (term.name, 0)
        elif isinstance(term, Struct):
            key = (term.functor, len(term.args))
        else:
            return False

        return key in self._builtins

    # Builtin implementations - uniform signature: (engine, args_tuple) -> bool
    def _port(self, kind: str, pred: str) -> None:
        """Record a debug port.

        Args:
            kind: The port type (CALL, EXIT, REDO, FAIL).
            pred: The predicate reference.
        """
        self._ports.append(kind)  # Can also append f"{kind} {pred}" if needed

    @property
    def debug_ports(self) -> List[str]:
        """Get the list of recorded debug ports."""
        return list(self._ports)

    @property
    def debug_trail_depth(self) -> int:
        """Get current trail depth."""
        return self.trail.position()

    @property
    def debug_cp_stack_size(self) -> int:
        """Get current choicepoint stack size."""
        return len(self.cp_stack)

    @property
    def debug_goal_stack_size(self) -> int:
        """Get current goal stack size."""
        return self.goal_stack.height()

    @property
    def debug_frame_stack_size(self) -> int:
        """Get current frame stack size."""
        return len(self.frame_stack)

    @property
    def goals(self):
        """Get current goal stack for test compatibility."""
        # Return the GoalStack itself for compatibility with tests
        # that might access ._stack directly
        return self.goal_stack

    @property
    def choices(self):
        """Get current choicepoint stack for test compatibility."""
        return list(self.cp_stack)

    def _builtin_cut(self, args: tuple) -> bool:
        """! - cut builtin."""
        self._dispatch_cut()
        return True

    def _builtin_call(self, args: tuple) -> bool:
        """call/1 - meta-call builtin.

        Executes the argument as a goal after dereferencing.
        Fails if the argument is unbound or not callable.
        """
        if len(args) != 1:
            return False

        term = args[0]

        # Full iterative dereference
        while isinstance(term, Var):
            result = self.store.deref(term.id)
            if result[0] == "UNBOUND":
                # Unbound variable - cannot call
                return False
            _, _, term = result

        # Check if term is callable (Atom or Struct)
        if not isinstance(term, (Atom, Struct)):
            # Integers, lists, etc are not callable
            return False

        # Schedule internal CONTROL op that pushes the goal, keeping call/1 transparent
        meta_goal = Goal.from_term(term)
        self._push_goal(
            Goal(
                GoalType.CONTROL,
                None,
                payload={
                    "op": "CALL_META",
                    "goal": meta_goal,
                    "goal_depth": len(self.frame_stack),
                },
            )
        )
        return True

    def _builtin_unify(self, args: tuple) -> bool:
        """=(X, Y) - unification builtin."""
        if len(args) != 2:
            return False
        left, right = args

        if self.metrics:
            self.metrics.record_unification_attempt()

        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
        if self.trace:
            self._trace_log.append(f"[UNIFY] attempting unify: {left} = {right}")
        result = unify(
            left,
            right,
            self.store,
            trail_adapter,  # type: ignore
            occurs_check=self.occurs_check,
        )

        if self.metrics and result:
            self.metrics.record_unification_success()
        if self.trace:
            self._trace_log.append(f"[UNIFY] result: {result}")

        return result

    def _builtin_not_unify(self, args: tuple) -> bool:
        """\\=(X, Y) - negation of unification."""
        if len(args) != 2:
            return False
        left, right = args
        # Create a temporary trail position
        trail_pos = self.trail.position()
        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
        success = unify(
            left,
            right,
            self.store,
            trail_adapter,  # type: ignore
            occurs_check=self.occurs_check,
        )
        # Undo any bindings
        self.trail.unwind_to(trail_pos, self.store)
        return not success

    def _copy_term_with_fresh_vars(self, term: Term) -> Term:
        """Create a copy of a term with fresh variables."""
        return copy_term_with_fresh_vars(self.store, term)

    def _copy_term_recursive(
        self, term: Term, var_mapping: Dict[int, int], target_store=None
    ) -> Term:
        """Recursively copy a term, mapping old var IDs to new ones."""
        if target_store is None:
            target_store = self.store
        return copy_term_recursive(term, var_mapping, target_store)

    def _build_prolog_list(self, items: List[Term]) -> Term:
        """Build a Prolog list term from a list of items."""
        return build_prolog_list(items)

    def _builtin_call_n(self, args: tuple) -> bool:
        """call/2 through call/8 - extended meta-call with additional arguments.

        call(Goal, Arg1) is equivalent to calling Goal with Arg1 added as first argument.
        call(Goal, Arg1, Arg2) adds Arg1 and Arg2 as first and second arguments, etc.

        NOTE: Unlike call/1 which uses CONTROL operations for transparency, call/2-8
        directly push goals. This may affect trace/port behavior but provides correct
        semantics. Future versions may align with call/1's transparency approach.
        """
        if len(args) < 2:
            return False

        goal_arg = args[0]
        extra_args = args[1:]

        # Dereference the goal argument
        if isinstance(goal_arg, Var):
            result = self.store.deref(goal_arg.id)
            if result[0] == "BOUND":
                _, _, bound_term = result
                goal_arg = bound_term
            else:
                return False  # Unbound variable

        # Build new goal with additional arguments
        if isinstance(goal_arg, Atom):
            # call(foo, X, Y) -> foo(X, Y)
            new_goal = Struct(goal_arg.name, extra_args)
        elif isinstance(goal_arg, Struct):
            # call(foo(A), X, Y) -> foo(A, X, Y)
            combined_args = goal_arg.args + extra_args
            new_goal = Struct(goal_arg.functor, combined_args)
        else:
            return False  # Goal must be atom or compound

        # Check if the new goal is callable
        if not isinstance(new_goal, (Atom, Struct)):
            return False

        # Push the new goal for execution
        self._push_goal(Goal.from_term(new_goal))
        return True

    def _builtin_not_provable(self, args: tuple) -> bool:
        """\\+/1 - negation as failure.

        \\+(Goal) succeeds if Goal fails (cannot be proved).
        This implements negation as failure semantics.
        """
        if len(args) != 1:
            return False

        goal_arg = args[0]

        # Save current engine state (query state only - no store changes)
        saved_solutions = self.solutions[:]
        saved_query_vars = self._query_vars[:]

        try:
            # Create a minimal sub-engine to test the goal
            sub_engine = Engine(
                self.program,
                occurs_check=self.occurs_check,
                max_solutions=1,  # We only need to know if it succeeds once
                trace=False,
                use_indexing=False,
            )

            # Copy goal directly into sub-engine (avoid main store allocation)
            var_mapping = {}
            goal_for_sub = self._copy_term_recursive(
                goal_arg, var_mapping, sub_engine.store
            )

            solutions = sub_engine.solve(goal_for_sub)

            # Negation as failure: succeed if goal has no solutions
            return len(solutions) == 0

        except Exception:
            # In ISO Prolog, exceptions should propagate through negation
            raise
        finally:
            # Restore engine state (no store/trail changes to undo)
            self.solutions = saved_solutions
            self._query_vars = saved_query_vars

    def _builtin_copy_term(self, args: tuple) -> bool:
        """copy_term(+Term, -Copy) - create a copy of term with fresh variables."""
        if len(args) != 2:
            return False

        original, copy = args

        # Create a copy with fresh variables
        copied_term = self._copy_term_with_fresh_vars(original)

        # Unify the copy with the second argument
        trail_adapter = TrailAdapter(self.trail)
        return unify(copy, copied_term, self.store, trail_adapter, self.occurs_check)

    def _builtin_unify_with_occurs_check(self, args: tuple) -> bool:
        """unify_with_occurs_check(+Term1, +Term2) - unify with occurs check enabled."""
        if len(args) != 2:
            return False

        term1, term2 = args

        # Unify with occurs check forced on
        trail_adapter = TrailAdapter(self.trail)
        return unify(term1, term2, self.store, trail_adapter, occurs_check=True)

    def _eval_int(self, t: Term) -> int:
        """Evaluate a term as an integer."""
        return eval_int(self.store, t)

    def _builtin_univ(self, args: tuple) -> bool:
        """=..(Term, List) - structure ↔ list conversion (univ).

        Three modes:
        1. Decomposition: foo(a,b) =.. L binds L to [foo,a,b]
        2. Construction: X =.. [foo,a,b] binds X to foo(a,b)
        3. Checking: foo(a,b) =.. [foo,a,b] succeeds

        Note: List in AST is aliased as PrologList here for clarity.
        """
        if len(args) != 2:
            return False

        left, right = args
        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)

        # Dereference both sides
        if isinstance(left, Var):
            left_result = self.store.deref(left.id)
            if left_result[0] == "BOUND":
                left = left_result[2]
                left_unbound = False
            else:
                # Left is unbound - must be construction mode
                left_unbound = True
        else:
            left_unbound = False

        if isinstance(right, Var):
            right_result = self.store.deref(right.id)
            if right_result[0] == "BOUND":
                right = right_result[2]
                right_unbound = False
            else:
                # Right is unbound - must be decomposition mode
                right_unbound = True
        else:
            right_unbound = False

        # Case 1: Decomposition mode (Term =.. X where X unbound)
        if not left_unbound and right_unbound:
            decomposed = self._decompose_to_list(left)
            if decomposed is None:
                return False
            return unify(
                right,
                decomposed,
                self.store,
                trail_adapter,
                occurs_check=self.occurs_check,
            )

        # Case 2: Construction mode (X =.. [foo,a,b] where X unbound)
        elif left_unbound and not right_unbound:
            # Right side must be a proper list
            if not isinstance(right, PrologList):
                return False

            # Convert to Python list
            list_items = self._prolog_list_to_python_list(right)
            if list_items is None:  # Not a proper list
                return False

            if len(list_items) == 0:
                # Empty RHS list is invalid in construction mode
                return False

            # Single element list - becomes atomic
            if len(list_items) == 1:
                constructed = list_items[0]
            else:
                # Multi-element list - first is functor, rest are args
                functor_term = list_items[0]

                # Special case: ['.', a, tail] constructs a list
                if isinstance(functor_term, Atom) and functor_term.name == ".":
                    if len(list_items) != 3:
                        return False  # Dot must have exactly 2 args
                    # Construct list [a|tail]
                    # Note: ['.', A, Tail] may construct an improper list if Tail is not a list
                    constructed = PrologList((list_items[1],), list_items[2])
                # Regular structure
                elif isinstance(functor_term, Atom):
                    constructed = Struct(functor_term.name, tuple(list_items[1:]))
                else:
                    # Non-atom functor with args is invalid
                    return False

            # Unify the constructed term with the left side
            return unify(
                left,
                constructed,
                self.store,
                trail_adapter,
                occurs_check=self.occurs_check,
            )

        # Case 3: Checking mode (both sides bound) or both unbound (fail)
        elif not left_unbound and not right_unbound:
            # Both bound - decompose left and check equality with right
            decomposed = self._decompose_to_list(left)
            if decomposed is None:
                return False
            return unify(
                decomposed,
                right,
                self.store,
                trail_adapter,
                occurs_check=self.occurs_check,
            )
        else:
            # Both unbound - can't decompose or construct
            return False

    def _decompose_to_list(self, term: Term) -> Optional[PrologList]:
        """Decompose a term into list form for =../2.

        Args:
            term: The term to decompose

        Returns:
            PrologList representation or None if term cannot be decomposed
        """
        if isinstance(term, Struct):
            # Structure: foo(a,b) -> [foo, a, b]
            items = [Atom(term.functor)] + list(term.args)
            return self._make_prolog_list(items)
        elif isinstance(term, PrologList):
            # List: [a,b] -> ['.', a, [b]]
            if (
                not term.items
                and isinstance(term.tail, Atom)
                and term.tail.name == "[]"
            ):
                # Empty list [] -> [[]]
                return self._make_prolog_list([Atom("[]")])
            else:
                # Non-empty list becomes dot structure
                if term.items:
                    tail = (
                        PrologList(term.items[1:], term.tail)
                        if len(term.items) > 1
                        else term.tail
                    )
                    return self._make_prolog_list([Atom("."), term.items[0], tail])
                else:
                    # Shouldn't happen but handle gracefully
                    return None
        elif isinstance(term, Atom):
            # Atom: foo -> [foo]
            return self._make_prolog_list([term])
        elif isinstance(term, Int):
            # Integer: 42 -> [42]
            return self._make_prolog_list([term])
        else:
            return None

    def _make_prolog_list(self, items: list) -> PrologList:
        """Create a proper Prolog list from Python list."""
        if not items:
            return PrologList((), Atom("[]"))
        return PrologList(tuple(items), Atom("[]"))

    def _prolog_list_to_python_list(self, lst: PrologList) -> Optional[list]:
        """Convert Prolog list to Python list, or None if improper."""
        result = []
        current = lst

        while True:
            if isinstance(current, PrologList):
                result.extend(current.items)
                current = current.tail
            elif isinstance(current, Atom) and current.name == "[]":
                # Proper list terminator
                return result
            elif isinstance(current, Var):
                # Check if bound
                var_result = self.store.deref(current.id)
                if var_result[0] == "BOUND":
                    current = var_result[2]
                else:
                    # Unbound tail - improper list
                    return None
            else:
                # Non-[] tail - improper list
                return None

    def _extract_functor_arity(self, term: Term) -> Optional[Tuple[Term, Term]]:
        """Extract functor and arity from a term.

        Returns:
            Tuple of (functor_term, arity_term) or None if term type unknown
        """
        if isinstance(term, Struct):
            # Structure: functor is atom, arity is arg count
            return (Atom(term.functor), Int(len(term.args)))
        elif isinstance(term, PrologList):
            # List: special case
            if (
                not term.items
                and isinstance(term.tail, Atom)
                and term.tail.name == "[]"
            ):
                # Empty list: [] has functor '[]' and arity 0
                return (Atom("[]"), Int(0))
            else:
                # Non-empty list: has functor '.' and arity 2
                return (Atom("."), Int(2))
        elif isinstance(term, Atom):
            # Atom: functor is itself, arity is 0
            return (term, Int(0))
        elif isinstance(term, Int):
            # Integer: functor is itself, arity is 0
            return (term, Int(0))
        else:
            # Unknown term type
            return None

    def _builtin_functor(self, args: tuple) -> bool:
        """functor(Term, Functor, Arity) - functor/arity manipulation.

        Three modes:
        1. Extraction: functor(foo(a,b), F, A) binds F=foo, A=2
        2. Construction: functor(X, foo, 2) binds X=foo(_,_) with fresh variables
        3. Checking: functor(foo(a,b), foo, 2) succeeds

        Special cases:
        - Atoms have arity 0: functor(foo, foo, 0)
        - Integers have arity 0: functor(42, 42, 0)
        - Lists are '.'/2: functor([a], '.', 2)
        - Empty list is '[]'/0: functor([], '[]', 0)
        """
        if len(args) != 3:
            return False

        term_arg, functor_arg, arity_arg = args
        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)

        # Dereference all arguments
        if isinstance(term_arg, Var):
            term_result = self.store.deref(term_arg.id)
            if term_result[0] == "BOUND":
                term = term_result[2]
                term_unbound = False
            else:
                term_unbound = True
                term = term_arg
        else:
            term = term_arg
            term_unbound = False

        if isinstance(functor_arg, Var):
            functor_result = self.store.deref(functor_arg.id)
            if functor_result[0] == "BOUND":
                functor = functor_result[2]
                functor_unbound = False
            else:
                functor_unbound = True
                functor = functor_arg
        else:
            functor = functor_arg
            functor_unbound = False

        if isinstance(arity_arg, Var):
            arity_result = self.store.deref(arity_arg.id)
            if arity_result[0] == "BOUND":
                arity = arity_result[2]
                arity_unbound = False
            else:
                arity_unbound = True
                arity = arity_arg
        else:
            arity = arity_arg
            arity_unbound = False

        # Mode 1: Extraction (Term is bound, extract Functor and/or Arity)
        if not term_unbound:
            extraction = self._extract_functor_arity(term)
            if extraction is None:
                # Unknown term type
                return False

            extracted_functor, extracted_arity = extraction

            # Unify functor
            if not unify(
                functor, extracted_functor, self.store, trail_adapter, self.occurs_check
            ):
                return False

            # Unify arity
            if not unify(
                arity, extracted_arity, self.store, trail_adapter, self.occurs_check
            ):
                return False

            return True

        # Mode 2: Construction (Term is unbound, construct from Functor and Arity)
        elif term_unbound and not functor_unbound and not arity_unbound:
            # Validate arity is an integer >= 0
            if not isinstance(arity, Int):
                return False  # Dev-mode: fail instead of type error
            if arity.value < 0:
                return False  # Dev-mode: fail instead of domain error

            # Construct based on functor type
            if isinstance(functor, Atom):
                functor_name = functor.name

                if arity.value == 0:
                    # Zero arity: result is the atom itself
                    constructed = functor
                elif functor_name == "." and arity.value == 2:
                    # Special case: '.'/2 constructs a pair structure, not List
                    # Create two fresh variables
                    head_id = self.store.new_var("_")
                    tail_id = self.store.new_var("_")
                    head_var = Var(head_id, f"_G{head_id}")
                    tail_var = Var(tail_id, f"_G{tail_id}")
                    # Construct as Struct, not List
                    constructed = Struct(".", (head_var, tail_var))
                else:
                    # General case: construct structure with fresh variables
                    # Preallocate tuple for performance
                    fresh_vars = tuple(
                        Var(var_id := self.store.new_var("_"), f"_G{var_id}")
                        for _ in range(arity.value)
                    )
                    constructed = Struct(functor_name, fresh_vars)

            elif isinstance(functor, Int):
                # Integer functor
                if arity.value == 0:
                    # Zero arity: result is the integer itself
                    constructed = functor
                else:
                    # Can't create structure with integer functor and non-zero arity
                    return False  # Dev-mode: fail instead of type error
            else:
                # Functor must be atom or integer
                return False  # Dev-mode: fail instead of type error

            # Unify the constructed term with the term variable
            return unify(
                term, constructed, self.store, trail_adapter, self.occurs_check
            )

        # Mode 3: Insufficient instantiation or over-instantiation
        else:
            # Either:
            # - Term is unbound but functor or arity (or both) are also unbound
            # - All three are bound (handled by extraction mode above)
            return False  # Dev-mode: fail instead of instantiation error

    def _builtin_arg(self, args: tuple) -> bool:
        """arg(N, Term, Arg) - extract or check argument at position N.

        Modes:
        1. Extraction: arg(1, foo(a,b), X) binds X=a
        2. Checking: arg(1, foo(a,b), a) succeeds

        Arguments are 1-indexed. Fails for:
        - N <= 0 or N > arity
        - Non-integer N
        - Non-structure Term (atoms, integers, lists)
        - Unbound N or Term (insufficient instantiation)
        """
        if len(args) != 3:
            return False

        n_arg, term_arg, arg_arg = args
        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)

        # Dereference N
        if isinstance(n_arg, Var):
            n_result = self.store.deref(n_arg.id)
            if n_result[0] == "BOUND":
                n = n_result[2]
            else:
                # N is unbound - insufficient instantiation
                return False  # Dev-mode: fail instead of instantiation_error
        else:
            n = n_arg

        # Check N is an integer
        if not isinstance(n, Int):
            return False  # Dev-mode: fail instead of type_error

        # Check N is positive (1-indexed)
        if n.value <= 0:
            return False  # Dev-mode: fail instead of domain_error

        # Dereference Term
        if isinstance(term_arg, Var):
            term_result = self.store.deref(term_arg.id)
            if term_result[0] == "BOUND":
                term = term_result[2]
            else:
                # Term is unbound - insufficient instantiation
                return False  # Dev-mode: fail instead of instantiation_error
        else:
            term = term_arg

        # Term must be a structure
        if not isinstance(term, Struct):
            return False  # Lists, atoms, integers have no extractable args

        # Check bounds
        if n.value > len(term.args):
            return False  # Out of bounds

        # Get the argument (1-indexed)
        extracted_arg = term.args[n.value - 1]

        # Unify with the third argument
        return unify(
            arg_arg, extracted_arg, self.store, trail_adapter, self.occurs_check
        )

    def _builtin_once(self, args: tuple) -> bool:
        """once(Goal) - succeed at most once.

        Semantically equivalent to (call(Goal), !) - executes Goal
        and commits to first solution by cutting away remaining choicepoints.

        Fails if Goal fails. Succeeds exactly once if Goal succeeds.
        """
        if len(args) != 1:
            return False

        goal_arg = args[0]

        # Dereference the goal
        if isinstance(goal_arg, Var):
            goal_result = self.store.deref(goal_arg.id)
            if goal_result[0] == "BOUND":
                goal = goal_result[2]
            else:
                # Unbound goal - insufficient instantiation
                return False  # Dev-mode: fail instead of instantiation_error
        else:
            goal = goal_arg

        # Goal must be callable (Atom or Struct)
        if not isinstance(goal, (Atom, Struct)):
            return False  # Dev-mode: fail instead of type_error

        # Create a frame to isolate the cut
        # This ensures the cut only affects choicepoints created by Goal
        frame_id = self._next_frame_id
        self._next_frame_id += 1

        # Capture current choicepoint stack height as cut barrier
        cut_barrier = len(self.cp_stack)

        # Create frame to isolate the cut
        frame = Frame(
            frame_id=frame_id,
            cut_barrier=cut_barrier,
            goal_height=self.goal_stack.height(),
            pred=None,  # No predicate reference for once/1
            call_depth=len(self.frame_stack),
        )
        self.frame_stack.append(frame)

        # Push frame cleanup, cut, and goal
        # The sequence is: POP_FRAME, !, Goal
        # This ensures cut happens after goal succeeds, using our frame's barrier
        self._push_goal(
            Goal(
                GoalType.POP_FRAME,
                None,
                payload={"op": "POP_FRAME", "frame_id": frame_id},
            )
        )
        self._push_goal(Goal.from_term(Atom("!")))
        self._push_goal(Goal.from_term(goal))

        return True

    def _builtin_throw(self, args: tuple) -> bool:
        """throw(Ball) - throw an exception.

        Raises an exception with the given ball term. The exception
        propagates up the execution stack until caught by catch/3.
        """
        if len(args) != 1:
            return False

        ball = args[0]

        # Dereference the ball
        if isinstance(ball, Var):
            ball_result = self.store.deref(ball.id)
            if ball_result[0] == "BOUND":
                ball = ball_result[2]
            else:
                # Stage-1 policy: require instantiated ball
                # ISO would allow throwing unbound variables
                return False  # Dev-mode: fail on unbound

        # Reify the ball to capture current bindings
        # This ensures that variable bindings are preserved in the thrown term
        reified_ball = self._reify_term(ball)

        # Record metrics for exception thrown
        if self.metrics:
            self.metrics.record_exception_thrown()

        # Raise PrologThrow directly
        raise PrologThrow(reified_ball)

    def _builtin_catch(self, args: tuple) -> bool:
        """catch(Goal, Catcher, Recovery) - catch exceptions.

        Executes Goal. If Goal throws an exception that unifies with
        Catcher, executes Recovery. Otherwise the exception propagates.
        """
        if len(args) != 3:
            return False

        goal_arg, catcher_arg, recovery_arg = args

        # Dereference the goal
        if isinstance(goal_arg, Var):
            goal_result = self.store.deref(goal_arg.id)
            if goal_result[0] == "BOUND":
                goal = goal_result[2]
            else:
                # Unbound goal - insufficient instantiation
                return False  # Dev-mode: fail
        else:
            goal = goal_arg

        # Goal must be callable
        if not isinstance(goal, (Atom, Struct)):
            return False  # Dev-mode: fail on non-callable

        # Capture baseline heights BEFORE pushing anything
        # These represent the state at the call site of catch/3
        base_trail_top = self.trail.position()
        base_goal_height = self.goal_stack.height()
        base_frame_height = len(self.frame_stack)
        base_cp_height = len(self.cp_stack)

        # Check if there's a POP_FRAME sentinel on top of the goal stack
        # If so, we need to track that it might be consumed before we backtrack
        top_goal = self.goal_stack.peek()
        has_pop_frame_sentinel = (
            top_goal is not None and top_goal.type == GoalType.POP_FRAME
        )

        # Count frames for the same predicate for recursion tracking
        recursion_depth = 0
        if self.frame_stack:
            current_pred = self.frame_stack[-1].pred
            for frame in self.frame_stack:
                if frame.pred == current_pred:
                    recursion_depth += 1

        # Snapshot active streaming cursors for restoration after exception
        cursor_snapshots = {}
        for cp in self.cp_stack:
            if cp.kind == ChoicepointKind.PREDICATE:
                cursor = cp.payload.get("cursor")
                if hasattr(cursor, "clone") and callable(cursor.clone):
                    # Clone the cursor for restoration
                    cursor_snapshots[id(cursor)] = cursor.clone()

        # Create CATCH choicepoint (phase=GOAL)
        stamp = self.trail.next_stamp()
        catch_cp = Choicepoint(
            kind=ChoicepointKind.CATCH,
            trail_top=base_trail_top,
            goal_stack_height=base_goal_height,
            frame_stack_height=base_frame_height,
            payload={
                "phase": "GOAL",
                "catcher": catcher_arg,
                "recovery": recovery_arg,
                "cp_height": base_cp_height,  # Store CP height for restoration
                "has_pop_frame": has_pop_frame_sentinel,  # Track if we have a POP_FRAME that might be consumed
                "cursor_snapshots": cursor_snapshots,  # Snapshot for cursor restoration
                "recursion_depth": recursion_depth,  # Track recursion depth for frame validation
            },
            stamp=stamp,
        )
        self.cp_stack.append(catch_cp)

        # Emit internal event for CP push
        if self.tracer:
            self.tracer.emit_internal_event(
                "cp_push", {"pred_id": "catch", "trail_top": base_trail_top}
            )

        # Create a frame to establish cut barrier for the catch scope
        # This prevents cuts within Goal from escaping the catch boundary
        self._next_frame_id += 1
        catch_frame = Frame(
            frame_id=self._next_frame_id,
            cut_barrier=len(self.cp_stack),  # Cut stops at the CATCH CP
            goal_height=self.goal_stack.height(),
            pred=("catch", 3),
        )
        self.frame_stack.append(catch_frame)

        # Push POP_FRAME to clean up the catch frame after Goal completes
        self._push_goal(
            Goal(GoalType.POP_FRAME, None, payload={"frame_id": self._next_frame_id})
        )

        # Push the user's Goal to execute
        self._push_goal(Goal.from_term(goal))

        return True

    def _builtin_put_attr(self, args: tuple) -> bool:
        """put_attr(Var, Module, Value) - attach attribute to variable.

        Args:
            args: (variable, module_name, value)

        Returns:
            True if successful, False if failed
        """
        if len(args) != 3:
            return False

        var_arg, module_arg, value_arg = args

        # Module must be an atom
        if not isinstance(module_arg, Atom):
            return False
        module_name = module_arg.name

        # Variable must be unbound after deref
        if not isinstance(var_arg, Var):
            return False

        result = self.store.deref(var_arg.id)
        if result[0] != "UNBOUND":
            # Can only put attributes on unbound variables
            return False

        _, root_id = result

        # Trail the old attribute state before modification
        if root_id in self.store.attrs:
            if module_name in self.store.attrs[root_id]:
                # Trail existing value
                old_value = self.store.attrs[root_id][module_name]
                self.trail.push_attr(root_id, module_name, old_value)
            else:
                # Trail absence (None indicates no previous value)
                self.trail.push_attr(root_id, module_name, None)
        else:
            # Trail absence of entire attr dict
            self.trail.push_attr(root_id, module_name, None)

        # Set the attribute
        if root_id not in self.store.attrs:
            self.store.attrs[root_id] = {}
        self.store.attrs[root_id][module_name] = value_arg

        return True

    def _builtin_get_attr(self, args: tuple) -> bool:
        """get_attr(Var, Module, Value) - retrieve attribute from variable.

        Args:
            args: (variable, module_name, value)

        Returns:
            True if attribute exists and unifies with value, False otherwise
        """
        if len(args) != 3:
            return False

        var_arg, module_arg, value_arg = args

        # Module must be an atom
        if not isinstance(module_arg, Atom):
            return False
        module_name = module_arg.name

        # Variable must be unbound after deref
        if not isinstance(var_arg, Var):
            return False

        result = self.store.deref(var_arg.id)
        if result[0] != "UNBOUND":
            # Can only get attributes from unbound variables
            return False

        _, root_id = result

        # Check if attribute exists
        if root_id not in self.store.attrs:
            return False
        if module_name not in self.store.attrs[root_id]:
            return False

        # Unify the value with the third argument
        stored_value = self.store.attrs[root_id][module_name]
        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
        return unify(
            value_arg,
            stored_value,
            self.store,
            trail_adapter,  # type: ignore
            occurs_check=self.occurs_check,
        )

    def _builtin_del_attr(self, args: tuple) -> bool:
        """del_attr(Var, Module) - remove attribute from variable.

        Args:
            args: (variable, module_name)

        Returns:
            True always (succeeds even if attribute doesn't exist)
        """
        if len(args) != 2:
            return False

        var_arg, module_arg = args

        # Module must be an atom
        if not isinstance(module_arg, Atom):
            return False
        module_name = module_arg.name

        # Variable must be unbound after deref
        if not isinstance(var_arg, Var):
            return False

        result = self.store.deref(var_arg.id)
        if result[0] != "UNBOUND":
            # Can only delete attributes from unbound variables
            return False

        _, root_id = result

        # If no attributes or module not present, succeed without trailing
        if root_id not in self.store.attrs:
            return True
        if module_name not in self.store.attrs[root_id]:
            return True

        # Trail the old value before deletion
        old_value = self.store.attrs[root_id][module_name]
        self.trail.push_attr(root_id, module_name, old_value)

        # Delete the attribute
        del self.store.attrs[root_id][module_name]

        # Clean up empty attribute dicts
        if not self.store.attrs[root_id]:
            del self.store.attrs[root_id]

        return True

    def _builtin_structural_equal(self, args: tuple) -> bool:
        """==(X, Y) - structural equality comparison."""
        if len(args) != 2:
            return False
        left, right = args
        return self._structural_compare(left, right) == 0

    def _builtin_structural_not_equal(self, args: tuple) -> bool:
        """\\==(X, Y) - structural inequality comparison."""
        if len(args) != 2:
            return False
        left, right = args
        return self._structural_compare(left, right) != 0

    def _builtin_term_less(self, args: tuple) -> bool:
        """@<(X, Y) - term ordering less than."""
        if len(args) != 2:
            return False
        left, right = args
        return self._structural_compare(left, right) < 0

    def _builtin_term_greater(self, args: tuple) -> bool:
        """@>(X, Y) - term ordering greater than."""
        if len(args) != 2:
            return False
        left, right = args
        return self._structural_compare(left, right) > 0

    def _builtin_term_less_equal(self, args: tuple) -> bool:
        """@=<(X, Y) - term ordering less than or equal."""
        if len(args) != 2:
            return False
        left, right = args
        return self._structural_compare(left, right) <= 0

    def _builtin_term_greater_equal(self, args: tuple) -> bool:
        """@>=(X, Y) - term ordering greater than or equal."""
        if len(args) != 2:
            return False
        left, right = args
        return self._structural_compare(left, right) >= 0

    def _structural_compare(self, term1: Term, term2: Term) -> int:
        """Standard term ordering comparison.

        Returns: -1 if term1 < term2, 0 if equal, 1 if term1 > term2
        Standard order: variables < numbers < atoms < compound terms
        """
        # Dereference terms first
        term1 = self._deref_term(term1)
        term2 = self._deref_term(term2)

        # Get type ordering values
        type1 = self._term_order_type(term1)
        type2 = self._term_order_type(term2)

        if type1 != type2:
            return type1 - type2

        # Same type - compare within type
        if isinstance(term1, Var) and isinstance(term2, Var):
            return term1.id - term2.id
        elif isinstance(term1, Int) and isinstance(term2, Int):
            if term1.value < term2.value:
                return -1
            elif term1.value > term2.value:
                return 1
            else:
                return 0
        elif isinstance(term1, Atom) and isinstance(term2, Atom):
            if term1.name < term2.name:
                return -1
            elif term1.name > term2.name:
                return 1
            else:
                return 0
        elif isinstance(term1, Struct) and isinstance(term2, Struct):
            # Compare arity first, then functor, then args
            if len(term1.args) != len(term2.args):
                return len(term1.args) - len(term2.args)
            if term1.functor != term2.functor:
                if term1.functor < term2.functor:
                    return -1
                else:
                    return 1
            # Compare arguments left to right
            for arg1, arg2 in zip(term1.args, term2.args):
                cmp = self._structural_compare(arg1, arg2)
                if cmp != 0:
                    return cmp
            return 0
        elif isinstance(term1, PrologList) and isinstance(term2, PrologList):
            # Compare list elements and tail
            min_len = min(len(term1.items), len(term2.items))
            for i in range(min_len):
                cmp = self._structural_compare(term1.items[i], term2.items[i])
                if cmp != 0:
                    return cmp
            # If one list is shorter, it comes first
            if len(term1.items) != len(term2.items):
                return len(term1.items) - len(term2.items)
            # Compare tails
            return self._structural_compare(term1.tail, term2.tail)
        else:
            # Fallback: convert to string and compare
            str1 = str(term1)
            str2 = str(term2)
            if str1 < str2:
                return -1
            elif str1 > str2:
                return 1
            else:
                return 0

    def _term_order_type(self, term: Term) -> int:
        """Get term ordering type value."""
        if isinstance(term, Var):
            return 0
        elif isinstance(term, Int):
            return 1
        elif isinstance(term, Atom):
            return 2
        elif isinstance(term, (Struct, PrologList)):
            return 3
        else:
            return 4

    def _deref_term(self, term: Term) -> Term:
        """Dereference a term if it's a variable."""
        if isinstance(term, Var):
            result = self.store.deref(term.id)
            if result[0] == "BOUND":
                return result[2]
        return term

    def register_attr_hook(self, module: str, hook_fn) -> None:
        """Register an attribute hook for a module.

        Args:
            module: Module name
            hook_fn: Hook function with signature (engine, varid, other) -> bool
        """
        self._attr_hooks[module] = hook_fn

    def dispatch_attr_hooks(self, varid: int, other: Term) -> bool:
        """Dispatch hooks for attributed variable unification.

        Called when unifying an attributed variable with a term.
        Hooks are called in sorted module name order for determinism.

        Args:
            varid: Variable ID (will be dereferenced)
            other: Term being unified with

        Returns:
            True if all hooks accept, False if any reject
        """
        # Dereference to get root variable
        result = self.store.deref(varid)
        if result[0] != "UNBOUND":
            # Not an unbound variable, no hooks to dispatch
            return True

        _, root_id = result

        # Fast path: no attributes on this variable
        if root_id not in self.store.attrs:
            return True

        # Get attributes for this variable
        attrs = self.store.attrs[root_id]

        # Call hooks in sorted order for modules that have attributes
        for module in sorted(attrs.keys()):
            # Skip if no hook registered for this module
            if module not in self._attr_hooks:
                continue

            # Call the hook
            hook = self._attr_hooks[module]
            if not hook(self, root_id, other):
                # Hook rejected the unification
                return False

        # All hooks accepted
        return True

    # Debug methods
    def get_trace(self) -> List[str]:
        """Get the debug ports for debugging."""
        return self._ports

    @property
    def debug_frame_pops(self) -> int:
        """Get number of frame pops for debugging."""
        return self._debug_frame_pops

    @property
    def debug_trail_writes(self) -> int:
        """Get number of trail writes for debugging."""
        return self._debug_trail_writes

    def consult_string(self, text: str) -> None:
        """Load clauses from a string into the program.

        Args:
            text: Prolog program text to parse and load
        """
        clauses = parse_program(text)
        # Create new program with existing clauses plus new ones
        all_clauses = list(self.program.clauses) + clauses
        self._set_program_clauses(all_clauses)

    # --- Dynamic database helpers and builtins ---
    def _set_program_clauses(self, clauses: list[Clause]) -> None:
        """Replace the engine program with given clauses, preserving indexing mode."""
        if self.use_indexing:
            self.program = IndexedProgram.from_clauses(clauses)
        else:
            self.program = ProgramClass(tuple(clauses))

    def _parse_pred_indicator(self, term: Term) -> Optional[tuple[str, int]]:
        """Parse a predicate indicator Name/Arity into (name, arity)."""
        if isinstance(term, Struct) and term.functor == "/" and len(term.args) == 2:
            name_t, arity_t = term.args
            if isinstance(name_t, Atom) and isinstance(arity_t, Int):
                return (name_t.name, int(arity_t.value))
        return None

    def _flatten_conjunction(self, term: Term) -> tuple[Term, ...]:
        """Flatten a conjunction term into a tuple of goals."""
        if isinstance(term, Struct) and term.functor == "," and len(term.args) == 2:
            left, right = term.args
            return self._flatten_conjunction(left) + self._flatten_conjunction(right)
        return (term,)

    def _term_to_clause(self, term: Term) -> Optional[Clause]:
        """Convert a term to a Clause (fact or rule)."""
        if isinstance(term, (Atom, Struct)) and not (
            isinstance(term, Struct) and term.functor == ":-"
        ):
            return Clause(term, tuple())
        if isinstance(term, Struct) and term.functor == ":-" and len(term.args) == 2:
            head_t, body_t = term.args
            if isinstance(head_t, (Atom, Struct)):
                body_goals = self._flatten_conjunction(body_t)
                return Clause(head_t, tuple(body_goals))
        return None

    def _require_dynamic(self, key: tuple[str, int]) -> bool:
        return key in self._dynamic_preds

    def _builtin_dynamic(self, args: tuple) -> bool:
        if len(args) != 1:
            return False
        term = args[0]
        # Allow single Name/Arity or list of Name/Arity
        single = self._parse_pred_indicator(term)
        if single:
            self._dynamic_preds.add(single)
            return True
        # List form: dynamic([p/1, q/2, ...])
        if isinstance(term, PrologList):
            items = self._prolog_list_to_python_list(term)
            if items is None:
                return False
            preds: list[tuple[str, int]] = []
            for it in items:
                pi = self._parse_pred_indicator(it)
                if not pi:
                    return False
                preds.append(pi)
            # All valid — add them
            for pi in preds:
                self._dynamic_preds.add(pi)
            return True
        # Comma-conjunction form: dynamic((p/1, q/2)) or nested ,
        if isinstance(term, Struct) and term.functor == ",":
            items = self._flatten_conjunction(term)
            preds: list[tuple[str, int]] = []
            for it in items:
                pi = self._parse_pred_indicator(it)
                if not pi:
                    return False
                preds.append(pi)
            for pi in preds:
                self._dynamic_preds.add(pi)
            return True
        return False

    def _builtin_assert(self, args: tuple, append: bool = True) -> bool:
        if len(args) != 1:
            return False
        cl = self._term_to_clause(args[0])
        if cl is None:
            return False
        head = cl.head
        if isinstance(head, Atom):
            key = (head.name, 0)
        else:
            key = (head.functor, len(head.args))  # type: ignore
        if not self._require_dynamic(key):
            return False
        clauses = list(self.program.clauses)
        if append:
            # insert after last same-predicate clause
            insert_pos = len(clauses)
            for i in range(len(clauses) - 1, -1, -1):
                h = clauses[i].head
                if isinstance(h, Atom):
                    k = (h.name, 0)
                else:
                    k = (h.functor, len(h.args))  # type: ignore
                if k == key:
                    insert_pos = i + 1
                    break
            clauses.insert(insert_pos, cl)
        else:
            # insert before first same-predicate clause
            insert_pos = 0
            for i, existing in enumerate(clauses):
                h = existing.head
                if isinstance(h, Atom):
                    k = (h.name, 0)
                else:
                    k = (h.functor, len(h.args))  # type: ignore
                if k == key:
                    insert_pos = i
                    break
                insert_pos = i + 1
            clauses.insert(insert_pos, cl)
        self._set_program_clauses(clauses)
        return True

    def _builtin_retract(self, args: tuple) -> bool:
        if len(args) != 1:
            return False
        pattern = args[0]
        key = self._extract_predicate_key(pattern)
        if key is None or not self._require_dynamic(key):
            return False

        # Try unification against each clause; bind variables in pattern on success
        clauses = list(self.program.clauses)
        for i, existing in enumerate(clauses):
            # Quick skip if different predicate
            eh = existing.head
            ek = self._clause_key_from_head(eh)
            if ek != key:
                continue

            clause_term = self._build_clause_term_copy(existing)

            # Try unify pattern with clause_term; keep bindings on success
            trail_pos = self.trail.position()
            trail_adapter = TrailAdapter(self.trail)
            if unify(
                pattern, clause_term, self.store, trail_adapter, self.occurs_check
            ):
                # Remove the clause and succeed
                del clauses[i]
                self._set_program_clauses(clauses)
                return True
            else:
                # Undo bindings and try next
                self.trail.unwind_to(trail_pos, self.store)
        return False

    def _builtin_abolish(self, args: tuple) -> bool:
        if len(args) != 1:
            return False
        term = args[0]
        pis: list[tuple[str, int]] = []
        single = self._parse_pred_indicator(term)
        if single:
            pis = [single]
        elif isinstance(term, PrologList):
            items = self._prolog_list_to_python_list(term)
            if items is None:
                return False
            for it in items:
                pi = self._parse_pred_indicator(it)
                if not pi:
                    return False
                pis.append(pi)
        elif isinstance(term, Struct) and term.functor == ",":
            for it in self._flatten_conjunction(term):
                pi = self._parse_pred_indicator(it)
                if not pi:
                    return False
                pis.append(pi)
        else:
            return False

        # Check dynamic for all
        for pred in pis:
            if not self._require_dynamic(pred):
                return False

        # Remove all clauses for each pred
        remaining = list(self.program.clauses)
        to_remove = set(pis)
        new_clauses = []
        for cl in remaining:
            if isinstance(cl.head, Atom):
                k = (cl.head.name, 0)
            else:
                k = (cl.head.functor, len(cl.head.args))  # type: ignore
            if k in to_remove:
                continue
            new_clauses.append(cl)
        self._set_program_clauses(new_clauses)
        return True

    def _builtin_retractall(self, args: tuple) -> bool:
        """retractall(+HeadOrClause) — remove all matching clauses for a dynamic predicate.

        Matches by unification against the full clause term if provided (Head :- Body),
        or just the Head if a head-only term is given. Does not bind variables in the
        query (bindings are unwound after matching).
        """
        if len(args) != 1:
            return False
        pattern = args[0]
        # Extract key for candidate filtering; also detect if pattern includes a body
        key = self._extract_predicate_key(pattern)
        if key is None or not self._require_dynamic(key):
            return False

        # Use a temporary store+trail per clause match to avoid touching the engine store/trail
        remaining: list[Clause] = []
        for cl in self.program.clauses:
            # Skip non-key predicates quickly
            eh = cl.head
            ek = self._clause_key_from_head(eh)
            if ek != key:
                remaining.append(cl)
                continue

            # Build terms for matching within a temporary store
            temp_store = Store()
            # Copy clause into temp store
            clause_term = self._build_clause_term_copy(cl, target_store=temp_store)
            # Copy pattern into temp store
            pat_copy = self._copy_term_recursive(pattern, {}, target_store=temp_store)
            # For head-only patterns and rules, match on head only
            candidate: Term = clause_term
            if (
                isinstance(pat_copy, (Atom, Struct))
                and not (isinstance(pat_copy, Struct) and pat_copy.functor == ":-")
                and isinstance(clause_term, Struct)
                and clause_term.functor == ":-"
            ):
                candidate = clause_term.args[0]

            temp_trail = Trail()
            temp_adapter = TrailAdapter(temp_trail)
            if unify(pat_copy, candidate, temp_store, temp_adapter, self.occurs_check):
                # Match: drop clause (do not keep)
                continue
            else:
                remaining.append(cl)

        # Update program if any clause was removed
        if len(remaining) != len(self.program.clauses):
            self._set_program_clauses(remaining)

        return True

    # --- Helpers (dynamic DB) ---
    def _clause_key_from_head(self, head: Term) -> tuple[str, int]:
        """Compute (name, arity) from a clause head (Atom or Struct)."""
        if isinstance(head, Atom):
            return (head.name, 0)
        assert isinstance(head, Struct)
        return (head.functor, len(head.args))

    def _extract_predicate_key(self, pattern: Term) -> Optional[tuple[str, int]]:
        """Extract predicate (name, arity) from a head or Head :- Body pattern."""
        if isinstance(pattern, Atom):
            return (pattern.name, 0)
        if isinstance(pattern, Struct) and pattern.functor != ":-":
            return (pattern.functor, len(pattern.args))
        if (
            isinstance(pattern, Struct)
            and pattern.functor == ":-"
            and len(pattern.args) == 2
            and isinstance(pattern.args[0], (Atom, Struct))
        ):
            head_t = pattern.args[0]
            if isinstance(head_t, Atom):
                return (head_t.name, 0)
            return (head_t.functor, len(head_t.args))
        return None

    def _build_clause_term_copy(
        self, existing: Clause, target_store: Optional[Store] = None
    ) -> Term:
        """Copy a clause (fact or rule) into a term; optionally into target_store."""
        var_map: Dict[int, int] = {}
        head_copy = self._copy_term_recursive(
            existing.head, var_map, target_store=target_store
        )
        if existing.body:
            # Rebuild conjunction from body goals: g1, (g2, (...))
            body_term: Optional[Term] = None
            for g in reversed(existing.body):
                g_copy = self._copy_term_recursive(
                    g, var_map, target_store=target_store
                )
                body_term = (
                    g_copy if body_term is None else Struct(",", (g_copy, body_term))
                )
            return Struct(":-", (head_copy, body_term))  # type: ignore
        else:
            return head_copy

    # Dict builtin predicates

    def _builtin_dict_create(self, args: tuple) -> bool:
        """dict_create(-Dict, +Tag, +Data) - create dict from key-value data.

        Args:
            args: (dict_var, tag, data_list)

        Returns:
            True if dict created successfully, False otherwise
        """
        if len(args) != 3:
            return False

        dict_arg, tag_arg, data_arg = args

        # Tag must be 'none' for now (anonymous dicts only)
        if not isinstance(tag_arg, Atom) or tag_arg.name != "none":
            return False

        # Data must be a list
        if not isinstance(data_arg, PrologList):
            return False

        # Extract key-value pairs from the data list
        try:
            pairs = self._extract_dict_pairs(data_arg)
        except (ValueError, TypeError):
            return False

        # Create the dict
        try:
            result_dict = PrologDict(pairs)
        except ValueError:
            # Duplicate keys or invalid key types
            return False

        # Unify with the dict argument
        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
        return unify(dict_arg, result_dict, self.store, trail_adapter)

    def _builtin_get_dict(self, args: tuple) -> bool:
        """get_dict(+Key, +Dict, -Value) - get value for key from dict.

        Args:
            args: (key, dict, value)

        Returns:
            True if key found and value unified, False otherwise
        """
        if len(args) != 3:
            return False

        key_arg, dict_arg, value_arg = args

        # Dict must be a PrologDict
        if not isinstance(dict_arg, PrologDict):
            return False

        # Key must be ground and valid type
        if isinstance(key_arg, Var):
            return False
        if not isinstance(key_arg, (Atom, Int)):
            return False

        # Look up the value using binary search
        value = self._dict_lookup(dict_arg, key_arg)
        if value is None:
            return False

        # Unify with the value argument
        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
        return unify(value_arg, value, self.store, trail_adapter)

    def _builtin_put_dict(self, args: tuple) -> bool:
        """put_dict(+KeyValuePairs, +DictIn, -DictOut) - add/update pairs in dict.

        Args:
            args: (pairs_list, input_dict, output_dict)

        Returns:
            True if dict updated successfully, False otherwise
        """
        if len(args) != 3:
            return False

        pairs_arg, dict_in_arg, dict_out_arg = args

        # Input dict must be a PrologDict
        if not isinstance(dict_in_arg, PrologDict):
            return False

        # Pairs must be a list
        if not isinstance(pairs_arg, PrologList):
            return False

        # Extract new key-value pairs (allow duplicates for put_dict)
        try:
            new_pairs = self._extract_dict_pairs(pairs_arg, allow_duplicates=True)
        except (ValueError, TypeError):
            return False

        # Merge with existing dict
        try:
            merged_pairs = self._merge_dict_pairs(dict_in_arg.pairs, new_pairs)
            result_dict = PrologDict(merged_pairs)
        except ValueError:
            return False

        # Unify with the output argument
        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
        return unify(dict_out_arg, result_dict, self.store, trail_adapter)

    def _extract_dict_pairs(
        self, data_list: PrologList, allow_duplicates: bool = False
    ) -> Tuple[Tuple[Term, Term], ...]:
        """Extract key-value pairs from various formats in a list.

        Supports:
        - key-value: [a-1, b-2]
        - key:value: [a:1, b:2]
        - key(value): [a(1), b(2)]

        Args:
            data_list: List containing key-value data
            allow_duplicates: If True, allows duplicate keys (for put_dict)

        Returns:
            Tuple of (key, value) pairs

        Raises:
            ValueError: If invalid format or duplicate keys (when not allowed)
        """
        pairs = []
        seen_keys = set()

        for item in data_list.items:
            key, value = None, None

            if isinstance(item, Struct):
                if item.functor == "-" and len(item.args) == 2:
                    # key-value format
                    key, value = item.args
                elif item.functor == ":" and len(item.args) == 2:
                    # key:value format
                    key, value = item.args
                elif len(item.args) == 1:
                    # key(value) format
                    key = Atom(item.functor)
                    value = item.args[0]
                else:
                    raise ValueError(f"Invalid pair format: {item}")
            else:
                raise ValueError(f"Invalid pair format: {item}")

            # Validate key type
            if not isinstance(key, (Atom, Int)):
                raise ValueError(f"Invalid key type: {key}")

            # Check for duplicates only if not allowed
            if not allow_duplicates and key in seen_keys:
                raise ValueError(f"Duplicate key: {key}")
            seen_keys.add(key)

            pairs.append((key, value))

        return tuple(pairs)

    def _dict_lookup(self, dict_term: PrologDict, key: Term) -> Optional[Term]:
        """Look up a value in a dict using binary search.

        Args:
            dict_term: The dict to search
            key: The key to find

        Returns:
            The value if found, None otherwise
        """
        pairs = dict_term.pairs
        left, right = 0, len(pairs) - 1

        while left <= right:
            mid = (left + right) // 2
            mid_key = pairs[mid][0]

            cmp = self._key_less_than(key, mid_key)
            if cmp == 0:  # Equal
                return pairs[mid][1]
            elif cmp < 0:  # key < mid_key
                right = mid - 1
            else:  # key > mid_key
                left = mid + 1

        return None

    def _key_less_than(self, key1: Term, key2: Term) -> int:
        """Compare two dict keys for sorting.

        Args:
            key1, key2: Keys to compare

        Returns:
            < 0 if key1 < key2, 0 if equal, > 0 if key1 > key2
        """

        # Get sort values (atoms sort before integers)
        def sort_value(key):
            if isinstance(key, Atom):
                return (0, key.name)
            elif isinstance(key, Int):
                return (1, key.value)
            else:
                raise ValueError(f"Invalid key type: {key}")

        val1 = sort_value(key1)
        val2 = sort_value(key2)

        if val1 < val2:
            return -1
        elif val1 > val2:
            return 1
        else:
            return 0

    def _merge_dict_pairs(
        self,
        existing_pairs: Tuple[Tuple[Term, Term], ...],
        new_pairs: Tuple[Tuple[Term, Term], ...],
    ) -> Tuple[Tuple[Term, Term], ...]:
        """Merge new pairs with existing dict pairs.

        Args:
            existing_pairs: Existing key-value pairs (sorted)
            new_pairs: New key-value pairs to add/update

        Returns:
            Merged pairs (sorted)
        """
        # Convert to dict for easy merging (last value wins for duplicates)
        merged = {}

        # Add existing pairs
        for key, value in existing_pairs:
            merged[key] = value

        # Add/update with new pairs (last wins for duplicates in new_pairs)
        for key, value in new_pairs:
            merged[key] = value

        # Convert back to sorted tuple using our key comparison
        def sort_key(pair):
            key = pair[0]
            if isinstance(key, Atom):
                return (0, key.name)
            elif isinstance(key, Int):
                return (1, key.value)
            else:
                raise ValueError(f"Invalid key type: {key}")

        sorted_pairs = sorted(merged.items(), key=sort_key)
        return tuple(sorted_pairs)

    # JSON builtin predicates

    def _builtin_json_read(self, args: tuple) -> bool:
        """json_read(+Stream, -Term, +Options) - read JSON from stream.

        Args:
            args: (stream, term, options) where stream is input stream,
                  term is variable to unify with result, options is option list

        Returns:
            True if JSON was successfully read and unified, False otherwise
        """
        if len(args) != 3:
            return False

        stream_arg, term_arg, options_arg = args

        # Check if stream argument supports reading
        if not hasattr(stream_arg, "read"):
            return False

        try:
            # Read JSON text from stream
            json_text = stream_arg.read()

            # Parse JSON
            json_obj = json.loads(json_text)

            # Convert to Prolog term (classic mode by default)
            mode = self._extract_json_mode(options_arg, CLASSIC_MODE)
            constants = self._extract_json_constants(options_arg, mode)
            prolog_term = json_to_prolog(json_obj, mode, constants)

            # Unify with term argument
            trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
            return unify(term_arg, prolog_term, self.store, trail_adapter)

        except (json.JSONDecodeError, ValueError, IOError):
            # Failed to parse JSON or convert
            return False
        except Exception:
            # Unexpected error
            return False

    def _builtin_json_write(self, args: tuple) -> bool:
        """json_write(+Stream, +Term, +Options) - write JSON to stream.

        Args:
            args: (stream, term, options) where stream is output stream,
                  term is term to convert to JSON, options is option list

        Returns:
            True if term was successfully converted and written, False otherwise
        """
        if len(args) != 3:
            return False

        stream_arg, term_arg, options_arg = args

        # Check if stream argument supports writing
        if not hasattr(stream_arg, "write"):
            return False

        try:
            # Convert Prolog term to JSON (classic mode by default)
            mode = self._extract_json_mode(options_arg, CLASSIC_MODE)
            constants = self._extract_json_constants(options_arg, mode)
            json_obj = prolog_to_json(term_arg, mode, constants)

            # Write JSON to stream
            json_text = json.dumps(json_obj, allow_nan=False)
            stream_arg.write(json_text)

            return True

        except (ValueError, IOError):
            # Failed to convert term or write to stream
            return False
        except Exception:
            # Unexpected error
            return False

    def _builtin_json_read_dict(self, args: tuple) -> bool:
        """json_read_dict(+Stream, -Dict, +Options) - read JSON as dict.

        Args:
            args: (stream, dict, options) where stream is input stream,
                  dict is variable to unify with result, options is option list

        Returns:
            True if JSON was successfully read and unified, False otherwise
        """
        if len(args) != 3:
            return False

        stream_arg, dict_arg, options_arg = args

        # Check if stream argument supports reading
        if not hasattr(stream_arg, "read"):
            return False

        try:
            # Read JSON text from stream
            json_text = stream_arg.read()

            # Parse JSON
            json_obj = json.loads(json_text)

            # Convert to Prolog term (dict mode)
            mode = self._extract_json_mode(options_arg, DICT_MODE)
            constants = self._extract_json_constants(options_arg, mode)
            prolog_term = json_to_prolog(json_obj, mode, constants)

            # Unify with dict argument
            trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
            return unify(dict_arg, prolog_term, self.store, trail_adapter)

        except (json.JSONDecodeError, ValueError, IOError):
            # Failed to parse JSON or convert
            return False
        except Exception:
            # Unexpected error
            return False

    def _builtin_json_write_dict(self, args: tuple) -> bool:
        """json_write_dict(+Stream, +Dict, +Options) - write dict as JSON.

        Args:
            args: (stream, dict, options) where stream is output stream,
                  dict is term to convert to JSON, options is option list

        Returns:
            True if dict was successfully converted and written, False otherwise
        """
        if len(args) != 3:
            return False

        stream_arg, dict_arg, options_arg = args

        # Check if stream argument supports writing
        if not hasattr(stream_arg, "write"):
            return False

        try:
            # Convert Prolog term to JSON (dict mode)
            mode = self._extract_json_mode(options_arg, DICT_MODE)
            constants = self._extract_json_constants(options_arg, mode)
            json_obj = prolog_to_json(dict_arg, mode, constants)

            # Write JSON to stream
            json_text = json.dumps(json_obj, allow_nan=False)
            stream_arg.write(json_text)

            return True

        except (ValueError, IOError):
            # Failed to convert term or write to stream
            return False
        except Exception:
            # Unexpected error
            return False

    def _builtin_atom_json_term(self, args: tuple) -> bool:
        """atom_json_term(?Atom, ?Term, +Options) - convert between atom and term.

        Args:
            args: (atom, term, options) where atom and term can be bound or unbound,
                  options is option list

        Returns:
            True if conversion and unification succeeded, False otherwise
        """
        if len(args) != 3:
            return False

        atom_arg, term_arg, options_arg = args
        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)

        # Determine conversion mode
        mode = self._extract_json_mode(options_arg, CLASSIC_MODE)
        constants = self._extract_json_constants(options_arg, mode)

        try:
            # Check what's bound - need to handle dereferencing properly
            if isinstance(atom_arg, Var):
                atom_deref_result = self.store.deref(atom_arg.id)
                if atom_deref_result[0] == "BOUND":
                    atom_deref = atom_deref_result[2]
                else:
                    atom_deref = atom_arg  # Still a variable
            else:
                atom_deref = atom_arg

            if isinstance(term_arg, Var):
                term_deref_result = self.store.deref(term_arg.id)
                if term_deref_result[0] == "BOUND":
                    term_deref = term_deref_result[2]
                else:
                    term_deref = term_arg  # Still a variable
            else:
                term_deref = term_arg

            if isinstance(atom_deref, Atom) and not isinstance(term_deref, Var):
                # Both bound - check consistency
                try:
                    # Parse atom as JSON and convert to term
                    json_obj = json.loads(atom_deref.name)
                    expected_term = json_to_prolog(json_obj, mode, constants)
                    return unify(term_arg, expected_term, self.store, trail_adapter)
                except (json.JSONDecodeError, ValueError):
                    return False

            elif isinstance(atom_deref, Atom):
                # Atom bound, term unbound - convert atom to term
                try:
                    json_obj = json.loads(atom_deref.name)
                    prolog_term = json_to_prolog(json_obj, mode, constants)
                    return unify(term_arg, prolog_term, self.store, trail_adapter)
                except (json.JSONDecodeError, ValueError):
                    return False

            elif not isinstance(term_deref, Var):
                # Term bound, atom unbound - convert term to atom
                try:
                    json_obj = prolog_to_json(term_deref, mode, constants)
                    json_text = json.dumps(json_obj, allow_nan=False)
                    atom_result = Atom(json_text)
                    return unify(atom_arg, atom_result, self.store, trail_adapter)
                except ValueError:
                    return False

            else:
                # Both unbound - cannot proceed
                return False

        except Exception:
            # Unexpected error
            return False

    def _extract_json_mode(self, options_arg: Term, default: str) -> str:
        """Extract JSON mode from options list.

        Args:
            options_arg: Options term (should be a list)
            default: Default mode if not specified

        Returns:
            Mode string (CLASSIC_MODE or DICT_MODE)
        """
        try:
            if isinstance(options_arg, PrologList):
                for option in options_arg.items:
                    if (
                        isinstance(option, Struct)
                        and option.functor == "mode"
                        and len(option.args) == 1
                    ):
                        mode_arg = option.args[0]
                        if isinstance(mode_arg, Atom):
                            if mode_arg.name == "classic":
                                return CLASSIC_MODE
                            elif mode_arg.name == "dict":
                                return DICT_MODE
                    # SWI-Prolog compatibility: json_object(dict|term)
                    elif (
                        isinstance(option, Struct)
                        and option.functor == "json_object"
                        and len(option.args) == 1
                    ):
                        json_object_arg = option.args[0]
                        if isinstance(json_object_arg, Atom):
                            if json_object_arg.name == "dict":
                                return DICT_MODE
                            elif json_object_arg.name == "term":
                                return CLASSIC_MODE
            return default
        except Exception:
            return default

    def _extract_json_constants(
        self, options_arg: Term, mode: str
    ) -> Optional[Dict[str, Term]]:
        """Extract custom constants from options list.

        Args:
            options_arg: Options term (should be a list)
            mode: Current JSON mode

        Returns:
            Custom constants dict or None for default
        """
        try:
            if isinstance(options_arg, PrologList):
                for option in options_arg.items:
                    if (
                        isinstance(option, Struct)
                        and option.functor == "constants"
                        and len(option.args) == 1
                    ):
                        # For now, return None to use defaults
                        # Full custom constants implementation could be added later
                        pass
            return None
        except Exception:
            return None

    def query(self, query_text: str) -> List[Dict[str, Any]]:
        """Execute a query and return all solutions.

        Args:
            query_text: Query string like "p(X), q(Y)" (without ?- and .)

        Returns:
            List of solution dictionaries
        """
        # Add ?- and . if not present
        if not query_text.strip().startswith("?-"):
            query_text = "?- " + query_text
        if not query_text.strip().endswith("."):
            query_text = query_text + "."
        goals = parse_query(query_text)
        return self.run(goals)
