"""Prolog Engine with Explicit Stacks (Stage 0) - Fixed Implementation."""

from typing import Dict, List, Optional, Any, Tuple
from prolog.ast.terms import Term, Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Program, ClauseCursor
from prolog.engine.rename import VarRenamer
from prolog.unify.store import Store, Cell
from prolog.unify.unify import unify

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
from prolog.engine.errors import PrologThrow


class Engine:
    """Prolog inference engine with explicit stacks (no Python recursion)."""

    def __init__(
        self,
        program: Program,
        occurs_check: bool = False,
        max_solutions: Optional[int] = None,
        trace: bool = False,
        max_steps: Optional[int] = None,
    ):
        """Initialize engine with a program.

        Args:
            program: The Prolog program to execute.
            occurs_check: Whether to use occurs check in unification.
            max_solutions: Maximum number of solutions to find.
            trace: Whether to enable tracing.
            max_steps: Maximum number of steps to execute (for debugging infinite loops).
        """
        self.program = program
        self.occurs_check = occurs_check

        # Core state - using new runtime types
        self.store = Store()
        self.trail = Trail()
        self.goal_stack = GoalStack()
        self.frame_stack: List[Frame] = []
        self.cp_stack: List[Choicepoint] = []

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

        # Builtin registry: maps (name, arity) -> callable
        self._builtins = {}
        self._register_builtins()

        # Debug counters and frame tracking
        self._debug_frame_pops = 0
        self._debug_trail_writes = 0
        self._next_frame_id = 0  # Monotonic frame ID counter
        self._cut_barrier = None  # Legacy field for tests
        
        # Exception handling
        self._exception = None  # Current uncaught exception
        self._catch_frames = []  # Stack of active catch frames

        # Debug ports for tracing
        self._ports: List[str] = []

        # Variable renaming for clause isolation
        self._renamer = VarRenamer(self.store)

    def reset(self):
        """Reset engine state for reuse."""
        self.store = Store()
        self.trail = Trail()
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
        self._exception = None  # Clear any exception
        self._catch_frames = []  # Clear catch frames
        # Don't reset ports - they accumulate across runs

    def _register_builtins(self):
        """Register all builtin predicates.

        All builtins use uniform signature: fn(engine, args_tuple) -> bool
        This simplifies dispatch and makes nondeterministic builtins easier.
        """
        # Core control flow
        self._builtins[("true", 0)] = lambda eng, args: True
        self._builtins[("fail", 0)] = lambda eng, args: False
        self._builtins[("!", 0)] = lambda eng, args: eng._builtin_cut(args)
        self._builtins[("call", 1)] = lambda eng, args: eng._builtin_call(args)
        self._builtins[("=", 2)] = lambda eng, args: eng._builtin_unify(args)
        self._builtins[("\\=", 2)] = lambda eng, args: eng._builtin_not_unify(args)
        self._builtins[("var", 1)] = lambda eng, args: eng._builtin_var(args)
        self._builtins[("nonvar", 1)] = lambda eng, args: eng._builtin_nonvar(args)
        self._builtins[("atom", 1)] = lambda eng, args: eng._builtin_atom(args)
        self._builtins[("is", 2)] = lambda eng, args: eng._builtin_is(args)
        self._builtins[(">", 2)] = lambda eng, args: eng._builtin_gt(args)
        self._builtins[("=:=", 2)] = lambda eng, args: eng._builtin_num_eq(args)
        self._builtins[("=..", 2)] = lambda eng, args: eng._builtin_univ(args)
        self._builtins[("functor", 3)] = lambda eng, args: eng._builtin_functor(args)
        self._builtins[("arg", 3)] = lambda eng, args: eng._builtin_arg(args)
        self._builtins[("once", 1)] = lambda eng, args: eng._builtin_once(args)
        self._builtins[("throw", 1)] = lambda eng, args: eng._builtin_throw(args)
        self._builtins[("catch", 3)] = lambda eng, args: eng._builtin_catch(args)

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
            self.goal_stack.shrink_to(0)
            self.frame_stack.clear()
            self.cp_stack.clear()
            return self.solutions

        # Reset step counter for new query
        self._steps_taken = 0

        # Push initial goals (in reverse for left-to-right execution)
        for goal in reversed(renamed_goals):
            g = Goal.from_term(goal)
            self.goal_stack.push(g)

        # Main single iterative loop
        while True:
            # Check step budget
            if self.max_steps is not None:
                self._steps_taken += 1
                if self._steps_taken > self.max_steps:
                    # Step budget exceeded - stop execution
                    break
            
            # Check for exception during goal execution
            if self._exception is not None:
                # Exception is propagating - check for catch frames
                catch_found = False
                
                # Try each catch frame from innermost to outermost
                while self._catch_frames:
                    catch_frame = self._catch_frames.pop()
                    
                    # Try to unify the exception with the catcher
                    trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
                    saved_trail = self.trail.position()
                    
                    if unify(self._exception, catch_frame["catcher"], self.store, trail_adapter, self.occurs_check):
                        # Exception caught!
                        self._exception = None
                        # Restore state to where catch was invoked
                        self.goal_stack.shrink_to(catch_frame["goal_height"])
                        # Restore frame stack to catch point
                        while len(self.frame_stack) > catch_frame["frame_height"]:
                            self.frame_stack.pop()
                        # Remove choicepoints created by the throwing goal
                        while len(self.cp_stack) > catch_frame["cp_height"]:
                            self.cp_stack.pop()
                        # Push recovery goal
                        self.goal_stack.push(Goal.from_term(catch_frame["recovery"]))
                        catch_found = True
                        break
                    else:
                        # Doesn't match this catcher - restore and try next
                        self.trail.unwind_to(saved_trail, self.store)
                
                if catch_found:
                    # Exception was caught, continue execution
                    continue
                else:
                    # Exception not caught - it will propagate out
                    # Clear all execution state
                    self.cp_stack.clear()
                    self.goal_stack.shrink_to(0)
                    break  # Exit main loop with exception
            
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

            # Trace if enabled
            if self.trace:
                self._trace_log.append(f"Goal: {goal.term}")

            # Dispatch based on goal type
            if goal.type == GoalType.PREDICATE:
                # Check if it's actually a builtin (PREDICATE goals always have terms)
                if goal.term and self._is_builtin(goal.term):
                    if not self._dispatch_builtin(goal):
                        if not self._backtrack():
                            break
                else:
                    if not self._dispatch_predicate(goal):
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

        # Check for uncaught exception before cleanup
        if self._exception is not None:
            exc = self._exception
            self._exception = None
            # Clean up state before raising
            self.trail.unwind_to(0, self.store)
            self.goal_stack.shrink_to(0)
            self.frame_stack.clear()
            self.cp_stack.clear()
            raise PrologThrow(exc)
        
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

        # Clean up all state before returning
        self.trail.unwind_to(0, self.store)
        self.goal_stack.shrink_to(0)
        self.frame_stack.clear()
        self.cp_stack.clear()

        return self.solutions

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

            elif isinstance(current, (Atom, Int)):
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
            else:
                raise TypeError(f"Unknown term type: {type(current)}")

        return processed[id(term)]

    def _dispatch_predicate(self, goal: Goal) -> bool:
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

        # Emit CALL port before processing
        self._port("CALL", f"{functor}/{arity}")

        # Get matching clauses
        matches = self.program.clauses_for(functor, arity)
        cursor = ClauseCursor(matches=matches, functor=functor, arity=arity)

        if not cursor.has_more():
            # No matching clauses - emit FAIL port
            self._port("FAIL", f"{functor}/{arity}")
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
            self.trail.next_stamp()

            # Save the continuation (goals below the call) as an immutable snapshot
            continuation = self.goal_stack.snapshot()

            # Save store size for allocation cleanup
            store_top = self.store.size()

            # Debug: log what we're saving
            if self.trace:
                self._trace_log.append(
                    f"Creating PREDICATE CP for {functor}/{arity}, saving {len(continuation)} continuation goals"
                )

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
                    "store_top": store_top,  # Store size for allocation cleanup
                },
            )
            self.cp_stack.append(cp)

        # Rename clause with fresh variables
        renamed_clause = self._renamer.rename_clause(clause)

        # Try to unify with clause head
        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
        if unify(
            renamed_clause.head,
            goal.term,
            self.store,
            trail_adapter,  # type: ignore
            occurs_check=self.occurs_check,
        ):
            # Unification succeeded - now push frame for body execution
            # Use the cut_barrier saved before creating choicepoint
            frame_id = self._next_frame_id
            self._next_frame_id += 1
            frame = Frame(
                frame_id=frame_id,
                cut_barrier=cut_barrier,
                goal_height=self.goal_stack.height(),
                pred=f"{functor}/{arity}",
            )
            self.frame_stack.append(frame)

            # Don't call next_stamp() here - only when creating CPs

            # Push POP_FRAME sentinel first, then body goals
            self.goal_stack.push(
                Goal(
                    GoalType.POP_FRAME,
                    None,  # Internal goals don't need terms
                    payload={"op": "POP_FRAME", "frame_id": frame_id},
                )
            )

            # Push body goals in reverse order
            for body_term in reversed(renamed_clause.body):
                body_goal = Goal.from_term(body_term)
                self.goal_stack.push(body_goal)
            return True
        else:
            # Unification failed - no frame was created
            return False

    def _dispatch_builtin(self, goal: Goal) -> bool:
        """Dispatch a builtin goal.

        Args:
            goal: The builtin goal to dispatch.

        Returns:
            True if successful, False if failed.
        """
        if isinstance(goal.term, Atom):
            key = (goal.term.name, 0)
            args = ()
        elif isinstance(goal.term, Struct):
            key = (goal.term.functor, len(goal.term.args))
            args = goal.term.args
        else:
            return False

        builtin_fn = self._builtins.get(key)
        if builtin_fn is None:
            # Not a recognized builtin
            return False

        # Execute the builtin with uniform signature
        try:
            return builtin_fn(self, args)
        except (ValueError, TypeError) as e:
            # Expected failures from arithmetic or type errors
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
            self.goal_stack.push(right_goal)
            self.goal_stack.push(left_goal)

    def _dispatch_disjunction(self, goal: Goal):
        """Dispatch a disjunction goal.

        Args:
            goal: The disjunction goal to dispatch.
        """
        # (A ; B) - try A first, create choicepoint for B
        disj = goal.term
        if isinstance(disj, Struct) and disj.functor == ";" and len(disj.args) == 2:
            left, right = disj.args

            # Create choicepoint for right alternative
            # Save continuation for re-execution after alternative
            continuation = self.goal_stack.snapshot()

            self.trail.next_stamp()
            cp = Choicepoint(
                kind=ChoicepointKind.DISJUNCTION,
                trail_top=self.trail.position(),
                goal_stack_height=len(continuation),  # Height = continuation length
                frame_stack_height=len(self.frame_stack),
                payload={
                    "alternative": Goal.from_term(right),
                    "continuation": continuation,  # Save continuation for backtrack
                },
            )
            self.cp_stack.append(cp)

            # Try left first
            left_goal = Goal.from_term(left)
            self.goal_stack.push(left_goal)

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
        self.cp_stack.append(
            Choicepoint(
                kind=ChoicepointKind.IF_THEN_ELSE,
                trail_top=self.trail.position(),
                goal_stack_height=self.goal_stack.height(),
                frame_stack_height=len(self.frame_stack),
                payload={
                    "else_goal": Goal.from_term(else_term),
                    "tmp_barrier": tmp_barrier,
                },
            )
        )

        # Push control goal that will commit and run Then on first success
        self.goal_stack.push(
            Goal(
                GoalType.CONTROL,
                None,  # Internal control goal
                payload={
                    "op": "ITE_THEN",
                    "tmp_barrier": tmp_barrier,
                    "then_goal": Goal.from_term(then_term),
                },
            )
        )

        # Push condition to evaluate
        self.goal_stack.push(Goal.from_term(cond))

        return True

    def _dispatch_cut(self):
        """Execute cut (!) - remove choicepoints up to cut barrier."""
        if self.frame_stack:
            # Normal cut within a predicate
            current_frame = self.frame_stack[-1]
            cut_barrier = current_frame.cut_barrier

            # Remove all choicepoints above the barrier
            while len(self.cp_stack) > cut_barrier:
                self.cp_stack.pop()
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

        Args:
            goal: The POP_FRAME goal with frame_id payload.
        """
        frame_id = goal.payload.get("frame_id")
        # Sanity check: should be popping the frame with matching ID
        if self.frame_stack and self.frame_stack[-1].frame_id == frame_id:
            frame = self.frame_stack.pop()
            self._debug_frame_pops += 1
            # Emit EXIT port when frame is popped
            self._port("EXIT", frame.pred or "unknown")
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
            self.goal_stack.push(inner_goal)
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
            self.goal_stack.push(goal.payload["then_goal"])
        elif op == "CATCH_BEGIN":
            # Set up a catch frame
            catch_frame = {
                "catch_id": goal.payload["catch_id"],
                "catcher": goal.payload["catcher"],
                "recovery": goal.payload["recovery"],
                "trail_top": goal.payload["trail_top"],
                "goal_height": goal.payload["goal_height"],
                "frame_height": goal.payload["frame_height"],
                "cp_height": goal.payload["cp_height"],
            }
            self._catch_frames.append(catch_frame)
        elif op == "CATCH_END":
            # Goal succeeded normally - catch frame stays active for backtracking
            # We don't remove the catch frame here because if we backtrack
            # into the goal and it throws, we still need to catch it
            pass

    def _backtrack(self) -> bool:
        """Backtrack to the most recent choicepoint.

        Returns:
            True if backtracking succeeded, False if no more choicepoints.
        """
        while self.cp_stack:
            cp = self.cp_stack.pop()

            # Restore state - unwind first, then restore heights
            # This order is safer if future features attach watchers to frames
            self.trail.unwind_to(cp.trail_top, self.store)

            # Restore goal stack to checkpoint height
            # For CPs with continuation, we handle restoration specially in their handlers
            if (
                cp.kind in [ChoicepointKind.PREDICATE, ChoicepointKind.DISJUNCTION]
                and "continuation" in cp.payload
            ):
                # We'll handle goal stack restoration in the CP-specific handler below
                if self.trace:
                    self._trace_log.append(
                        f"Deferring goal stack restoration for {cp.kind} CP"
                    )
            else:
                # Standard restoration for other CP kinds
                if self.trace:
                    self._trace_log.append(
                        f"Restoring goal stack from {self.goal_stack.height()} to {cp.goal_stack_height}"
                    )

                self.goal_stack.shrink_to(cp.goal_stack_height)

                # Assert invariant: goal stack restored correctly
                assert (
                    self.goal_stack.height() == cp.goal_stack_height
                ), f"Goal stack height mismatch after restore: {self.goal_stack.height()} != {cp.goal_stack_height}"

            # Pop frames above checkpoint
            # PREDICATE CPs don't manage frames (they use frame_stack_height=0 as a sentinel)
            if cp.kind != ChoicepointKind.PREDICATE:
                while len(self.frame_stack) > cp.frame_stack_height:
                    self.frame_stack.pop()
                    self._debug_frame_pops += 1

                # Debug assertion: verify frame restoration
                assert (
                    len(self.frame_stack) == cp.frame_stack_height
                ), f"Frame stack height mismatch: {len(self.frame_stack)} != {cp.frame_stack_height}"

            # Resume based on choicepoint kind
            if cp.kind == ChoicepointKind.PREDICATE:
                # Check if this is a terminal CP (just emits FAIL)
                if cp.payload.get("terminal", False):
                    # Terminal CP - just emit FAIL and continue backtracking
                    self._port("FAIL", cp.payload["pred_ref"])
                    continue

                # Emit REDO port before resuming normal predicate
                self._port("REDO", cp.payload["pred_ref"])

                # Try next clause
                goal = cp.payload["goal"]
                cursor = cp.payload["cursor"]

                # Restore goal stack for PREDICATE CPs
                if "continuation" in cp.payload:
                    continuation = cp.payload["continuation"]
                    target_height = cp.goal_stack_height
                    current_height = self.goal_stack.height()

                    if self.trace:
                        self._trace_log.append(
                            f"PREDICATE CP: current_height={current_height}, target_height={target_height}"
                        )

                    # Shrink if needed
                    if current_height > target_height:
                        self.goal_stack.shrink_to(target_height)
                    # Re-push missing goals if needed
                    elif current_height < target_height:
                        for g in continuation[current_height:target_height]:
                            self.goal_stack.push(g)

                        if self.trace:
                            self._trace_log.append(
                                f"Re-pushed {target_height - current_height} continuation goals"
                            )

                    # Assert we restored correctly
                    assert (
                        self.goal_stack.height() == target_height
                    ), f"Failed to restore continuation: height={self.goal_stack.height()}, expected={target_height}"

                if cursor.has_more():
                    clause_idx = cursor.take()
                    clause = self.program.clauses[clause_idx]

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
                            },
                        )
                        self.cp_stack.append(new_cp)
                    else:
                        # No more clauses - push a terminal CP that will emit FAIL
                        continuation = cp.payload.get("continuation", ())
                        terminal_cp = Choicepoint(
                            kind=ChoicepointKind.PREDICATE,
                            trail_top=self.trail.position(),
                            goal_stack_height=len(continuation),
                            frame_stack_height=0,
                            payload={
                                "goal": goal,
                                "cursor": cursor,  # cursor with no more clauses
                                "pred_ref": cp.payload["pred_ref"],
                                "continuation": continuation,
                                "terminal": True,  # Mark as terminal
                            },
                        )
                        self.cp_stack.append(terminal_cp)

                    # Rename clause with fresh variables
                    renamed_clause = self._renamer.rename_clause(clause)

                    # Try to unify with clause head
                    trail_adapter = TrailAdapter(
                        self.trail, engine=self, store=self.store
                    )
                    if unify(
                        renamed_clause.head,
                        goal.term,
                        self.store,
                        trail_adapter,  # type: ignore
                        occurs_check=self.occurs_check,
                    ):
                        # Unification succeeded - create frame for body execution
                        # cut_barrier was already computed above before pushing CP
                        frame_id = self._next_frame_id
                        self._next_frame_id += 1
                        frame = Frame(
                            frame_id=frame_id,
                            cut_barrier=cut_barrier,
                            goal_height=self.goal_stack.height(),
                            pred=cp.payload["pred_ref"],
                        )
                        self.frame_stack.append(frame)

                        # Don't call next_stamp() here - only when creating CPs

                        # Push POP_FRAME sentinel and body goals
                        self.goal_stack.push(
                            Goal(
                                GoalType.POP_FRAME,
                                None,  # Internal goals don't need terms
                                payload={"op": "POP_FRAME", "frame_id": frame_id},
                            )
                        )
                        for body_term in reversed(renamed_clause.body):
                            body_goal = Goal.from_term(body_term)
                            self.goal_stack.push(body_goal)
                        return True
                    else:
                        # Unification failed, continue backtracking
                        # No frame was created
                        continue
                else:
                    # No more clauses to try - emit FAIL port
                    self._port("FAIL", cp.payload["pred_ref"])
                    # Continue backtracking to find earlier choicepoints
                    continue

            elif cp.kind == ChoicepointKind.DISJUNCTION:
                # Restore continuation if present
                if "continuation" in cp.payload:
                    continuation = cp.payload["continuation"]
                    target_height = cp.goal_stack_height
                    current_height = self.goal_stack.height()

                    # Restore goal stack to have continuation
                    if current_height > target_height:
                        self.goal_stack.shrink_to(target_height)
                    elif current_height < target_height:
                        for g in continuation[current_height:target_height]:
                            self.goal_stack.push(g)

                # Try alternative branch
                alternative = cp.payload["alternative"]
                self.goal_stack.push(alternative)
                return True

            elif cp.kind == ChoicepointKind.IF_THEN_ELSE:
                # Try else branch (only reached if condition failed exhaustively)
                else_branch = cp.payload["else_goal"]
                self.goal_stack.push(else_branch)
                return True

        # No more choicepoints
        return False

    def _record_solution(self):
        """Record the current solution (bindings of query variables)."""
        solution = {}

        for varid, var_name in self._query_vars:
            # Reify the variable to get its value
            value = self._reify_var(varid)
            solution[var_name] = value

        self.solutions.append(solution)

    def _reify_var(self, varid: int) -> Any:
        """Follow bindings to get the value of a variable.

        Args:
            varid: The variable ID to reify.

        Returns:
            The ground term or a variable representation.
        """
        # Dereference to find what it's bound to
        result = self.store.deref(varid)

        if result[0] == "UNBOUND":
            # Unbound variable - return a representation
            _, ref = result
            # Fast lookup for query var name, default to _<id>
            hint = self._qname_by_id.get(ref, f"_{ref}")
            return Var(ref, hint)

        elif result[0] == "BOUND":
            # Bound to a term - reify iteratively
            _, ref, term = result
            return self._reify_term(term)

        else:
            raise ValueError(f"Unknown cell tag: {result[0]}")

    def _reify_term(self, term: Term) -> Any:
        """Reify a term, following variable bindings (iterative).

        Reified lists are always flattened: nested PrologList structures
        where the tail is also a PrologList will be combined into a single
        PrologList. Improper lists (with non-list tails) retain their tail.

        Args:
            term: The term to reify.

        Returns:
            The ground term with variables reified.
        """
        # Use iterative approach to avoid stack overflow
        stack = [term]
        visited = set()
        all_terms = []

        # First pass: collect all terms in dependency order
        while stack:
            current = stack.pop()
            term_id = id(current)

            if term_id in visited:
                continue
            visited.add(term_id)
            all_terms.append(current)

            if isinstance(current, Var):
                # Will need to check binding
                result = self.store.deref(current.id)
                if result[0] == "BOUND":
                    _, _, bound_term = result
                    stack.append(bound_term)
            elif isinstance(current, Struct):
                for arg in reversed(current.args):
                    stack.append(arg)
            elif isinstance(current, PrologList):
                stack.append(current.tail)
                for item in reversed(current.items):
                    stack.append(item)

        # Second pass: reify bottom-up
        reified = {}
        for current in reversed(all_terms):
            term_id = id(current)

            if isinstance(current, Var):
                result = self.store.deref(current.id)
                if result[0] == "UNBOUND":
                    # Unbound variable - return a representation
                    _, ref = result
                    # Fast lookup for query var name
                    hint = self._qname_by_id.get(ref, f"_{ref}")
                    reified[term_id] = Var(ref, hint)
                elif result[0] == "BOUND":
                    # Use reified version of bound term
                    _, _, bound_term = result
                    reified[term_id] = reified.get(id(bound_term), bound_term)
                else:
                    raise ValueError(f"Unknown cell tag: {result[0]}")

            elif isinstance(current, (Atom, Int)):
                reified[term_id] = current

            elif isinstance(current, Struct):
                reified_args = tuple(reified.get(id(arg), arg) for arg in current.args)
                reified[term_id] = Struct(current.functor, reified_args)

            elif isinstance(current, PrologList):
                reified_items = list(
                    reified.get(id(item), item) for item in current.items
                )
                reified_tail = reified.get(id(current.tail), current.tail)

                # Flatten if tail is also a PrologList
                if isinstance(reified_tail, PrologList):
                    # Combine items from current list with items from tail list
                    all_items = reified_items + list(reified_tail.items)
                    final_tail = reified_tail.tail
                    reified[term_id] = PrologList(tuple(all_items), tail=final_tail)
                else:
                    reified[term_id] = PrologList(
                        tuple(reified_items), tail=reified_tail
                    )
            else:
                # Unknown term type
                reified[term_id] = current

        return reified.get(id(term), term)

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
        self.goal_stack.push(
            Goal(
                GoalType.CONTROL,
                None,
                payload={"op": "CALL_META", "goal": Goal.from_term(term)},
            )
        )
        return True

    def _builtin_unify(self, args: tuple) -> bool:
        """=(X, Y) - unification builtin."""
        if len(args) != 2:
            return False
        left, right = args
        trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
        return unify(
            left,
            right,
            self.store,
            trail_adapter,  # type: ignore
            occurs_check=self.occurs_check,
        )

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

    def _builtin_var(self, args: tuple) -> bool:
        """var(X) - true if X is an unbound variable."""
        if len(args) != 1:
            return False
        term = args[0]
        if isinstance(term, Var):
            result = self.store.deref(term.id)
            return result[0] == "UNBOUND"
        return False

    def _builtin_nonvar(self, args: tuple) -> bool:
        """nonvar(X) - true if X is not an unbound variable."""
        if len(args) != 1:
            return False
        return not self._builtin_var(args)

    def _builtin_atom(self, args: tuple) -> bool:
        """atom(X) - true if X is an atom."""
        if len(args) != 1:
            return False
        term = args[0]
        if isinstance(term, Var):
            result = self.store.deref(term.id)
            if result[0] == "BOUND":
                _, _, bound_term = result
                return isinstance(bound_term, Atom)
            return False
        return isinstance(term, Atom)

    def _eval_int(self, t: Term) -> int:
        """Evaluate a term as an integer.

        Args:
            t: The term to evaluate.

        Returns:
            The integer value.

        Raises:
            ValueError: If the term is not an integer.
        """
        if isinstance(t, Int):
            return t.value
        if isinstance(t, Var):
            r = self.store.deref(t.id)
            if r[0] == "BOUND":
                _, _, b = r
                return self._eval_int(b)
        raise ValueError("non-integer")

    def _builtin_gt(self, args: tuple) -> bool:
        """>(X, Y) - arithmetic greater-than comparison."""
        if len(args) != 2:
            return False
        try:
            return self._eval_int(args[0]) > self._eval_int(args[1])
        except ValueError:
            return False  # ISO: type/instantiation errors

    def _builtin_num_eq(self, args: tuple) -> bool:
        """=:=(X, Y) - arithmetic equality comparison."""
        if len(args) != 2:
            return False
        try:
            return self._eval_int(args[0]) == self._eval_int(args[1])
        except ValueError:
            return False  # ISO: type/instantiation errors

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
        )
        self.frame_stack.append(frame)

        # Push frame cleanup, cut, and goal
        # The sequence is: POP_FRAME, !, Goal
        # This ensures cut happens after goal succeeds, using our frame's barrier
        self.goal_stack.push(
            Goal(
                GoalType.POP_FRAME,
                None,
                payload={"op": "POP_FRAME", "frame_id": frame_id},
            )
        )
        self.goal_stack.push(Goal.from_term(Atom("!")))
        self.goal_stack.push(Goal.from_term(goal))

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
        
        # Set the exception and trigger unwinding
        self._exception = ball
        # Don't clear goal stack here - let exception handler do it
        # Return True to avoid triggering backtracking
        return True
    
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
        
        # Push goals in the right order:
        # 1. First push a CATCH_END marker that will clean up if goal succeeds
        # 2. Then push the goal
        # 3. Then push a CATCH_BEGIN marker with the catch info
        
        # The stack will be: [CATCH_END, Goal, CATCH_BEGIN] (top to bottom)
        # When we execute:
        # - CATCH_BEGIN gets popped and sets up the catch
        # - Goal executes (may throw)
        # - CATCH_END gets popped and cleans up (only if no throw)
        
        catch_id = id(args)  # Unique identifier for this catch
        
        # Push end marker (executed only if goal succeeds)
        self.goal_stack.push(
            Goal(
                GoalType.CONTROL,
                None,
                payload={
                    "op": "CATCH_END",
                    "catch_id": catch_id,
                }
            )
        )
        
        # Push the goal to execute
        self.goal_stack.push(Goal.from_term(goal))
        
        # Push begin marker (executed first, sets up catch)
        self.goal_stack.push(
            Goal(
                GoalType.CONTROL,
                None,
                payload={
                    "op": "CATCH_BEGIN",
                    "catch_id": catch_id,
                    "catcher": catcher_arg,
                    "recovery": recovery_arg,
                    "trail_top": self.trail.position(),
                    "goal_height": self.goal_stack.height(),
                    "frame_height": len(self.frame_stack),
                    "cp_height": len(self.cp_stack),
                }
            )
        )
        
        return True

    def _builtin_is(self, args: tuple) -> bool:
        """is(X, Y) - arithmetic evaluation."""
        if len(args) != 2:
            return False
        left, right = args
        # Evaluate right side
        try:
            value = self._eval_arithmetic(right)
            # Unify with left side
            result_term = Int(value)
            trail_adapter = TrailAdapter(self.trail, engine=self, store=self.store)
            return unify(
                left,
                result_term,
                self.store,
                trail_adapter,  # type: ignore
                occurs_check=self.occurs_check,
            )
        except (ValueError, TypeError):
            return False

    def _eval_arithmetic(self, term: Term) -> int:
        """Evaluate an arithmetic expression iteratively.

        Args:
            term: The term to evaluate.

        Returns:
            The integer result.

        Raises:
            ValueError: If evaluation fails.
        """
        # Stack-based evaluation to avoid recursion
        # Each entry is either ('eval', term) or ('apply', op, values)
        eval_stack = [("eval", term)]
        value_stack = []

        while eval_stack:
            action, data = eval_stack.pop()

            if action == "eval":
                t = data

                # Dereference if variable
                if isinstance(t, Var):
                    result = self.store.deref(t.id)
                    if result[0] == "BOUND":
                        _, _, bound_term = result
                        t = bound_term
                    else:
                        raise ValueError("Unbound variable in arithmetic")

                if isinstance(t, Int):
                    value_stack.append(t.value)
                elif isinstance(t, Struct):
                    if t.functor in ["+", "-", "*", "//", "mod"] and len(t.args) == 2:
                        # Binary operator: evaluate args then apply
                        eval_stack.append(("apply", Atom(t.functor)))
                        # Push args in reverse order for correct evaluation
                        eval_stack.append(("eval", t.args[1]))
                        eval_stack.append(("eval", t.args[0]))
                    elif t.functor == "-" and len(t.args) == 1:
                        # Unary minus
                        eval_stack.append(("apply_unary", Atom("-")))
                        eval_stack.append(("eval", t.args[0]))
                    else:
                        raise ValueError(
                            f"Unknown arithmetic operator: {t.functor}/{len(t.args)}"
                        )
                else:
                    raise ValueError(f"Cannot evaluate arithmetic: {t}")

            elif action == "apply":
                op_atom = data
                # Extract operator name from Atom
                op = op_atom.name if isinstance(op_atom, Atom) else str(op_atom)
                if len(value_stack) < 2:
                    raise ValueError(f"Not enough values for operator {op}")

                right = value_stack.pop()
                left = value_stack.pop()

                if op == "+":
                    value_stack.append(left + right)
                elif op == "-":
                    value_stack.append(left - right)
                elif op == "*":
                    value_stack.append(left * right)
                elif op == "//":
                    if right == 0:
                        raise ValueError("Division by zero")
                    value_stack.append(left // right)
                elif op == "mod":
                    if right == 0:
                        raise ValueError("Modulo by zero")
                    value_stack.append(left % right)

            elif action == "apply_unary":
                op_atom = data
                # Extract operator name from Atom
                op = op_atom.name if isinstance(op_atom, Atom) else str(op_atom)
                if not value_stack:
                    raise ValueError("Not enough values for unary operator")
                value = value_stack.pop()

                if op == "-":
                    value_stack.append(-value)
                else:
                    raise ValueError(f"Unknown unary operator: {op}")

        if len(value_stack) != 1:
            raise ValueError(
                f"Arithmetic evaluation error: expected 1 value, got {len(value_stack)}"
            )

        return value_stack[0]

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
