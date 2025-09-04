"""Prolog Engine with Explicit Stacks (Stage 0)."""

from typing import Dict, List, Optional, Any, Tuple
from prolog.ast.terms import Term, Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause, Program
from prolog.engine.goals import Goal, GoalStack
from prolog.engine.choicepoint import Choicepoint, ChoiceStack
from prolog.engine.rename import VarRenamer
from prolog.unify.store import Store, Cell
from prolog.unify.unify import unify


class Engine:
    """Prolog inference engine with explicit stacks (no Python recursion)."""
    
    def __init__(self, program: Program, occurs_check: bool = False, 
                 max_solutions: Optional[int] = None, trace: bool = False):
        """Initialize engine with a program.
        
        Args:
            program: The Prolog program to execute.
            occurs_check: Whether to use occurs check in unification.
            max_solutions: Maximum number of solutions to find.
            trace: Whether to enable tracing.
        """
        self.program = program
        self.occurs_check = occurs_check
        
        # Core state
        self.store = Store()
        self.trail: List[Tuple] = []
        self.goals = GoalStack()
        self.choices = ChoiceStack()
        
        # Query tracking
        self._query_vars: List[Tuple[int, str]] = []  # [(varid, name), ...]
        self._initial_var_cutoff = 0  # Vars below this are query vars
        
        # Solution collection
        self.solutions: List[Dict[str, Any]] = []
        self.max_solutions = max_solutions
        
        # Configuration
        self.trace = trace
        self._cut_barrier: Optional[int] = None
        
        # Builtin registry: maps (name, arity) -> callable
        self._builtins = {}
        self._register_builtins()
    
    def reset(self):
        """Reset engine state for reuse."""
        self.store = Store()
        self.trail = []
        self.goals = GoalStack()
        self.choices = ChoiceStack()
        self._query_vars = []
        self._initial_var_cutoff = 0
        self.solutions = []
        self._cut_barrier = None
    
    def _register_builtins(self):
        """Register all builtin predicates."""
        # Core control flow
        self._builtins[("!", 0)] = self._builtin_cut
        self._builtins[("true", 0)] = self._builtin_true
        self._builtins[("fail", 0)] = self._builtin_fail
        self._builtins[("call", 1)] = self._builtin_call
    
    def run(self, goals: List[Term], max_solutions: Optional[int] = None) -> List[Dict[str, Any]]:
        """Run a query with given goals.
        
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
        
        # Push initial goals (in reverse for left-to-right execution)
        for goal in reversed(renamed_goals):
            self.goals.push(Goal(term=goal))
        
        # Main execution loop
        while self._step():
            pass  # The _step method handles max_solutions check
        
        # Clean up query variables - restore them to unbound state
        if self._query_vars:
            for var_id, _ in self._query_vars:
                if var_id < len(self.store.cells):
                    self.store.cells[var_id] = Cell(
                        tag="unbound", ref=var_id, term=None, rank=0
                    )
            # Also clear trail since we're resetting query vars
            self.trail = []
        
        # Reset cut barrier after query completes
        self._cut_barrier = None
        
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
        stack = [term]
        processed = {}  # Maps id(original_term) -> new_term
        
        # First pass: identify all terms in depth-first order
        all_terms = []
        visited = set()
        temp_stack = [term]
        
        while temp_stack:
            current = temp_stack.pop()
            if id(current) in visited:
                continue
            visited.add(id(current))
            all_terms.append(current)
            
            if isinstance(current, Struct):
                # Add args in reverse order for depth-first
                for arg in reversed(current.args):
                    temp_stack.append(arg)
            elif isinstance(current, PrologList):
                temp_stack.append(current.tail)
                for item in reversed(current.items):
                    temp_stack.append(item)
        
        # Second pass: process terms bottom-up
        for current in reversed(all_terms):
            if isinstance(current, Var):
                # Check if we've seen this variable name
                var_name = current.hint or f"_{current.id}"
                # Look for existing allocation
                result = None
                for varid, name in self._query_vars:
                    if name == var_name:
                        result = Var(varid, var_name)
                        break
                if result is None:
                    # Allocate new variable
                    varid = self.store.new_var(hint=var_name)
                    self._query_vars.append((varid, var_name))
                    result = Var(varid, var_name)
                processed[id(current)] = result
                
            elif isinstance(current, (Atom, Int)):
                processed[id(current)] = current
                
            elif isinstance(current, Struct):
                # All args should be processed already
                new_args = tuple(processed[id(arg)] for arg in current.args)
                processed[id(current)] = Struct(current.functor, new_args)
                
            elif isinstance(current, PrologList):
                # All items and tail should be processed already
                new_items = tuple(processed[id(item)] for item in current.items)
                new_tail = processed[id(current.tail)]
                processed[id(current)] = PrologList(new_items, tail=new_tail)
            else:
                raise TypeError(f"Unknown term type: {type(current)}")
        
        return processed[id(term)]
    
    def _step(self) -> bool:
        """Execute one step of the inference engine.
        
        Returns:
            True to continue, False when done.
        """
        # Pop next goal
        goal = self.goals.pop()
        
        if goal is None:
            # No more goals - found a solution
            self._record_solution()
            # Check if we've found enough solutions
            if self.max_solutions and len(self.solutions) >= self.max_solutions:
                return False  # Stop searching
            # Backtrack to find more solutions
            return self._backtrack()
        
        # Check if it's a builtin
        if self._is_builtin(goal.term):
            return self._execute_builtin(goal.term)
        
        # Try to match against clauses
        return self._try_goal(goal)
    
    def _try_goal(self, goal: Goal) -> bool:
        """Try to prove a goal by matching against program clauses.
        
        Args:
            goal: The goal to prove.
            
        Returns:
            True to continue, False when done.
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
            return self._backtrack()
        
        # Get cursor for matching clauses
        matches = self.program.clauses_for(functor, arity)
        from prolog.ast.clauses import ClauseCursor
        cursor = ClauseCursor(matches=matches, functor=functor, arity=arity)
        
        if not cursor.has_more():
            # No matching clauses
            return self._backtrack()
        
        # Try first clause
        clause_idx = cursor.take()
        
        # Determine cut barrier for this clause
        clause_cut_barrier = None
        
        # If there are more clauses, create choicepoint
        if cursor.has_more():
            # Capture cut barrier BEFORE creating choicepoint
            # This allows cut to remove the choicepoint for this goal
            clause_cut_barrier = self.choices.size()
            
            # Pre-goal snapshot (goal still on stack)
            self.goals.push(goal)
            goal_snapshot = self.goals.snapshot()
            self.goals.pop()  # Remove it again
            
            cp = Choicepoint(
                id=self.choices.current_id(),
                goals=goal_snapshot,
                cursor=cursor,
                trail_mark=len(self.trail),
                cut_barrier=self._cut_barrier,
                store_size=len(self.store.cells)
            )
            cp_id = self.choices.push(cp)
        
        # Try the clause with the goal term, passing the cut barrier
        return self._try_clause(clause_idx, goal.term, clause_cut_barrier)
    
    def _try_clause(self, clause_idx: int, goal_term: Term, 
                     clause_cut_barrier: Optional[int] = None) -> bool:
        """Try to apply a clause.
        
        Args:
            clause_idx: Index of clause in program.
            goal_term: The goal term to unify with clause head.
            clause_cut_barrier: Cut barrier for this clause (before its choicepoint).
            
        Returns:
            True if successful, False to backtrack.
        """
        clause = self.program.clauses[clause_idx]
        
        # Rename clause with fresh variables
        renamer = VarRenamer(self.store)
        renamed_clause = renamer.rename_clause(clause)
        
        # Set cut barrier (for cut implementation)
        # Only set if this clause was selected from multiple alternatives
        old_cut_barrier = self._cut_barrier
        if clause_cut_barrier is not None:
            self._cut_barrier = clause_cut_barrier
        
        # Try to unify with clause head
        if unify(renamed_clause.head, goal_term, self.store, self.trail, 
                occurs_check=self.occurs_check):
            # Unification succeeded - push body goals
            self.goals.push_body(renamed_clause.body)
            return True
        else:
            # Unification failed - restore cut barrier and backtrack
            self._cut_barrier = old_cut_barrier
            return self._backtrack()
    
    def _backtrack(self) -> bool:
        """Backtrack to the most recent choicepoint.
        
        Returns:
            True to continue, False when no more choicepoints.
        """
        cp = self.choices.pop()
        
        if cp is None:
            # No more choicepoints - search is done
            return False
        
        # Restore state from choicepoint
        self._restore_from_choicepoint(cp)
        
        # Get the goal that was being tried (top of restored stack)
        goal = self.goals.pop()
        if goal is None:
            # Shouldn't happen
            return False
        
        # Try next clause
        clause_idx = cp.cursor.take()
        
        if clause_idx is None:
            # No more clauses - backtrack again
            return self._backtrack()
        
        # If there are still more clauses, push choicepoint back
        if cp.cursor.has_more():
            # Re-create choicepoint with current state
            self.goals.push(goal)
            goal_snapshot = self.goals.snapshot()
            self.goals.pop()
            
            new_cp = Choicepoint(
                id=self.choices.current_id(),
                goals=goal_snapshot,
                cursor=cp.cursor,
                trail_mark=len(self.trail),
                cut_barrier=cp.cut_barrier,
                store_size=len(self.store.cells)
            )
            self.choices.push(new_cp)
        
        # Try the clause with same cut barrier (size before this choicepoint)
        # The cut barrier should be the stack size when this goal was first tried
        clause_cut_barrier = self.choices.size()  # Current size after re-pushing
        if cp.cursor.has_more():
            # We just re-pushed, so subtract 1
            clause_cut_barrier = max(0, clause_cut_barrier - 1)
        return self._try_clause(clause_idx, goal.term, clause_cut_barrier)
    
    def _restore_from_choicepoint(self, cp: Choicepoint):
        """Restore engine state from a choicepoint.
        
        Args:
            cp: The choicepoint to restore from.
        """
        # Restore goal stack
        self.goals.restore(cp.goals)
        
        # Restore store (undo trail entries)
        while len(self.trail) > cp.trail_mark:
            entry = self.trail.pop()
            if entry[0] == 'bind':
                _, varid, old_cell = entry
                self.store.cells[varid] = old_cell
            elif entry[0] == 'parent':
                _, varid, old_parent = entry
                self.store.cells[varid].ref = old_parent
        
        # Shrink store to original size (remove variables created during failed attempt)
        self.store.cells = self.store.cells[:cp.store_size]
        
        # Restore cut barrier
        self._cut_barrier = cp.cut_barrier
    
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
        
        if result[0] == 'UNBOUND':
            # Unbound variable - return a representation
            _, ref = result
            hint = f"_{ref}" if ref >= self._initial_var_cutoff else None
            for vid, name in self._query_vars:
                if vid == ref:
                    hint = name
                    break
            return Var(ref, hint or f"_{ref}")
        
        elif result[0] == 'BOUND':
            # Bound to a term - reify iteratively
            _, ref, term = result
            return self._reify_term(term)
        
        else:
            raise ValueError(f"Unknown cell tag: {result[0]}")
    
    def _reify_term(self, term: Term) -> Any:
        """Reify a term, following variable bindings (iterative).
        
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
                if result[0] == 'BOUND':
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
                if result[0] == 'UNBOUND':
                    # Unbound variable - return a representation
                    _, ref = result
                    hint = f"_{ref}" if ref >= self._initial_var_cutoff else None
                    for vid, name in self._query_vars:
                        if vid == ref:
                            hint = name
                            break
                    reified[term_id] = Var(ref, hint or f"_{ref}")
                elif result[0] == 'BOUND':
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
                reified_items = tuple(reified.get(id(item), item) for item in current.items)
                reified_tail = reified.get(id(current.tail), current.tail)
                reified[term_id] = PrologList(reified_items, tail=reified_tail)
            else:
                raise TypeError(f"Unknown term type: {type(current)}")
        
        return reified.get(id(term), term)
    
    def _is_builtin(self, term: Term) -> bool:
        """Check if a term is a builtin predicate.
        
        Args:
            term: The term to check.
            
        Returns:
            True if it's a builtin, False otherwise.
        """
        if isinstance(term, Atom):
            return (term.name, 0) in self._builtins
        elif isinstance(term, Struct):
            return (term.functor, len(term.args)) in self._builtins
        return False
    
    def _execute_builtin(self, term: Term) -> bool:
        """Execute a builtin predicate.
        
        Args:
            term: The builtin term to execute.
            
        Returns:
            True to continue, False to backtrack.
        """
        if isinstance(term, Atom):
            key = (term.name, 0)
            args = ()
        elif isinstance(term, Struct):
            key = (term.functor, len(term.args))
            args = term.args
        else:
            return False
        
        # Dispatch to builtin handler
        if key in self._builtins:
            return self._builtins[key](args)
        
        # Unknown builtin
        return False
    
    def _builtin_cut(self, args: tuple) -> bool:
        """Execute cut (!).
        
        Args:
            args: Arguments (should be empty for !).
            
        Returns:
            Always True (cut always succeeds).
        """
        # Execute cut: remove choicepoints newer than cut barrier
        if self._cut_barrier is not None:
            self.choices.cut_to(self._cut_barrier)
        # Cut always succeeds
        return True
    
    def _builtin_true(self, args: tuple) -> bool:
        """Execute true/0.
        
        Args:
            args: Arguments (should be empty).
            
        Returns:
            Always True.
        """
        return True
    
    def _builtin_fail(self, args: tuple) -> bool:
        """Execute fail/0.
        
        Args:
            args: Arguments (should be empty).
            
        Returns:
            Always False.
        """
        return False
    
    def _builtin_call(self, args: tuple) -> bool:
        """Execute call/1.
        
        Args:
            args: Single argument - the goal to call.
            
        Returns:
            True if goal pushed successfully, False otherwise.
        """
        if len(args) != 1:
            return False
        
        goal_term = args[0]
        
        # Dereference the goal term to handle variables
        if isinstance(goal_term, Var):
            result = self.store.deref(goal_term.id)
            if result[0] == 'UNBOUND':
                # Unbound variable - fail
                return False
            elif result[0] == 'BOUND':
                # Bound variable - use the bound term
                _, _, goal_term = result
        
        # Check if the dereferenced term is callable
        if not isinstance(goal_term, (Atom, Struct)):
            # Not callable (e.g., Int, List)
            return False
        
        # Push the goal onto the stack
        self.goals.push(Goal(goal_term))
        return True