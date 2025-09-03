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
            if self.max_solutions and len(self.solutions) >= self.max_solutions:
                break
        
        # Clean up query variables - restore them to unbound state
        if self._query_vars:
            for var_id, _ in self._query_vars:
                if var_id < len(self.store.cells):
                    self.store.cells[var_id] = Cell(
                        tag="unbound", ref=var_id, term=None, rank=0
                    )
            # Also clear trail since we're resetting query vars
            self.trail = []
        
        return self.solutions
    
    def _allocate_query_vars(self, term: Term) -> Term:
        """Allocate fresh variables for query terms and track them.
        
        Args:
            term: A query goal term.
            
        Returns:
            Term with variables replaced by fresh allocated vars.
        """
        if isinstance(term, Var):
            # Check if we've seen this variable name
            var_name = term.hint or f"_{term.id}"
            # Look for existing allocation
            for varid, name in self._query_vars:
                if name == var_name:
                    return Var(varid, var_name)
            # Allocate new variable
            varid = self.store.new_var(hint=var_name)
            self._query_vars.append((varid, var_name))
            return Var(varid, var_name)
        
        elif isinstance(term, (Atom, Int)):
            return term
        
        elif isinstance(term, Struct):
            new_args = tuple(self._allocate_query_vars(arg) for arg in term.args)
            return Struct(term.functor, new_args)
        
        elif isinstance(term, PrologList):
            new_items = tuple(self._allocate_query_vars(item) for item in term.items)
            new_tail = self._allocate_query_vars(term.tail)
            return PrologList(new_items, tail=new_tail)
        
        else:
            raise TypeError(f"Unknown term type: {type(term)}")
    
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
        
        # If there are more clauses, create choicepoint
        if cursor.has_more():
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
        
        # Try the clause with the goal term
        return self._try_clause(clause_idx, goal.term)
    
    def _try_clause(self, clause_idx: int, goal_term: Term) -> bool:
        """Try to apply a clause.
        
        Args:
            clause_idx: Index of clause in program.
            goal_term: The goal term to unify with clause head.
            
        Returns:
            True if successful, False to backtrack.
        """
        clause = self.program.clauses[clause_idx]
        
        # Rename clause with fresh variables
        renamer = VarRenamer(self.store)
        renamed_clause = renamer.rename_clause(clause)
        
        # Set cut barrier to current choice point (for cut implementation)
        old_cut_barrier = self._cut_barrier
        self._cut_barrier = self.choices.top_id()
        
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
        
        # Try the clause
        return self._try_clause(clause_idx, goal.term)
    
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
            # Bound to a term - reify recursively
            _, ref, term = result
            return self._reify_term(term)
        
        else:
            raise ValueError(f"Unknown cell tag: {result[0]}")
    
    def _reify_term(self, term: Term) -> Any:
        """Reify a term, following variable bindings.
        
        Args:
            term: The term to reify.
            
        Returns:
            The ground term with variables reified.
        """
        if isinstance(term, Var):
            return self._reify_var(term.id)
        
        elif isinstance(term, (Atom, Int)):
            return term
        
        elif isinstance(term, Struct):
            reified_args = tuple(self._reify_term(arg) for arg in term.args)
            return Struct(term.functor, reified_args)
        
        elif isinstance(term, PrologList):
            reified_items = tuple(self._reify_term(item) for item in term.items)
            reified_tail = self._reify_term(term.tail)
            return PrologList(reified_items, tail=reified_tail)
        
        else:
            raise TypeError(f"Unknown term type: {type(term)}")
    
    def _is_builtin(self, term: Term) -> bool:
        """Check if a term is a builtin predicate.
        
        Args:
            term: The term to check.
            
        Returns:
            True if it's a builtin, False otherwise.
        """
        # For now, no builtins implemented
        return False
    
    def _execute_builtin(self, term: Term) -> bool:
        """Execute a builtin predicate.
        
        Args:
            term: The builtin term to execute.
            
        Returns:
            True to continue, False to backtrack.
        """
        # For now, no builtins implemented
        return False