"""All-solutions builtin predicates.

This module contains builtin predicates for collecting solutions:
- findall/3 - collect all solutions (always succeeds, returns [] if no solutions)
- bagof/3 - collect solutions with free variable grouping (fails if no solutions)
- setof/3 - collect sorted unique solutions (fails if no solutions)

These predicates are extracted from engine.py as part of Phase 5 of the
engine refactoring plan.
"""

from typing import Dict, Tuple, Callable, List, Any, TYPE_CHECKING
from prolog.ast.terms import Term, Atom, Int, Float, Var, Struct, List as PrologList
from prolog.unify.unify import unify
from prolog.engine.trail_adapter import TrailAdapter
from prolog.engine.utils.copy import (
    copy_term_recursive,
    build_prolog_list,
)

if TYPE_CHECKING:
    pass

__all__ = [
    "register",
    "builtin_findall",
    "builtin_bagof",
    "builtin_setof",
    "collect_all_solutions",
]


def register(registry: Dict[Tuple[str, int], Callable]) -> None:
    """Register all-solutions builtin predicates.

    Args:
        registry: Dictionary mapping (predicate_name, arity) -> callable
    """
    registry[("findall", 3)] = builtin_findall
    registry[("bagof", 3)] = builtin_bagof
    registry[("setof", 3)] = builtin_setof


def builtin_findall(engine, args: tuple) -> bool:
    """findall(+Template, :Goal, -List) - collect all solutions.

    findall/3 always succeeds. If Goal has no solutions, List is bound to [].
    All free variables in Goal are automatically existentially quantified.
    """
    if len(args) != 3:
        return False

    template, goal, result_list = args

    # Collect all solutions by running goal in isolation
    solutions = collect_all_solutions(engine, template, goal, existential_all=True)

    # Create result list term
    if not solutions:
        result_term = Atom("[]")  # Empty list
    else:
        result_term = build_prolog_list(solutions)

    # Unify with result list
    trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)
    return unify(
        result_list, result_term, engine.store, trail_adapter, engine.occurs_check
    )


def builtin_bagof(engine, args: tuple) -> bool:
    """bagof(+Template, :Goal, -Bag) - collect solutions with free variable grouping.

    bagof/3 fails if Goal has no solutions. It backtracks over free variables
    unless they are existentially quantified with the ^ operator.

    CURRENT LIMITATIONS:
    - No support for existential quantification (^/2) - all variables treated as existential
    - No grouping/backtracking by free variables - simplified to behave like findall that fails on empty
    - These will be addressed in future implementations for full ISO compliance
    """
    if len(args) != 3:
        return False

    template, goal, result_bag = args

    # Extract existentially quantified variables and clean goal
    clean_goal, existential_vars = _extract_existential_vars(goal)

    # Find free variables in clean_goal that are not in template or existential_vars
    free_vars = _find_free_variables(clean_goal, template, existential_vars)

    if not free_vars:
        # No free variables - behave like findall but fail on empty
        solutions = collect_all_solutions(
            engine, template, clean_goal, existential_all=True
        )
        if not solutions:
            return False  # bagof fails on empty results

        result_term = build_prolog_list(solutions)

        trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)
        return unify(
            result_bag, result_term, engine.store, trail_adapter, engine.occurs_check
        )
    else:
        # Has free variables - create choicepoint to backtrack over them
        # This is a simplified implementation - full bagof/3 is quite complex
        # For now, just collect all solutions treating free vars as existential
        solutions = collect_all_solutions(
            engine, template, clean_goal, existential_all=True
        )
        if not solutions:
            return False

        result_term = build_prolog_list(solutions)

        trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)
        return unify(
            result_bag, result_term, engine.store, trail_adapter, engine.occurs_check
        )


def builtin_setof(engine, args: tuple) -> bool:
    """setof(+Template, :Goal, -Set) - collect sorted unique solutions.

    setof/3 is like bagof/3 but sorts results and removes duplicates.
    Fails if Goal has no solutions.

    CURRENT LIMITATIONS:
    - No support for existential quantification (^/2) - all variables treated as existential
    - No grouping/backtracking by free variables - simplified to behave like findall that fails on empty
    - Uses string representation for sorting/deduplication, not standard term ordering
    - These will be addressed in future implementations for full ISO compliance
    """
    if len(args) != 3:
        return False

    template, goal, result_set = args

    # Use bagof logic but sort and deduplicate
    clean_goal, existential_vars = _extract_existential_vars(goal)
    free_vars = _find_free_variables(clean_goal, template, existential_vars)

    if not free_vars:
        solutions = collect_all_solutions(
            engine, template, clean_goal, existential_all=True
        )
        if not solutions:
            return False  # setof fails on empty results

        # Sort and deduplicate solutions
        unique_solutions = _sort_and_deduplicate(solutions)
        result_term = build_prolog_list(unique_solutions)

        trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)
        return unify(
            result_set, result_term, engine.store, trail_adapter, engine.occurs_check
        )
    else:
        # Simplified implementation for now
        solutions = collect_all_solutions(
            engine, template, clean_goal, existential_all=True
        )
        if not solutions:
            return False

        unique_solutions = _sort_and_deduplicate(solutions)
        result_term = build_prolog_list(unique_solutions)

        trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)
        return unify(
            result_set, result_term, engine.store, trail_adapter, engine.occurs_check
        )


# Helper functions


def collect_all_solutions(
    engine, template: Term, goal: Term, existential_all: bool = False
) -> List[Term]:
    """Collect all solutions for a goal, instantiating template for each solution.

    Args:
        engine: The engine instance
        template: Template term to collect instances of
        goal: Goal to solve
        existential_all: If True, all variables in goal are existentially quantified

    Returns:
        List of template instances for each solution
    """
    # Create a fresh engine instance to avoid state interference
    # Create minimal sub-engine with same program
    # Use engine.__class__ to avoid circular import
    sub_engine = engine.__class__(
        engine.program,
        occurs_check=engine.occurs_check,
        max_solutions=None,
        trace=False,
        use_indexing=False,
    )

    try:
        # Copy template and goal with fresh variables for the sub-engine
        var_mapping = {}
        template_copy = copy_term_recursive(template, var_mapping, sub_engine.store)
        goal_copy = copy_term_recursive(goal, var_mapping, sub_engine.store)

        # Run the goal to collect all solutions
        solutions = sub_engine.solve(goal_copy)

        # Extract template instances from solutions
        result_instances = []
        for solution in solutions:
            # Create a reified version of the template with solution bindings
            instantiated_template = _reify_term_with_solution(template_copy, solution)
            result_instances.append(instantiated_template)

        return result_instances

    except Exception:
        # If goal execution fails, return empty list (findall semantics)
        return []


def _extract_existential_vars(goal: Term) -> Tuple[Term, List[int]]:
    """Extract existentially quantified variables (^) from goal.

    Returns (clean_goal, existential_var_ids)
    For now, this is a stub - full ^ operator support would need parser changes.
    """
    # Simplified: no ^ operator support yet
    return goal, []


def _find_free_variables(
    goal: Term, template: Term, existential_vars: List[int]
) -> List[int]:
    """Find free variables in goal that are not in template or existential list.

    For now, returns empty list (simplified implementation).
    """
    # Simplified: assume no free variables for now
    return []


def _sort_and_deduplicate(solutions: List[Term]) -> List[Term]:
    """Sort solutions and remove duplicates for setof/3."""
    # Simple deduplication by converting to string and back
    # This is not the most efficient but works for basic cases
    seen = set()
    unique = []

    for solution in solutions:
        str_repr = str(solution)  # Using string representation for comparison
        if str_repr not in seen:
            seen.add(str_repr)
            unique.append(solution)

    # Sort by string representation (simplified)
    unique.sort(key=str)
    return unique


def _reify_term_with_solution(term: Term, solution: Dict[str, Any]) -> Term:
    """Apply a solution dictionary to instantiate a term."""
    if isinstance(term, Var):
        # Look for variable in solution
        var_name = term.hint or f"_{term.id}"
        if var_name in solution:
            return solution[var_name]
        else:
            # Unbound variable remains as-is
            return term
    elif isinstance(term, (Atom, Int, Float)):
        return term
    elif isinstance(term, Struct):
        new_args = tuple(_reify_term_with_solution(arg, solution) for arg in term.args)
        return Struct(term.functor, new_args)
    elif isinstance(term, PrologList):
        new_items = tuple(
            _reify_term_with_solution(item, solution) for item in term.items
        )
        new_tail = _reify_term_with_solution(term.tail, solution)
        return PrologList(new_items, new_tail)
    return term
