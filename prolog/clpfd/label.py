"""CLP(FD) labeling strategies for systematic search.

Implements variable selection and value choice strategies for
finding solutions to constraint problems.
"""

import itertools
import random
from prolog.ast.terms import Atom, Int, Var, List, Struct
from prolog.ast.terms import List as PrologList
from prolog.clpfd.api import get_domain, get_fd_attrs
from prolog.engine.runtime import Goal, GoalType


def _builtin_label(engine, vars_term):
    """Label variables using default strategies.

    Default strategy: first variable, minimum value first.

    Args:
        engine: Engine instance
        vars_term: List of variables to label

    Returns:
        True if labeling goals were pushed successfully
    """
    # Flush any pending CLP(FD) propagation before making new choices
    try:
        if hasattr(engine, "clpfd_queue") and engine.clpfd_queue is not None:
            if not engine.clpfd_queue.is_empty():
                engine.clpfd_queue.run_to_fixpoint(engine.store, engine.trail, engine)
    except Exception:
        pass

    # Use default options: first variable, indomain_min
    return _builtin_labeling(
        engine, List([Atom("first"), Atom("indomain_min")]), vars_term
    )


def _builtin_labeling(engine, options_term, vars_term):
    """Label variables with specified strategies.

    Pushes choice points onto engine goal stack to avoid Python recursion.
    Each choice point tries one value from a variable's domain.

    Args:
        engine: Engine instance
        options_term: List of strategy atoms
        vars_term: List of variables to label

    Returns:
        True if labeling goals were pushed successfully
    """
    # Flush any pending CLP(FD) propagation before making new choices
    try:
        if hasattr(engine, "clpfd_queue") and engine.clpfd_queue is not None:
            if not engine.clpfd_queue.is_empty():
                engine.clpfd_queue.run_to_fixpoint(engine.store, engine.trail, engine)
    except Exception:
        pass

    # Parse options
    var_select = parse_var_selection(options_term)
    val_select = parse_value_selection(options_term)
    rng_seed = parse_random_seed(options_term)

    # Extract variable list
    vars_list = extract_var_list(vars_term)

    # Push labeling goal onto engine stack
    push_labeling_choices(engine, vars_list, var_select, val_select, rng_seed=rng_seed)

    return True


def parse_var_selection(options_term):
    """Parse variable selection strategy from options.

    Args:
        options_term: List of option atoms

    Returns:
        Variable selection strategy name
    """
    if not isinstance(options_term, List):
        return "first"

    # Look for variable selection strategies
    var_strategies = ["first", "first_fail", "most_constrained", "smallest", "largest"]

    for item in options_term.items:
        if isinstance(item, Atom) and item.name in var_strategies:
            return item.name

    return "first"  # Default


def parse_value_selection(options_term):
    """Parse value selection strategy from options.

    Args:
        options_term: List of option atoms

    Returns:
        Value selection strategy name
    """
    if not isinstance(options_term, List):
        return "indomain_min"

    # Look for value selection strategies
    val_strategies = [
        "indomain_min",
        "indomain_max",
        "indomain_middle",
        "indomain_random",
        "indomain_split",
    ]

    for item in options_term.items:
        if isinstance(item, Atom) and item.name in val_strategies:
            return item.name

    return "indomain_min"  # Default


def parse_random_seed(options_term):
    """Parse optional random seed for indomain_random.

    Looks for seed(N) term in options.

    Args:
        options_term: List of option atoms/terms

    Returns:
        Integer seed if provided, otherwise None
    """
    if not isinstance(options_term, List):
        return None

    for item in options_term.items:
        if isinstance(item, Struct) and item.functor == "seed" and len(item.args) == 1:
            arg = item.args[0]
            if isinstance(arg, Int):
                return arg.value
    return None


def extract_var_list(vars_term):
    """Extract list of variables from term.

    Args:
        vars_term: List term containing variables

    Returns:
        Python list of variable IDs
    """
    if not isinstance(vars_term, List):
        return []

    vars_list = []
    for item in vars_term.items:
        if isinstance(item, Var):
            vars_list.append(item.id)

    return vars_list


def push_labeling_choices(engine, vars, var_select, val_select, rng_seed=None):
    """Push labeling alternatives onto engine goal stack.

    Creates a choice point for each unbound FD variable, with alternatives
    for each value in its domain.

    Args:
        engine: Engine instance
        vars: List of variable IDs to label
        var_select: Variable selection strategy name
        val_select: Value selection strategy name
    """
    # Find unbound FD variables
    unbound = []
    for var_id in vars:
        var_deref = engine.store.deref(var_id)
        if var_deref[0] == "UNBOUND":
            # Check if it has a domain
            domain = get_domain(engine.store, var_deref[1])
            if domain and not domain.is_empty():
                unbound.append((var_deref[1], domain))

    if not unbound:
        # All variables labeled or no FD variables
        return

    if getattr(engine, "trace", False):
        try:
            engine._trace_log.append(
                "[LABEL] candidates: "
                + ", ".join(f"_G{vid}:{dom.intervals}" for vid, dom in unbound)
            )
        except Exception:
            pass

    # Select next variable to label
    selected_var, selected_domain = select_variable(unbound, var_select, engine)

    # Get values to try in order (already limited for large domains)
    values = select_values(selected_domain, val_select, rng_seed=rng_seed)

    if not values:
        # Empty domain - nothing to do, will fail
        return

    # Build branches as (X = Value, label(Vars)) to chain labeling deterministically
    var_term = Var(selected_var, f"_G{selected_var}")

    # Reconstruct a List term from the provided var IDs for recursive call
    # Preserve original query variable names when available

    items = []
    for vid in vars:
        name = getattr(engine, "_qname_by_id", {}).get(vid, f"_G{vid}")
        items.append(Var(vid, name))
    vars_term = PrologList(tuple(items))

    def branch_struct(value):
        assign = Struct("=", (var_term, Int(value)))
        # Recurse via label/1 using same vars list; bound vars are skipped
        label_goal = Struct("label", (vars_term,))
        return Struct(",", (assign, label_goal))

    if len(values) == 1:
        # Deterministic: push continuation BELOW, then the unification as BUILTIN on top
        # so unify executes first and continuation follows.
        # Continuation via calling label/1 recursively on the same vars list.
        cont_call = Goal.from_term(Struct("label", (vars_term,)))
        engine._push_goal(cont_call)
        unify_goal = Struct("=", (var_term, Int(values[0])))
        engine._push_goal(Goal(GoalType.BUILTIN, unify_goal))
    else:
        # Non-deterministic: (X=V1, label(Vars)) ; (X=V2, label(Vars)) ; ...
        def build_disjunction(vals):
            if len(vals) == 1:
                return branch_struct(vals[0])
            left = branch_struct(vals[0])
            right = build_disjunction(vals[1:])
            return Struct(";", (left, right))

        disj_goal = build_disjunction(values)
        # Ensure a fresh trail stamp for this labeling branch to avoid
        # cross-branch leakage of FD domain/attr changes when nested.
        engine._push_goal(
            Goal(GoalType.CONTROL, None, payload={"op": "LABEL_BRANCH_STAMP"})
        )
        engine._push_goal(Goal.from_term(disj_goal))


def select_variable(unbound, strategy, engine):
    """Select next variable to label based on strategy.

    Args:
        unbound: List of (var_id, domain) tuples
        strategy: Selection strategy name
        engine: Engine instance

    Returns:
        Tuple of (selected_var_id, selected_domain)
    """
    if not unbound:
        return None, None

    if strategy == "first":
        # Select first in list
        return unbound[0]

    elif strategy == "first_fail" or strategy == "smallest":
        # Select variable with smallest domain
        return min(unbound, key=lambda x: x[1].size())

    elif strategy == "largest":
        # Select variable with largest domain
        return max(unbound, key=lambda x: x[1].size())

    elif strategy == "most_constrained":
        # Select variable with most constraints (most watchers)
        def count_watchers(var_domain_tuple):
            var_id, domain = var_domain_tuple

            attrs = get_fd_attrs(engine.store, var_id)
            if attrs and "watchers" in attrs:
                total = 0
                for priority_set in attrs["watchers"].values():
                    total += len(priority_set)
                return total
            return 0

        return max(unbound, key=count_watchers)

    else:
        # Default to first
        return unbound[0]


def select_values(domain, strategy, rng_seed=None):
    """Select values from domain in order based on strategy.

    Args:
        domain: Domain object
        strategy: Value selection strategy name
        rng_seed: Random seed for indomain_random strategy

    Returns:
        List of values to try in order (for compatibility with existing code)
    """
    if domain.is_empty():
        return []

    # For small domains, use the old behavior to maintain compatibility
    # For large domains, limit the number of values to prevent memory explosion
    domain_size = domain.size()
    max_values = 1000  # Reasonable limit to prevent memory explosion

    if domain_size <= max_values:
        # Small domain - materialize all values for full compatibility
        if strategy == "indomain_min":
            return list(domain.iter_values())
        elif strategy == "indomain_max":
            return list(domain.iter_values_ordered("max_first"))
        elif strategy == "indomain_middle":
            return list(domain.iter_values_ordered("middle_out"))
        elif strategy == "indomain_random":
            values = list(domain.iter_values())
            rng = random.Random(rng_seed if rng_seed is not None else 42)
            rng.shuffle(values)
            return values
        elif strategy == "indomain_split":
            return list(domain.iter_values_ordered("split"))
        else:
            return list(domain.iter_values())
    else:
        # Large domain - limit values to prevent memory explosion
        if strategy == "indomain_min":
            return list(itertools.islice(domain.iter_values(), max_values))
        elif strategy == "indomain_max":
            return list(
                itertools.islice(domain.iter_values_ordered("max_first"), max_values)
            )
        elif strategy == "indomain_middle":
            return list(
                itertools.islice(domain.iter_values_ordered("middle_out"), max_values)
            )
        elif strategy == "indomain_random":
            # For very large domains, sample random subset
            values = list(itertools.islice(domain.iter_values(), max_values))
            rng = random.Random(rng_seed if rng_seed is not None else 42)
            rng.shuffle(values)
            return values
        elif strategy == "indomain_split":
            return list(
                itertools.islice(domain.iter_values_ordered("split"), max_values)
            )
        else:
            return list(itertools.islice(domain.iter_values(), max_values))


def create_labeling_goals(var_id, values, remaining_vars, var_select, val_select):
    """Create goals for labeling a variable with choice points.

    Instead of custom goal classes, we create standard Prolog goals
    that the engine can handle directly.

    Args:
        var_id: Variable ID to label
        values: List of values to try
        remaining_vars: List of all variables to label
        var_select: Variable selection strategy
        val_select: Value selection strategy

    Returns:
        List of Goal objects representing alternatives
    """
    if not values:
        # No values - fail
        return []

    goals = []
    for value in values:
        # Create unification goal: Var = Value
        var_term = Var(var_id, f"_G{var_id}")
        value_term = Int(value)
        unify_goal = Struct("=", (var_term, value_term))

        # After unification, continue labeling
        # We'll handle this by pushing a special labeling continuation goal
        goals.append(
            Goal(
                GoalType.PREDICATE,
                unify_goal,
                {
                    "labeling_continuation": {
                        "vars": remaining_vars,
                        "var_select": var_select,
                        "val_select": val_select,
                    }
                },
            )
        )

    return goals
