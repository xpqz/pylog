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

    # Get values to try in order using lazy iteration
    values_iter = select_values_iter(selected_domain, val_select, rng_seed=rng_seed)

    # Convert to list for the current implementation
    # TODO: Make the entire labeling lazy to avoid materializing large domains
    try:
        values = list(values_iter)
    except Exception:
        # If materialization fails due to memory, use limited fallback
        values = list(
            itertools.islice(
                select_values_iter(selected_domain, val_select, rng_seed=rng_seed), 1000
            )
        )

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

    # Use cached selector if feature flag is enabled
    if hasattr(engine, '_uses_variable_selection_caching') and engine._uses_variable_selection_caching:
        # Create or reuse selector instance
        if not hasattr(engine, '_variable_selector'):
            engine._variable_selector = VariableSelector()

        selector = engine._variable_selector

        if strategy == "first_fail" or strategy == "smallest":
            return selector.select_first_fail(unbound, engine)
        elif strategy == "most_constrained":
            return selector.select_most_constrained(unbound, engine)

    # Fallback to original logic for other strategies or when caching disabled
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


def select_values_iter(domain, strategy, rng_seed=None):
    """Select values from domain in order based on strategy.

    Returns an iterator to avoid memory explosion with large domains.

    Args:
        domain: Domain object
        strategy: Value selection strategy name
        rng_seed: Random seed for indomain_random strategy

    Returns:
        Iterator over values to try in order
    """
    if domain.is_empty():
        return iter([])

    if strategy == "indomain_min":
        return domain.iter_values()
    elif strategy == "indomain_max":
        return domain.iter_values_ordered("max_first")
    elif strategy == "indomain_middle":
        return domain.iter_values_ordered("middle_out")
    elif strategy == "indomain_random":
        # For random, we need to materialize values to shuffle them
        # Use a reasonable limit to prevent memory explosion
        max_values = 10000  # Generous limit for random sampling
        values = list(itertools.islice(domain.iter_values(), max_values))
        rng = random.Random(rng_seed if rng_seed is not None else 42)
        rng.shuffle(values)
        return iter(values)
    elif strategy == "indomain_split":
        return domain.iter_values_ordered("split")
    else:
        return domain.iter_values()


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


class VariableSelector:
    """Cached variable selector for improved labeling performance.

    Caches domain sizes and watcher counts to avoid recomputation
    during repeated variable selection operations.
    """

    def __init__(self):
        # Cache for domain sizes: var_id -> (domain_rev, size)
        self._domain_size_cache = {}
        # Cache for watcher counts: var_id -> (watchers_version, count)
        self._watcher_count_cache = {}

    def select_first_fail(self, variables, engine):
        """Select variable with smallest domain (first_fail strategy).

        Args:
            variables: List of (var_id, domain) tuples
            engine: Engine instance

        Returns:
            Tuple of (selected_var_id, selected_domain)
        """
        if not variables:
            return None, None

        # Find variable with minimum domain size using cached sizes
        min_var = None
        min_domain = None
        min_size = float("inf")

        for var_id, domain in variables:
            size = self._get_cached_domain_size(var_id, domain)
            if size < min_size:
                min_size = size
                min_var = var_id
                min_domain = domain

        return min_var, min_domain

    def select_most_constrained(self, variables, engine):
        """Select variable with most constraints (most watchers).

        Args:
            variables: List of (var_id, domain) tuples
            engine: Engine instance

        Returns:
            Tuple of (selected_var_id, selected_domain)
        """
        if not variables:
            return None, None

        # Find variable with maximum watcher count using cached counts
        max_var = None
        max_domain = None
        max_count = -1

        for var_id, domain in variables:
            count = self._get_cached_watcher_count(var_id, engine)
            if count > max_count:
                max_count = count
                max_var = var_id
                max_domain = domain

        return max_var, max_domain

    def _get_cached_domain_size(self, var_id, domain):
        """Get domain size from cache or compute and cache it.

        Args:
            var_id: Variable ID
            domain: Domain object

        Returns:
            Domain size (integer)
        """
        # Use domain object identity for cache invalidation instead of just rev
        # This prevents issues with different Domain objects having the same rev
        domain_identity = id(domain)
        cache_key = (var_id, domain_identity)

        if cache_key in self._domain_size_cache:
            # Cache hit - same domain object
            return self._domain_size_cache[cache_key]

        # Cache miss - compute and cache
        size = domain.size()

        # Clean up old cache entries for this variable to prevent memory leaks
        old_keys = [k for k in self._domain_size_cache.keys() if k[0] == var_id]
        for old_key in old_keys:
            del self._domain_size_cache[old_key]

        # Cache new value
        self._domain_size_cache[cache_key] = size
        return size

    def _get_cached_watcher_count(self, var_id, engine):
        """Get watcher count from cache or compute and cache it.

        Args:
            var_id: Variable ID
            engine: Engine instance

        Returns:
            Total watcher count (integer)
        """
        attrs = get_fd_attrs(engine.store, var_id)

        # Use watchers dict itself as version indicator
        # If watchers dict changes, the object identity changes
        watchers_version = id(attrs.get("watchers", {})) if attrs else 0
        cache_key = var_id

        if cache_key in self._watcher_count_cache:
            cached_version, cached_count = self._watcher_count_cache[cache_key]
            if cached_version == watchers_version:
                # Cache hit - watchers haven't changed
                return cached_count

        # Cache miss or invalidated - recompute
        count = 0
        if attrs and "watchers" in attrs:
            for priority_set in attrs["watchers"].values():
                count += len(priority_set)

        self._watcher_count_cache[cache_key] = (watchers_version, count)
        return count
