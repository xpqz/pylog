"""
Stage 2: First-argument indexing infrastructure for performance optimization.

This module implements per-predicate indexing based on the principal functor
of the first argument. It provides significant speedup for clause selection
without changing Prolog semantics.

Key invariants:
- Source order is always preserved (Order ∩ Candidates pattern)
- Per-predicate isolation (no cross-contamination)
- Static program assumption (no dynamic predicates in Stage 2)
"""

from typing import Dict, List, Set, Tuple, Union, Iterator
from prolog.ast.terms import Term, Atom, Int, Var, Struct, List as PrologList
from prolog.unify.store import Store
from prolog.ast.clauses import Clause

# Type aliases for clarity
PredKey = Tuple[str, int]  # (predicate_name, arity)
ClauseID = int
FunctorKey = Tuple[str, int]  # (functor, arity)


class PredIndex:
    """
    Per-predicate index structure organizing clauses by first-argument type.

    Uses buckets to categorize clauses for efficient selection while
    maintaining source order through the order list.
    """

    __slots__ = (
        "order",  # List[int]: clause IDs in source order
        "var_ids",  # Set[int]: clauses with variable first arg
        "empty_list_ids",  # Set[int]: clauses with [] first arg
        "int_ids",  # Set[int]: clauses with integer first arg (type-based)
        "list_nonempty_ids",  # Set[int]: clauses with [H|T] first arg
        "struct_functor",  # Dict[FunctorKey, Set[ClauseID]]: (functor, arity) -> clause IDs
        "float_ids",  # Set[int]: placeholder for future float support
    )

    def __init__(self):
        """Initialize empty per-predicate index."""
        self.order: List[int] = []
        self.var_ids: Set[int] = set()
        self.empty_list_ids: Set[int] = set()
        self.int_ids: Set[int] = set()  # Type-based indexing for integers
        self.list_nonempty_ids: Set[int] = set()
        self.struct_functor: Dict[FunctorKey, Set[ClauseID]] = {}
        self.float_ids: Set[int] = set()  # Future extension


class ClauseIndex:
    """
    Global index mapping predicates to their clause indices.

    Maintains complete isolation between predicates and supports
    efficient clause selection based on first-argument indexing.
    """

    __slots__ = (
        "preds",  # Dict[PredKey, PredIndex]: (name, arity) -> per-pred index
        "clauses",  # Dict[Tuple[PredKey, ClauseID], Clause]: ((name, arity), id) -> clause
        "finalized",  # bool: whether index is finalized (static program)
        "supports_rebuild",  # bool: whether rebuilding is supported
    )

    def __init__(self):
        """Initialize empty global index."""
        self.preds: Dict[PredKey, PredIndex] = {}
        self.clauses: Dict[Tuple[PredKey, ClauseID], Clause] = {}
        self.finalized: bool = False
        self.supports_rebuild: bool = False  # Stage 2 assumes static programs

    def add_clause(self, clause: Clause) -> None:
        """
        Add a clause to the index.

        Raises AssertionError if index is finalized (static program assumption).
        """
        assert (
            not self.finalized
        ), "Cannot add clauses after index is built (static program assumption)"
        # This will be called during build_from_clauses
        raise NotImplementedError("Use build_from_clauses instead")

    def select(self, pred_key: PredKey, goal: Term, store: Store) -> Iterator[Clause]:
        """
        Select clauses for the given predicate and goal.

        Uses first-argument indexing to filter candidates, then yields
        clauses in source order (Order ∩ Candidates pattern).

        Args:
            pred_key: (predicate_name, arity) tuple
            goal: The goal term to match
            store: Store for dereferencing variables

        Yields:
            Matching clauses in source order
        """
        # Check if predicate exists (empty iterator if not)
        if pred_key not in self.preds:
            return

        pred_idx = self.preds[pred_key]

        # Handle zero-arity predicates: yield all in source order
        if pred_key[1] == 0:
            # All clauses match for zero-arity predicates
            for clause_id in pred_idx.order:
                yield self.clauses[(pred_key, clause_id)]
            return

        # Get the first argument of the goal
        if isinstance(goal, Struct) and goal.args and len(goal.args) >= 1:
            first_arg = goal.args[0]
        else:
            # Defensive: malformed goal shape for non-zero arity
            return

        # Dereference if it's a variable
        if isinstance(first_arg, Var):
            # Check if variable exists in store
            # TODO: Consider Store.has_var(varid) API to avoid knowing about cells
            if first_arg.id < len(store.cells):
                deref_result = store.deref(first_arg.id)
                if deref_result[0] == "BOUND":
                    # Use the bound term
                    first_arg = deref_result[2]
                else:
                    # Fast-path: Unbound variable matches everything
                    # Stream all clauses in source order without building a set
                    for clause_id in pred_idx.order:
                        yield self.clauses[(pred_key, clause_id)]
                    return
            else:
                # Variable not in store - treat as unbound
                # Fast-path: Unbound variable matches everything
                for clause_id in pred_idx.order:
                    yield self.clauses[(pred_key, clause_id)]
                return

        # Optimize: Instead of building a union set, check membership on-the-fly
        # This avoids O(N) memory allocation for large buckets

        # Determine which buckets match based on first argument type
        typed_bucket = None  # Primary type-specific bucket
        use_struct_bucket = False  # Whether to check struct_functor
        functor_key = None  # Key for struct_functor lookup

        if isinstance(first_arg, Int):
            # Integer matches all integer clauses (type-based)
            typed_bucket = pred_idx.int_ids
        elif isinstance(first_arg, PrologList):
            if len(first_arg.items) == 0:
                # Empty list
                typed_bucket = pred_idx.empty_list_ids
            else:
                # Non-empty list
                typed_bucket = pred_idx.list_nonempty_ids
        elif isinstance(first_arg, Atom):
            if first_arg.name == "[]":
                # Empty list atom
                typed_bucket = pred_idx.empty_list_ids
            else:
                # Regular atom
                functor_key = (first_arg.name, 0)
                use_struct_bucket = True
        elif isinstance(first_arg, Struct):
            # Check for canonical list representation
            if first_arg.functor == "." and len(first_arg.args) == 2:
                # Non-empty list in canonical form
                typed_bucket = pred_idx.list_nonempty_ids
            else:
                # Regular structure
                functor_key = (first_arg.functor, len(first_arg.args))
                use_struct_bucket = True

        # Yield clauses in source order (Order ∩ Candidates pattern)
        # Check membership on-the-fly without building union set

        # Hoist lookups outside loop to avoid repeated dict/attribute access
        var_ids = pred_idx.var_ids
        struct_set = (
            pred_idx.struct_functor.get(functor_key) if use_struct_bucket else None
        )

        for clause_id in pred_idx.order:
            # Check if clause matches: either in typed bucket, struct bucket, or var bucket
            match = False

            # Check primary typed bucket
            if typed_bucket is not None and clause_id in typed_bucket:
                match = True
            # Check struct functor bucket
            elif struct_set and clause_id in struct_set:
                match = True

            # Always check variable bucket (clauses with var first arg match everything)
            if not match and clause_id in var_ids:
                match = True

            if match:
                yield self.clauses[(pred_key, clause_id)]


def analyze_first_arg(head: Term, store: Store) -> Union[str, Tuple[str, str, int]]:
    """
    Analyze the first argument of a clause head to determine its type.

    Returns:
        - "no_args" for zero-arity predicates
        - "var" for variable first arguments
        - "empty_list" for the empty list []
        - "list_nonempty" for non-empty lists [H|T]
        - "int" for integer first arguments (type-based)
        - ("atom", name, 0) for atoms (treated as 0-arity structures)
        - ("struct", functor, arity) for compound structures
    """
    # Handle zero-arity predicates
    if isinstance(head, Atom):
        return "no_args"

    if not isinstance(head, Struct) or not head.args:
        return "no_args"

    # Get and dereference the first argument
    first_arg = head.args[0]
    # Note: In actual implementation, we'd dereference if we had a store with bindings
    # For index building, variables are unbound, so deref would return the variable itself

    # Check variable
    if isinstance(first_arg, Var):
        return "var"

    # Check integer (positive or negative) - type-based indexing
    if isinstance(first_arg, Int):
        return "int"

    # Check empty list - both representations
    if isinstance(first_arg, PrologList):
        if len(first_arg.items) == 0:
            return "empty_list"
        else:
            return "list_nonempty"

    # Check atom (including special [] atom)
    if isinstance(first_arg, Atom):
        if first_arg.name == "[]":
            return "empty_list"
        else:
            # Atoms are keyed in struct_functor with arity 0
            return ("atom", first_arg.name, 0)

    # Check struct
    if isinstance(first_arg, Struct):
        # Special case: canonical list representation '.'/2
        if first_arg.functor == "." and len(first_arg.args) == 2:
            return "list_nonempty"
        else:
            return ("struct", first_arg.functor, len(first_arg.args))

    # Should not reach here for well-formed Prolog terms
    raise ValueError(
        f"Unknown term type for first-argument analysis: {type(first_arg)}"
    )


def build_from_clauses(clauses: List[Clause]) -> ClauseIndex:
    """
    Build a complete index from a list of clauses.

    Maintains source order and ensures per-predicate isolation.
    The resulting index is finalized and assumes a static program.
    """
    idx = ClauseIndex()
    # Store reserved for future deref support (e.g., attributed variables)
    # Currently unused as we analyze clause heads at build time without bindings
    store = Store()

    # Track clause IDs per predicate
    pred_counters: Dict[PredKey, ClauseID] = {}

    for clause in clauses:
        # Determine predicate key
        head = clause.head
        if isinstance(head, Atom):
            pred_key = (head.name, 0)
        elif isinstance(head, Struct):
            pred_key = (head.functor, len(head.args))
        else:
            raise ValueError(f"Invalid clause head type: {type(head)}")

        # Ensure predicate has an index
        if pred_key not in idx.preds:
            idx.preds[pred_key] = PredIndex()
            pred_counters[pred_key] = 0

        # Get clause ID for this predicate (monotonic, starts at 0)
        clause_id = pred_counters[pred_key]
        pred_counters[pred_key] += 1

        # Add to order list
        pred_idx = idx.preds[pred_key]
        pred_idx.order.append(clause_id)

        # Store clause in global mapping
        idx.clauses[(pred_key, clause_id)] = clause

        # Analyze first argument and add to appropriate bucket
        arg_type = analyze_first_arg(head, store)

        if arg_type == "no_args":
            # Zero-arity predicates don't need bucketing
            pass
        elif arg_type == "var":
            pred_idx.var_ids.add(clause_id)
        elif arg_type == "empty_list":
            pred_idx.empty_list_ids.add(clause_id)
        elif arg_type == "list_nonempty":
            pred_idx.list_nonempty_ids.add(clause_id)
        elif arg_type == "int":
            pred_idx.int_ids.add(clause_id)
        elif isinstance(arg_type, tuple):
            if arg_type[0] == "atom":
                # Atoms go to struct_functor with arity 0
                functor_key = (arg_type[1], arg_type[2])
                if functor_key not in pred_idx.struct_functor:
                    pred_idx.struct_functor[functor_key] = set()
                pred_idx.struct_functor[functor_key].add(clause_id)
            elif arg_type[0] == "struct":
                # Structures go to struct_functor with their arity
                functor_key = (arg_type[1], arg_type[2])
                if functor_key not in pred_idx.struct_functor:
                    pred_idx.struct_functor[functor_key] = set()
                pred_idx.struct_functor[functor_key].add(clause_id)

    # Mark index as finalized
    idx.finalized = True

    return idx
