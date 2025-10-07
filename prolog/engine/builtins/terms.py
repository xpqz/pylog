"""Term construction and inspection builtin predicates.

This module contains builtin predicates for term manipulation:
- =../2 (univ) - structure ↔ list conversion
- functor/3 - functor/arity manipulation
- Structural comparisons: ==/2, \\==/2, @</2, @>/2, @=</2, @>=/2

These predicates are extracted from engine.py as part of Phase 4 of the
engine refactoring plan.
"""

from typing import Dict, Tuple, Callable, Optional
from prolog.ast.terms import Term, Atom, Int, Float, Var, Struct, List as PrologList
from prolog.unify.unify import unify
from prolog.engine.trail_adapter import TrailAdapter

__all__ = [
    "register",
    "builtin_univ",
    "builtin_functor",
    "builtin_structural_equal",
    "builtin_structural_not_equal",
    "builtin_term_less",
    "builtin_term_greater",
    "builtin_term_less_equal",
    "builtin_term_greater_equal",
]


def register(registry: Dict[Tuple[str, int], Callable]) -> None:
    """Register term construction/inspection builtin predicates.

    Args:
        registry: Dictionary mapping (predicate_name, arity) -> callable
    """
    registry[("=..", 2)] = builtin_univ
    registry[("functor", 3)] = builtin_functor
    registry[("==", 2)] = builtin_structural_equal
    registry[("\\==", 2)] = builtin_structural_not_equal
    registry[("@<", 2)] = builtin_term_less
    registry[("@>", 2)] = builtin_term_greater
    registry[("@=<", 2)] = builtin_term_less_equal
    registry[("@>=", 2)] = builtin_term_greater_equal


def builtin_univ(engine, args: tuple) -> bool:
    """=..(Term, List) - structure ↔ list conversion (univ).

    Three modes:
    1. Decomposition: foo(a,b) =.. L binds L to [foo,a,b]
    2. Construction: X =.. [foo,a,b] binds X to foo(a,b)
    3. Checking: foo(a,b) =.. [foo,a,b] succeeds
    """
    if len(args) != 2:
        return False

    left, right = args
    trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)

    # Dereference both arguments
    if isinstance(left, Var):
        left_result = engine.store.deref(left.id)
        if left_result[0] == "BOUND":
            left = left_result[2]
            left_unbound = False
        else:
            left_unbound = True
    else:
        left_unbound = False

    if isinstance(right, Var):
        right_result = engine.store.deref(right.id)
        if right_result[0] == "BOUND":
            right = right_result[2]
            right_unbound = False
        else:
            right_unbound = True
    else:
        right_unbound = False

    # Case 1: Decomposition mode (Term =.. X where X unbound)
    if not left_unbound and right_unbound:
        decomposed = _decompose_to_list(left)
        if decomposed is None:
            return False
        return unify(
            right, decomposed, engine.store, trail_adapter, engine.occurs_check
        )

    # Case 2: Construction mode (X =.. [foo,a,b] where X unbound)
    elif left_unbound and not right_unbound:
        if not isinstance(right, PrologList):
            return False

        # Convert to Python list
        python_list = _prolog_list_to_python_list(engine, right)
        if python_list is None:
            return False

        if len(python_list) == 0:
            return False

        if len(python_list) == 1:
            # Single element - just unify with the element
            constructed = python_list[0]
        else:
            # Multi-element list - first is functor, rest are args
            functor_term = python_list[0]

            # Handle special case: dot constructor
            if isinstance(functor_term, Atom) and functor_term.name == ".":
                if len(python_list) != 3:
                    return False
                # Construct list [second|third]
                constructed = PrologList((python_list[1],), python_list[2])
            elif isinstance(functor_term, Atom):
                constructed = Struct(functor_term.name, tuple(python_list[1:]))
            else:
                # Non-atom functor with args is invalid
                return False

        return unify(
            left, constructed, engine.store, trail_adapter, engine.occurs_check
        )

    # Case 3: Checking mode (both bound) or unified decomposition/construction
    else:
        # If both are bound, check that they match
        if not left_unbound and not right_unbound:
            decomposed = _decompose_to_list(left)
            if decomposed is None:
                return False
            return unify(
                decomposed, right, engine.store, trail_adapter, engine.occurs_check
            )
        else:
            # Both unbound - fail
            return False


def builtin_functor(engine, args: tuple) -> bool:
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
    trail_adapter = TrailAdapter(engine.trail, engine=engine, store=engine.store)

    # Dereference term argument
    if isinstance(term_arg, Var):
        term_result = engine.store.deref(term_arg.id)
        if term_result[0] == "BOUND":
            term = term_result[2]
            term_unbound = False
        else:
            term_unbound = True
            term = term_arg
    else:
        term = term_arg
        term_unbound = False

    # Dereference functor argument
    if isinstance(functor_arg, Var):
        functor_result = engine.store.deref(functor_arg.id)
        if functor_result[0] == "BOUND":
            functor = functor_result[2]
            functor_unbound = False
        else:
            functor_unbound = True
            functor = functor_arg
    else:
        functor = functor_arg
        functor_unbound = False

    # Dereference arity argument
    if isinstance(arity_arg, Var):
        arity_result = engine.store.deref(arity_arg.id)
        if arity_result[0] == "BOUND":
            arity = arity_result[2]
            arity_unbound = False
        else:
            arity_unbound = True
            arity = arity_arg
    else:
        arity = arity_arg
        arity_unbound = False

    # Mode 1: Extraction (term is bound, functor and/or arity unbound)
    if not term_unbound:
        extraction = _extract_functor_arity(term)
        if extraction is None:
            return False

        extracted_functor, extracted_arity = extraction

        # Unify functor
        if not unify(
            functor, extracted_functor, engine.store, trail_adapter, engine.occurs_check
        ):
            return False

        # Unify arity
        return unify(
            arity, extracted_arity, engine.store, trail_adapter, engine.occurs_check
        )

    # Mode 2: Construction (term unbound, functor and arity bound)
    elif term_unbound and not functor_unbound and not arity_unbound:
        # Validate arity is integer
        if not isinstance(arity, Int):
            return False

        if arity.value < 0:
            return False

        # Construct based on functor type
        if isinstance(functor, Atom):
            functor_name = functor.name

            if arity.value == 0:
                # Zero arity - just the atom itself
                constructed = functor
            else:
                # Create structure with fresh variables
                # Note: Even for "." functor, we create a Struct, not a PrologList
                # The conversion between Struct('.', (A, B)) and PrologList happens
                # at a different layer, not in functor/3
                fresh_vars = tuple(
                    Var(engine.store.new_var(), "_") for _ in range(arity.value)
                )
                constructed = Struct(functor_name, fresh_vars)

        elif isinstance(functor, Int):
            # Integer functor
            if arity.value == 0:
                # Zero arity - just the integer itself
                constructed = functor
            else:
                # Can't create structure with integer functor and non-zero arity
                return False
        else:
            # Invalid functor type
            return False

        return unify(
            term, constructed, engine.store, trail_adapter, engine.occurs_check
        )

    # Mode 3: All other cases fail
    else:
        # - Term is unbound but functor or arity (or both) are also unbound
        # - This is insufficient instantiation for construction
        return False


def builtin_structural_equal(engine, args: tuple) -> bool:
    """==(X, Y) - structural equality comparison."""
    if len(args) != 2:
        return False

    left, right = args
    return _structural_equality_compare(engine, left, right)


def builtin_structural_not_equal(engine, args: tuple) -> bool:
    """\\==(X, Y) - structural inequality comparison."""
    if len(args) != 2:
        return False

    left, right = args
    return not _structural_equality_compare(engine, left, right)


def builtin_term_less(engine, args: tuple) -> bool:
    """@<(X, Y) - term ordering less than."""
    if len(args) != 2:
        return False

    left, right = args
    return _structural_compare(engine, left, right) < 0


def builtin_term_greater(engine, args: tuple) -> bool:
    """@>(X, Y) - term ordering greater than."""
    if len(args) != 2:
        return False

    left, right = args
    return _structural_compare(engine, left, right) > 0


def builtin_term_less_equal(engine, args: tuple) -> bool:
    """@=<(X, Y) - term ordering less than or equal."""
    if len(args) != 2:
        return False

    left, right = args
    return _structural_compare(engine, left, right) <= 0


def builtin_term_greater_equal(engine, args: tuple) -> bool:
    """@>=(X, Y) - term ordering greater than or equal."""
    if len(args) != 2:
        return False

    left, right = args
    return _structural_compare(engine, left, right) >= 0


# Helper functions


def _decompose_to_list(term: Term) -> Optional[PrologList]:
    """Decompose a term into list form for =../2.

    Args:
        term: The term to decompose

    Returns:
        PrologList representation or None if term cannot be decomposed
    """
    if isinstance(term, Struct):
        # Structure: foo(a,b) -> [foo, a, b]
        items = [Atom(term.functor)] + list(term.args)
        return _make_prolog_list(items)
    elif isinstance(term, PrologList):
        # List: [a,b] -> ['.', a, [b]]
        if not term.items and isinstance(term.tail, Atom) and term.tail.name == "[]":
            # Empty list [] -> [[]]
            return _make_prolog_list([Atom("[]")])
        else:
            # Non-empty list becomes dot structure
            if term.items:
                tail = (
                    PrologList(term.items[1:], term.tail)
                    if len(term.items) > 1
                    else term.tail
                )
                return _make_prolog_list([Atom("."), term.items[0], tail])
            else:
                # Shouldn't happen but handle gracefully
                return None
    elif isinstance(term, Atom):
        # Atom: foo -> [foo]
        return _make_prolog_list([term])
    elif isinstance(term, (Int, Float)):
        # Number: 42 -> [42]
        return _make_prolog_list([term])
    else:
        return None


def _make_prolog_list(items: list) -> PrologList:
    """Create a proper Prolog list from Python list."""
    if not items:
        return PrologList((), Atom("[]"))
    return PrologList(tuple(items), Atom("[]"))


def _prolog_list_to_python_list(engine, lst: PrologList) -> Optional[list]:
    """Convert Prolog list to Python list, or None if improper."""
    result = []
    current = lst

    while True:
        if isinstance(current, PrologList):
            result.extend(current.items)
            current = current.tail
        elif isinstance(current, Atom) and current.name == "[]":
            # Proper list termination
            break
        elif isinstance(current, Var):
            var_result = engine.store.deref(current.id)
            if var_result[0] == "BOUND":
                current = var_result[2]
                continue
            else:
                # Unbound tail variable - improper list
                return None
        else:
            # Improper list (non-[] tail that's not a list)
            return None

    return result


def _extract_functor_arity(term: Term) -> Optional[Tuple[Term, Term]]:
    """Extract functor and arity from a term.

    Returns:
        Tuple of (functor_term, arity_term) or None if term type unknown
    """
    if isinstance(term, Struct):
        # Structure: functor is atom, arity is arg count
        return (Atom(term.functor), Int(len(term.args)))
    elif isinstance(term, PrologList):
        if not term.items and isinstance(term.tail, Atom) and term.tail.name == "[]":
            # Empty list: [] has functor '[]' and arity 0
            return (Atom("[]"), Int(0))
        else:
            # Non-empty list: has functor '.' and arity 2
            return (Atom("."), Int(2))
    elif isinstance(term, Atom):
        # Atom: functor is itself, arity is 0
        return (term, Int(0))
    elif isinstance(term, (Int, Float)):
        # Number: functor is itself, arity is 0
        return (term, Int(0))
    else:
        return None


def _structural_compare(engine, term1: Term, term2: Term) -> int:
    """Standard term ordering comparison.

    Returns: -1 if term1 < term2, 0 if equal, 1 if term1 > term2
    Standard order: variables < numbers < atoms < compound terms

    This implements ISO Prolog standard term ordering with proper float support.
    """
    # Dereference terms first
    term1 = _deref_term(engine, term1)
    term2 = _deref_term(engine, term2)

    # Get type ordering values
    type1 = _term_order_type(term1)
    type2 = _term_order_type(term2)

    if type1 != type2:
        return type1 - type2

    # Same type - compare within type
    if isinstance(term1, Var) and isinstance(term2, Var):
        if term1.id < term2.id:
            return -1
        elif term1.id > term2.id:
            return 1
        else:
            return 0
    elif isinstance(term1, (Int, Float)) and isinstance(term2, (Int, Float)):
        # Compare numbers by numeric value (supports Int/Float mixing)
        val1 = term1.value
        val2 = term2.value
        if val1 < val2:
            return -1
        elif val1 > val2:
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
            cmp = _structural_compare(engine, arg1, arg2)
            if cmp != 0:
                return cmp
        return 0
    elif isinstance(term1, PrologList) and isinstance(term2, PrologList):
        # Compare list elements and tail
        min_len = min(len(term1.items), len(term2.items))
        for i in range(min_len):
            cmp = _structural_compare(engine, term1.items[i], term2.items[i])
            if cmp != 0:
                return cmp
        # If one list is shorter, it comes first
        if len(term1.items) != len(term2.items):
            return len(term1.items) - len(term2.items)
        # Compare tails
        return _structural_compare(engine, term1.tail, term2.tail)
    else:
        # Mixed compound types (Struct vs PrologList) - fallback comparison
        str1 = str(term1)
        str2 = str(term2)
        if str1 < str2:
            return -1
        elif str1 > str2:
            return 1
        else:
            return 0


def _term_order_type(term: Term) -> int:
    """Get term ordering type value.

    ISO Prolog standard ordering:
    - Variables: 0
    - Numbers (Int/Float): 1
    - Atoms: 2
    - Compound terms (Struct/PrologList): 3
    """
    if isinstance(term, Var):
        return 0
    elif isinstance(term, (Int, Float)):  # Both Int and Float are numbers
        return 1
    elif isinstance(term, Atom):
        return 2
    elif isinstance(term, (Struct, PrologList)):
        return 3
    else:
        return 4


def _deref_term(engine, term: Term) -> Term:
    """Dereference a term if it's a variable."""
    if isinstance(term, Var):
        result = engine.store.deref(term.id)
        if result[0] == "BOUND":
            return result[2]
    return term


def _structural_equality_compare(engine, term1: Term, term2: Term) -> bool:
    """Structural equality comparison (exact type matching required).

    Unlike term ordering, structural equality requires exact type matching:
    - Int(3) != Float(3.0) (different types)
    - Var(0) == Var(0) (same variable)
    - Var(0) != Var(1) (different variables)
    """
    # Dereference terms first
    term1 = _deref_term(engine, term1)
    term2 = _deref_term(engine, term2)

    # Different types are never equal
    if type(term1) is not type(term2):
        return False

    # Same type - compare within type
    if isinstance(term1, Var):
        return term1.id == term2.id
    elif isinstance(term1, Int):
        return term1.value == term2.value
    elif isinstance(term1, Float):
        return term1.value == term2.value
    elif isinstance(term1, Atom):
        return term1.name == term2.name
    elif isinstance(term1, Struct):
        if term1.functor != term2.functor or len(term1.args) != len(term2.args):
            return False
        return all(
            _structural_equality_compare(engine, arg1, arg2)
            for arg1, arg2 in zip(term1.args, term2.args)
        )
    elif isinstance(term1, PrologList):
        if len(term1.items) != len(term2.items):
            return False
        return all(
            _structural_equality_compare(engine, item1, item2)
            for item1, item2 in zip(term1.items, term2.items)
        ) and _structural_equality_compare(engine, term1.tail, term2.tail)
    else:
        # Unknown types - use string comparison as fallback
        return str(term1) == str(term2)
