"""Safe binding helper for propagators."""

from prolog.ast.terms import Int
from prolog.unify.unify_helpers import bind_root_to_term


def safe_bind_singleton(varid, value, trail, store):
    """Safely bind a variable to a singleton value.

    This is safe to call during propagation because:
    1. It checks if the variable is already bound
    2. If bound, it verifies it's bound to the same value
    3. Only binds if the variable is unbound

    Args:
        varid: Variable ID to bind
        value: Integer value to bind to
        trail: Trail for recording changes
        store: Variable store

    Returns:
        True if binding succeeded or var already bound to same value
        False if var is bound to a different value
    """
    # Check current state
    cell = store.cells[varid]

    if cell.tag == "bound":
        # Already bound - check if it's the same value
        if isinstance(cell.term, Int) and cell.term.value == value:
            # Already bound to the same value, that's fine
            return True
        else:
            # Bound to different value - constraint violation
            return False

    # Not bound yet, safe to bind
    try:
        bind_root_to_term(varid, Int(value), trail, store)
        return True
    except ValueError:
        # Shouldn't happen but be defensive
        return False
