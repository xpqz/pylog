"""Unit tests for the occurs check.

The occurs check prevents creating cyclic structures by detecting when
a variable would occur in its own binding. This is essential for
sound unification in many Prolog systems.

Examples of cycles that occurs check prevents:
- X = f(X)  (variable directly in its binding)
- X = f(g(X))  (variable nested in its binding)
- X = Y, Y = f(X)  (variable indirectly in binding through aliases)
"""

import pytest

from prolog.unify.store import Store
from prolog.unify.occurs import occurs
from prolog.ast.terms import Atom, Int, Var, Struct, List


# Basic non-occurrence tests
def test_occurs_var_not_in_atom():
    """Test var doesn't occur in atom."""
    store = Store()

    x = store.new_var()
    atom = Atom("test")

    assert occurs(x, atom, store) is False


def test_occurs_var_not_in_int():
    """Test var doesn't occur in integer."""
    store = Store()

    x = store.new_var()
    num = Int(42)

    assert occurs(x, num, store) is False


def test_occurs_var_not_in_different_var():
    """Test var doesn't occur in different unbound var."""
    store = Store()

    x = store.new_var()
    y = store.new_var()

    assert occurs(x, Var(y), store) is False


def test_occurs_var_in_itself():
    """Test var occurs in itself."""
    store = Store()

    x = store.new_var()

    assert occurs(x, Var(x), store) is True


# Structural occurrence tests
def test_occurs_var_in_struct_containing_it():
    """Test var occurs in struct containing it."""
    store = Store()

    x = store.new_var()
    # f(X)
    struct = Struct("f", (Var(x),))

    assert occurs(x, struct, store) is True


def test_occurs_var_in_nested_struct():
    """Test var occurs in deeply nested structure."""
    store = Store()

    x = store.new_var()
    # f(g(h(X)))
    struct = Struct("f", (Struct("g", (Struct("h", (Var(x),)),)),))

    assert occurs(x, struct, store) is True


def test_occurs_var_not_in_struct():
    """Test var doesn't occur in struct without it."""
    store = Store()

    x = store.new_var()
    y = store.new_var()
    # f(Y, a)
    struct = Struct("f", (Var(y), Atom("a")))

    assert occurs(x, struct, store) is False


def test_occurs_var_in_multiple_positions():
    """Test var occurring multiple times is still detected."""
    store = Store()

    x = store.new_var()
    # f(X, g(X), X)
    struct = Struct("f", (Var(x), Struct("g", (Var(x),)), Var(x)))

    assert occurs(x, struct, store) is True


# List occurrence tests
def test_occurs_var_in_list():
    """Test var occurs in list containing it."""
    store = Store()

    x = store.new_var()
    # [a, X, b]
    lst = List((Atom("a"), Var(x), Atom("b")))

    assert occurs(x, lst, store) is True


def test_occurs_var_in_list_tail():
    """Test var occurs in list tail."""
    store = Store()

    x = store.new_var()
    # [a, b | X]
    lst = List((Atom("a"), Atom("b")), tail=Var(x))

    assert occurs(x, lst, store) is True


def test_occurs_var_not_in_list():
    """Test var doesn't occur in list without it."""
    store = Store()

    x = store.new_var()
    y = store.new_var()
    # [a, Y, b]
    lst = List((Atom("a"), Var(y), Atom("b")))

    assert occurs(x, lst, store) is False


def test_occurs_var_in_nested_list():
    """Test var occurs in nested list structure."""
    store = Store()

    x = store.new_var()
    # [[X], [a, X]]
    lst = List((List((Var(x),)), List((Atom("a"), Var(x)))))

    assert occurs(x, lst, store) is True


# Alias chain tests
def test_occurs_through_alias_chain():
    """Test occurs check follows variable chains."""
    store = Store()

    x = store.new_var()
    y = store.new_var()
    z = store.new_var()

    # Create chain: Y -> Z
    store.cells[y].ref = z

    # Check if X occurs in Y (which points to Z)
    # This should follow Y to Z and check if X occurs in Z
    assert occurs(x, Var(y), store) is False

    # Now if we check if Z occurs in Y, it should be True
    # because Y dereferences to Z
    assert occurs(z, Var(y), store) is True


def test_occurs_in_bound_var():
    """Test occurs check on bound variable checks its binding."""
    store = Store()

    x = store.new_var()
    y = store.new_var()

    # Bind Y to f(X)
    store.cells[y].tag = "bound"
    store.cells[y].term = Struct("f", (Var(x),))

    # X occurs in Y because Y is bound to f(X)
    assert occurs(x, Var(y), store) is True


def test_occurs_with_aliased_occurrence():
    """Test occurs with variable that aliases to occurrence."""
    store = Store()

    x = store.new_var()
    y = store.new_var()
    z = store.new_var()

    # Y points to X
    store.cells[y].ref = x

    # f(Y) should detect occurrence of X since Y aliases to X
    struct = Struct("f", (Var(y),))
    assert occurs(x, struct, store) is True


# Cyclic structure tests
def test_occurs_prevents_direct_cycle():
    """Test occurs check prevents X = f(X)."""
    store = Store()

    x = store.new_var()
    # Trying to bind X to f(X) should be detected
    cycle = Struct("f", (Var(x),))

    assert occurs(x, cycle, store) is True


def test_occurs_prevents_indirect_cycle():
    """Test occurs check prevents X = Y, Y = f(X)."""
    store = Store()

    x = store.new_var()
    y = store.new_var()

    # X -> Y (X aliases to Y)
    store.cells[x].ref = y

    # Trying to bind Y to f(X) would create cycle
    cycle = Struct("f", (Var(x),))

    # Should detect that Y occurs in f(X) through the alias
    assert occurs(y, cycle, store) is True


def test_occurs_with_mutual_recursion():
    """Test occurs with mutually recursive structures."""
    store = Store()

    x = store.new_var()
    y = store.new_var()

    # Bind Y to g(X)
    store.cells[y].tag = "bound"
    store.cells[y].term = Struct("g", (Var(x),))

    # Trying to bind X to f(Y) would create mutual recursion
    # X = f(Y) = f(g(X))
    struct = Struct("f", (Var(y),))

    assert occurs(x, struct, store) is True


# Side effects and safety tests
def test_occurs_has_no_side_effects():
    """Test occurs never mutates the store."""
    store = Store()

    # Build a chain long enough that compression would be tempting
    vids = [store.new_var() for _ in range(8)]
    for i in range(len(vids) - 1):
        store.cells[vids[i]].ref = vids[i + 1]

    x = store.new_var()

    # Take snapshot before occurs check
    snapshot = [(c.tag, c.ref, c.term, c.rank) for c in store.cells]

    # Occurs on a term without X
    term = Struct("f", (Var(vids[0]), Atom("a")))
    assert occurs(x, term, store) is False

    # Verify no mutations occurred
    assert snapshot == [(c.tag, c.ref, c.term, c.rank) for c in store.cells]


def test_occurs_direct_alias_to_x():
    """Test occurs detects direct alias to the variable."""
    store = Store()

    x = store.new_var()
    y = store.new_var()

    # Y directly aliases to X
    store.cells[y].ref = x

    assert occurs(x, Var(y), store) is True


def test_occurs_invalid_varid_raises():
    """Test occurs raises on invalid variable IDs."""
    store = Store()

    with pytest.raises((IndexError, ValueError)):
        occurs(-1, Atom("a"), store)

    with pytest.raises((IndexError, ValueError)):
        occurs(10_000, Atom("a"), store)


def test_occurs_terminates_on_cyclic_term_not_involving_x():
    """Test occurs terminates on cyclic terms even when X not involved."""
    store = Store()

    x = store.new_var()
    y = store.new_var()

    # Create a rational tree for Y: Y = f(Y)
    # This simulates what might happen if occurs-check was off
    cycle = Struct("f", (Var(y),))
    store.cells[y].tag = "bound"
    store.cells[y].term = cycle

    # X is not part of the cycle, occurs() must return False and terminate
    assert occurs(x, Var(y), store) is False


def test_occurs_terminates_on_cyclic_term_involving_x():
    """Test occurs detects X in cyclic term."""
    store = Store()

    x = store.new_var()
    y = store.new_var()

    # Create a cyclic term that includes X: Y = f(X, Y)
    cycle = Struct("f", (Var(x), Var(y)))
    store.cells[y].tag = "bound"
    store.cells[y].term = cycle

    # X is part of the cycle
    assert occurs(x, Var(y), store) is True


def test_occurs_complex_list_tail_aliasing():
    """Test occurs with complex tail aliasing patterns."""
    store = Store()

    x = store.new_var()
    t = store.new_var()

    # [X | X] - X in both head and tail positions
    lst1 = List((Var(x),), tail=Var(x))
    assert occurs(x, lst1, store) is True

    # [[a | T] | T] - complex tail aliasing
    inner = List((Atom("a"),), tail=Var(t))
    lst2 = List((inner,), tail=Var(t))
    assert occurs(t, lst2, store) is True


# Edge cases
def test_occurs_empty_list():
    """Test occurs check on empty list."""
    store = Store()

    x = store.new_var()
    empty = List(())

    assert occurs(x, empty, store) is False


def test_occurs_empty_struct():
    """Test occurs check on struct with no arguments."""
    store = Store()

    x = store.new_var()
    empty_struct = Struct("f", ())

    assert occurs(x, empty_struct, store) is False


def test_occurs_deeply_nested_negative():
    """Test occurs check on deep structure without occurrence."""
    store = Store()

    x = store.new_var()
    y = store.new_var()
    z = store.new_var()

    # Deep structure without X
    deep = Struct(
        "f",
        (
            List(
                (
                    Struct("g", (Var(y), Atom("a"))),
                    Struct("h", (List((Var(z), Int(42))), Struct("i", (Atom("b"),)))),
                )
            ),
            Atom("c"),
        ),
    )

    assert occurs(x, deep, store) is False


def test_occurs_deeply_nested_positive():
    """Test occurs check finds variable in deep structure."""
    store = Store()

    x = store.new_var()
    y = store.new_var()

    # Deep structure with X buried inside
    deep = Struct(
        "f",
        (
            List(
                (
                    Struct("g", (Var(y), Atom("a"))),
                    Struct(
                        "h",
                        (
                            List((Int(42), Int(43))),
                            Struct(
                                "i",
                                (Struct("j", (Struct("k", (Var(x),)),)),),  # X is here
                            ),
                        ),
                    ),
                )
            ),
            Atom("c"),
        ),
    )

    assert occurs(x, deep, store) is True


def test_occurs_mixed_list_tail():
    """Test occurs in complex list with tail."""
    store = Store()

    x = store.new_var()
    y = store.new_var()

    # [f(X), g(a) | Y]
    lst = List((Struct("f", (Var(x),)), Struct("g", (Atom("a"),))), tail=Var(y))

    assert occurs(x, lst, store) is True
    assert occurs(y, lst, store) is True

    # Check a var that doesn't occur
    z = store.new_var()
    assert occurs(z, lst, store) is False


def test_occurs_handles_explicit_empty_tail():
    """Test occurs handles List with explicit empty tail."""
    store = Store()

    x = store.new_var()

    # List with explicit empty tail
    lst = List((Atom("a"), Var(x)), tail=Atom("[]"))

    assert occurs(x, lst, store) is True


# Performance/stress tests
def test_occurs_wide_structure():
    """Test occurs on structure with many arguments."""
    store = Store()

    x = store.new_var()

    # f(a1, a2, ..., X, ..., a100)
    args: list = [Atom(f"a{i}") for i in range(50)]
    args.append(Var(x))  # Add X in the middle
    args.extend([Atom(f"a{i}") for i in range(50, 100)])

    struct = Struct("f", tuple(args))

    assert occurs(x, struct, store) is True


def test_occurs_deep_nesting_iterative():
    """Test occurs handles deep nesting without recursion limit."""
    store = Store()

    x = store.new_var()

    # Build deeply nested structure: f(f(f(...f(X)...)))
    term = Var(x)
    for _ in range(1000):  # Deep enough to hit Python recursion limit
        term = Struct("f", (term,))

    # Should handle this iteratively without stack overflow
    assert occurs(x, term, store) is True


def test_occurs_returns_early_on_first_occurrence():
    """Test occurs returns as soon as it finds first occurrence."""
    store = Store()

    x = store.new_var()

    # Structure where X appears early
    # f(X, <lots of other stuff that shouldn't be checked>)
    struct = Struct(
        "f",
        (
            Var(x),  # Found immediately
            Struct(
                "g", tuple(Atom(f"a{i}") for i in range(1000))
            ),  # Shouldn't check all this
        ),
    )

    # Should return True quickly
    assert occurs(x, struct, store) is True


def test_occurs_finds_late_occurrence_too():
    """Test occurs still finds variable even after large non-X structure."""
    store = Store()

    x = store.new_var()

    # Structure where X appears late, after exploring a big chunk
    big = Struct("g", tuple(Atom(f"a{i}") for i in range(2000)))
    term = Struct("f", (big, Var(x)))  # X is at the end

    assert occurs(x, term, store) is True
