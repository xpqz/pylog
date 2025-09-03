"""Unit tests for the main unification algorithm.

The unify() function is the core of Prolog's pattern matching:
- Atomic terms (Atom, Int) unify if equal
- Variables unify with anything
- Structures unify if same functor/arity and all args unify
- Lists unify elementwise with tail unification
"""

import pytest

from prolog.unify.store import Store
from prolog.unify.trail import undo_to
from prolog.unify.unify import unify
from prolog.ast.terms import Atom, Int, Var, Struct, List


# Atomic Unification Tests
def test_unify_equal_atoms_succeeds():
    """Test unify equal atoms succeeds."""
    store = Store()
    trail = []
    
    a1 = Atom("hello")
    a2 = Atom("hello")
    
    result = unify(a1, a2, trail, store)
    assert result is True
    assert len(trail) == 0  # No variables, no trail


def test_unify_different_atoms_fails():
    """Test unify different atoms fails."""
    store = Store()
    trail = []
    
    a1 = Atom("hello")
    a2 = Atom("world")
    
    result = unify(a1, a2, trail, store)
    assert result is False
    assert len(trail) == 0


def test_unify_equal_ints_succeeds():
    """Test unify equal ints succeeds."""
    store = Store()
    trail = []
    
    i1 = Int(42)
    i2 = Int(42)
    
    result = unify(i1, i2, trail, store)
    assert result is True
    assert len(trail) == 0


def test_unify_different_ints_fails():
    """Test unify different ints fails."""
    store = Store()
    trail = []
    
    i1 = Int(42)
    i2 = Int(99)
    
    result = unify(i1, i2, trail, store)
    assert result is False
    assert len(trail) == 0


def test_unify_atom_with_int_fails():
    """Test unify atom with int fails."""
    store = Store()
    trail = []
    
    atom = Atom("test")
    num = Int(42)
    
    result = unify(atom, num, trail, store)
    assert result is False
    assert len(trail) == 0


# Variable Unification Tests
def test_unify_var_with_atom_binds_var():
    """Test unify var with atom binds var."""
    store = Store()
    trail = []
    
    vid = store.new_var()
    var = Var(vid)
    atom = Atom("test")
    
    result = unify(var, atom, trail, store)
    assert result is True
    
    # Variable should be bound to atom
    assert store.cells[vid].tag == "bound"
    assert store.cells[vid].term == atom
    assert len(trail) == 1  # One bind entry


def test_unify_var_with_struct_binds_var():
    """Test unify var with struct binds var."""
    store = Store()
    trail = []
    
    vid = store.new_var()
    var = Var(vid)
    struct = Struct("f", (Atom("a"), Int(1)))
    
    result = unify(var, struct, trail, store)
    assert result is True
    
    # Variable should be bound to struct
    assert store.cells[vid].tag == "bound"
    assert store.cells[vid].term == struct
    assert len(trail) == 1


def test_unify_two_vars_creates_union():
    """Test unify two vars creates union."""
    store = Store()
    trail = []
    
    v1 = store.new_var()
    v2 = store.new_var()
    var1 = Var(v1)
    var2 = Var(v2)
    
    result = unify(var1, var2, trail, store)
    assert result is True
    
    # Should be unified (point to same root)
    _, root1 = store.deref(v1)
    _, root2 = store.deref(v2)
    assert root1 == root2
    assert len(trail) >= 1  # At least one parent change


def test_unify_var_with_itself_succeeds():
    """Test unify var with itself succeeds (no-op)."""
    store = Store()
    trail = []
    
    vid = store.new_var()
    var = Var(vid)
    
    result = unify(var, var, trail, store)
    assert result is True
    assert len(trail) == 0  # No-op, no trail


def test_unify_atom_with_var_binds_var():
    """Test unify is symmetric: atom with var also binds var."""
    store = Store()
    trail = []
    
    vid = store.new_var()
    var = Var(vid)
    atom = Atom("test")
    
    # Reversed order from earlier test
    result = unify(atom, var, trail, store)
    assert result is True
    
    # Variable should be bound to atom
    assert store.cells[vid].tag == "bound"
    assert store.cells[vid].term == atom


def test_unify_bound_var_with_matching_term_succeeds():
    """Test unify bound var with matching term succeeds."""
    store = Store()
    trail = []
    
    vid = store.new_var()
    var = Var(vid)
    atom1 = Atom("test")
    atom2 = Atom("test")
    
    # First bind var to atom1
    unify(var, atom1, trail, store)
    mark = len(trail)
    
    # Now unify with matching atom2
    result = unify(var, atom2, trail, store)
    assert result is True
    assert len(trail) == mark  # No new trail entries


def test_unify_bound_var_with_different_term_fails():
    """Test unify bound var with different term fails."""
    store = Store()
    trail = []
    
    vid = store.new_var()
    var = Var(vid)
    atom1 = Atom("test1")
    atom2 = Atom("test2")
    
    # First bind var to atom1
    unify(var, atom1, trail, store)
    mark = len(trail)
    
    # Now try to unify with different atom2
    result = unify(var, atom2, trail, store)
    assert result is False
    
    # Should have undone any changes made during failed attempt
    assert len(trail) == mark


# Structural Unification Tests
def test_unify_structs_different_functors_fail():
    """Test structs with different functors fail."""
    store = Store()
    trail = []
    
    s1 = Struct("f", (Atom("a"),))
    s2 = Struct("g", (Atom("a"),))
    
    result = unify(s1, s2, trail, store)
    assert result is False
    assert len(trail) == 0


def test_unify_structs_different_arities_fail():
    """Test structs with different arities fail."""
    store = Store()
    trail = []
    
    s1 = Struct("f", (Atom("a"),))
    s2 = Struct("f", (Atom("a"), Atom("b")))
    
    result = unify(s1, s2, trail, store)
    assert result is False
    assert len(trail) == 0


def test_unify_structs_same_shape_unify_args():
    """Test structs with same shape unify args."""
    store = Store()
    trail = []
    
    v1 = store.new_var()
    v2 = store.new_var()
    
    s1 = Struct("f", (Var(v1), Atom("b")))
    s2 = Struct("f", (Atom("a"), Var(v2)))
    
    result = unify(s1, s2, trail, store)
    assert result is True
    
    # v1 should be bound to Atom("a")
    assert store.cells[v1].tag == "bound"
    assert store.cells[v1].term == Atom("a")
    
    # v2 should be bound to Atom("b")
    assert store.cells[v2].tag == "bound"
    assert store.cells[v2].term == Atom("b")


def test_unify_nested_structs():
    """Test nested structs unify recursively."""
    store = Store()
    trail = []
    
    v = store.new_var()
    
    # f(g(X), h(a))
    s1 = Struct("f", (
        Struct("g", (Var(v),)),
        Struct("h", (Atom("a"),))
    ))
    
    # f(g(b), h(a))
    s2 = Struct("f", (
        Struct("g", (Atom("b"),)),
        Struct("h", (Atom("a"),))
    ))
    
    result = unify(s1, s2, trail, store)
    assert result is True
    
    # X should be bound to b
    assert store.cells[v].tag == "bound"
    assert store.cells[v].term == Atom("b")


def test_unify_struct_argument_failure_undoes_partial():
    """Test that struct unification undoes partial work on failure."""
    store = Store()
    trail = []
    
    v1 = store.new_var()
    v2 = store.new_var()
    
    # f(X, Y)
    s1 = Struct("f", (Var(v1), Var(v2)))
    # f(a, a) but Y is already bound to b
    store.cells[v2].tag = "bound"
    store.cells[v2].term = Atom("b")
    
    s2 = Struct("f", (Atom("a"), Atom("a")))
    
    mark = len(trail)
    result = unify(s1, s2, trail, store)
    assert result is False
    
    # v1 should not remain bound (undone)
    assert store.cells[v1].tag == "unbound"
    assert len(trail) == mark


# List Unification Tests
def test_unify_empty_lists_succeed():
    """Test empty lists unify."""
    store = Store()
    trail = []
    
    l1 = List(())
    l2 = List(())
    
    result = unify(l1, l2, trail, store)
    assert result is True
    assert len(trail) == 0


def test_unify_lists_same_length():
    """Test lists of same length unify elementwise."""
    store = Store()
    trail = []
    
    v = store.new_var()
    
    l1 = List((Atom("a"), Var(v), Atom("c")))
    l2 = List((Atom("a"), Atom("b"), Atom("c")))
    
    result = unify(l1, l2, trail, store)
    assert result is True
    
    # Variable should be bound to b
    assert store.cells[v].tag == "bound"
    assert store.cells[v].term == Atom("b")


def test_unify_lists_different_length_fail():
    """Test lists of different length fail."""
    store = Store()
    trail = []
    
    l1 = List((Atom("a"), Atom("b")))
    l2 = List((Atom("a"), Atom("b"), Atom("c")))
    
    result = unify(l1, l2, trail, store)
    assert result is False


def test_unify_lists_with_tail():
    """Test lists unify with custom tail."""
    store = Store()
    trail = []
    
    v_tail = store.new_var()
    
    # [a, b | Tail]
    l1 = List((Atom("a"), Atom("b")), tail=Var(v_tail))
    # [a, b, c, d]
    l2 = List((Atom("a"), Atom("b"), Atom("c"), Atom("d")))
    
    result = unify(l1, l2, trail, store)
    assert result is True
    
    # Tail should be bound to [c, d]
    assert store.cells[v_tail].tag == "bound"
    expected_tail = List((Atom("c"), Atom("d")))
    assert store.cells[v_tail].term == expected_tail


def test_unify_lists_both_with_tails():
    """Test lists with tails unify tails."""
    store = Store()
    trail = []
    
    v1 = store.new_var()
    v2 = store.new_var()
    
    # [a | X]
    l1 = List((Atom("a"),), tail=Var(v1))
    # [a, b | Y]
    l2 = List((Atom("a"), Atom("b")), tail=Var(v2))
    
    result = unify(l1, l2, trail, store)
    assert result is True
    
    # X should be [b | Y]
    assert store.cells[v1].tag == "bound"
    assert store.cells[v1].term == List((Atom("b"),), tail=Var(v2))


def test_unify_nested_lists():
    """Test nested lists unify recursively."""
    store = Store()
    trail = []
    
    v = store.new_var()
    
    # [[a, X], [b]]
    l1 = List((
        List((Atom("a"), Var(v))),
        List((Atom("b"),))
    ))
    
    # [[a, c], [b]]
    l2 = List((
        List((Atom("a"), Atom("c"))),
        List((Atom("b"),))
    ))
    
    result = unify(l1, l2, trail, store)
    assert result is True
    
    # X should be bound to c
    assert store.cells[v].tag == "bound"
    assert store.cells[v].term == Atom("c")


# Mixed Structure Tests
def test_unify_struct_with_list_fails():
    """Test struct doesn't unify with list."""
    store = Store()
    trail = []
    
    s = Struct("f", (Atom("a"),))
    l = List((Atom("a"),))
    
    result = unify(s, l, trail, store)
    assert result is False


def test_unify_complex_nested_structure():
    """Test complex nested structure unification."""
    store = Store()
    trail = []
    
    x = store.new_var()
    y = store.new_var()
    z = store.new_var()
    
    # f(g(X, [a, Y]), Z)
    t1 = Struct("f", (
        Struct("g", (
            Var(x),
            List((Atom("a"), Var(y)))
        )),
        Var(z)
    ))
    
    # f(g(h(1), [a, b]), c)
    t2 = Struct("f", (
        Struct("g", (
            Struct("h", (Int(1),)),
            List((Atom("a"), Atom("b")))
        )),
        Atom("c")
    ))
    
    result = unify(t1, t2, trail, store)
    assert result is True
    
    # X = h(1)
    assert store.cells[x].tag == "bound"
    assert store.cells[x].term == Struct("h", (Int(1),))
    
    # Y = b
    assert store.cells[y].tag == "bound"
    assert store.cells[y].term == Atom("b")
    
    # Z = c
    assert store.cells[z].tag == "bound"
    assert store.cells[z].term == Atom("c")


# Undo/Backtracking Tests
def test_unify_failure_undoes_all_bindings():
    """Test that failed unification undoes all bindings."""
    store = Store()
    trail = []
    
    x = store.new_var()
    y = store.new_var()
    
    # f(X, Y) with Y pre-bound to 'conflict'
    store.cells[y].tag = "bound"
    store.cells[y].term = Atom("conflict")
    trail.append(("bind", y, store.cells[y]))  # Trail the pre-binding
    
    t1 = Struct("f", (Var(x), Var(y)))
    t2 = Struct("f", (Atom("a"), Atom("different")))
    
    mark = len(trail)
    result = unify(t1, t2, trail, store)
    assert result is False
    
    # X should not remain bound
    assert store.cells[x].tag == "unbound"
    # Trail should be back to mark
    assert len(trail) == mark


def test_unify_is_idempotent():
    """Test that repeated unification produces no new trail."""
    store = Store()
    trail = []
    
    v = store.new_var()
    t1 = Struct("f", (Var(v), Atom("a")))
    t2 = Struct("f", (Atom("b"), Atom("a")))
    
    # First unification
    result1 = unify(t1, t2, trail, store)
    assert result1 is True
    mark = len(trail)
    
    # Second unification (idempotent)
    result2 = unify(t1, t2, trail, store)
    assert result2 is True
    assert len(trail) == mark  # No new trail entries