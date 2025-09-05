"""Unit tests for Term representation classes.

Terms are the AST nodes for Prolog:
- Atom: Named constants (frozen)
- Int: Integer values (frozen)
- Var: Variables with ID and optional hint (mutable ID reference)
- Struct: Compound terms with functor and args (frozen)
- List: Prolog lists with items and tail (frozen)
"""

import pytest
from dataclasses import FrozenInstanceError

from prolog.ast.terms import Atom, Int, Var, Struct, List


# Test Atom class
def test_atom_creation():
    """Test creating an Atom."""

    atom = Atom(name="foo")
    assert atom.name == "foo"


def test_atom_immutability():
    """Test that Atom is immutable (frozen dataclass)."""

    atom = Atom(name="bar")
    with pytest.raises(FrozenInstanceError):
        atom.name = "baz"


def test_atom_equality():
    """Test Atom equality based on name."""

    atom1 = Atom(name="test")
    atom2 = Atom(name="test")
    atom3 = Atom(name="other")

    assert atom1 == atom2
    assert atom1 != atom3


def test_atom_hashable():
    """Test that Atoms are hashable (can be used in sets/dicts)."""

    atom1 = Atom(name="x")
    atom2 = Atom(name="x")
    atom3 = Atom(name="y")

    # Should be able to create a set
    atom_set = {atom1, atom2, atom3}
    assert len(atom_set) == 2  # atom1 and atom2 are equal


# Test Int class
def test_int_creation():
    """Test creating an Int."""

    int_term = Int(value=42)
    assert int_term.value == 42


def test_int_immutability():
    """Test that Int is immutable (frozen dataclass)."""

    int_term = Int(value=10)
    with pytest.raises(FrozenInstanceError):
        int_term.value = 20


def test_int_equality():
    """Test Int equality based on value."""

    int1 = Int(value=100)
    int2 = Int(value=100)
    int3 = Int(value=200)

    assert int1 == int2
    assert int1 != int3


def test_int_hashable():
    """Test that Ints are hashable."""

    int1 = Int(value=5)
    int2 = Int(value=5)
    int3 = Int(value=10)

    int_set = {int1, int2, int3}
    assert len(int_set) == 2


# Test Var class
def test_var_creation():
    """Test creating a Var."""

    var = Var(id=0)
    assert var.id == 0
    assert var.hint is None


def test_var_creation_with_hint():
    """Test creating a Var with a hint."""

    var = Var(id=5, hint="X")
    assert var.id == 5
    assert var.hint == "X"


def test_var_mutable():
    """Test that Var is mutable (not frozen)."""

    var = Var(id=0, hint="Y")
    # Should be able to modify
    var.id = 10
    var.hint = "Z"
    assert var.id == 10
    assert var.hint == "Z"


def test_var_equality():
    """Test Var equality based on id only."""

    var1 = Var(id=1, hint="A")
    var2 = Var(id=1, hint="B")  # Different hint, same id
    var3 = Var(id=2, hint="A")  # Same hint, different id

    assert var1 == var2  # Equal based on id
    assert var1 != var3


# Test Struct class
def test_struct_creation():
    """Test creating a Struct."""

    struct = Struct(functor="f", args=(Atom("a"), Int(1)))
    assert struct.functor == "f"
    assert len(struct.args) == 2
    assert struct.args[0] == Atom("a")
    assert struct.args[1] == Int(1)


def test_struct_immutability():
    """Test that Struct is immutable."""

    struct = Struct(functor="g", args=(Atom("x"),))

    with pytest.raises(FrozenInstanceError):
        struct.functor = "h"

    with pytest.raises(FrozenInstanceError):
        struct.args = (Atom("y"),)


def test_struct_empty_args():
    """Test Struct with no arguments."""

    struct = Struct(functor="nil", args=())
    assert struct.functor == "nil"
    assert len(struct.args) == 0


def test_struct_nested():
    """Test nested Struct creation."""

    inner = Struct(functor="g", args=(Atom("x"),))
    outer = Struct(functor="f", args=(inner, Atom("y")))

    assert outer.functor == "f"
    assert outer.args[0].functor == "g"
    assert outer.args[0].args[0] == Atom("x")


def test_struct_equality():
    """Test Struct equality."""

    s1 = Struct(functor="f", args=(Atom("a"), Int(1)))
    s2 = Struct(functor="f", args=(Atom("a"), Int(1)))
    s3 = Struct(functor="f", args=(Atom("b"), Int(1)))
    s4 = Struct(functor="g", args=(Atom("a"), Int(1)))

    assert s1 == s2
    assert s1 != s3  # Different args
    assert s1 != s4  # Different functor


def test_struct_hashable():
    """Test that Structs are hashable."""

    s1 = Struct(functor="f", args=(Atom("x"),))
    s2 = Struct(functor="f", args=(Atom("x"),))
    s3 = Struct(functor="f", args=(Atom("y"),))

    struct_set = {s1, s2, s3}
    assert len(struct_set) == 2


# Test List class
def test_list_creation_empty():
    """Test creating an empty List."""

    lst = List(items=())
    assert len(lst.items) == 0
    assert lst.tail == Atom("[]")  # Default empty tail


def test_list_creation_with_items():
    """Test creating a List with items."""

    lst = List(items=(Atom("a"), Int(1), Atom("b")))
    assert len(lst.items) == 3
    assert lst.items[0] == Atom("a")
    assert lst.items[1] == Int(1)
    assert lst.items[2] == Atom("b")
    assert lst.tail == Atom("[]")


def test_list_creation_with_custom_tail():
    """Test creating a List with custom tail."""

    tail_var = Var(id=0, hint="Tail")
    lst = List(items=(Atom("a"), Atom("b")), tail=tail_var)

    assert len(lst.items) == 2
    assert lst.tail == tail_var


def test_list_immutability():
    """Test that List is immutable."""

    lst = List(items=(Atom("x"),))

    with pytest.raises(FrozenInstanceError):
        lst.items = (Atom("y"),)

    with pytest.raises(FrozenInstanceError):
        lst.tail = Atom("different")


def test_list_nested():
    """Test nested List creation."""

    inner = List(items=(Atom("x"), Atom("y")))
    outer = List(items=(Atom("a"), inner, Atom("b")))

    assert len(outer.items) == 3
    assert outer.items[1] == inner


def test_list_equality():
    """Test List equality."""

    l1 = List(items=(Atom("a"), Atom("b")))
    l2 = List(items=(Atom("a"), Atom("b")))
    l3 = List(items=(Atom("a"), Atom("c")))

    var = Var(id=0)
    l4 = List(items=(Atom("a"), Atom("b")), tail=var)
    l5 = List(items=(Atom("a"), Atom("b")), tail=var)

    assert l1 == l2
    assert l1 != l3  # Different items
    assert l1 != l4  # Different tail
    assert l4 == l5  # Same items and tail


def test_list_hashable():
    """Test that Lists are hashable."""

    l1 = List(items=(Atom("a"), Atom("b")))
    l2 = List(items=(Atom("a"), Atom("b")))
    l3 = List(items=(Atom("a"), Atom("c")))

    list_set = {l1, l2, l3}
    assert len(list_set) == 2
