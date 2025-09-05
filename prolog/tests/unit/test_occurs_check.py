"""Test occurs check behavior in unification."""

import pytest
from prolog.ast.terms import Var, Struct, Atom, List as PrologList
from prolog.ast.clauses import Clause
from prolog.engine.engine import Engine
from prolog.tests.helpers import program


def test_occurs_check_on_prevents_cycles():
    """With occurs_check=True, X = f(X) should fail."""
    # Empty program
    prog = program()

    # Run with occurs_check=True
    engine = Engine(prog, occurs_check=True)

    # Query: X = f(X)
    # This is =(X, f(X))
    X = Var(0, "X")
    query = [Struct("=", (X, Struct("f", (X,))))]

    solutions = engine.run(query)
    assert len(solutions) == 0, "X = f(X) should fail with occurs_check=True"


def test_occurs_check_off_allows_cycles():
    """With occurs_check=False, X = f(X) should succeed."""
    # Empty program
    prog = program()

    # Run with occurs_check=False
    engine = Engine(prog, occurs_check=False)

    # Query: X = f(X)
    X = Var(0, "X")
    query = [Struct("=", (X, Struct("f", (X,))))]

    solutions = engine.run(query)
    assert len(solutions) == 1, "X = f(X) should succeed with occurs_check=False"
    # The solution will have X bound to f(X) creating a cycle


def test_occurs_check_deep_structure():
    """Test occurs check with deeper nesting."""
    # Empty program
    prog = program()

    # Query: X = g(f(X))
    X = Var(0, "X")
    query = [Struct("=", (X, Struct("g", (Struct("f", (X,)),))))]

    # With occurs_check=True, should fail
    engine_on = Engine(prog, occurs_check=True)
    solutions_on = engine_on.run(query)
    assert len(solutions_on) == 0, "X = g(f(X)) should fail with occurs_check=True"

    # With occurs_check=False, should succeed
    engine_off = Engine(prog, occurs_check=False)
    solutions_off = engine_off.run(query)
    assert len(solutions_off) == 1, "X = g(f(X)) should succeed with occurs_check=False"


def test_occurs_check_indirect():
    """Test occurs check with indirect cycles through multiple variables."""
    # Empty program
    prog = program()

    # Query: X = Y, Y = f(X)
    X = Var(0, "X")
    Y = Var(1, "Y")
    query = [Struct(",", (Struct("=", (X, Y)), Struct("=", (Y, Struct("f", (X,))))))]

    # With occurs_check=True, should fail
    engine_on = Engine(prog, occurs_check=True)
    solutions_on = engine_on.run(query)
    assert len(solutions_on) == 0, "Indirect cycle should fail with occurs_check=True"

    # With occurs_check=False, should succeed
    engine_off = Engine(prog, occurs_check=False)
    solutions_off = engine_off.run(query)
    assert (
        len(solutions_off) == 1
    ), "Indirect cycle should succeed with occurs_check=False"


def test_occurs_check_list():
    """Test occurs check with lists."""
    # Empty program
    prog = program()

    # Query: X = [a, X]
    X = Var(0, "X")
    query = [Struct("=", (X, PrologList((Atom("a"), X))))]

    # With occurs_check=True, should fail
    engine_on = Engine(prog, occurs_check=True)
    solutions_on = engine_on.run(query)
    assert len(solutions_on) == 0, "X = [a, X] should fail with occurs_check=True"

    # With occurs_check=False, should succeed
    engine_off = Engine(prog, occurs_check=False)
    solutions_off = engine_off.run(query)
    assert len(solutions_off) == 1, "X = [a, X] should succeed with occurs_check=False"


def test_occurs_check_list_tail():
    """Test occurs check with list tails."""
    # Empty program
    prog = program()

    # Query: X = [a|X]
    X = Var(0, "X")
    query = [Struct("=", (X, PrologList((Atom("a"),), tail=X)))]

    # With occurs_check=True, should fail
    engine_on = Engine(prog, occurs_check=True)
    solutions_on = engine_on.run(query)
    assert len(solutions_on) == 0, "X = [a|X] should fail with occurs_check=True"

    # With occurs_check=False, should succeed
    engine_off = Engine(prog, occurs_check=False)
    solutions_off = engine_off.run(query)
    assert len(solutions_off) == 1, "X = [a|X] should succeed with occurs_check=False"


def test_occurs_check_no_cycle():
    """Test that occurs check doesn't affect normal unification."""
    # Empty program
    prog = program()

    # Query: X = f(Y), Y = a
    X = Var(0, "X")
    Y = Var(1, "Y")
    query = [
        Struct(",", (Struct("=", (X, Struct("f", (Y,)))), Struct("=", (Y, Atom("a")))))
    ]

    # Should succeed with both occurs_check=True and False
    engine_on = Engine(prog, occurs_check=True)
    solutions_on = engine_on.run(query)
    assert (
        len(solutions_on) == 1
    ), "Normal unification should succeed with occurs_check=True"

    engine_off = Engine(prog, occurs_check=False)
    solutions_off = engine_off.run(query)
    assert (
        len(solutions_off) == 1
    ), "Normal unification should succeed with occurs_check=False"
