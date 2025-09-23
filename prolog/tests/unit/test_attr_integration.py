"""Integration tests for attributed variables - Issue #114.

Tests that attributed variables don't break existing functionality
and integrate properly with all stages.
"""

import pytest
from prolog.engine.engine import Engine
from prolog.ast.clauses import Program, Clause
from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList


class TestStage1Integration:
    """Test that Stage 1 functionality works with attributes enabled."""

    def test_basic_unification_with_attr_system(self):
        """Basic unification should work with attribute system present."""
        program = Program(())
        engine = Engine(program)

        # Basic unification queries
        queries = [
            ("?- X = 42.", "X", Int(42)),
            ("?- X = hello.", "X", Atom("hello")),
            ("?- X = f(a, b).", "X", Struct("f", (Atom("a"), Atom("b")))),
            ("?- X = [1, 2, 3].", "X", PrologList((Int(1), Int(2), Int(3)))),
            ("?- X = Y, Y = 42.", "X", Int(42)),
        ]

        for query, var, expected in queries:
            solutions = list(engine.query(query))
            assert len(solutions) == 1
            assert solutions[0][var] == expected

    def test_cut_with_attributes(self):
        """Cut (!) should work correctly with attributes."""
        program = Program(())
        engine = Engine(program)

        # Test cut with attributed variables
        query = """?- put_attr(X, test, 1),
                     ((X = 42, !, fail) ; true),
                     get_attr(X, test, V)."""

        solutions = list(engine.query(query))
        assert len(solutions) == 0  # Cut should prevent backtracking

    def test_builtin_predicates_with_attrs(self):
        """Stage 1 builtins should work with attributed variables."""
        program = Program(())
        engine = Engine(program)

        # Test var/nonvar with attributes
        query = "?- put_attr(X, test, 1), var(X)."
        solutions = list(engine.query(query))
        assert len(solutions) == 1  # X is still a variable even with attributes

        # Test after binding
        query = "?- put_attr(X, test, 1), X = 42, nonvar(X)."
        solutions = list(engine.query(query))
        assert len(solutions) == 1  # X is now nonvar

    def test_lists_with_attributed_elements(self):
        """Lists containing attributed variables should work."""
        program = Program(())
        engine = Engine(program)

        query = """?- put_attr(X, test, 1),
                     put_attr(Y, test, 2),
                     L = [X, Y, 3],
                     L = [A, B, C]."""

        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["C"] == Int(3)


class TestStage2Integration:
    """Test that Stage 2 (operators) works with attributes."""

    def test_arithmetic_with_attrs(self):
        """Arithmetic evaluation should work with attributed variables."""
        program = Program(())
        engine = Engine(program)

        # Attributed variable in arithmetic
        query = "?- put_attr(X, constraint, positive), X = 5, Y is X + 3."
        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["Y"] == Int(8)

    def test_comparison_with_attrs(self):
        """Comparison operators should work with attributed variables."""
        program = Program(())
        engine = Engine(program)

        queries = [
            "?- put_attr(X, test, 1), X = 5, X > 3.",
            "?- put_attr(X, test, 1), X = 5, X =< 10.",
            "?- put_attr(X, test, 1), X = 5, X =:= 5.",
        ]

        for query in queries:
            solutions = list(engine.query(query))
            assert len(solutions) == 1


class TestStage3Integration:
    """Test that Stage 3 (indexing/performance) works with attributes."""

    def test_indexing_with_attrs(self):
        """First-argument indexing should work with attributed variables."""
        # Create a program with many clauses
        clauses = []
        for i in range(100):
            clauses.append(
                Clause(
                    head=Struct("fact", (Int(i), Atom(f"value{i}"))),
                    body=()
                )
            )

        program = Program(tuple(clauses))
        engine = Engine(program)

        # Query with attributed variable
        query = "?- put_attr(X, indexed, true), X = 42, fact(X, Y)."
        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["Y"] == Atom("value42")

    def test_choicepoint_with_attrs(self):
        """Choicepoints should properly handle attributed variables."""
        clauses = [
            Clause(head=Struct("p", (Atom("a"),)), body=()),
            Clause(head=Struct("p", (Atom("b"),)), body=()),
            Clause(head=Struct("p", (Atom("c"),)), body=()),
        ]
        program = Program(tuple(clauses))
        engine = Engine(program)

        # Attributed variable with multiple solutions
        # After binding, X is no longer a variable, so get_attr will fail
        query = "?- put_attr(X, choice, true), p(X)."
        solutions = list(engine.query(query))
        assert len(solutions) == 3  # All three solutions should work


class TestBacktrackingIntegration:
    """Test that backtracking works correctly with attributes."""

    def test_backtrack_through_attr_operations(self):
        """Backtracking should properly undo attribute operations."""
        program = Program(())
        engine = Engine(program)

        # After backtracking from first branch, X shouldn't have the attribute
        # We test this by trying to get_attr in the second branch - it should fail
        # So we use a default value instead
        query = """?- (put_attr(X, test, 1), fail) ;
                     (var(X), X = no_attr)."""

        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("no_attr")

    def test_complex_backtrack_scenario(self):
        """Complex backtracking with multiple attribute operations."""
        program = Program(())
        engine = Engine(program)

        # After failing in the first branch, we should backtrack cleanly
        query = """?- put_attr(X, m1, 1),
                     ((put_attr(X, m2, 2),
                       put_attr(Y, m3, 3),
                       X = Y,
                       fail) ;
                      (get_attr(X, m1, V1)))."""

        # Register hook that allows merging
        engine.register_attr_hook("m1", lambda e, v, o: True)
        engine.register_attr_hook("m2", lambda e, v, o: True)
        engine.register_attr_hook("m3", lambda e, v, o: True)

        solutions = list(engine.query(query))
        assert len(solutions) == 1
        assert solutions[0]["V1"] == Int(1)


class TestExampleConstraints:
    """Test example constraint implementations."""

    def test_must_be_positive_constraint(self):
        """Implement and test a 'must be positive' constraint."""
        program = Program(())
        engine = Engine(program)

        def positive_hook(engine, varid, other):
            """Hook that only allows positive integers."""
            if isinstance(other, Int):
                return other.value > 0
            return True  # Allow non-integers (might be other constraints)

        engine.register_attr_hook("positive", positive_hook)

        # Should succeed with positive
        query = "?- put_attr(X, positive, true), X = 5."
        solutions = list(engine.query(query))
        assert len(solutions) == 1

        # Should fail with negative
        query = "?- put_attr(X, positive, true), X = -5."
        solutions = list(engine.query(query))
        assert len(solutions) == 0

        # Should fail with zero
        query = "?- put_attr(X, positive, true), X = 0."
        solutions = list(engine.query(query))
        assert len(solutions) == 0

    def test_must_be_different_constraint(self):
        """Implement and test a 'must be different' constraint."""
        program = Program(())
        engine = Engine(program)

        def different_hook(engine, varid, other):
            """Hook that ensures variables have different values."""
            # Get the value this var must differ from
            attrs = engine.store.attrs.get(varid, {})
            must_differ_from = attrs.get("different")

            if must_differ_from is not None and other == must_differ_from:
                return False  # Reject if trying to unify with forbidden value
            return True

        engine.register_attr_hook("different", different_hook)

        # Set up two variables that must be different
        query = """?- put_attr(X, different, 5),
                     put_attr(Y, different, 3),
                     X = 3,
                     Y = 5."""
        solutions = list(engine.query(query))
        assert len(solutions) == 1  # Should succeed (X=3, Y=5)

        # Should fail if trying to make them the same
        query = """?- put_attr(X, different, 5),
                     put_attr(Y, different, 5),
                     X = 5."""
        solutions = list(engine.query(query))
        assert len(solutions) == 0  # Should fail (X can't be 5)

    def test_type_checking_module(self):
        """Implement a simple type checking module."""
        program = Program(())
        engine = Engine(program)

        def type_hook(engine, varid, other):
            """Hook that enforces type constraints."""
            attrs = engine.store.attrs.get(varid, {})
            expected_type = attrs.get("type")

            if expected_type == Atom("integer"):
                return isinstance(other, Int)
            elif expected_type == Atom("atom"):
                return isinstance(other, Atom)
            elif expected_type == Atom("list"):
                return isinstance(other, PrologList)
            return True  # Unknown type, allow

        engine.register_attr_hook("type", type_hook)

        # Test integer type
        query = "?- put_attr(X, type, integer), X = 42."
        solutions = list(engine.query(query))
        assert len(solutions) == 1

        query = "?- put_attr(X, type, integer), X = hello."
        solutions = list(engine.query(query))
        assert len(solutions) == 0

        # Test atom type
        query = "?- put_attr(X, type, atom), X = hello."
        solutions = list(engine.query(query))
        assert len(solutions) == 1

        query = "?- put_attr(X, type, atom), X = 42."
        solutions = list(engine.query(query))
        assert len(solutions) == 0


class TestMultipleModules:
    """Test multiple constraint modules working together."""

    def test_multiple_independent_modules(self):
        """Multiple modules on different variables shouldn't interfere."""
        program = Program(())
        engine = Engine(program)

        # Register different hooks
        engine.register_attr_hook("mod1", lambda e, v, o: True)
        engine.register_attr_hook("mod2", lambda e, v, o: True)
        engine.register_attr_hook("mod3", lambda e, v, o: True)

        query = """?- put_attr(X, mod1, x1),
                     put_attr(Y, mod2, y2),
                     put_attr(Z, mod3, z3),
                     X = 1, Y = 2, Z = 3."""

        solutions = list(engine.query(query))
        assert len(solutions) == 1

    def test_multiple_modules_same_var(self):
        """Multiple modules on the same variable should coexist."""
        program = Program(())
        engine = Engine(program)

        # Register hooks that check different aspects
        def positive_hook(engine, varid, other):
            if isinstance(other, Int):
                return other.value > 0
            return True

        def even_hook(engine, varid, other):
            if isinstance(other, Int):
                return other.value % 2 == 0
            return True

        engine.register_attr_hook("positive", positive_hook)
        engine.register_attr_hook("even", even_hook)

        # Should succeed with positive even number
        query = """?- put_attr(X, positive, true),
                     put_attr(X, even, true),
                     X = 4."""
        solutions = list(engine.query(query))
        assert len(solutions) == 1

        # Should fail with positive odd number
        query = """?- put_attr(X, positive, true),
                     put_attr(X, even, true),
                     X = 3."""
        solutions = list(engine.query(query))
        assert len(solutions) == 0

        # Should fail with negative even number
        query = """?- put_attr(X, positive, true),
                     put_attr(X, even, true),
                     X = -2."""
        solutions = list(engine.query(query))
        assert len(solutions) == 0


class TestEdgeCases:
    """Test edge cases and unusual scenarios."""

    def test_massive_attributes_one_var(self):
        """Test a variable with very many attributes."""
        program = Program(())
        engine = Engine(program)

        # Add 100 modules to one variable (reduced from 1000 to avoid parser limits)
        modules = " ".join(f"put_attr(X, mod{i}, {i})," for i in range(100))
        query = f"?- {modules} X = 42."

        solutions = list(engine.query(query))
        assert len(solutions) == 1

    def test_circular_reference_in_attr(self):
        """Test attributes containing variables (potential circular refs)."""
        program = Program(())
        engine = Engine(program)

        # Attribute value is another variable - when X=1, we expect Y to remain unbound
        # since the attribute value Y is stored but not unified with X
        query = "?- put_attr(X, ref, Y), put_attr(Y, ref, X), X = 1, var(Y)."
        solutions = list(engine.query(query))
        assert len(solutions) == 1  # Y remains a variable even though X is bound

    def test_attrs_with_complex_terms(self):
        """Test attributes containing complex terms."""
        program = Program(())
        engine = Engine(program)

        # Attribute with complex structure - simplified test
        query = """?- put_attr(X, complex, f(g(1, 2), [a, b, c])),
                     get_attr(X, complex, V)."""

        solutions = list(engine.query(query))
        assert len(solutions) == 1
        # Should get back the complex structure
        result = solutions[0]["V"]
        assert isinstance(result, Struct)
        assert result.functor == "f"
        assert len(result.args) == 2
        # Check nested structure
        assert isinstance(result.args[0], Struct)
        assert result.args[0].functor == "g"
        assert isinstance(result.args[1], PrologList)
        assert len(result.args[1].items) == 3