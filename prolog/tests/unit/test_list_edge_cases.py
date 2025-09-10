"""
Tests for list edge cases in engine.
Covers improper lists, unbound tails, and special list handling.
"""

import pytest
from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.engine.engine import Engine
from prolog.tests.helpers import program, mk_fact, mk_rule


@pytest.fixture
def empty_engine():
    """Create an engine with an empty program."""
    return Engine(program())


class TestImproperLists:
    """Tests for improper list handling."""

    def test_improper_list_unification(self, empty_engine):
        """Test unification with improper list [H|T] where T is not a list."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # [1|2] - improper list with 2 as tail
        improper = Struct(".", (Int(1), Int(2)))
        query = Struct("=", (Struct(".", (X, Y)), improper))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
        assert solutions[0]["Y"] == Int(2)

    def test_improper_list_with_atom_tail(self, empty_engine):
        """Test improper list with atom as tail."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # [a|b] - improper list with atom b as tail
        improper = Struct(".", (Atom("a"), Atom("b")))
        query = Struct("=", (Struct(".", (X, Y)), improper))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("a")
        assert solutions[0]["Y"] == Atom("b")

    def test_improper_list_nested(self, empty_engine):
        """Test nested improper list."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        Z = Var(2, "Z")
        # [1, 2 | 3] - improper list
        improper = Struct(".", (Int(1), Struct(".", (Int(2), Int(3)))))
        query = Struct("=", (Struct(".", (X, Struct(".", (Y, Z)))), improper))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
        assert solutions[0]["Y"] == Int(2)
        assert solutions[0]["Z"] == Int(3)

    def test_improper_list_structure(self, empty_engine):
        """Test that improper lists have correct structure."""
        # [1|2] - improper list
        improper = Struct(".", (Int(1), Int(2)))
        X = Var(0, "X")
        Y = Var(1, "Y")
        # Check structure matches [X|Y] where Y=2
        query = Struct("=", (improper, Struct(".", (X, Y))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
        assert solutions[0]["Y"] == Int(2)


class TestUnboundTails:
    """Tests for lists with unbound tail variables."""

    def test_list_with_unbound_tail(self, empty_engine):
        """Test list with unbound tail variable."""
        X = Var(0, "X")
        Tail = Var(1, "Tail")
        # [1, 2 | Tail] where Tail is unbound
        partial_list = Struct(".", (Int(1), Struct(".", (Int(2), Tail))))
        query = Struct("=", (X, partial_list))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        # X should be the partial list structure
        result = solutions[0]["X"]
        assert isinstance(result, Struct)
        assert result.functor == "."
        # Check the structure: [1, 2 | Tail]
        assert result.args[0] == Int(1)
        second = result.args[1]
        assert isinstance(second, Struct) and second.functor == "."
        assert second.args[0] == Int(2)
        # The tail should be an unbound variable
        assert isinstance(second.args[1], Var)

    def test_unify_lists_with_unbound_tails(self, empty_engine):
        """Test unifying two lists with unbound tails."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        # [1, 2 | X] = [1, 2 | Y]
        list1 = Struct(".", (Int(1), Struct(".", (Int(2), X))))
        list2 = Struct(".", (Int(1), Struct(".", (Int(2), Y))))
        query = Struct("=", (list1, list2))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        # X and Y should be unified (both should be bound to the same unbound variable)
        # In the solution, X and Y will refer to the same variable
        sol_x = solutions[0].get("X")
        sol_y = solutions[0].get("Y")
        # Both should be variables and should have the same ID after unification
        assert isinstance(sol_x, Var)
        assert isinstance(sol_y, Var)
        assert sol_x.id == sol_y.id

    def test_append_like_unification(self, empty_engine):
        """Test list concatenation through unification patterns."""
        # Test that we can unify list patterns that would be used in append
        # [1, 2 | [3, 4]] = X
        X = Var(0, "X")
        # Build [1, 2 | [3, 4]]
        concat_list = Struct(".", (
            Int(1),
            Struct(".", (
                Int(2),
                List((Int(3), Int(4)))
            ))
        ))
        query = Struct("=", (concat_list, X))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        # X should be [1, 2, 3, 4]
        result = solutions[0]["X"]
        # Verify it's equivalent to the full list
        expected = List((Int(1), Int(2), Int(3), Int(4)))
        verify_query = Struct("=", (result, expected))
        verify_sols = empty_engine.run([verify_query])
        assert len(verify_sols) == 1

    def test_partial_list_unification(self, empty_engine):
        """Test unification with partial list (unbound tail)."""
        Tail = Var(0, "Tail")
        X = Var(1, "X")
        # [1, 2 | Tail] = X
        partial_list = Struct(".", (Int(1), Struct(".", (Int(2), Tail))))
        query = Struct("=", (partial_list, X))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        # X should be the partial list
        result = solutions[0]["X"]
        assert isinstance(result, Struct)
        assert result.functor == "."


class TestListStructures:
    """Tests for list structure handling."""

    def test_empty_list_unification(self, empty_engine):
        """Test [] unification."""
        X = Var(0, "X")
        query = Struct("=", (X, Atom("[]")))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Atom("[]")

    def test_dot_structure_as_list(self, empty_engine):
        """Test dot structure represents lists correctly."""
        # Manually construct [1, 2, 3] as dot structure
        list_struct = Struct(".", (
            Int(1),
            Struct(".", (
                Int(2),
                Struct(".", (Int(3), Atom("[]")))
            ))
        ))
        # Unify with List form
        query = Struct("=", (list_struct, List((Int(1), Int(2), Int(3)))))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1

    def test_list_with_variable_elements(self, empty_engine):
        """Test list with variable elements."""
        X = Var(0, "X")
        Y = Var(1, "Y")
        Z = Var(2, "Z")
        # [X, Y] = [1, 2]
        list_with_vars = List((X, Y))
        list_ground = List((Int(1), Int(2)))
        query = Struct("=", (list_with_vars, list_ground))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)
        assert solutions[0]["Y"] == Int(2)

    def test_nested_lists(self, empty_engine):
        """Test nested list structures."""
        # [[1, 2], [3, 4]]
        nested = List((
            List((Int(1), Int(2))),
            List((Int(3), Int(4)))
        ))
        X = Var(0, "X")
        query = Struct("=", (X, nested))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        # Check result is the nested list
        assert solutions[0]["X"] == nested

    def test_list_vs_structure_distinction(self, empty_engine):
        """Test that regular structures are distinct from lists."""
        # f(1, 2) vs [1, 2]
        struct = Struct("f", (Int(1), Int(2)))
        list_term = List((Int(1), Int(2)))
        query = Struct("\\=", (struct, list_term))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1  # They are not unifiable


class TestListPatternMatching:
    """Tests for list pattern matching."""

    def test_match_list_head_tail(self, empty_engine):
        """Test matching list with [H|T] pattern."""
        H = Var(0, "H")
        T = Var(1, "T")
        # [1, 2, 3] = [H|T]
        list_term = List((Int(1), Int(2), Int(3)))
        pattern = Struct(".", (H, T))
        query = Struct("=", (list_term, pattern))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["H"] == Int(1)
        # T should be [2, 3]
        assert solutions[0]["T"] == List((Int(2), Int(3)))

    def test_match_empty_list(self, empty_engine):
        """Test that [] doesn't match [H|T]."""
        H = Var(0, "H")
        T = Var(1, "T")
        # [] = [H|T] - should fail
        pattern = Struct(".", (H, T))
        query = Struct("=", (Atom("[]"), pattern))
        solutions = empty_engine.run([query])
        assert len(solutions) == 0

    def test_match_singleton_list(self, empty_engine):
        """Test matching [X] with [H|T]."""
        H = Var(0, "H")
        T = Var(1, "T")
        # [42] = [H|T]
        singleton = List((Int(42),))
        pattern = Struct(".", (H, T))
        query = Struct("=", (singleton, pattern))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["H"] == Int(42)
        assert solutions[0]["T"] == Atom("[]")

    def test_multi_level_destructuring(self, empty_engine):
        """Test matching [H1, H2 | T]."""
        H1 = Var(0, "H1")
        H2 = Var(1, "H2")
        T = Var(2, "T")
        # [a, b, c, d] = [H1, H2 | T]
        list_term = List((Atom("a"), Atom("b"), Atom("c"), Atom("d")))
        pattern = Struct(".", (H1, Struct(".", (H2, T))))
        query = Struct("=", (list_term, pattern))
        solutions = empty_engine.run([query])
        assert len(solutions) == 1
        assert solutions[0]["H1"] == Atom("a")
        assert solutions[0]["H2"] == Atom("b")
        assert solutions[0]["T"] == List((Atom("c"), Atom("d")))


class TestListMembershipEdgeCases:
    """Tests for member/2 edge cases."""

    def test_member_with_improper_list(self):
        """Test member/2 behavior with improper list."""
        # Define member/2
        p = program(
            # member(X, [X|_]).
            mk_fact("member", Var(0, "X"), Struct(".", (Var(0, "X"), Var(1, "_")))),
            # member(X, [_|T]) :- member(X, T).
            mk_rule("member", (Var(0, "X"), Struct(".", (Var(1, "_"), Var(2, "T")))),
                Struct("member", (Var(0, "X"), Var(2, "T")))
            )
        )
        engine = Engine(p)
        
        # member(2, [1|2]) - improper list with 2 as tail
        improper = Struct(".", (Int(1), Int(2)))
        query = Struct("member", (Int(2), improper))
        solutions = engine.run([query])
        # In standard Prolog, this fails when trying to recurse on atom tail
        # member(2, 2) would fail because 2 is not a list structure
        assert len(solutions) == 0

    def test_member_with_circular_list(self):
        """Test member/2 with potentially circular structure."""
        # Define member/2
        p = program(
            mk_fact("member", Var(0, "X"), Struct(".", (Var(0, "X"), Var(1, "_")))),
            mk_rule("member", (Var(0, "X"), Struct(".", (Var(1, "_"), Var(2, "T")))),
                Struct("member", (Var(0, "X"), Var(2, "T")))
            )
        )
        engine = Engine(p)
        
        X = Var(0, "X")
        # X = [1, 2 | X] - would create circular structure if allowed
        # This should fail with occurs check
        circular = Struct(".", (Int(1), Struct(".", (Int(2), X))))
        query_circular = Struct("=", (X, circular))
        
        if engine.occurs_check:
            solutions = engine.run([query_circular])
            assert len(solutions) == 0  # Should fail with occurs check

    def test_member_generates_list_elements(self):
        """Test member/2 can generate list elements."""
        # Define member/2
        p = program(
            mk_fact("member", Var(0, "X"), Struct(".", (Var(0, "X"), Var(1, "_")))),
            mk_rule("member", (Var(0, "X"), Struct(".", (Var(1, "_"), Var(2, "T")))),
                Struct("member", (Var(0, "X"), Var(2, "T")))
            )
        )
        engine = Engine(p)
        
        X = Var(0, "X")
        # member(X, [a, b, c])
        list_abc = List((Atom("a"), Atom("b"), Atom("c")))
        query = Struct("member", (X, list_abc))
        solutions = engine.run([query])
        
        # Should generate all three elements
        assert len(solutions) == 3
        values = [sol["X"] for sol in solutions]
        assert Atom("a") in values
        assert Atom("b") in values
        assert Atom("c") in values