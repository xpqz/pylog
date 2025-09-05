"""Tests for Clause and Program structures (Stage 0)."""

import sys
import pytest
from dataclasses import FrozenInstanceError
from typing import Tuple

from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause, Program, ClauseCursor


# =============================================================================
# Test Helpers
# =============================================================================

from prolog.tests.helpers import mk_fact, mk_rule, program


# =============================================================================
# Clause Tests
# =============================================================================


class TestClause:
    """Tests for Clause dataclass."""

    def test_clause_fact_creation(self):
        """Test creating a fact (clause with empty body)."""
        fact = mk_fact("parent", Atom("john"), Atom("mary"))
        assert fact.head == Struct("parent", (Atom("john"), Atom("mary")))
        assert fact.body == ()
        assert len(fact.body) == 0

    def test_clause_rule_creation(self):
        """Test creating a rule (clause with body)."""
        # grandfather(X, Z) :- parent(X, Y), parent(Y, Z)
        rule = mk_rule(
            "grandfather",
            (Var(0, "X"), Var(2, "Z")),
            Struct("parent", (Var(0, "X"), Var(1, "Y"))),
            Struct("parent", (Var(1, "Y"), Var(2, "Z"))),
        )
        assert rule.head == Struct("grandfather", (Var(0, "X"), Var(2, "Z")))
        assert len(rule.body) == 2
        assert rule.body[0] == Struct("parent", (Var(0, "X"), Var(1, "Y")))
        assert rule.body[1] == Struct("parent", (Var(1, "Y"), Var(2, "Z")))

    def test_clause_immutability(self):
        """Test that Clause is immutable (frozen dataclass)."""
        fact = mk_fact("test", Atom("a"))

        # Should not be able to modify fields
        with pytest.raises(FrozenInstanceError):
            fact.head = Atom("other")

        with pytest.raises(FrozenInstanceError):
            fact.body = (Atom("b"),)

        # Body should be a tuple (immutable)
        assert isinstance(fact.body, tuple)

    def test_clause_atom_head_as_zero_arity(self):
        """Test that Atom heads are supported as 0-arity functors."""
        # true.  (0-arity fact)
        fact = Clause(head=Atom("true"), body=())
        assert fact.head == Atom("true")
        assert fact.body == ()

        # Also test with helper
        fact2 = mk_fact("true")
        assert fact2.head == Atom("true")
        assert fact2.body == ()

    def test_clause_equality(self):
        """Test clause equality."""
        fact1 = mk_fact("parent", Atom("john"), Atom("mary"))
        fact2 = mk_fact("parent", Atom("john"), Atom("mary"))
        fact3 = mk_fact("parent", Atom("john"), Atom("sue"))

        assert fact1 == fact2  # Same structure
        assert fact1 != fact3  # Different args

    def test_clause_with_complex_terms(self):
        """Test clause with lists and nested structures."""
        # member(X, [H|T]) :- member(X, T).
        rule = mk_rule(
            "member",
            (Var(0, "X"), PrologList((Var(1, "H"),), tail=Var(2, "T"))),
            Struct("member", (Var(0, "X"), Var(2, "T"))),
        )

        assert isinstance(rule.head, Struct)
        assert isinstance(rule.head.args[1], PrologList)
        assert rule.head.args[1].tail == Var(2, "T")


# =============================================================================
# Program Tests
# =============================================================================


class TestProgram:
    """Tests for Program class."""

    def test_program_stores_clauses_in_order(self):
        """Test that Program preserves clause order."""
        c1 = mk_fact("first", Atom("a"))
        c2 = mk_fact("second", Atom("b"))
        c3 = mk_fact("third", Atom("c"))

        prog = program(c1, c2, c3)

        assert len(prog.clauses) == 3
        assert prog.clauses[0] == c1
        assert prog.clauses[1] == c2
        assert prog.clauses[2] == c3

    def test_clauses_for_matching_functor_arity(self):
        """Test clauses_for returns matching functor/arity."""
        prog = program(
            mk_fact("parent", Atom("john"), Atom("mary")),
            mk_fact("parent", Atom("john"), Atom("sue")),
            mk_fact("child", Atom("mary"), Atom("john")),
            mk_rule("parent", (Var(0), Var(1)), Atom("true")),
        )

        # Should return indices of parent/2 clauses
        matches = prog.clauses_for("parent", 2)
        assert len(matches) == 3  # Two facts and one rule
        assert matches == [0, 1, 3]  # Indices in original order

        # Should return child/2 clauses
        matches = prog.clauses_for("child", 2)
        assert len(matches) == 1
        assert matches == [2]

    def test_clauses_for_discriminates_by_arity(self):
        """Test clauses_for discriminates by arity when same functor."""
        prog = program(
            mk_fact("f", Atom("a")),  # f/1
            mk_fact("f", Atom("a"), Atom("b")),  # f/2
            mk_fact("f"),  # f/0
            mk_fact("f", Atom("x"), Atom("y")),  # f/2
            mk_fact("f", Atom("z")),  # f/1
        )

        # f/0
        matches = prog.clauses_for("f", 0)
        assert matches == [2]

        # f/1
        matches = prog.clauses_for("f", 1)
        assert matches == [0, 4]

        # f/2
        matches = prog.clauses_for("f", 2)
        assert matches == [1, 3]

    def test_clauses_for_returns_empty_for_no_matches(self):
        """Test clauses_for returns empty list when no matches."""
        prog = program(
            mk_fact("parent", Atom("john"), Atom("mary")),
            mk_fact("child", Atom("mary"), Atom("john")),
        )

        # Non-existent predicate
        assert prog.clauses_for("grandparent", 2) == []

        # Wrong arity
        assert prog.clauses_for("parent", 3) == []
        assert prog.clauses_for("parent", 1) == []
        assert prog.clauses_for("parent", 0) == []

    def test_clauses_for_preserves_order(self):
        """Test clauses_for preserves source order."""
        prog = program(
            mk_fact("p", Int(3)),
            mk_fact("p", Int(1)),
            mk_fact("p", Int(2)),
            mk_fact("q", Atom("x")),
            mk_fact("p", Int(4)),
        )

        matches = prog.clauses_for("p", 1)
        assert matches == [0, 1, 2, 4]  # Original order preserved

    def test_clauses_for_repeated_calls_stable(self):
        """Test repeated calls to clauses_for return same order & indices."""
        prog = program(
            mk_fact("test", Atom("a")),
            mk_fact("test", Atom("b")),
            mk_fact("test", Atom("c")),
        )

        matches1 = prog.clauses_for("test", 1)
        matches2 = prog.clauses_for("test", 1)
        matches3 = prog.clauses_for("test", 1)

        assert matches1 == matches2 == matches3
        assert matches1 == [0, 1, 2]

    def test_program_clauses_immutable(self):
        """Test that Program clauses are immutable (tuple)."""
        c1 = mk_fact("test", Atom("a"))
        c2 = mk_fact("test", Atom("b"))
        prog = program(c1, c2)

        # clauses should be a tuple
        assert isinstance(prog.clauses, tuple)

        # Cannot modify the tuple
        with pytest.raises(TypeError):
            prog.clauses[0] = mk_fact("other")

    def test_program_handles_atom_heads(self):
        """Test Program handles 0-arity predicates (Atom heads)."""
        prog = program(
            Clause(head=Atom("true"), body=()),
            Clause(head=Atom("false"), body=()),
            mk_fact("true"),  # Another true/0
            mk_fact("test", Atom("a")),
        )

        # true/0 should match clauses with Atom("true") head
        matches = prog.clauses_for("true", 0)
        assert matches == [0, 2]

        # false/0
        matches = prog.clauses_for("false", 0)
        assert matches == [1]

    def test_clauses_for_mixed_arities(self):
        """Test clauses_for correctly discriminates between many arities."""
        prog = program(
            mk_fact("p"),  # p/0
            mk_fact("p", Atom("a")),  # p/1
            mk_fact("p", Atom("b"), Atom("c")),  # p/2
            mk_fact("p", Atom("d"), Atom("e"), Atom("f")),  # p/3
            mk_fact("p", Atom("g")),  # p/1
            mk_fact("q", Atom("x")),  # q/1
            mk_fact("p", Atom("h"), Atom("i")),  # p/2
        )

        # Test each arity
        assert prog.clauses_for("p", 0) == [0]
        assert prog.clauses_for("p", 1) == [1, 4]
        assert prog.clauses_for("p", 2) == [2, 6]
        assert prog.clauses_for("p", 3) == [3]
        assert prog.clauses_for("p", 4) == []  # No p/4
        assert prog.clauses_for("q", 1) == [5]

    def test_program_empty(self):
        """Test Program with no clauses."""
        prog = program()
        assert len(prog.clauses) == 0
        assert prog.clauses_for("anything", 0) == []
        assert prog.clauses_for("anything", 5) == []


# =============================================================================
# ClauseCursor Tests
# =============================================================================


class TestClauseCursor:
    """Tests for ClauseCursor class."""

    def test_cursor_initialization_with_matches(self):
        """Test cursor initialization with matches list."""
        cursor = ClauseCursor("test", 2, [0, 3, 5, 7])

        assert cursor.functor == "test"
        assert cursor.arity == 2
        assert cursor.matches == [0, 3, 5, 7]
        assert cursor.pos == 0

    def test_has_more_when_clauses_available(self):
        """Test has_more returns true when clauses available."""
        cursor = ClauseCursor("test", 1, [0, 1, 2])

        assert cursor.has_more() is True
        cursor.take()
        assert cursor.has_more() is True  # Still have 2 more
        cursor.take()
        assert cursor.has_more() is True  # Still have 1 more
        cursor.take()
        assert cursor.has_more() is False  # Exhausted

    def test_has_more_returns_false_when_exhausted(self):
        """Test has_more returns false when no more clauses."""
        cursor = ClauseCursor("test", 0, [])
        assert cursor.has_more() is False

        cursor2 = ClauseCursor("test", 1, [5])
        cursor2.take()
        assert cursor2.has_more() is False

    def test_peek_returns_next_without_advancing(self):
        """Test peek returns next clause index without advancing."""
        cursor = ClauseCursor("test", 2, [10, 20, 30])

        # Multiple peeks return same value
        assert cursor.peek() == 10
        assert cursor.peek() == 10
        assert cursor.peek() == 10

        # Position unchanged
        assert cursor.pos == 0

    def test_peek_after_take_reflects_next_element(self):
        """Test peek after take shows the next element."""
        cursor = ClauseCursor("test", 1, [5, 10, 15])

        assert cursor.peek() == 5
        cursor.take()
        assert cursor.peek() == 10
        cursor.take()
        assert cursor.peek() == 15
        cursor.take()
        assert cursor.peek() is None  # Exhausted

    def test_peek_take_when_empty_returns_none(self):
        """Test peek/take when empty returns None."""
        cursor = ClauseCursor("test", 0, [])
        assert cursor.peek() is None
        assert cursor.take() is None

        cursor2 = ClauseCursor("test", 1, [1])
        cursor2.take()  # Exhaust it
        assert cursor2.peek() is None
        assert cursor2.take() is None

    def test_take_returns_next_and_advances_once(self):
        """Test take returns next clause index and advances exactly once."""
        cursor = ClauseCursor("test", 3, [100, 200, 300])

        assert cursor.pos == 0
        assert cursor.take() == 100
        assert cursor.pos == 1
        assert cursor.take() == 200
        assert cursor.pos == 2
        assert cursor.take() == 300
        assert cursor.pos == 3
        assert cursor.take() is None  # Exhausted
        assert cursor.pos == 3  # Doesn't advance past end

    def test_clone_preserves_position(self):
        """Test clone creates copy at current position."""
        cursor = ClauseCursor("pred", 2, [10, 20, 30, 40])
        cursor.take()  # Advance to position 1
        cursor.take()  # Advance to position 2

        clone = cursor.clone()

        assert clone.functor == "pred"
        assert clone.arity == 2
        assert clone.matches == [10, 20, 30, 40]
        assert clone.pos == 2
        assert clone.peek() == 30

    def test_clone_isolation(self):
        """Test clone mutations don't affect original.

        Note: We test position isolation. The matches list can be shared
        (shallow copy) as it's not mutated - only the position changes.
        """
        cursor = ClauseCursor("test", 1, [1, 2, 3])
        cursor.take()  # pos = 1

        clone = cursor.clone()

        # Advance clone
        assert clone.take() == 2
        assert clone.pos == 2

        # Original unchanged
        assert cursor.pos == 1
        assert cursor.peek() == 2

        # Advance original
        assert cursor.take() == 2
        assert cursor.pos == 2

        # Clone unchanged by original's advance
        assert clone.pos == 2
        assert clone.peek() == 3

    def test_cursor_exhausts_exactly_matches_list(self):
        """Test cursor exhausts exactly the matches list (no duplicates/skips)."""
        matches = [5, 10, 15, 20, 25]
        cursor = ClauseCursor("test", 2, matches)

        collected = []
        while cursor.has_more():
            collected.append(cursor.take())

        assert collected == matches  # Exact match, no duplicates or skips

        # Further takes return None
        assert cursor.take() is None
        assert cursor.take() is None

    def test_cursor_with_empty_matches(self):
        """Test cursor handles empty matches list correctly."""
        cursor = ClauseCursor("nonexistent", 5, [])

        assert cursor.has_more() is False
        assert cursor.peek() is None
        assert cursor.take() is None
        assert cursor.pos == 0

        # Clone of empty cursor
        clone = cursor.clone()
        assert clone.has_more() is False
        assert clone.matches == []
        assert clone.pos == 0
