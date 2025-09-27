"""Tests for reification operator parsing."""

import pytest
from prolog.parser.reader import Reader, ReaderError
from prolog.ast.terms import Struct, Var


class TestReificationOperatorParsing:
    """Test that reification operators parse correctly."""

    def test_equivalence_operator_parses(self):
        """Test B #<=> C parses as Struct."""
        reader = Reader()
        term = reader.read_term("B #<=> (X #= Y)")

        assert isinstance(term, Struct)
        assert term.functor == "#<=>"
        assert len(term.args) == 2

        # First arg should be variable B
        assert isinstance(term.args[0], Var)

        # Second arg should be the constraint (X #= Y)
        constraint = term.args[1]
        assert isinstance(constraint, Struct)
        assert constraint.functor == "#="

    def test_implication_operators_parse(self):
        """Test #==> and #<== operators."""
        reader = Reader()

        # Forward implication
        forward = reader.read_term("B #==> (X #< 5)")
        assert isinstance(forward, Struct)
        assert forward.functor == "#==>"
        assert len(forward.args) == 2

        # Backward implication
        backward = reader.read_term("B #<== (Y #> 3)")
        assert isinstance(backward, Struct)
        assert backward.functor == "#<=="
        assert len(backward.args) == 2

    def test_precedence_with_other_operators(self):
        """Test precedence interactions."""
        reader = Reader()

        # Should parse as (B #<=> (X #= Y)), not ((B #<=> X) #= Y)
        term = reader.read_term("B #<=> X #= Y")
        assert term.functor == "#<=>"

        # The second argument should be (X #= Y)
        inner = term.args[1]
        assert isinstance(inner, Struct)
        assert inner.functor == "#="
        assert len(inner.args) == 2

    def test_complex_nested_expressions(self):
        """Test complex nested reification expressions."""
        reader = Reader()

        # Test nested equivalence with comparison
        term = reader.read_term("B1 #<=> (X #< Y + 2)")
        assert term.functor == "#<=>"
        assert isinstance(term.args[1], Struct)
        assert term.args[1].functor == "#<"

        # Test chained implications
        term2 = reader.read_term("B1 #==> (B2 #<=> (X #= 5))")
        assert term2.functor == "#==>"
        nested = term2.args[1]
        assert isinstance(nested, Struct)
        assert nested.functor == "#<=>"

    def test_all_three_operators_together(self):
        """Test that all three reification operators work in one expression."""
        reader = Reader()

        # Complex expression using all operators
        expr = "B1 #<=> (X #= Y), B2 #==> (X #< 10), B3 #<== (Y #> 0)"
        term = reader.read_term(expr)

        # This should parse as conjunction of three constraints
        assert isinstance(term, Struct)
        assert term.functor == ","  # Conjunction

        # Walk the conjunction tree to find all three reification operators
        operators_found = set()

        def collect_functors(t):
            if isinstance(t, Struct):
                operators_found.add(t.functor)
                for arg in t.args:
                    collect_functors(arg)

        collect_functors(term)

        # Should find all three reification operators
        assert "#<=>" in operators_found
        assert "#==>" in operators_found
        assert "#<==" in operators_found

    def test_operator_associativity(self):
        """Test that operators are non-associative (xfx)."""
        reader = Reader()

        # These should fail to parse due to non-associativity
        with pytest.raises(ReaderError) as exc_info:
            # Can't chain without parentheses
            reader.read_term("B1 #<=> B2 #<=> B3")

        # Check that error message mentions non-chainable xfx operator
        error_msg = str(exc_info.value)
        assert "xfx operator" in error_msg or "non-chainable" in error_msg

        # But this should work with parentheses
        term = reader.read_term("B1 #<=> (B2 #<=> B3)")
        assert term.functor == "#<=>"
        assert isinstance(term.args[1], Struct)
        assert term.args[1].functor == "#<=>"