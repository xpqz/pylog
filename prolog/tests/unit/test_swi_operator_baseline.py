"""SWI-Prolog baseline tests for operator precedence and associativity - Issue #43.

This test module validates that PyLog's operator precedence and associativity
matches SWI-Prolog's behavior, ensuring standard ISO/SWI compliance.

Test Coverage:
- Control flow operators (,, ;, ->) precedence
- Arithmetic operator precedence and associativity
- Comparison operator non-chainability
- List/operator interaction
- Canonical form equivalence
"""

import pytest
from prolog.parser.parser import Parser
from prolog.parser.reader import Reader
from prolog.ast.terms import Struct, Atom, Int, Var, List
from prolog.ast.pretty import pretty


@pytest.mark.swi_baseline
class TestSWIPrecedenceBaseline:
    """Test that PyLog matches SWI-Prolog's operator precedence."""
    
    def test_comma_vs_semicolon_precedence(self, swi):
        """Verify , binds tighter than ; matching SWI."""
        reader = Reader()
        parser = Parser()
        
        # Test 'A , B ; C' - comma should bind tighter
        result1 = reader.read_term("a , b ; c")
        expected1 = Struct(";", (Struct(",", (Atom("a"), Atom("b"))), Atom("c")))
        assert result1 == expected1
        
        # Test 'A ; B , C' - semicolon has lower precedence
        result2 = reader.read_term("a ; b , c")
        expected2 = Struct(";", (Atom("a"), Struct(",", (Atom("b"), Atom("c")))))
        assert result2 == expected2
        
        # Verify SWI parses these the same way (using canonical forms)
        # SWI should parse "a , b ; c" as ";(','(a, b), c)"
        # We'll verify by checking execution counts
        prog = """
            test1 :- a , b ; c.
            test2 :- a ; b , c.
            a :- fail.
            b :- fail.
            c.
        """
        # test1 should succeed via c (after a,b fails)
        assert swi.count(prog, "test1") == 1
        # test2 should also succeed via b,c (after a fails)
        assert swi.count(prog, "test2") == 1
    
    def test_arrow_vs_semicolon_precedence(self, swi):
        """Verify -> binds tighter than ; matching SWI."""
        reader = Reader()
        
        # Test 'A -> B ; C' - arrow should bind tighter
        result = reader.read_term("a -> b ; c")
        expected = Struct(";", (Struct("->", (Atom("a"), Atom("b"))), Atom("c")))
        assert result == expected
        
        # Verify SWI behavior with if-then-else
        prog = """
            test1 :- (true -> b ; c).
            test2 :- (fail -> b ; c).
            b.
            c.
        """
        # test1 should succeed via b (true branch)
        assert swi.count(prog, "test1") == 1
        # test2 should succeed via c (false branch)
        assert swi.count(prog, "test2") == 1
    
    def test_arithmetic_precedence(self, swi):
        """Verify arithmetic operator precedence matches SWI."""
        reader = Reader()
        
        # Test '1 + 2 * 3' - multiplication binds tighter
        result1 = reader.read_term("1 + 2 * 3")
        expected1 = Struct("+", (Int(1), Struct("*", (Int(2), Int(3)))))
        assert result1 == expected1
        
        # Test '(1 + 2) * 3' - parentheses override
        result2 = reader.read_term("(1 + 2) * 3")
        expected2 = Struct("*", (Struct("+", (Int(1), Int(2))), Int(3)))
        assert result2 == expected2
        
        # Verify SWI evaluates these correctly
        # 1 + 2 * 3 = 1 + 6 = 7
        values1 = swi.onevar("", "X is 1 + 2 * 3", "X")
        assert values1 == ["7"]
        
        # (1 + 2) * 3 = 3 * 3 = 9
        values2 = swi.onevar("", "X is (1 + 2) * 3", "X")
        assert values2 == ["9"]
    
    def test_power_right_associativity(self, swi):
        """Verify ** is right-associative matching SWI."""
        reader = Reader()
        
        # Test '2 ** 3 ** 2' - should group as 2 ** (3 ** 2)
        result = reader.read_term("2 ** 3 ** 2")
        expected = Struct("**", (Int(2), Struct("**", (Int(3), Int(2)))))
        assert result == expected
        
        # Verify SWI evaluates this as 2 ** (3 ** 2) = 2 ** 9 = 512
        values = swi.onevar("", "X is 2 ** 3 ** 2", "X")
        # Note: SWI returns float for ** operator
        assert values == ["512.0"] or values == ["512"]
    
    def test_comparison_non_chainable(self, swi):
        """Verify comparison operators don't chain in SWI."""
        reader = Reader()
        
        # These should fail to parse in PyLog (xfx non-chainable)
        with pytest.raises(Exception) as exc1:
            reader.read_term("X = Y = Z")
        assert "non-chainable" in str(exc1.value).lower()
        
        with pytest.raises(Exception) as exc2:
            reader.read_term("X < Y < Z")
        assert "non-chainable" in str(exc2.value).lower()
        
        # SWI also doesn't allow chaining without parentheses
        # We can't directly test parse errors in SWI via our harness,
        # but we can verify that parenthesized forms work
        prog = """
            test1 :- X = 1, (X = Y), (Y = Z), Z = 1.
            test2 :- 1 < 2, 2 < 3.
        """
        assert swi.count(prog, "test1") == 1  # Works with explicit parentheses
        assert swi.count(prog, "test2") == 1  # Works as separate goals
    
    def test_list_operator_interaction(self, swi):
        """Verify operators work correctly with lists."""
        reader = Reader()
        
        # Test 'member(X, [1,2,3]), X > 0'
        result = reader.read_term("member(X, [1,2,3]), X > 0")
        expected = Struct(",", (
            Struct("member", (
                Var(0, "X"),
                List((Int(1), Int(2), Int(3)), None)
            )),
            Struct(">", (Var(0, "X"), Int(0)))
        ))
        assert result == expected
        
        # Verify SWI handles this correctly
        values = swi.onevar("", "member(X, [1,2,3]), X > 0", "X")
        assert values == ["1", "2", "3"]


@pytest.mark.swi_baseline
class TestSemanticEquivalence:
    """Test that operator forms execute identically to canonical forms."""
    
    def test_conjunction_equivalence(self, swi):
        """Verify 'A, B' executes same as ','(A, B)."""
        # Both forms should behave identically
        prog = """
            test1 :- a, b.
            test2 :- ','(a, b).
            a.
            b.
        """
        assert swi.count(prog, "test1") == 1
        assert swi.count(prog, "test2") == 1
        
        # Test with failure
        prog2 = """
            test3 :- a, fail.
            test4 :- ','(a, fail).
            a.
        """
        assert swi.count(prog2, "test3") == 0
        assert swi.count(prog2, "test4") == 0
    
    def test_disjunction_equivalence(self, swi):
        """Verify 'A; B' executes same as ';'(A, B)."""
        prog = """
            test1 :- a ; b.
            test2 :- ';'(a, b).
            a.
        """
        # Both succeed via a
        assert swi.count(prog, "test1") == 1
        assert swi.count(prog, "test2") == 1
        
        # Test backtracking
        prog2 = """
            test3 :- (a ; b), c.
            test4 :- ';'(a, b), c.
            a.
            b.
            c.
        """
        # Both should find 2 solutions (a,c and b,c)
        assert swi.count(prog2, "test3") == 2
        assert swi.count(prog2, "test4") == 2
    
    def test_if_then_else_equivalence(self, swi):
        """Verify '(A -> B; C)' executes same as canonical form."""
        prog = """
            test1 :- (true -> yes ; no).
            test2 :- ';'('->'(true, yes), no).
            test3 :- (fail -> yes ; no).
            test4 :- ';'('->'(fail, yes), no).
            yes.
            no.
        """
        # test1 and test2 should both succeed via yes
        assert swi.count(prog, "test1") == 1
        assert swi.count(prog, "test2") == 1
        # test3 and test4 should both succeed via no
        assert swi.count(prog, "test3") == 1
        assert swi.count(prog, "test4") == 1
    
    def test_arithmetic_evaluation_equivalence(self, swi):
        """Verify arithmetic expressions evaluate correctly."""
        # Test operator form
        values1 = swi.onevar("", "X is 3 + 4 * 2", "X")
        assert values1 == ["11"]
        
        # Test canonical form
        values2 = swi.onevar("", "X is '+'(3, '*'(4, 2))", "X")
        assert values2 == ["11"]
        
        # Test unary minus
        values3 = swi.onevar("", "X is -5 + 3", "X")
        assert values3 == ["-2"]
        
        values4 = swi.onevar("", "X is '+'('-'(5), 3)", "X")
        assert values4 == ["-2"]
    
    def test_comparison_evaluation_equivalence(self, swi):
        """Verify comparison operators work correctly."""
        prog = """
            test1 :- 3 < 5.
            test2 :- '<'(3, 5).
            test3 :- 5 =< 5.
            test4 :- '=<'(5, 5).
            test5 :- X = Y, X == Y.
            test6 :- '='(X, Y), '=='(X, Y).
        """
        assert swi.count(prog, "test1") == 1
        assert swi.count(prog, "test2") == 1
        assert swi.count(prog, "test3") == 1
        assert swi.count(prog, "test4") == 1
        assert swi.count(prog, "test5") == 1
        assert swi.count(prog, "test6") == 1


@pytest.mark.swi_baseline
class TestUnsupportedOperators:
    """Test that unsupported operators are handled correctly."""
    
    def test_integer_division_parses_but_fails_runtime(self):
        """// operator should parse but fail at runtime in dev mode."""
        reader = Reader()
        
        # Should parse successfully
        result = reader.read_term("X is 7 // 2")
        expected = Struct("is", (
            Var(0, "X"),
            Struct("//", (Int(7), Int(2)))
        ))
        assert result == expected
        
        # Note: We can't test runtime failure without engine implementation
        # This will be verified in integration tests
    
    def test_mod_operator_parses_but_fails_runtime(self):
        """mod operator should parse but fail at runtime in dev mode."""
        reader = Reader()
        
        # Should parse successfully
        result = reader.read_term("X is 7 mod 3")
        expected = Struct("is", (
            Var(0, "X"),
            Struct("mod", (Int(7), Int(3)))
        ))
        assert result == expected
        
        # Runtime behavior will be tested in integration tests
    
    def test_power_operator_parses(self):
        """** operator should parse (runtime support may vary)."""
        reader = Reader()
        
        # Should parse successfully
        result = reader.read_term("X is 2 ** 8")
        expected = Struct("is", (
            Var(0, "X"),
            Struct("**", (Int(2), Int(8)))
        ))
        assert result == expected


class TestPropertyBasedEquivalence:
    """Property-based tests for operator/canonical equivalence."""
    
    def test_parse_pretty_roundtrip_property(self):
        """Property: parse_op(s) == parse_canonical(pretty_canonical(parse_op(s)))."""
        reader = Reader()
        
        test_cases = [
            "a , b",
            "a ; b",
            "a -> b",
            "a , b ; c",
            "a ; b , c",
            "(a -> b ; c)",
            "1 + 2 * 3",
            "X = Y",
            "[1, 2, 3]",
            "foo(a, b, c)",
        ]
        
        for expr in test_cases:
            # Parse with operators
            parsed = reader.read_term(expr)
            
            # Convert to canonical form
            canonical_str = pretty(parsed, operator_mode=False)
            
            # Parse canonical form
            reparsed = reader.read_term(canonical_str)
            
            # Should be identical AST
            assert parsed == reparsed, f"Failed roundtrip for: {expr}"
    
    def test_operator_pretty_maintains_semantics(self):
        """Pretty printing with operators should maintain semantics."""
        reader = Reader()
        
        test_cases = [
            ("a , b ; c", "';'(','(a, b), c)"),
            ("a ; b , c", "';'(a, ','(b, c))"),
            ("1 + 2 * 3", "'+'(1, '*'(2, 3))"),
            ("(1 + 2) * 3", "'*'('+'(1, 2), 3)"),
        ]
        
        for op_form, canonical_form in test_cases:
            # Parse both forms
            op_ast = reader.read_term(op_form)
            canonical_ast = reader.read_term(canonical_form)
            
            # Should be identical
            assert op_ast == canonical_ast
            
            # Pretty print with operators should preserve precedence
            pretty_op = pretty(op_ast, operator_mode=True)
            reparsed = reader.read_term(pretty_op)
            assert reparsed == op_ast