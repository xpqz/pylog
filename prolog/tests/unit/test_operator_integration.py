"""Integration tests for operator support - Issue #43.

This test module validates that operators work correctly with the full
PyLog system, including the engine, REPL, and file loading.

Test Coverage:
- Full Stage 1 test suite compatibility
- Operator execution matches canonical forms
- Backtracking order preservation
- REPL operator input
- File loading with operators
- Backwards compatibility
"""

import pytest
from prolog.engine.engine import Engine
from prolog.engine.patches import create_dev_engine
from prolog.ast.clauses import Program
from prolog.ast.terms import Atom, Int, Struct
from prolog.parser import parser
from prolog.parser.reader import Reader


# Create dev mode engine class
DevEngine = create_dev_engine(Engine)


class TestOperatorExecution:
    """Test that operator forms execute correctly in the engine."""

    def test_conjunction_execution(self):
        """Conjunction operator executes same as canonical form."""
        # Test with operator form
        clauses1 = parser.parse_program(
            """
            test1 :- a, b.
            a.
            b.
        """
        )
        engine1 = DevEngine(Program(tuple(clauses1)))
        goals1 = parser.parse_query("?- test1.")
        solutions1 = list(engine1.run(goals1))
        assert len(solutions1) == 1

        # Test with canonical form
        clauses2 = parser.parse_program(
            """
            test2 :- ','(a, b).
            a.
            b.
        """
        )
        engine2 = DevEngine(Program(tuple(clauses2)))
        goals2 = parser.parse_query("?- test2.")
        solutions2 = list(engine2.run(goals2))
        assert len(solutions2) == 1

    def test_disjunction_execution(self):
        """Disjunction operator executes same as canonical form."""
        # Test with operator form
        clauses1 = parser.parse_program(
            """
            test1(X) :- (X = a ; X = b).
        """
        )
        engine1 = DevEngine(Program(tuple(clauses1)))
        goals1 = parser.parse_query("?- test1(X).")
        solutions1 = list(engine1.run(goals1))
        assert len(solutions1) == 2
        assert solutions1[0]["X"] == Atom("a")
        assert solutions1[1]["X"] == Atom("b")

        # Test with canonical form
        clauses2 = parser.parse_program(
            """
            test2(X) :- ';'('='(X, a), '='(X, b)).
        """
        )
        engine2 = DevEngine(Program(tuple(clauses2)))
        goals2 = parser.parse_query("?- test2(X).")
        solutions2 = list(engine2.run(goals2))
        assert len(solutions2) == 2
        assert solutions2[0]["X"] == Atom("a")
        assert solutions2[1]["X"] == Atom("b")

    def test_if_then_else_execution(self):
        """If-then-else operator executes correctly."""
        clauses = parser.parse_program(
            """
            test(X, Y) :- (X > 0 -> Y = positive ; Y = negative).
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        # Test positive case
        goals1 = parser.parse_query("?- test(5, Y).")
        solutions1 = list(engine.run(goals1))
        assert len(solutions1) == 1
        assert solutions1[0]["Y"] == Atom("positive")

        # Test negative case
        goals2 = parser.parse_query("?- test(-3, Y).")
        solutions2 = list(engine.run(goals2))
        assert len(solutions2) == 1
        assert solutions2[0]["Y"] == Atom("negative")

    def test_arithmetic_operators_in_is(self):
        """Arithmetic operators work correctly with is/2."""
        clauses = parser.parse_program(
            """
            calc1(X) :- X is 2 + 3 * 4.
            calc2(X) :- X is (2 + 3) * 4.
            calc3(X) :- X is 10 - 3 - 2.
            calc4(X) :- X is -5 + 10.
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        # 2 + 3 * 4 = 2 + 12 = 14
        goals1 = parser.parse_query("?- calc1(X).")
        solutions1 = list(engine.run(goals1))
        assert len(solutions1) == 1
        assert solutions1[0]["X"] == Int(14)

        # (2 + 3) * 4 = 5 * 4 = 20
        goals2 = parser.parse_query("?- calc2(X).")
        solutions2 = list(engine.run(goals2))
        assert len(solutions2) == 1
        assert solutions2[0]["X"] == Int(20)

        # 10 - 3 - 2 = 5 (left associative)
        goals3 = parser.parse_query("?- calc3(X).")
        solutions3 = list(engine.run(goals3))
        assert len(solutions3) == 1
        assert solutions3[0]["X"] == Int(5)

        # -5 + 10 = 5
        goals4 = parser.parse_query("?- calc4(X).")
        solutions4 = list(engine.run(goals4))
        assert len(solutions4) == 1
        assert solutions4[0]["X"] == Int(5)

    def test_arithmetic_comparison_operators(self):
        """Arithmetic comparison operators implemented: <, =<, >, >=, =:=, =\\=."""
        clauses = parser.parse_program(
            """
            test1 :- 5 > 3.
            test2 :- 4 =:= 4.
            test3 :- 10 > 20.
            test4(X) :- X is 2 + 2, X =:= 4.
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        # > operator should work
        goals1 = parser.parse_query("?- test1.")
        solutions1 = list(engine.run(goals1))
        assert len(solutions1) == 1

        # =:= operator works with literals
        goals2 = parser.parse_query("?- test2.")
        solutions2 = list(engine.run(goals2))
        assert len(solutions2) == 1

        # Failed comparison
        goals3 = parser.parse_query("?- test3.")
        solutions3 = list(engine.run(goals3))
        assert len(solutions3) == 0

        # =:= with evaluated expression
        goals4 = parser.parse_query("?- test4(X).")
        solutions4 = list(engine.run(goals4))
        assert len(solutions4) == 1
        assert solutions4[0]["X"] == Int(4)

    def test_comparison_operators(self):
        """Comparison operators work correctly."""
        clauses = parser.parse_program(
            """
            test1 :- 3 < 5.
            test2 :- 5 =< 5.
            test3 :- 7 > 4.
            test4 :- 8 >= 8.
            test5 :- 2 + 2 =:= 4.
            test6 :- 3 =\\= 4.
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        for i in range(1, 7):
            goals = parser.parse_query(f"?- test{i}.")
            solutions = list(engine.run(goals))
            assert len(solutions) == 1, f"test{i} failed"

    def test_structural_comparison(self):
        """Structural comparison operators work correctly."""
        clauses = parser.parse_program(
            """
            test1 :- foo(a) = foo(a).
            test2 :- foo(X) = foo(a), X == a.
            test3 :- a \\= b.
            test4 :- foo(a) \\== foo(b).
            test5 :- atom(a) @< atom(b).
            test6 :- f(2) @> f(1).
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        for i in range(1, 7):
            goals = parser.parse_query(f"?- test{i}.")
            solutions = list(engine.run(goals))
            assert len(solutions) == 1, f"test{i} failed"

    def test_unbound_var_in_comparison_fails_dev_mode(self):
        """Unbound variables in arithmetic comparisons fail (don't throw) in dev mode."""
        clauses = parser.parse_program("t :- X > 1.")
        engine = DevEngine(Program(tuple(clauses)))
        goals = parser.parse_query("?- t.")
        # Should fail, not throw
        assert list(engine.run(goals)) == []

    def test_comparison_with_comma_precedence(self):
        """Comparison operators work correctly with comma precedence."""
        reader = Reader()

        # Comma should have lower precedence than comparisons
        ast = reader.read_term("X =< 5, Y >= 2")
        assert isinstance(ast, Struct)
        assert ast.functor == ","
        # Left arg is X =< 5
        assert isinstance(ast.args[0], Struct)
        assert ast.args[0].functor == "=<"
        # Right arg is Y >= 2
        assert isinstance(ast.args[1], Struct)
        assert ast.args[1].functor == ">="


class TestBacktrackingOrder:
    """Test that backtracking order is preserved with operators."""

    def test_disjunction_backtracking_order(self):
        """Disjunction backtracks in correct order."""
        clauses = parser.parse_program(
            """
            test(X) :- X = 1 ; X = 2 ; X = 3.
        """
        )
        engine = DevEngine(Program(tuple(clauses)))
        goals = parser.parse_query("?- test(X).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 3
        assert solutions[0]["X"] == Int(1)
        assert solutions[1]["X"] == Int(2)
        assert solutions[2]["X"] == Int(3)

    def test_nested_disjunction_backtracking(self):
        """Nested disjunctions backtrack correctly."""
        clauses = parser.parse_program(
            """
            test(X, Y) :- (X = a ; X = b), (Y = 1 ; Y = 2).
        """
        )
        engine = DevEngine(Program(tuple(clauses)))
        goals = parser.parse_query("?- test(X, Y).")
        solutions = list(engine.run(goals))

        assert len(solutions) == 4
        # Order should be: (a,1), (a,2), (b,1), (b,2)
        assert solutions[0]["X"] == Atom("a") and solutions[0]["Y"] == Int(1)
        assert solutions[1]["X"] == Atom("a") and solutions[1]["Y"] == Int(2)
        assert solutions[2]["X"] == Atom("b") and solutions[2]["Y"] == Int(1)
        assert solutions[3]["X"] == Atom("b") and solutions[3]["Y"] == Int(2)

    def test_cut_in_disjunction(self):
        """Cut in disjunction prevents backtracking correctly."""
        clauses = parser.parse_program(
            """
            test1(X) :- (X = 1, ! ; X = 2).
            test2(X) :- (X = 1 ; X = 2, !).
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        # First test: cut after X=1 prevents X=2
        goals1 = parser.parse_query("?- test1(X).")
        solutions1 = list(engine.run(goals1))
        assert len(solutions1) == 1
        assert solutions1[0]["X"] == Int(1)

        # Second test: both solutions available, cut after X=2
        goals2 = parser.parse_query("?- test2(X).")
        solutions2 = list(engine.run(goals2))
        assert len(solutions2) == 2
        assert solutions2[0]["X"] == Int(1)
        assert solutions2[1]["X"] == Int(2)


class TestStage1Compatibility:
    """Test that all Stage 1 tests still pass with operators enabled."""

    def test_basic_unification_still_works(self):
        """Basic unification works with operators enabled."""
        clauses = parser.parse_program(
            """
            unify(X, X).
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        # Test with operator in query
        goals = parser.parse_query("?- unify(A, B), A = 5, B = 5.")
        solutions = list(engine.run(goals))
        assert len(solutions) == 1
        assert solutions[0]["A"] == Int(5)
        assert solutions[0]["B"] == Int(5)

    def test_list_operations_with_operators(self):
        """List operations work with operator syntax."""
        clauses = parser.parse_program(
            """
            member(X, [X|_]).
            member(X, [_|T]) :- member(X, T).
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        # Test with operators in query
        goals = parser.parse_query("?- member(X, [1,2,3]), X > 1.")
        solutions = list(engine.run(goals))
        assert len(solutions) == 2
        assert solutions[0]["X"] == Int(2)
        assert solutions[1]["X"] == Int(3)

    def test_canonical_forms_still_work_for_comma_and_eq(self):
        """Canonical ',' and '=' continue to work alongside operators."""
        clauses = parser.parse_program(
            """
            test1 :- ','(true, true).
            test2(X) :- '='(X, 5).
        """
        )
        engine = DevEngine(Program(tuple(clauses)))

        goals1 = parser.parse_query("?- test1.")
        assert len(list(engine.run(goals1))) == 1

        goals2 = parser.parse_query("?- test2(X).")
        solutions2 = list(engine.run(goals2))
        assert len(solutions2) == 1
        assert solutions2[0]["X"] == Int(5)

    @pytest.mark.skip(reason="Canonical ';' form not in Stage 1.5 engine scope")
    def test_canonical_semicolon_form_later(self):
        """Canonical ';' form will be supported in later stages."""
        clauses = parser.parse_program("t(X,Y) :- ';'('='(X,a), '='(Y,b)).")
        engine = DevEngine(Program(tuple(clauses)))
        goals = parser.parse_query("?- t(X,Y).")
        # Just check it's a list - actual execution would need ';' support
        assert isinstance(list(engine.run(goals)), list)


class TestUnsupportedOperatorRuntime:
    """Test runtime behavior of unsupported operators in dev mode."""

    def test_integer_division_works(self):
        """// operator is implemented and works correctly."""
        clauses = parser.parse_program(
            """
            test(X) :- X is 7 // 2.
        """
        )
        engine = DevEngine(Program(tuple(clauses)))
        goals = parser.parse_query("?- test(X).")

        # Integer division should work
        solutions = list(engine.run(goals))
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(3)  # 7 // 2 = 3

    def test_mod_operator_works(self):
        """mod operator is implemented and works correctly."""
        clauses = parser.parse_program(
            """
            test(X) :- X is 7 mod 3.
        """
        )
        engine = DevEngine(Program(tuple(clauses)))
        goals = parser.parse_query("?- test(X).")

        # Modulo should work
        solutions = list(engine.run(goals))
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(1)  # 7 mod 3 = 1

    def test_power_operator_runtime(self):
        """** operator behavior is now implemented."""
        clauses = parser.parse_program(
            """
            test(X) :- X is 2 ** 3.
        """
        )
        engine = DevEngine(Program(tuple(clauses)))
        goals = parser.parse_query("?- test(X).")

        # Power operator is now implemented
        solutions = list(engine.run(goals))
        assert len(solutions) == 1
        assert solutions[0]["X"] == Int(8)  # 2 ** 3 = 8


class TestFileLoadingWithOperators:
    """Test that files with operators can be loaded correctly."""

    def test_parse_file_with_operators(self, tmp_path):
        """Files containing operators parse correctly."""
        # Create a test file with operators
        test_file = tmp_path / "test_ops.pl"
        test_file.write_text(
            """
            % Test file with various operators
            factorial(0, 1).
            factorial(N, F) :-
                N > 0,
                N1 is N - 1,
                factorial(N1, F1),
                F is N * F1.
            
            max(X, Y, X) :- X >= Y.
            max(X, Y, Y) :- Y > X.
            
            classify(X, positive) :- X > 0.
            classify(0, zero).
            classify(X, negative) :- X < 0.
            
            % Test tight tokenization
            tight_order(X, Y) :- X @=< Y.
        """
        )

        # Parse the file
        with open(test_file) as f:
            content = f.read()
        clauses = parser.parse_program(content)

        # Should parse successfully
        assert len(clauses) > 0

        # Create engine and test
        engine = DevEngine(Program(tuple(clauses)))

        # Test factorial
        goals1 = parser.parse_query("?- factorial(3, F).")
        solutions1 = list(engine.run(goals1))
        assert len(solutions1) == 1
        assert solutions1[0]["F"] == Int(6)

        # Test max
        goals2 = parser.parse_query("?- max(5, 3, M).")
        solutions2 = list(engine.run(goals2))
        assert len(solutions2) == 1
        assert solutions2[0]["M"] == Int(5)

        # Test classify
        goals3 = parser.parse_query("?- classify(-2, C).")
        solutions3 = list(engine.run(goals3))
        assert len(solutions3) == 1
        assert solutions3[0]["C"] == Atom("negative")

        # Test tight tokenization
        goals4 = parser.parse_query("?- tight_order(1, 2).")
        solutions4 = list(engine.run(goals4))
        assert len(solutions4) == 1  # Should succeed
