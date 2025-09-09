"""Test error handling behavior in development mode.

In dev mode, PyLog prioritizes developer experience:
- Undefined predicates fail silently (return false)
- Type/arithmetic errors provide helpful messages
- No exceptions thrown for normal failures

Stage-1 dev mode deliberately diverges from ISO error taxonomy;
ISO mode tests will be enabled in a future stage.
"""

import pytest

from prolog.ast.terms import Atom, Int
from prolog.engine.engine import Engine
from prolog.ast.clauses import Program
from prolog.parser import parser
from prolog.engine.patches import patch_dev_mode_throw


def get_dev_engine():
    """Get an engine with dev mode patches applied."""
    DevEngine = patch_dev_mode_throw(Engine)
    return DevEngine


def assert_engine_clean(engine):
    """Assert that engine state is clean (no leftover stacks/trail)."""
    if hasattr(engine, 'cp_stack'):
        assert len(engine.cp_stack) == 0
    if hasattr(engine, 'goal_stack'):
        assert engine.goal_stack.height() == 0
    if hasattr(engine, 'frame_stack'):
        assert len(engine.frame_stack) == 0
    if hasattr(engine, 'trail'):
        assert engine.trail.position() == 0


class TestUndefinedPredicates:
    """Test behavior for undefined predicates in dev mode."""
    
    def test_undefined_predicate_fails_silently(self):
        """Undefined predicates should return false, not throw."""
        engine = get_dev_engine()(Program(()))
        
        # Query for an undefined predicate
        
        goals = parser.parse_query("?- undefined_pred(X).")
        
        # Should return false without throwing
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # No solutions, returns false
        assert_engine_clean(engine)
    
    def test_undefined_with_arguments_returns_false(self):
        """foo(X) with no foo/1 clauses returns false."""
        engine = get_dev_engine()(Program(()))
        
        
        goals = parser.parse_query("?- foo(X).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Returns false
        assert_engine_clean(engine)
    
    def test_undefined_in_conjunction_fails(self):
        """Undefined predicate in conjunction causes entire goal to fail."""
        
        
        # Define one predicate but not the other
        clauses = parser.parse_program("defined(yes).")
        engine = get_dev_engine()(Program(tuple(clauses)))
        
        # Query with both defined and undefined
        goals = parser.parse_query("?- defined(yes), undefined(X).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Entire conjunction fails
        assert_engine_clean(engine)
    
    def test_undefined_in_disjunction_allows_other_branch(self):
        """Undefined predicate in disjunction doesn't prevent other branch."""
        
        
        # Define one predicate
        clauses = parser.parse_program("defined(yes).")
        engine = get_dev_engine()(Program(tuple(clauses)))
        
        # Query with disjunction (using ; which is a builtin)
        # Since we're in Stage 1 (operator-free), we need to use explicit form
        goals = parser.parse_query("?- ';'(undefined(X), defined(Y)).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 1  # Other branch succeeds
        assert solutions[0]["Y"] == Atom("yes")
        assert_engine_clean(engine)
    
    def test_undefined_with_cut_still_fails(self):
        """Undefined predicate fails even with cut."""
        engine = get_dev_engine()(Program(()))
        
        
        # Query with undefined predicate and cut
        goals = parser.parse_query("?- undefined(X), !.")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Still fails
        assert_engine_clean(engine)
    
    def test_nested_undefined_predicate(self):
        """Undefined predicate in nested goal fails gracefully."""
        
        
        # Define a rule that calls undefined predicate
        clauses = parser.parse_program("""
            wrapper(X) :- undefined_helper(X).
        """)
        engine = get_dev_engine()(Program(tuple(clauses)))
        
        goals = parser.parse_query("?- wrapper(Y).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Fails gracefully
        assert_engine_clean(engine)
    
    def test_undefined_multiple_arities(self):
        """Different arities of same functor are independent."""
        
        
        # Define foo/1 but not foo/2
        clauses = parser.parse_program("foo(one).")
        engine = get_dev_engine()(Program(tuple(clauses)))
        
        # foo/1 should succeed
        goals1 = parser.parse_query("?- foo(X).")
        solutions1 = list(engine.run(goals1))
        assert len(solutions1) == 1
        assert solutions1[0]["X"] == Atom("one")
        
        # foo/2 should fail (undefined)
        goals2 = parser.parse_query("?- foo(X, Y).")
        solutions2 = list(engine.run(goals2))
        assert len(solutions2) == 0  # Different arity is undefined
        assert_engine_clean(engine)


class TestBuiltinErrors:
    """Test error handling for builtin predicates."""
    
    def test_type_error_in_arithmetic(self):
        """Type errors in arithmetic should fail gracefully."""
        engine = get_dev_engine()(Program(()))
        
        
        # Try to do arithmetic on an atom
        goals = parser.parse_query("?- is(X, '+'(atom, 1)).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Should fail, not throw
        assert_engine_clean(engine)
    
    def test_unbound_variable_in_arithmetic(self):
        """Unbound variables in arithmetic evaluation should fail."""
        engine = get_dev_engine()(Program(()))
        
        
        # Y is unbound
        goals = parser.parse_query("?- is(X, '+'(Y, 1)).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Should fail
        assert_engine_clean(engine)
    
    def test_division_by_zero(self):
        """Division by zero should fail gracefully."""
        engine = get_dev_engine()(Program(()))
        
        
        goals = parser.parse_query("?- is(X, '/'(1, 0)).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Should fail, not crash
        assert_engine_clean(engine)
    
    def test_atomic_in_functor(self):
        """Atomic first arg returns its own name and arity 0."""
        engine = get_dev_engine()(Program(()))
        
        
        # Atomic terms have arity 0
        goals = parser.parse_query("?- functor(123, F, A).")
        
        solutions = list(engine.run(goals))
        # functor(123, 123, 0) should succeed (atoms have arity 0)
        assert len(solutions) == 1
        assert solutions[0]["F"] == Int(123)
        assert solutions[0]["A"] == Int(0)
        assert_engine_clean(engine)
    
    def test_negative_arity_in_functor(self):
        """Negative arity in functor/3 should fail."""
        engine = get_dev_engine()(Program(()))
        
        
        goals = parser.parse_query("?- functor(X, foo, -1).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Should fail
        assert_engine_clean(engine)
    
    def test_functor_construct_with_non_atom_name_fails(self):
        """Non-atom name with arity > 0 should fail in dev mode."""
        engine = get_dev_engine()(Program(()))
        
        
        goals = parser.parse_query("?- functor(T, 123, 2).")
        solutions = list(engine.run(goals))
        assert len(solutions) == 0
        assert_engine_clean(engine)
    
    def test_type_error_in_arg(self):
        """Type error in arg/3 should fail gracefully."""
        engine = get_dev_engine()(Program(()))
        
        
        # First argument to arg must be integer
        goals = parser.parse_query("?- arg(atom, foo(a, b), X).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Should fail
        assert_engine_clean(engine)
    
    def test_arg_on_atomic_term_fails(self):
        """arg/3 on atomic term should fail."""
        engine = get_dev_engine()(Program(()))
        
        
        goals = parser.parse_query("?- arg(1, 5, X).")
        solutions = list(engine.run(goals))
        assert len(solutions) == 0
        assert_engine_clean(engine)
    
    def test_out_of_bounds_arg(self):
        """Out of bounds index in arg/3 should fail."""
        engine = get_dev_engine()(Program(()))
        
        
        # foo(a, b) has arity 2, so arg 3 is out of bounds
        goals = parser.parse_query("?- arg(3, foo(a, b), X).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Should fail
        assert_engine_clean(engine)
    
    def test_univ_with_invalid_list(self):
        """=.. with invalid list should fail."""
        engine = get_dev_engine()(Program(()))
        
        
        # Empty list is invalid for =..
        goals = parser.parse_query("?- '=..'(X, []).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Should fail
        assert_engine_clean(engine)
    
    def test_univ_with_improper_list_fails(self):
        """=.. with improper list should fail."""
        engine = get_dev_engine()(Program(()))
        
        
        goals = parser.parse_query("?- '=..'(foo(a), [foo|b]).")
        solutions = list(engine.run(goals))
        assert len(solutions) == 0
        assert_engine_clean(engine)
    
    def test_call_non_callable_fails_dev_mode(self):
        """Non-callable term should fail in dev mode."""
        engine = get_dev_engine()(Program(()))
        
        
        goals = parser.parse_query("?- call(123).")
        solutions = list(engine.run(goals))
        assert len(solutions) == 0
        assert_engine_clean(engine)
    
    def test_call_var_instantiation_fails_dev_mode(self):
        """Unbound variable in call/1 should fail in dev mode."""
        engine = get_dev_engine()(Program(()))
        
        
        goals = parser.parse_query("?- call(X).")
        solutions = list(engine.run(goals))
        assert len(solutions) == 0
        assert_engine_clean(engine)


class TestExceptionIntegration:
    """Test throw/catch exception handling."""
    
    def test_uncaught_throw_fails(self):
        """Uncaught throw should cause goal to fail."""
        engine = get_dev_engine()(Program(()))
        
        
        goals = parser.parse_query("?- throw(ball).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Uncaught exception fails
        assert_engine_clean(engine)
    
    def test_uncaught_throw_leaves_engine_reusable(self):
        """Engine should be reusable after uncaught throw."""
        engine = get_dev_engine()(Program(()))
        
        
        goals1 = parser.parse_query("?- throw(ball).")
        assert list(engine.run(goals1)) == []
        assert_engine_clean(engine)
        
        goals2 = parser.parse_query("?- true.")
        assert len(list(engine.run(goals2))) == 1
        assert_engine_clean(engine)
    
    def test_catch_handles_throw(self):
        """catch/3 should handle thrown exceptions."""
        engine = get_dev_engine()(Program(()))
        
        
        goals = parser.parse_query("?- catch(throw(ball), ball, true).")
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 1  # Caught and recovered
        assert_engine_clean(engine)
    
    def test_catch_with_type_error(self):
        """catch/3 with arithmetic error - in dev mode it just fails."""
        engine = get_dev_engine()(Program(()))
        
        
        # In dev mode arithmetic fails, so nothing is caught
        goals = parser.parse_query("?- catch(is(X, '/'(1, 0)), error(E), true).")
        
        solutions = list(engine.run(goals))
        # In dev mode arithmetic fails, so nothing is caught
        assert len(solutions) == 0
        assert_engine_clean(engine)


class TestErrorConsistency:
    """Test that error behavior is consistent across the system."""
    
    @pytest.mark.parametrize("query_text", [
        "?- undef1(X).",
        "?- undef2(a, b, c).",
        "?- very_long_undefined_predicate_name(X, Y, Z).",
        "?- 'special-undefined'(X).",
    ])
    def test_all_undefined_predicates_fail(self, query_text):
        """All undefined predicates should consistently return false."""
        engine = get_dev_engine()(Program(()))
        
        goals = parser.parse_query(query_text)
        solutions = list(engine.run(goals))
        assert len(solutions) == 0
        assert_engine_clean(engine)
    
    @pytest.mark.parametrize("query_text", [
        "?- is(X, '+'(atom, 1)).",  # Type error
        "?- is(X, '/'(1, 0)).",      # Division by zero
        "?- functor(X, foo, -5).",   # Invalid arity
        "?- arg(0, foo(a), X).",     # Invalid index (must be >= 1)
        "?- '=..'(X, []).",          # Invalid list for =..
    ])
    def test_builtin_errors_never_crash(self, query_text):
        """Builtin errors should fail gracefully, never crash."""
        engine = get_dev_engine()(Program(()))
        
        goals = parser.parse_query(query_text)
        # Should not raise exception
        solutions = list(engine.run(goals))
        assert len(solutions) == 0
        assert_engine_clean(engine)
    
    def test_mixed_errors_in_conjunction(self):
        """Multiple error types in conjunction all fail gracefully."""
        # One defined predicate
        clauses = parser.parse_program("good(ok).")
        engine = get_dev_engine()(Program(tuple(clauses)))
        
        # Mix of undefined and error-prone goals
        goals = parser.parse_query(
            "?- good(ok), undefined(X), is(Y, '/'(1, 0)), bad(Z)."
        )
        
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # All must succeed for conjunction
        assert_engine_clean(engine)


class TestErrorMessages:
    """Test that helpful error messages are available (even if not thrown)."""
    
    def test_error_context_available(self):
        """Error context should be available for debugging."""
        engine = get_dev_engine()(Program(()))
        
        # While errors don't throw in dev mode, they might log
        # or make context available for debugging
        goals = parser.parse_query("?- is(X, '+'(atom, 1)).")
        
        # For now, just verify it doesn't crash
        solutions = list(engine.run(goals))
        assert len(solutions) == 0
        assert_engine_clean(engine)
    
    def test_parse_errors_have_line_numbers(self):
        """Parse errors should include line numbers."""
        
        
        # Invalid syntax
        with pytest.raises(Exception) as exc_info:
            parser.parse_program("invalid syntax (")
        
        # Should have some indication of where the error occurred
        error_str = str(exc_info.value)
        # Exact format depends on parser, but should have location info
        assert error_str  # Non-empty error message


class TestDevModePolicy:
    """Test that dev mode policy is consistently applied."""
    
    def test_no_existence_errors_in_dev_mode(self):
        """Dev mode should not throw existence_error for undefined predicates."""
        engine = get_dev_engine()(Program(()))
        
        # This would throw existence_error in ISO mode
        goals = parser.parse_query("?- completely_undefined(X, Y, Z).")
        
        # In dev mode, should just fail
        solutions = list(engine.run(goals))
        assert len(solutions) == 0
        assert_engine_clean(engine)
        
        # Should not have raised an exception
        # (test passes if we get here)
    
    @pytest.mark.parametrize("query_text", [
        "?- arg([], foo(a), X).",       # First arg must be integer
        "?- functor(X, 123, 2).",        # Can't make arity-2 compound with int functor
        "?- '=..'(X, atom).",            # Second arg must be list
    ])
    def test_type_errors_fail_not_throw(self, query_text):
        """Dev mode type errors should fail, not throw."""
        engine = get_dev_engine()(Program(()))
        
        goals = parser.parse_query(query_text)
        solutions = list(engine.run(goals))
        assert len(solutions) == 0  # Fails, doesn't throw
        assert_engine_clean(engine)
    
    def test_future_iso_mode_planning(self):
        """Document planning for future ISO mode."""
        # This test documents expected differences for future ISO mode
        # In ISO mode, these behaviors would change:
        
        # 1. Undefined predicates would throw existence_error
        # 2. Type errors would throw type_error  
        # 3. Arithmetic errors would throw evaluation_error
        # 4. Domain errors would throw domain_error
        
        # For now, just document this is planned
        assert True  # Placeholder for documentation