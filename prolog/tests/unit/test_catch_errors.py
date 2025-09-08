"""Tests for catch/3 with builtin error integration.

Tests that builtin predicates throw catchable exceptions for Stage 1.

In dev-mode (Stage 1), we have a simplified error policy:
- Arithmetic errors throw evaluation_error
- Type errors throw type_error  
- Domain errors throw domain_error
- These can be caught with catch/3

Full ISO error taxonomy will be implemented in later stages.
"""

import pytest
from prolog.ast.terms import Atom, Var, Struct, Int
from prolog.engine.engine import Engine
from prolog.engine.errors import PrologThrow
from prolog.tests.helpers import program, mk_fact, mk_rule


class TestArithmeticErrorsCatchable:
    """Test arithmetic errors can be caught."""
    
    def test_catch_division_by_zero(self):
        """Test division by zero throws evaluation_error."""
        engine = Engine(program())
        
        # Query: catch(X is 1/0, evaluation_error(zero_divisor), X=caught)
        # Division by zero should throw evaluation_error(zero_divisor)
        results = engine.run([
            Struct("catch", (
                Struct("is", (
                    Var(0, "X"),
                    Struct("/", (Int(1), Int(0)))
                )),
                Struct("evaluation_error", (Atom("zero_divisor"),)),
                Struct("=", (Var(0, "X"), Atom("caught")))
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("caught")
    
    def test_catch_undefined_arithmetic_operation(self):
        """Test undefined arithmetic operation throws evaluation_error."""
        engine = Engine(program())
        
        # Query: catch(X is foo(1), evaluation_error(_), X=caught)
        # Unknown arithmetic function should throw
        results = engine.run([
            Struct("catch", (
                Struct("is", (
                    Var(0, "X"),
                    Struct("foo", (Int(1),))
                )),
                Struct("evaluation_error", (Var(1, "_"),)),
                Struct("=", (Var(0, "X"), Atom("caught")))
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("caught")
    
    def test_catch_integer_overflow(self):
        """Test integer overflow throws evaluation_error."""
        pytest.skip("Integer overflow handling depends on implementation details")
        # This would test overflow if the implementation has bounded integers
        # Most Python implementations have arbitrary precision, so skip
    
    def test_uncaught_arithmetic_error_propagates(self):
        """Test uncaught arithmetic error propagates."""
        engine = Engine(program())
        
        # Query: catch(X is 1/0, type_error(_), fail)
        # Catching wrong error type, should propagate
        with pytest.raises(PrologThrow) as exc:
            engine.run([
                Struct("catch", (
                    Struct("is", (
                        Var(0, "X"),
                        Struct("/", (Int(1), Int(0)))
                    )),
                    Struct("type_error", (Var(1, "_"),)),
                    Atom("fail")
                ))
            ])
        # Check it's an evaluation_error
        assert isinstance(exc.value.ball, Struct)
        assert exc.value.ball.functor == "evaluation_error"


class TestTypeErrorsCatchable:
    """Test type errors can be caught."""
    
    def test_catch_non_numeric_arithmetic(self):
        """Test arithmetic with non-numeric argument throws type_error."""
        engine = Engine(program())
        
        # Query: catch(X is abc + 1, type_error(number, abc), X=caught)
        # Non-numeric in arithmetic should throw type_error
        results = engine.run([
            Struct("catch", (
                Struct("is", (
                    Var(0, "X"),
                    Struct("+", (Atom("abc"), Int(1)))
                )),
                Struct("type_error", (Atom("number"), Atom("abc"))),
                Struct("=", (Var(0, "X"), Atom("caught")))
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("caught")
    
    def test_catch_callable_type_error(self):
        """Test calling non-callable throws type_error."""
        engine = Engine(program())
        
        # Query: catch(call(123), type_error(callable, 123), true)
        # Calling an integer should throw type_error
        results = engine.run([
            Struct("catch", (
                Struct("call", (Int(123),)),
                Struct("type_error", (Atom("callable"), Int(123))),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1
    
    def test_catch_arg_type_error(self):
        """Test arg/3 with non-compound throws type_error."""
        engine = Engine(program())
        
        # Query: catch(arg(1, atom, X), type_error(compound, atom), X=caught)
        # arg/3 on non-compound should throw
        results = engine.run([
            Struct("catch", (
                Struct("arg", (Int(1), Atom("atom"), Var(0, "X"))),
                Struct("type_error", (Atom("compound"), Atom("atom"))),
                Struct("=", (Var(0, "X"), Atom("caught")))
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("caught")
    
    def test_catch_functor_type_error(self):
        """Test functor/3 with wrong types throws type_error."""
        engine = Engine(program())
        
        # Query: catch(functor(_, 123, _), type_error(atom, 123), true)
        # functor/3 with non-atom functor should throw
        results = engine.run([
            Struct("catch", (
                Struct("functor", (Var(0, "_"), Int(123), Var(1, "_"))),
                Struct("type_error", (Atom("atom"), Int(123))),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1


class TestDomainErrorsCatchable:
    """Test domain errors can be caught."""
    
    def test_catch_negative_arity_domain_error(self):
        """Test functor/3 with negative arity throws domain_error."""
        engine = Engine(program())
        
        # Query: catch(functor(X, foo, -1), domain_error(not_less_than_zero, -1), X=caught)
        # Negative arity should throw domain_error
        results = engine.run([
            Struct("catch", (
                Struct("functor", (Var(0, "X"), Atom("foo"), Int(-1))),
                Struct("domain_error", (Atom("not_less_than_zero"), Int(-1))),
                Struct("=", (Var(0, "X"), Atom("caught")))
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("caught")
    
    def test_catch_arg_out_of_range(self):
        """Test arg/3 with out-of-range index throws domain_error."""
        engine = Engine(program())
        
        # Query: catch(arg(5, foo(a,b), X), domain_error(argument_index, 5), X=caught)
        # Index out of range should throw
        results = engine.run([
            Struct("catch", (
                Struct("arg", (
                    Int(5),
                    Struct("foo", (Atom("a"), Atom("b"))),
                    Var(0, "X")
                )),
                Struct("domain_error", (Atom("argument_index"), Int(5))),
                Struct("=", (Var(0, "X"), Atom("caught")))
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("caught")
    
    def test_catch_zero_arg_index(self):
        """Test arg/3 with index 0 throws domain_error."""
        engine = Engine(program())
        
        # Query: catch(arg(0, foo(a), X), domain_error(argument_index, 0), X=caught)
        # Index 0 is invalid (1-based indexing)
        results = engine.run([
            Struct("catch", (
                Struct("arg", (
                    Int(0),
                    Struct("foo", (Atom("a"),)),
                    Var(0, "X")
                )),
                Struct("domain_error", (Atom("argument_index"), Int(0))),
                Struct("=", (Var(0, "X"), Atom("caught")))
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["X"] == Atom("caught")


class TestCatchWithVariableErrors:
    """Test catching errors with variable patterns."""
    
    def test_catch_any_error_with_variable(self):
        """Test catch(Goal, Error, ...) with variable catches any error."""
        engine = Engine(program())
        
        # Query: catch(X is 1/0, Error, true)
        # Variable pattern catches anything
        results = engine.run([
            Struct("catch", (
                Struct("is", (
                    Var(0, "X"),
                    Struct("/", (Int(1), Int(0)))
                )),
                Var(1, "Error"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["Error"] == Struct("evaluation_error", (Atom("zero_divisor"),))
    
    def test_catch_error_pattern_with_variables(self):
        """Test catch with partially specified error pattern."""
        engine = Engine(program())
        
        # Query: catch(X is abc + 1, type_error(Type, Culprit), true)
        # Pattern with variables for error details
        results = engine.run([
            Struct("catch", (
                Struct("is", (
                    Var(0, "X"),
                    Struct("+", (Atom("abc"), Int(1)))
                )),
                Struct("type_error", (Var(1, "Type"), Var(2, "Culprit"))),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1
        assert results[0]["Type"] == Atom("number")
        assert results[0]["Culprit"] == Atom("abc")
    
    def test_multiple_error_types_sequential_catches(self):
        """Test catching different error types with nested catches."""
        p = program(
            mk_rule("may_fail", (Var(0, "How"),),
                Struct(";", (
                    Struct(",", (
                        Struct("=", (Var(0, "How"), Atom("type"))),
                        Struct("call", (Int(123),))  # type_error
                    )),
                    Struct(",", (
                        Struct("=", (Var(0, "How"), Atom("eval"))),
                        Struct("is", (Var(1, "_"), Struct("/", (Int(1), Int(0)))))  # evaluation_error
                    ))
                ))
            )
        )
        engine = Engine(p)
        
        # Query: catch(catch(may_fail(X), type_error(_,_), X=caught_type), 
        #              evaluation_error(_), X=caught_eval)
        results = engine.run([
            Struct("catch", (
                Struct("catch", (
                    Struct("may_fail", (Var(0, "X"),)),
                    Struct("type_error", (Var(1, "_"), Var(2, "_"))),
                    Struct("=", (Var(0, "X"), Atom("caught_type")))
                )),
                Struct("evaluation_error", (Var(3, "_"),)),
                Struct("=", (Var(0, "X"), Atom("caught_eval")))
            ))
        ])
        
        # Should catch both error types appropriately
        assert len(results) == 2
        assert results[0]["X"] == Atom("caught_type")
        assert results[1]["X"] == Atom("caught_eval")


class TestErrorPolicy:
    """Test Stage-1 dev-mode error policy."""
    
    def test_ill_typed_builtin_fails_not_throws(self):
        """Test ill-typed builtin calls fail rather than throw in dev-mode."""
        engine = Engine(program())
        
        # Query: catch(atom_codes(123, X), _, X=caught)
        # In dev-mode, ill-typed call should fail, not throw
        # This means catch won't trigger
        results = engine.run([
            Struct("catch", (
                Struct("atom_codes", (Int(123), Var(0, "X"))),
                Var(1, "_"),
                Struct("=", (Var(0, "X"), Atom("caught")))
            ))
        ])
        
        # In dev-mode, should fail rather than throw
        assert len(results) == 0
    
    def test_undefined_predicate_fails_not_throws(self):
        """Test undefined predicates fail rather than throw in dev-mode."""
        engine = Engine(program())
        
        # Query: catch(undefined_pred(X), _, X=caught)
        # Undefined predicates fail in dev-mode
        results = engine.run([
            Struct("catch", (
                Struct("undefined_pred", (Var(0, "X"),)),
                Var(1, "_"),
                Struct("=", (Var(0, "X"), Atom("caught")))
            ))
        ])
        
        assert len(results) == 0
    
    def test_explicit_throw_always_catchable(self):
        """Test explicit throw/1 is always catchable regardless of mode."""
        engine = Engine(program())
        
        # Query: catch(throw(custom_error), custom_error, true)
        # Explicit throws are always catchable
        results = engine.run([
            Struct("catch", (
                Struct("throw", (Atom("custom_error"),)),
                Atom("custom_error"),
                Atom("true")
            ))
        ])
        
        assert len(results) == 1