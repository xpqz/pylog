"""Tests for ISO Prolog type testing predicates.

These tests verify the implementation of type predicates required by ISO Prolog:
- integer/1, float/1, number/1
- atomic/1, compound/1, callable/1, ground/1
"""

from prolog.ast.terms import Atom, Int, Float, Var, Struct, List as PrologList
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine
from prolog.engine.utils.arithmetic import eval_arithmetic
from prolog.unify.unify import bind


class TestISOTypePredicates:
    """Test ISO-compliant type predicates."""

    def test_integer_predicate(self):
        """Test integer/1 predicate."""
        program = Program([])
        engine = Engine(program)

        # Test with integer
        result = engine._builtin_integer((Int(42),))
        assert result is True

        # Test with float
        result = engine._builtin_integer((Float(3.14),))
        assert result is False

        # Test with atom
        result = engine._builtin_integer((Atom("hello"),))
        assert result is False

        # Test with variable
        var_id = engine.store.new_var("X")
        var = Var(var_id, "X")
        result = engine._builtin_integer((var,))
        assert result is False

        # Test with compound term
        result = engine._builtin_integer((Struct("foo", (Int(1),)),))
        assert result is False

        # Test with bound variable
        var_id2 = engine.store.new_var("Y")
        var2 = Var(var_id2, "Y")
        bind(engine.store, var_id2, Int(123), [])
        result = engine._builtin_integer((var2,))
        assert result is True

    def test_float_predicate(self):
        """Test float/1 predicate."""
        program = Program([])
        engine = Engine(program)

        # Test with float
        result = engine._builtin_float((Float(3.14),))
        assert result is True

        # Test with integer
        result = engine._builtin_float((Int(42),))
        assert result is False

        # Test with atom
        result = engine._builtin_float((Atom("hello"),))
        assert result is False

        # Test with variable
        var_id = engine.store.new_var("X")
        var = Var(var_id, "X")
        result = engine._builtin_float((var,))
        assert result is False

        # Test with bound variable
        var_id2 = engine.store.new_var("Y")
        var2 = Var(var_id2, "Y")
        bind(engine.store, var_id2, Float(2.71), [])
        result = engine._builtin_float((var2,))
        assert result is True

    def test_number_predicate(self):
        """Test number/1 predicate."""
        program = Program([])
        engine = Engine(program)

        # Test with integer
        result = engine._builtin_number((Int(42),))
        assert result is True

        # Test with float
        result = engine._builtin_number((Float(3.14),))
        assert result is True

        # Test with atom
        result = engine._builtin_number((Atom("hello"),))
        assert result is False

        # Test with variable
        var_id = engine.store.new_var("X")
        var = Var(var_id, "X")
        result = engine._builtin_number((var,))
        assert result is False

        # Test with compound term
        result = engine._builtin_number((Struct("foo", (Int(1),)),))
        assert result is False

    def test_atomic_predicate(self):
        """Test atomic/1 predicate."""
        program = Program([])
        engine = Engine(program)

        # Test with atom
        result = engine._builtin_atomic((Atom("hello"),))
        assert result is True

        # Test with integer
        result = engine._builtin_atomic((Int(42),))
        assert result is True

        # Test with float
        result = engine._builtin_atomic((Float(3.14),))
        assert result is True

        # Test with variable
        var_id = engine.store.new_var("X")
        var = Var(var_id, "X")
        result = engine._builtin_atomic((var,))
        assert result is False

        # Test with compound term
        result = engine._builtin_atomic((Struct("foo", (Int(1),)),))
        assert result is False

        # Test with list
        result = engine._builtin_atomic((PrologList((Int(1), Int(2))),))
        assert result is False

    def test_compound_predicate(self):
        """Test compound/1 predicate."""
        program = Program([])
        engine = Engine(program)

        # Test with compound term
        result = engine._builtin_compound((Struct("foo", (Int(1), Atom("bar"))),))
        assert result is True

        # Test with list
        result = engine._builtin_compound((PrologList((Int(1), Int(2))),))
        assert result is True

        # Test with atom
        result = engine._builtin_compound((Atom("hello"),))
        assert result is False

        # Test with integer
        result = engine._builtin_compound((Int(42),))
        assert result is False

        # Test with variable
        var_id = engine.store.new_var("X")
        var = Var(var_id, "X")
        result = engine._builtin_compound((var,))
        assert result is False

    def test_callable_predicate(self):
        """Test callable/1 predicate."""
        program = Program([])
        engine = Engine(program)

        # Test with atom (0-arity predicate)
        result = engine._builtin_callable((Atom("foo"),))
        assert result is True

        # Test with compound term
        result = engine._builtin_callable((Struct("foo", (Int(1), Atom("bar"))),))
        assert result is True

        # Test with list (callable as list/0, ./2, etc.)
        result = engine._builtin_callable((PrologList((Int(1), Int(2))),))
        assert result is True

        # Test with integer (not callable)
        result = engine._builtin_callable((Int(42),))
        assert result is False

        # Test with float (not callable)
        result = engine._builtin_callable((Float(3.14),))
        assert result is False

        # Test with variable
        var_id = engine.store.new_var("X")
        var = Var(var_id, "X")
        result = engine._builtin_callable((var,))
        assert result is False

    def test_ground_predicate(self):
        """Test ground/1 predicate."""
        program = Program([])
        engine = Engine(program)

        # Test with atom (ground)
        result = engine._builtin_ground((Atom("hello"),))
        assert result is True

        # Test with integer (ground)
        result = engine._builtin_ground((Int(42),))
        assert result is True

        # Test with float (ground)
        result = engine._builtin_ground((Float(3.14),))
        assert result is True

        # Test with unbound variable (not ground)
        var_id = engine.store.new_var("X")
        var = Var(var_id, "X")
        result = engine._builtin_ground((var,))
        assert result is False

        # Test with bound variable (ground if binding is ground)
        var_id2 = engine.store.new_var("Y")
        var2 = Var(var_id2, "Y")
        bind(engine.store, var_id2, Int(123), [])
        result = engine._builtin_ground((var2,))
        assert result is True

        # Test with compound term containing variables (not ground)
        var_id3 = engine.store.new_var("Z")
        var3 = Var(var_id3, "Z")
        compound_with_var = Struct("foo", (Int(1), var3))
        result = engine._builtin_ground((compound_with_var,))
        assert result is False

        # Test with compound term without variables (ground)
        compound_ground = Struct("foo", (Int(1), Atom("bar")))
        result = engine._builtin_ground((compound_ground,))
        assert result is True

        # Test with list containing variables (not ground)
        list_with_var = PrologList((Int(1), var3))
        result = engine._builtin_ground((list_with_var,))
        assert result is False

        # Test with ground list
        ground_list = PrologList((Int(1), Int(2), Atom("three")))
        result = engine._builtin_ground((ground_list,))
        assert result is True

    def test_ground_complex_structures(self):
        """Test ground/1 with complex nested structures."""
        program = Program([])
        engine = Engine(program)

        # Complex ground structure
        ground_struct = Struct(
            "complex",
            (
                Int(1),
                Struct("nested", (Atom("atom"), Float(2.5))),
                PrologList((Int(3), Int(4))),
            ),
        )
        result = engine._builtin_ground((ground_struct,))
        assert result is True

        # Complex structure with buried variable
        var_id = engine.store.new_var("X")
        var = Var(var_id, "X")
        non_ground_struct = Struct(
            "complex",
            (
                Int(1),
                Struct("nested", (Atom("atom"), var)),  # Variable buried deep
                PrologList((Int(3), Int(4))),
            ),
        )
        result = engine._builtin_ground((non_ground_struct,))
        assert result is False

        # List with variable tail
        var_tail_id = engine.store.new_var("Tail")
        var_tail = Var(var_tail_id, "Tail")
        list_with_var_tail = PrologList((Int(1), Int(2)), tail=var_tail)
        result = engine._builtin_ground((list_with_var_tail,))
        assert result is False

    def test_integration_with_solve(self):
        """Test type predicates working in actual queries."""
        program = Program([])
        engine = Engine(program)

        # Test integer/1 in a query
        query_int = Struct("integer", (Int(42),))
        solutions = list(engine.solve(query_int))
        assert len(solutions) == 1

        # Test float/1 in a query
        query_float = Struct("float", (Float(3.14),))
        solutions = list(engine.solve(query_float))
        assert len(solutions) == 1

        # Test number/1 with integer
        query_num_int = Struct("number", (Int(42),))
        solutions = list(engine.solve(query_num_int))
        assert len(solutions) == 1

        # Test number/1 with float
        query_num_float = Struct("number", (Float(3.14),))
        solutions = list(engine.solve(query_num_float))
        assert len(solutions) == 1

        # Test atomic/1 with atom
        query_atomic = Struct("atomic", (Atom("hello"),))
        solutions = list(engine.solve(query_atomic))
        assert len(solutions) == 1

        # Test callable/1 with atom
        query_callable = Struct("callable", (Atom("predicate"),))
        solutions = list(engine.solve(query_callable))
        assert len(solutions) == 1

        # Test ground/1 with ground term
        query_ground = Struct("ground", (Struct("foo", (Int(1), Atom("bar"))),))
        solutions = list(engine.solve(query_ground))
        assert len(solutions) == 1

    def test_arithmetic_with_floats(self):
        """Test that arithmetic now works with floats."""
        program = Program([])
        engine = Engine(program)

        # Test float arithmetic evaluation
        result = eval_arithmetic(engine.store, Float(3.14))
        assert result == 3.14
        assert isinstance(result, float)

        # Test mixed integer/float arithmetic
        expr = Struct("+", (Int(1), Float(2.5)))
        result = eval_arithmetic(engine.store, expr)
        assert result == 3.5
        assert isinstance(result, float)

        # Test float division
        expr = Struct("/", (Int(7), Int(2)))
        result = eval_arithmetic(engine.store, expr)
        assert result == 3.5
        assert isinstance(result, float)

        # Test integer division still works
        expr = Struct("//", (Int(7), Int(2)))
        result = eval_arithmetic(engine.store, expr)
        assert result == 3
        assert isinstance(result, int)
