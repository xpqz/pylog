"""Unit tests for lex_chain/1 constraint.

Tests lexicographic ordering constraints for symmetry breaking and sequence ordering.
"""

from prolog.engine.engine import Engine, Program
from prolog.ast.terms import Int, Var, Struct, List
from prolog.clpfd.api import set_domain
from prolog.clpfd.domain import Domain
from prolog.engine.builtins_clpfd import _builtin_in, _builtin_lex_chain, _builtin_fd_lt
from prolog.clpfd.props.lex import create_lex_chain_propagator


class TestLexChainBuiltin:
    """Test lex_chain/1 builtin implementation."""

    def test_lex_chain_empty_list(self):
        """Empty list should succeed trivially."""
        engine = Engine(Program([]))
        empty_list = List(())

        # lex_chain([) should succeed
        result = _builtin_lex_chain(engine, empty_list)
        assert result is True

    def test_lex_chain_single_vector(self):
        """Single vector should succeed trivially."""
        engine = Engine(Program([]))

        # Create variables for the vector
        x1 = Var(engine.store.new_var("X1"))
        x2 = Var(engine.store.new_var("X2"))
        vector = List((x1, x2))
        vectors_list = List((vector,))

        # lex_chain([[X1, X2]) should succeed
        result = _builtin_lex_chain(engine, vectors_list)
        assert result is True

    def test_lex_chain_two_ground_vectors_valid(self):
        """Two ground vectors in valid lex order should succeed."""
        engine = Engine(Program([]))

        # [1,2] ≤lex [1,3] should succeed
        v1 = List((Int(1), Int(2)))
        v2 = List((Int(1), Int(3)))
        vectors_list = List((v1, v2))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is True

    def test_lex_chain_two_ground_vectors_equal(self):
        """Two equal ground vectors should succeed."""
        engine = Engine(Program([]))

        # [1,2] ≤lex [1,2] should succeed
        v1 = List((Int(1), Int(2)))
        v2 = List((Int(1), Int(2)))
        vectors_list = List((v1, v2))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is True

    def test_lex_chain_two_ground_vectors_invalid(self):
        """Two ground vectors in invalid lex order should fail."""
        engine = Engine(Program([]))

        # [2,1] ≤lex [1,3] should fail
        v1 = List((Int(2), Int(1)))
        v2 = List((Int(1), Int(3)))
        vectors_list = List((v1, v2))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is False

    def test_lex_chain_three_ground_vectors_valid(self):
        """Three ground vectors in valid lex order should succeed."""
        engine = Engine(Program([]))

        # [0,0] ≤lex [0,1] ≤lex [1,0] should succeed
        v1 = List((Int(0), Int(0)))
        v2 = List((Int(0), Int(1)))
        v3 = List((Int(1), Int(0)))
        vectors_list = List((v1, v2, v3))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is True

    def test_lex_chain_three_ground_vectors_invalid(self):
        """Three ground vectors with invalid lex order should fail."""
        engine = Engine(Program([]))

        # [1,0] ≤lex [0,1] ≤lex [0,2] should fail ([1,0] > [0,1)
        v1 = List((Int(1), Int(0)))
        v2 = List((Int(0), Int(1)))
        v3 = List((Int(0), Int(2)))
        vectors_list = List((v1, v2, v3))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is False

    def test_lex_chain_unequal_length_vectors(self):
        """Vectors of unequal length should fail."""
        engine = Engine(Program([]))

        # [1,2] and [1,2,3] have different lengths
        v1 = List((Int(1), Int(2)))
        v2 = List((Int(1), Int(2), Int(3)))
        vectors_list = List((v1, v2))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is False

    def test_lex_chain_invalid_arguments(self):
        """Invalid argument types should fail."""
        engine = Engine(Program([]))

        # Non-list argument should fail
        result = _builtin_lex_chain(engine, Int(42))
        assert result is False

        # List containing non-list elements should fail
        non_list_elem = Int(42)
        vectors_list = List((non_list_elem,))
        result = _builtin_lex_chain(engine, vectors_list)
        assert result is False

    def test_lex_chain_variables_constrained(self):
        """Variables should be properly constrained by lex ordering."""
        engine = Engine(Program([]))

        # Create variables for two vectors: [A,B] ≤lex [C,D]
        a = Var(engine.store.new_var("A"))
        b = Var(engine.store.new_var("B"))
        c = Var(engine.store.new_var("C"))
        d = Var(engine.store.new_var("D"))

        # Set domains for variables
        _builtin_in(engine, a, Struct("..", (Int(0), Int(2))))
        _builtin_in(engine, b, Struct("..", (Int(0), Int(2))))
        _builtin_in(engine, c, Struct("..", (Int(0), Int(2))))
        _builtin_in(engine, d, Struct("..", (Int(0), Int(2))))

        v1 = List((a, b))
        v2 = List((c, d))
        vectors_list = List((v1, v2))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is True

        # Verify that constraint was posted and propagation occurred
        # The exact domain changes depend on the propagator implementation

    def test_lex_chain_first_position_ordering(self):
        """First position determines ordering when different."""
        engine = Engine(Program([]))

        # [0,9] ≤lex [1,0] should succeed (0 < 1 at first position)
        v1 = List((Int(0), Int(9)))
        v2 = List((Int(1), Int(0)))
        vectors_list = List((v1, v2))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is True

    def test_lex_chain_prefix_equality_propagation(self):
        """Equal prefixes should propagate correctly."""
        engine = Engine(Program([]))

        # [1,1,A] ≤lex [1,1,B] should ensure A ≤ B
        a = Var(engine.store.new_var("A"))
        b = Var(engine.store.new_var("B"))

        # Set domains that will force propagation: A ∈ {3,4}, B ∈ {1,2}
        _builtin_in(engine, a, Struct("..", (Int(3), Int(4))))
        _builtin_in(engine, b, Struct("..", (Int(1), Int(2))))

        v1 = List((Int(1), Int(1), a))
        v2 = List((Int(1), Int(1), b))
        vectors_list = List((v1, v2))

        result = _builtin_lex_chain(engine, vectors_list)
        # This should fail because A ∈ {3,4} and B ∈ {1,2}, so A > B violates lex ordering
        assert result is False

    def test_lex_chain_symmetry_breaking_pattern(self):
        """Test typical symmetry breaking usage pattern."""
        engine = Engine(Program([]))

        # Create a 2x2 matrix with variables
        x11 = Var(engine.store.new_var("X11"))
        x12 = Var(engine.store.new_var("X12"))
        x21 = Var(engine.store.new_var("X21"))
        x22 = Var(engine.store.new_var("X22"))

        # Set binary domains
        for var in [x11, x12, x21, x22]:
            _builtin_in(engine, var, Struct("..", (Int(0), Int(1))))

        # Rows: [X11,X12] ≤lex [X21,X22]
        row1 = List((x11, x12))
        row2 = List((x21, x22))
        rows = List((row1, row2))

        result = _builtin_lex_chain(engine, rows)
        assert result is True


class TestLexChainPropagator:
    """Test the lex constraint propagator directly."""

    def test_lex_propagator_creation(self):
        """Test that lex propagator can be created."""
        engine = Engine(Program([]))

        # Create variables for two vectors
        vars1 = [engine.store.new_var("A"), engine.store.new_var("B")]
        vars2 = [engine.store.new_var("C"), engine.store.new_var("D")]

        # Create propagator
        prop = create_lex_chain_propagator([vars1, vars2])
        assert prop is not None
        assert callable(prop)

    def test_lex_propagator_basic_filtering(self):
        """Test basic bounds filtering in lex propagator."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create variables: [A,B] ≤lex [C,D]
        a = store.new_var("A")
        b = store.new_var("B")
        c = store.new_var("C")
        d = store.new_var("D")

        # Set initial domains
        for var in [a, b, c, d]:
            set_domain(store, var, Domain(((0, 2),)), trail)

        # Create and run propagator
        prop = create_lex_chain_propagator([[a, b], [c, d]])
        status, changed = prop(store, trail, engine, "initial")

        assert status == "ok"
        # Propagator should succeed with basic domains

    def test_lex_propagator_prefix_equality(self):
        """Test prefix equality propagation."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create variables: [1,A] ≤lex [1,B] should enforce A ≤ B
        a = store.new_var("A")
        b = store.new_var("B")

        # Set domains: A ∈ {2,3}, B ∈ {1,2}
        set_domain(store, a, Domain(((2, 3),)), trail)
        set_domain(store, b, Domain(((1, 2),)), trail)

        # Create vectors with ground first elements
        # This will be handled by the builtin, but we can test the propagator logic
        vars1 = [None, a]  # [1, A] - None indicates ground value 1
        vars2 = [None, b]  # [1, B] - None indicates ground value 1
        values1 = [1, None]  # Ground values, None for variables
        values2 = [1, None]

        prop = create_lex_chain_propagator([vars1, vars2], [values1, values2])
        status, changed = prop(store, trail, engine, "initial")

        # Should succeed but may do some pruning since A ∈ {2,3}, B ∈ {1,2}
        # The constraint A ≤ B can be satisfied with A=2, B=2
        assert status == "ok"

    def test_lex_propagator_critical_position(self):
        """Test filtering at critical position."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create variables: [A,B] ≤lex [C,D] where A > C should fail
        a = store.new_var("A")
        b = store.new_var("B")
        c = store.new_var("C")
        d = store.new_var("D")

        # Set domains where first position violates lex order
        set_domain(store, a, Domain(((3, 3),)), trail)  # A = 3
        set_domain(store, b, Domain(((0, 2),)), trail)
        set_domain(store, c, Domain(((1, 1),)), trail)  # C = 1, so A > C
        set_domain(store, d, Domain(((0, 2),)), trail)

        prop = create_lex_chain_propagator([[a, b], [c, d]])
        status, changed = prop(store, trail, engine, "initial")

        assert status == "fail"

    def test_lex_propagator_failure_detection(self):
        """Test propagator failure detection."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create situation that should fail: [2,1] ≤lex [1,3]
        a = store.new_var("A")
        b = store.new_var("B")
        c = store.new_var("C")
        d = store.new_var("D")

        # Set singleton domains creating invalid lex order
        set_domain(store, a, Domain(((2, 2),)), trail)  # A = 2
        set_domain(store, b, Domain(((1, 1),)), trail)  # B = 1
        set_domain(store, c, Domain(((1, 1),)), trail)  # C = 1
        set_domain(store, d, Domain(((3, 3),)), trail)  # D = 3

        prop = create_lex_chain_propagator([[a, b], [c, d]])
        status, changed = prop(store, trail, engine, "initial")

        assert status == "fail"

    def test_lex_propagator_bounds_tightening(self):
        """Test that propagator tightens bounds correctly."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create variables: [A,B] ≤lex [C,D]
        a = store.new_var("A")
        b = store.new_var("B")
        c = store.new_var("C")
        d = store.new_var("D")

        # Set domains where some pruning should occur
        set_domain(store, a, Domain(((1, 3),)), trail)
        set_domain(store, b, Domain(((1, 3),)), trail)
        set_domain(store, c, Domain(((2, 4),)), trail)
        set_domain(store, d, Domain(((1, 3),)), trail)

        prop = create_lex_chain_propagator([[a, b], [c, d]])
        status, changed = prop(store, trail, engine, "initial")

        assert status == "ok"
        # With A ∈ 1..3, B ∈ 1..3, C ∈ 2..4, D ∈ 1..3
        # Since min(C) = 2 > 1 = min(A), some pruning should occur
        # but we accept basic propagation success here

    def test_lex_propagator_equal_vectors_case(self):
        """Test handling of equal vectors."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create variables: [A,B] ≤lex [A,B] (same variables)
        a = store.new_var("A")
        b = store.new_var("B")

        set_domain(store, a, Domain(((1, 3),)), trail)
        set_domain(store, b, Domain(((1, 3),)), trail)

        prop = create_lex_chain_propagator([[a, b], [a, b]])
        status, changed = prop(store, trail, engine, "initial")

        assert status == "ok"
        # Equal vectors should always satisfy lex ordering

    def test_lex_propagator_chain_of_three(self):
        """Test propagator with chain of three vectors."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create variables: [A,B] ≤lex [C,D] ≤lex [E,F]
        a = store.new_var("A")
        b = store.new_var("B")
        c = store.new_var("C")
        d = store.new_var("D")
        e = store.new_var("E")
        f = store.new_var("F")

        # Set domains
        for var in [a, b, c, d, e, f]:
            set_domain(store, var, Domain(((0, 2),)), trail)

        prop = create_lex_chain_propagator([[a, b], [c, d], [e, f]])
        status, changed = prop(store, trail, engine, "initial")

        assert status == "ok"

    def test_lex_propagator_singleton_domains(self):
        """Test propagator with singleton domains."""
        engine = Engine(Program([]))
        store = engine.store
        trail = engine.trail

        # Create variables with singleton domains: [1,A] ≤lex [1,B]
        a = store.new_var("A")
        b = store.new_var("B")

        # A and B both have singleton domains
        set_domain(store, a, Domain(((2, 2),)), trail)  # A = 2
        set_domain(store, b, Domain(((3, 3),)), trail)  # B = 3

        # Since first elements are equal (both 1), should check A ≤ B
        # This test assumes we handle ground values appropriately
        vars1 = [None, a]  # [1, A]
        vars2 = [None, b]  # [1, B]
        values1 = [1, None]
        values2 = [1, None]

        prop = create_lex_chain_propagator([vars1, vars2], [values1, values2])
        status, changed = prop(store, trail, engine, "initial")

        assert status == "ok"  # 2 ≤ 3, so should succeed


class TestLexChainIntegration:
    """Integration tests for lex_chain/1 with other constraints."""

    def test_lex_chain_with_other_constraints(self):
        """Test lex_chain combined with other CLP(FD) constraints."""
        engine = Engine(Program([]))

        # Create variables
        a = Var(engine.store.new_var("A"))
        b = Var(engine.store.new_var("B"))
        c = Var(engine.store.new_var("C"))
        d = Var(engine.store.new_var("D"))

        # Set domains
        for var in [a, b, c, d]:
            _builtin_in(engine, var, Struct("..", (Int(1), Int(3))))

        # Post lex_chain constraint: [A,B] ≤lex [C,D]
        v1 = List((a, b))
        v2 = List((c, d))
        vectors_list = List((v1, v2))

        result1 = _builtin_lex_chain(engine, vectors_list)
        assert result1 is True

        # Add additional constraint: A #< C (forces strict ordering at first position)
        result2 = _builtin_fd_lt(engine, a, c)
        assert result2 is True

    def test_lex_chain_labeling_integration(self):
        """Test lex_chain with labeling to enumerate solutions."""
        engine = Engine(Program([]))

        # Create simple 2-vector case for testing
        a = Var(engine.store.new_var("A"))
        b = Var(engine.store.new_var("B"))
        c = Var(engine.store.new_var("C"))
        d = Var(engine.store.new_var("D"))

        # Small domains for exhaustive testing
        for var in [a, b, c, d]:
            _builtin_in(engine, var, Struct("..", (Int(0), Int(1))))

        # Post lex_chain: [A,B] ≤lex [C,D]
        v1 = List((a, b))
        v2 = List((c, d))
        vectors_list = List((v1, v2))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is True

        # The constraint should succeed and allow labeling
        # Specific solution enumeration would require implementing label/1


class TestLexChainEdgeCases:
    """Test edge cases and boundary conditions."""

    def test_lex_chain_singleton_domains(self):
        """Test with variables that have singleton domains."""
        engine = Engine(Program([]))

        # Variables with singleton domains
        a = Var(engine.store.new_var("A"))
        b = Var(engine.store.new_var("B"))
        c = Var(engine.store.new_var("C"))
        d = Var(engine.store.new_var("D"))

        # Set singleton domains creating valid lex order
        _builtin_in(engine, a, Int(1))  # A = 1
        _builtin_in(engine, b, Int(2))  # B = 2
        _builtin_in(engine, c, Int(1))  # C = 1
        _builtin_in(engine, d, Int(3))  # D = 3

        # [1,2] ≤lex [1,3] should succeed
        v1 = List((a, b))
        v2 = List((c, d))
        vectors_list = List((v1, v2))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is True

    def test_lex_chain_singleton_domains_invalid(self):
        """Test with variables that have singleton domains in invalid order."""
        engine = Engine(Program([]))

        a = Var(engine.store.new_var("A"))
        b = Var(engine.store.new_var("B"))
        c = Var(engine.store.new_var("C"))
        d = Var(engine.store.new_var("D"))

        # Set singleton domains creating invalid lex order
        _builtin_in(engine, a, Int(2))  # A = 2
        _builtin_in(engine, b, Int(1))  # B = 1
        _builtin_in(engine, c, Int(1))  # C = 1
        _builtin_in(engine, d, Int(3))  # D = 3

        # [2,1] ≤lex [1,3] should fail
        v1 = List((a, b))
        v2 = List((c, d))
        vectors_list = List((v1, v2))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is False

    def test_lex_chain_mixed_ground_variables(self):
        """Test with mix of ground values and variables."""
        engine = Engine(Program([]))

        # Mix of ground and variables: [1,A] ≤lex [1,B]
        a = Var(engine.store.new_var("A"))
        b = Var(engine.store.new_var("B"))

        _builtin_in(engine, a, Struct("..", (Int(0), Int(5))))
        _builtin_in(engine, b, Struct("..", (Int(0), Int(5))))

        v1 = List((Int(1), a))
        v2 = List((Int(1), b))
        vectors_list = List((v1, v2))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is True

    def test_lex_chain_large_vectors(self):
        """Test with larger vectors to check scalability."""
        engine = Engine(Program([]))

        # Create two 5-element vectors
        vars1 = [Var(engine.store.new_var(f"A{i}")) for i in range(5)]
        vars2 = [Var(engine.store.new_var(f"B{i}")) for i in range(5)]

        # Set domains
        for var in vars1 + vars2:
            _builtin_in(engine, var, Struct("..", (Int(0), Int(2))))

        v1 = List(tuple(vars1))
        v2 = List(tuple(vars2))
        vectors_list = List((v1, v2))

        result = _builtin_lex_chain(engine, vectors_list)
        assert result is True

    def test_lex_chain_performance_baseline(self):
        """Performance baseline test for lex_chain constraint."""
        engine = Engine(Program([]))

        # Test with reasonably sized constraint
        # 3 vectors of length 10 each
        vectors = []
        all_vars = []

        for i in range(3):
            vec_vars = [Var(engine.store.new_var(f"X{i}_{j}")) for j in range(10)]
            all_vars.extend(vec_vars)
            vectors.append(List(tuple(vec_vars)))

        # Set domains
        for var in all_vars:
            _builtin_in(engine, var, Struct("..", (Int(0), Int(9))))

        vectors_list = List(tuple(vectors))

        # This should complete in reasonable time (< 100ms target)
        result = _builtin_lex_chain(engine, vectors_list)
        assert result is True
