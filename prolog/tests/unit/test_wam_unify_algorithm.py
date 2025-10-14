"""Unit tests for WAM unification algorithm."""

from prolog.wam.heap import new_con, new_list, new_ref, new_str
from prolog.wam.machine import Machine
from prolog.wam.unify import deref, unify


class TestBasicUnification:
    """Test basic unification cases."""

    def test_unify_same_address(self):
        """Unifying same address is always true."""
        m = Machine()
        addr = new_con(m, 42)
        assert unify(m, addr, addr)

    def test_unify_two_unbound_variables(self):
        """Unify two unbound variables."""
        m = Machine()
        var1 = new_ref(m)
        var2 = new_ref(m)

        assert unify(m, var1, var2)

        # Both should now deref to same root
        root1 = deref(m, var1)
        root2 = deref(m, var2)
        assert root1 == root2

    def test_unify_variable_with_constant(self):
        """Unify variable with constant."""
        m = Machine()
        var = new_ref(m)
        const = new_con(m, "foo")

        assert unify(m, var, const)

        # Variable should be bound to constant
        assert deref(m, var) == const

    def test_unify_constant_with_variable(self):
        """Unify constant with variable (reversed order)."""
        m = Machine()
        const = new_con(m, 99)
        var = new_ref(m)

        assert unify(m, const, var)

        # Variable should be bound to constant
        assert deref(m, var) == const

    def test_unify_identical_constants_succeeds(self):
        """Unifying identical constants succeeds."""
        m = Machine()
        const1 = new_con(m, "same")
        const2 = new_con(m, "same")

        assert unify(m, const1, const2)

    def test_unify_different_constants_fails(self):
        """Unifying different constants fails."""
        m = Machine()
        const1 = new_con(m, "foo")
        const2 = new_con(m, "bar")

        assert not unify(m, const1, const2)

    def test_unify_integer_constants(self):
        """Unify integer constants."""
        m = Machine()
        int1 = new_con(m, 42)
        int2 = new_con(m, 42)

        assert unify(m, int1, int2)

    def test_unify_different_integers_fails(self):
        """Unifying different integers fails."""
        m = Machine()
        int1 = new_con(m, 1)
        int2 = new_con(m, 2)

        assert not unify(m, int1, int2)


class TestStructureUnification:
    """Test structure unification."""

    def test_unify_identical_structures(self):
        """Unify two identical structures."""
        m = Machine()

        # f(a, b)
        str1 = new_str(m, "f", 2)
        new_con(m, "a")
        new_con(m, "b")

        # f(a, b)
        str2 = new_str(m, "f", 2)
        new_con(m, "a")
        new_con(m, "b")

        assert unify(m, str1, str2)

    def test_unify_structure_with_variables(self):
        """Unify f(X, b) with f(a, Y)."""
        m = Machine()

        # f(X, b)
        str1 = new_str(m, "f", 2)
        x = new_ref(m)
        new_con(m, "b")

        # f(a, Y)
        str2 = new_str(m, "f", 2)
        new_con(m, "a")
        y = new_ref(m)

        assert unify(m, str1, str2)

        # X should be bound to "a"
        assert m.heap[deref(m, x)] == (2, "a")  # TAG_CON, "a"
        # Y should be bound to "b"
        assert m.heap[deref(m, y)] == (2, "b")  # TAG_CON, "b"

    def test_unify_structures_with_shared_variable(self):
        """Unify f(X, X) with f(a, Y)."""
        m = Machine()

        # f(X, X)
        str1 = new_str(m, "f", 2)
        x = new_ref(m)
        # Second argument points to same X
        m.heap.append((0, x))  # REF pointing to X
        m.H += 1

        # f(a, Y)
        str2 = new_str(m, "f", 2)
        new_con(m, "a")
        y = new_ref(m)

        assert unify(m, str1, str2)

        # X should be bound to "a"
        x_root = deref(m, x)
        assert m.heap[x_root] == (2, "a")
        # Y should also be bound to "a" (through X)
        y_root = deref(m, y)
        assert m.heap[y_root] == (2, "a")

    def test_unify_structures_different_functors_fails(self):
        """Unifying structures with different functors fails."""
        m = Machine()

        # f(a)
        str1 = new_str(m, "f", 1)
        new_con(m, "a")

        # g(a)
        str2 = new_str(m, "g", 1)
        new_con(m, "a")

        assert not unify(m, str1, str2)

    def test_unify_structures_different_arity_fails(self):
        """Unifying structures with different arity fails."""
        m = Machine()

        # f(a)
        str1 = new_str(m, "f", 1)
        new_con(m, "a")

        # f(a, b)
        str2 = new_str(m, "f", 2)
        new_con(m, "a")
        new_con(m, "b")

        assert not unify(m, str1, str2)

    def test_unify_structure_with_multiple_variables(self):
        """Unify f(X, Y, Z) with f(1, 2, 3)."""
        m = Machine()

        # f(X, Y, Z)
        str1 = new_str(m, "f", 3)
        x = new_ref(m)
        y = new_ref(m)
        z = new_ref(m)

        # f(1, 2, 3)
        str2 = new_str(m, "f", 3)
        new_con(m, 1)
        new_con(m, 2)
        new_con(m, 3)

        assert unify(m, str1, str2)

        # Variables should be bound
        assert m.heap[deref(m, x)] == (2, 1)
        assert m.heap[deref(m, y)] == (2, 2)
        assert m.heap[deref(m, z)] == (2, 3)

    def test_unify_variable_with_structure(self):
        """Unify X with f(a, b)."""
        m = Machine()

        x = new_ref(m)

        # f(a, b)
        struct = new_str(m, "f", 2)
        new_con(m, "a")
        new_con(m, "b")

        assert unify(m, x, struct)

        # X should be bound to the structure
        assert deref(m, x) == struct


class TestListUnification:
    """Test list unification."""

    def test_unify_identical_lists(self):
        """Unify [1, 2] with [1, 2]."""
        m = Machine()

        # [1, 2] = [1 | [2 | []]]
        nil1 = new_con(m, "[]")
        inner1 = new_list(m, new_con(m, 2), nil1)
        list1 = new_list(m, new_con(m, 1), inner1)

        # [1, 2]
        nil2 = new_con(m, "[]")
        inner2 = new_list(m, new_con(m, 2), nil2)
        list2 = new_list(m, new_con(m, 1), inner2)

        assert unify(m, list1, list2)

    def test_unify_list_with_variables(self):
        """Unify [X, Y] with [1, 2]."""
        m = Machine()

        # [X, Y]
        x = new_ref(m)
        y = new_ref(m)
        nil1 = new_con(m, "[]")
        inner1 = new_list(m, y, nil1)
        list1 = new_list(m, x, inner1)

        # [1, 2]
        nil2 = new_con(m, "[]")
        inner2 = new_list(m, new_con(m, 2), nil2)
        list2 = new_list(m, new_con(m, 1), inner2)

        assert unify(m, list1, list2)

        # X should be 1, Y should be 2
        assert m.heap[deref(m, x)] == (2, 1)
        assert m.heap[deref(m, y)] == (2, 2)

    def test_unify_list_with_tail_variable(self):
        """Unify [H|T] with [1, 2, 3]."""
        m = Machine()

        # [H|T]
        h = new_ref(m)
        t = new_ref(m)
        list1 = new_list(m, h, t)

        # [1, 2, 3]
        nil = new_con(m, "[]")
        l3 = new_list(m, new_con(m, 3), nil)
        l2 = new_list(m, new_con(m, 2), l3)
        list2 = new_list(m, new_con(m, 1), l2)

        assert unify(m, list1, list2)

        # H should be 1
        assert m.heap[deref(m, h)] == (2, 1)
        # T should be [2, 3]
        assert deref(m, t) == l2

    def test_unify_variable_with_list(self):
        """Unify X with [a, b, c]."""
        m = Machine()

        x = new_ref(m)

        # [a, b, c]
        nil = new_con(m, "[]")
        l3 = new_list(m, new_con(m, "c"), nil)
        l2 = new_list(m, new_con(m, "b"), l3)
        list1 = new_list(m, new_con(m, "a"), l2)

        assert unify(m, x, list1)

        # X should be bound to the list
        assert deref(m, x) == list1


class TestTypeMismatches:
    """Test type mismatch failures."""

    def test_unify_constant_with_structure_fails(self):
        """Unifying constant with structure fails."""
        m = Machine()

        const = new_con(m, "foo")
        struct = new_str(m, "f", 1)
        new_con(m, "bar")

        assert not unify(m, const, struct)

    def test_unify_constant_with_list_fails(self):
        """Unifying constant with list fails."""
        m = Machine()

        const = new_con(m, "atom")
        nil = new_con(m, "[]")
        list1 = new_list(m, new_con(m, 1), nil)

        assert not unify(m, const, list1)

    def test_unify_structure_with_list_fails(self):
        """Unifying structure with list fails."""
        m = Machine()

        struct = new_str(m, "f", 1)
        new_con(m, "a")

        nil = new_con(m, "[]")
        list1 = new_list(m, new_con(m, 1), nil)

        assert not unify(m, struct, list1)


class TestComplexScenarios:
    """Test complex unification scenarios."""

    def test_unify_structure_with_complex_terms(self):
        """Unify structures with mixed argument types."""
        m = Machine()

        # f(1, X, "atom")
        str1 = new_str(m, "f", 3)
        new_con(m, 1)
        x = new_ref(m)
        new_con(m, "atom")

        # f(Y, 2, "atom")
        str2 = new_str(m, "f", 3)
        y = new_ref(m)
        new_con(m, 2)
        new_con(m, "atom")

        assert unify(m, str1, str2)

        # X should be bound to 2, Y should be bound to 1
        assert m.heap[deref(m, x)] == (2, 2)
        assert m.heap[deref(m, y)] == (2, 1)

    def test_unify_circular_structure_without_occurs_check(self):
        """Unify X with f(X) - creates circular structure (occurs-check off)."""
        m = Machine()

        x = new_ref(m)

        # f(X) - structure points to itself
        fx = new_str(m, "f", 1)
        m.heap.append((0, x))  # REF to X
        m.H += 1

        # This should succeed (occurs-check is off)
        assert unify(m, x, fx)

        # X is now bound to f(X), creating a cycle
        assert deref(m, x) == fx


class TestUnificationProperties:
    """Property-based tests for unification."""

    def test_symmetry_constants(self):
        """unify(A, B) == unify(B, A)."""
        m = Machine()
        const1 = new_con(m, 42)
        const2 = new_con(m, 42)

        result1 = unify(m, const1, const2)
        m.reset()
        const1 = new_con(m, 42)
        const2 = new_con(m, 42)
        result2 = unify(m, const2, const1)

        assert result1 == result2

    def test_symmetry_variables(self):
        """unify(X, Y) == unify(Y, X)."""
        m = Machine()
        x = new_ref(m)
        y = new_ref(m)

        result1 = unify(m, x, y)

        m.reset()
        x = new_ref(m)
        y = new_ref(m)
        result2 = unify(m, y, x)

        assert result1 == result2
        assert result1  # Both should succeed

    def test_idempotence(self):
        """Unifying twice gives same result."""
        m = Machine()
        x = new_ref(m)
        const = new_con(m, "foo")

        # First unification
        result1 = unify(m, x, const)
        assert result1

        # Second unification (X already bound)
        result2 = unify(m, x, const)
        assert result2

    def test_transitivity(self):
        """If X=Y and Y=Z, then X=Z."""
        m = Machine()
        x = new_ref(m)
        y = new_ref(m)
        z = new_ref(m)

        # X = Y
        assert unify(m, x, y)
        # Y = Z
        assert unify(m, y, z)

        # All should deref to same root
        root_x = deref(m, x)
        root_y = deref(m, y)
        root_z = deref(m, z)
        assert root_x == root_y == root_z


class TestHeapInvariants:
    """Test that unification maintains heap invariants."""

    def test_unification_maintains_invariants(self):
        """Unification maintains check_invariants."""
        m = Machine()

        # Build structures
        x = new_ref(m)
        struct = new_str(m, "f", 2)
        new_con(m, "a")
        new_ref(m)

        m.check_invariants()

        assert unify(m, x, struct)

        m.check_invariants()

    def test_failed_unification_maintains_invariants(self):
        """Failed unification still maintains invariants."""
        m = Machine()

        const1 = new_con(m, "foo")
        const2 = new_con(m, "bar")

        m.check_invariants()

        assert not unify(m, const1, const2)

        m.check_invariants()


class TestEdgeCases:
    """Test edge cases."""

    def test_unify_zero_arity_structures(self):
        """Unify zero-arity structures."""
        m = Machine()

        # f()
        str1 = new_str(m, "f", 0)
        # f()
        str2 = new_str(m, "f", 0)

        assert unify(m, str1, str2)

    def test_unify_large_arity_structure(self):
        """Unify structure with many arguments."""
        m = Machine()

        # f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        str1 = new_str(m, "f", 10)
        for i in range(1, 11):
            new_con(m, i)

        # f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        str2 = new_str(m, "f", 10)
        for i in range(1, 11):
            new_con(m, i)

        assert unify(m, str1, str2)
