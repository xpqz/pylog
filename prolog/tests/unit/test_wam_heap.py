"""Unit tests for WAM heap allocation helpers."""

from prolog.wam.heap import (
    TAG_CON,
    TAG_LIST,
    TAG_REF,
    TAG_STR,
    is_con,
    is_list,
    is_ref,
    is_str,
    make_con,
    make_functor,
    make_list,
    make_ref,
    make_str,
    new_con,
    new_list,
    new_ref,
    new_str,
)
from prolog.wam.machine import Machine


class TestCellConstructors:
    """Test cell constructor functions (make_* family)."""

    def test_make_ref_creates_ref_cell(self):
        """make_ref creates (TAG_REF, addr) tuple."""
        cell = make_ref(5)
        assert cell == (TAG_REF, 5)
        assert is_ref(cell)

    def test_make_str_creates_str_cell(self):
        """make_str creates (TAG_STR, functor_addr) tuple."""
        cell = make_str(10)
        assert cell == (TAG_STR, 10)
        assert is_str(cell)

    def test_make_con_creates_con_cell_with_atom(self):
        """make_con creates (TAG_CON, value) for atoms."""
        cell = make_con("foo")
        assert cell == (TAG_CON, "foo")
        assert is_con(cell)

    def test_make_con_creates_con_cell_with_int(self):
        """make_con creates (TAG_CON, value) for integers."""
        cell = make_con(42)
        assert cell == (TAG_CON, 42)
        assert is_con(cell)

    def test_make_list_creates_list_cell(self):
        """make_list creates (TAG_LIST, head_addr, tail_addr) tuple."""
        cell = make_list(3, 7)
        assert cell == (TAG_LIST, 3, 7)
        assert is_list(cell)

    def test_make_functor_creates_functor_cell(self):
        """make_functor creates (TAG_CON, (name, arity)) tuple."""
        cell = make_functor("foo", 2)
        assert cell == (TAG_CON, ("foo", 2))
        assert is_con(cell)


class TestCellTypePredicates:
    """Test cell type checking functions (is_* family)."""

    def test_is_ref_identifies_ref_cells(self):
        """is_ref returns True for REF cells only."""
        assert is_ref((TAG_REF, 0))
        assert not is_ref((TAG_STR, 0))
        assert not is_ref((TAG_CON, 0))
        assert not is_ref((TAG_LIST, 0, 0))

    def test_is_str_identifies_str_cells(self):
        """is_str returns True for STR cells only."""
        assert is_str((TAG_STR, 0))
        assert not is_str((TAG_REF, 0))
        assert not is_str((TAG_CON, 0))
        assert not is_str((TAG_LIST, 0, 0))

    def test_is_con_identifies_con_cells(self):
        """is_con returns True for CON cells only."""
        assert is_con((TAG_CON, 42))
        assert not is_con((TAG_REF, 0))
        assert not is_con((TAG_STR, 0))
        assert not is_con((TAG_LIST, 0, 0))

    def test_is_list_identifies_list_cells(self):
        """is_list returns True for LIST cells only."""
        assert is_list((TAG_LIST, 0, 0))
        assert not is_list((TAG_REF, 0))
        assert not is_list((TAG_STR, 0))
        assert not is_list((TAG_CON, 0))


class TestNewRef:
    """Test new_ref heap allocation."""

    def test_new_ref_allocates_self_referential_cell(self):
        """new_ref creates canonical unbound REF."""
        m = Machine()
        addr = new_ref(m)

        assert addr == 0
        assert m.H == 1
        assert len(m.heap) == 1
        assert m.heap[0] == (TAG_REF, 0)  # Self-referential
        assert is_ref(m.heap[0])

    def test_new_ref_increments_H_correctly(self):
        """new_ref maintains H == len(heap) invariant."""
        m = Machine()

        addr1 = new_ref(m)
        assert addr1 == 0
        assert m.H == 1
        assert m.H == len(m.heap)

        addr2 = new_ref(m)
        assert addr2 == 1
        assert m.H == 2
        assert m.H == len(m.heap)

    def test_new_ref_multiple_allocations(self):
        """Multiple new_ref calls allocate distinct cells."""
        m = Machine()

        addr1 = new_ref(m)
        addr2 = new_ref(m)
        addr3 = new_ref(m)

        assert addr1 == 0
        assert addr2 == 1
        assert addr3 == 2
        assert m.heap[0] == (TAG_REF, 0)
        assert m.heap[1] == (TAG_REF, 1)
        assert m.heap[2] == (TAG_REF, 2)

    def test_new_ref_passes_invariants(self):
        """new_ref maintains machine invariants."""
        m = Machine()
        new_ref(m)
        new_ref(m)
        m.check_invariants()  # Should not raise


class TestNewCon:
    """Test new_con heap allocation."""

    def test_new_con_allocates_atom(self):
        """new_con allocates atom constant."""
        m = Machine()
        addr = new_con(m, "foo")

        assert addr == 0
        assert m.H == 1
        assert len(m.heap) == 1
        assert m.heap[0] == (TAG_CON, "foo")
        assert is_con(m.heap[0])

    def test_new_con_allocates_integer(self):
        """new_con allocates integer constant."""
        m = Machine()
        addr = new_con(m, 42)

        assert addr == 0
        assert m.H == 1
        assert m.heap[0] == (TAG_CON, 42)

    def test_new_con_allocates_functor(self):
        """new_con can allocate functor (name, arity) pair."""
        m = Machine()
        addr = new_con(m, ("foo", 2))

        assert addr == 0
        assert m.H == 1
        assert m.heap[0] == (TAG_CON, ("foo", 2))

    def test_new_con_increments_H_correctly(self):
        """new_con maintains H == len(heap) invariant."""
        m = Machine()

        new_con(m, "a")
        assert m.H == 1
        assert m.H == len(m.heap)

        new_con(m, 99)
        assert m.H == 2
        assert m.H == len(m.heap)

    def test_new_con_passes_invariants(self):
        """new_con maintains machine invariants."""
        m = Machine()
        new_con(m, "atom")
        new_con(m, 123)
        m.check_invariants()


class TestNewStr:
    """Test new_str heap allocation."""

    def test_new_str_allocates_str_and_functor(self):
        """new_str allocates STR cell followed by functor."""
        m = Machine()
        str_addr = new_str(m, "foo", 2)

        assert str_addr == 0
        assert m.H == 2  # STR + functor
        assert len(m.heap) == 2

        # Check STR cell points to functor
        assert m.heap[0] == (TAG_STR, 1)
        assert is_str(m.heap[0])

        # Check functor cell
        assert m.heap[1] == (TAG_CON, ("foo", 2))
        assert is_con(m.heap[1])

    def test_new_str_functor_address_is_str_plus_one(self):
        """Functor is allocated at str_addr + 1."""
        m = Machine()
        str_addr = new_str(m, "bar", 3)
        functor_addr = str_addr + 1

        assert m.heap[str_addr] == (TAG_STR, functor_addr)
        assert m.heap[functor_addr] == (TAG_CON, ("bar", 3))

    def test_new_str_multiple_structures(self):
        """Multiple new_str calls allocate distinct structures."""
        m = Machine()

        str1 = new_str(m, "foo", 1)
        str2 = new_str(m, "bar", 2)

        assert str1 == 0
        assert str2 == 2  # After foo's STR+functor

        # foo/1 structure
        assert m.heap[0] == (TAG_STR, 1)
        assert m.heap[1] == (TAG_CON, ("foo", 1))

        # bar/2 structure
        assert m.heap[2] == (TAG_STR, 3)
        assert m.heap[3] == (TAG_CON, ("bar", 2))

    def test_new_str_zero_arity(self):
        """new_str handles zero-arity functors."""
        m = Machine()
        str_addr = new_str(m, "nil", 0)

        assert m.heap[str_addr] == (TAG_STR, 1)
        assert m.heap[1] == (TAG_CON, ("nil", 0))

    def test_new_str_increments_H_by_two(self):
        """new_str increments H by 2 (STR + functor)."""
        m = Machine()
        assert m.H == 0

        new_str(m, "test", 3)
        assert m.H == 2
        assert m.H == len(m.heap)

    def test_new_str_passes_invariants(self):
        """new_str maintains machine invariants."""
        m = Machine()
        new_str(m, "foo", 2)
        new_str(m, "bar", 0)
        m.check_invariants()


class TestNewList:
    """Test new_list heap allocation."""

    def test_new_list_allocates_list_cell(self):
        """new_list allocates LIST cell with head and tail addresses."""
        m = Machine()
        # Pre-allocate head and tail
        head_addr = new_con(m, 1)
        tail_addr = new_con(m, "[]")

        list_addr = new_list(m, head_addr, tail_addr)

        assert list_addr == 2
        assert m.H == 3
        assert m.heap[2] == (TAG_LIST, head_addr, tail_addr)
        assert is_list(m.heap[2])

    def test_new_list_with_ref_tail(self):
        """new_list can use REF cells for tail."""
        m = Machine()
        head = new_con(m, "a")
        tail = new_ref(m)

        list_addr = new_list(m, head, tail)

        assert m.heap[list_addr] == (TAG_LIST, head, tail)

    def test_new_list_increments_H_correctly(self):
        """new_list maintains H == len(heap) invariant."""
        m = Machine()

        h1 = new_con(m, 1)
        t1 = new_con(m, 2)
        list1 = new_list(m, h1, t1)

        assert list1 == 2
        assert m.H == 3
        assert m.H == len(m.heap)

    def test_new_list_nested_list(self):
        """new_list supports nested list construction."""
        m = Machine()

        # Build [3 | []]
        nil = new_con(m, "[]")
        inner_list = new_list(m, new_con(m, 3), nil)

        # Build [2 | [3 | []]]
        outer_list = new_list(m, new_con(m, 2), inner_list)

        # Verify structure
        assert is_list(m.heap[outer_list])
        assert is_list(m.heap[inner_list])

    def test_new_list_passes_invariants(self):
        """new_list maintains machine invariants."""
        m = Machine()
        h = new_con(m, "x")
        t = new_ref(m)
        new_list(m, h, t)
        m.check_invariants()


class TestHeapInvariant:
    """Test that H == len(heap) is maintained."""

    def test_invariant_holds_for_mixed_allocations(self):
        """H == len(heap) after various allocation patterns."""
        m = Machine()
        assert m.H == len(m.heap)

        new_ref(m)
        assert m.H == len(m.heap)

        new_con(m, "atom")
        assert m.H == len(m.heap)

        new_str(m, "foo", 2)
        assert m.H == len(m.heap)

        h = new_ref(m)
        t = new_ref(m)
        new_list(m, h, t)
        assert m.H == len(m.heap)

    def test_invariant_holds_after_many_allocations(self):
        """H == len(heap) after 100 allocations."""
        m = Machine()

        for i in range(100):
            if i % 4 == 0:
                new_ref(m)
            elif i % 4 == 1:
                new_con(m, i)
            elif i % 4 == 2:
                new_str(m, f"f{i}", i % 5)
            else:
                h = new_ref(m)
                t = new_ref(m)
                new_list(m, h, t)

            assert m.H == len(m.heap), f"Invariant violated at iteration {i}"


class TestIntegrationScenarios:
    """Test realistic heap construction scenarios."""

    def test_build_simple_structure(self):
        """Build foo(X, bar) on heap."""
        m = Machine()

        # foo(X, bar)
        str_addr = new_str(m, "foo", 2)
        functor_addr = str_addr + 1

        # Arguments at functor_addr + 1, functor_addr + 2
        arg1 = new_ref(m)  # X
        arg2 = new_con(m, "bar")

        # Verify structure
        assert m.heap[str_addr] == (TAG_STR, functor_addr)
        assert m.heap[functor_addr] == (TAG_CON, ("foo", 2))
        assert m.heap[arg1] == (TAG_REF, arg1)  # Unbound X
        assert m.heap[arg2] == (TAG_CON, "bar")

        m.check_invariants()

    def test_build_nested_structure(self):
        """Build foo(bar(X), Y) on heap."""
        m = Machine()

        # Inner: bar(X)
        inner_str = new_str(m, "bar", 1)
        new_ref(m)  # X

        # Outer: foo(bar(X), Y)
        outer_str = new_str(m, "foo", 2)
        new_ref(m)  # Y

        # At this point:
        # 0: STR -> 1
        # 1: CON ("bar", 1)
        # 2: REF 2 (X)
        # 3: STR -> 4
        # 4: CON ("foo", 2)
        # 5: REF 5 (Y)

        assert len(m.heap) == 6
        assert m.H == 6

        # Verify inner structure
        assert m.heap[inner_str] == (TAG_STR, 1)
        assert m.heap[1] == (TAG_CON, ("bar", 1))

        # Verify outer structure
        assert m.heap[outer_str] == (TAG_STR, 4)
        assert m.heap[4] == (TAG_CON, ("foo", 2))

        m.check_invariants()

    def test_build_list_of_atoms(self):
        """Build [a, b, c] on heap."""
        m = Machine()

        # Build from right: [] then [c|[]] then [b|[c|[]]] then [a|[b|[c|[]]]]
        nil = new_con(m, "[]")

        c = new_con(m, "c")
        list3 = new_list(m, c, nil)

        b = new_con(m, "b")
        list2 = new_list(m, b, list3)

        a = new_con(m, "a")
        list1 = new_list(m, a, list2)

        # Verify final list is at list1
        assert is_list(m.heap[list1])

        m.check_invariants()
