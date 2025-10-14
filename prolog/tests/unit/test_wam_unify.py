"""Unit tests for WAM unification operations."""


from prolog.wam.heap import TAG_REF, make_ref, new_con, new_ref
from prolog.wam.machine import Machine
from prolog.wam.unify import bind, deref, trail_if_needed, untrail


class TestDeref:
    """Test deref operation."""

    def test_deref_non_ref_returns_same_address(self):
        """Deref on constant returns same address."""
        m = Machine()
        addr = new_con(m, 42)
        assert deref(m, addr) == addr

    def test_deref_unbound_ref_returns_same_address(self):
        """Deref on unbound variable returns same address."""
        m = Machine()
        addr = new_ref(m)
        assert m.heap[addr] == (TAG_REF, addr)  # Self-referential
        assert deref(m, addr) == addr

    def test_deref_follows_single_ref_chain(self):
        """Deref follows one-hop REF chain."""
        m = Machine()
        const_addr = new_con(m, "foo")
        ref_addr = new_ref(m)

        # Bind ref to constant
        m.heap[ref_addr] = make_ref(const_addr)

        assert deref(m, ref_addr) == const_addr

    def test_deref_follows_two_hop_chain(self):
        """Deref follows two-hop REF chain."""
        m = Machine()
        const_addr = new_con(m, 99)
        ref1 = new_ref(m)
        ref2 = new_ref(m)

        # ref2 -> ref1 -> const
        m.heap[ref1] = make_ref(const_addr)
        m.heap[ref2] = make_ref(ref1)

        assert deref(m, ref2) == const_addr

    def test_deref_follows_long_chain(self):
        """Deref follows N-hop REF chain."""
        m = Machine()
        const_addr = new_con(m, "end")

        # Create chain: ref4 -> ref3 -> ref2 -> ref1 -> const
        ref1 = new_ref(m)
        ref2 = new_ref(m)
        ref3 = new_ref(m)
        ref4 = new_ref(m)

        m.heap[ref1] = make_ref(const_addr)
        m.heap[ref2] = make_ref(ref1)
        m.heap[ref3] = make_ref(ref2)
        m.heap[ref4] = make_ref(ref3)

        assert deref(m, ref4) == const_addr

    def test_deref_chain_to_unbound_var(self):
        """Deref chain ending in unbound variable."""
        m = Machine()
        unbound = new_ref(m)  # Self-referential
        ref = new_ref(m)

        # ref -> unbound
        m.heap[ref] = make_ref(unbound)

        assert deref(m, ref) == unbound

    def test_deref_is_iterative_no_stack_overflow(self):
        """Deref handles very long chains without stack overflow."""
        m = Machine()
        const_addr = new_con(m, "final")

        # Create 1000-hop chain
        prev = const_addr
        for _ in range(1000):
            ref = new_ref(m)
            m.heap[ref] = make_ref(prev)
            prev = ref

        # Should not raise RecursionError
        result = deref(m, prev)
        assert result == const_addr


class TestTrailIfNeeded:
    """Test trail_if_needed operation."""

    def test_trail_if_needed_with_hb_zero_does_not_trail(self):
        """In Phase 1, HB=0 means no trailing occurs."""
        m = Machine()
        m.HB = 0
        addr = new_ref(m)  # addr = 0

        initial_trail_len = len(m.trail)
        initial_TR = m.TR

        trail_if_needed(m, addr)

        # addr (0) is not < HB (0), so no trailing
        assert len(m.trail) == initial_trail_len
        assert m.TR == initial_TR

    def test_trail_if_needed_addr_equals_hb_does_not_trail(self):
        """Boundary case: addr == HB does not trail."""
        m = Machine()
        m.HB = 5
        addr = 5

        trail_if_needed(m, addr)

        assert len(m.trail) == 0
        assert m.TR == 0

    def test_trail_if_needed_addr_below_hb_trails(self):
        """addr < HB causes trailing."""
        m = Machine()
        m.HB = 10
        addr = 5

        trail_if_needed(m, addr)

        assert len(m.trail) == 1
        assert m.trail[0] == addr
        assert m.TR == 1

    def test_trail_if_needed_addr_above_hb_does_not_trail(self):
        """addr >= HB does not trail."""
        m = Machine()
        m.HB = 5
        addr = 10

        trail_if_needed(m, addr)

        assert len(m.trail) == 0
        assert m.TR == 0

    def test_trail_if_needed_multiple_calls(self):
        """Multiple calls accumulate in trail."""
        m = Machine()
        m.HB = 100

        trail_if_needed(m, 10)
        trail_if_needed(m, 20)
        trail_if_needed(m, 30)

        assert len(m.trail) == 3
        assert m.trail == [10, 20, 30]
        assert m.TR == 3


class TestBind:
    """Test bind operation."""

    def test_bind_var_to_constant(self):
        """Bind unbound variable to constant."""
        m = Machine()
        var = new_ref(m)
        const = new_con(m, 42)

        bind(m, var, const)

        # var now points to const
        assert m.heap[var] == (TAG_REF, const)
        assert deref(m, var) == const

    def test_bind_constant_to_var(self):
        """Bind with arguments reversed (var, const)."""
        m = Machine()
        const = new_con(m, "foo")
        var = new_ref(m)

        bind(m, const, var)

        # var should be bound to const
        assert m.heap[var] == (TAG_REF, const)
        assert deref(m, var) == const

    def test_bind_var_to_var(self):
        """Bind two unbound variables."""
        m = Machine()
        var1 = new_ref(m)
        var2 = new_ref(m)

        bind(m, var1, var2)

        # Newer var (var2) binds to older (var1), or vice versa
        # Both should deref to same root
        root1 = deref(m, var1)
        root2 = deref(m, var2)
        assert root1 == root2

    def test_bind_var_to_itself_is_noop(self):
        """Binding variable to itself does nothing."""
        m = Machine()
        var = new_ref(m)

        original_cell = m.heap[var]
        bind(m, var, var)

        # Should remain unchanged
        assert m.heap[var] == original_cell

    def test_bind_already_bound_vars_is_noop(self):
        """Binding two vars that deref to same value is no-op."""
        m = Machine()
        const = new_con(m, 99)
        var1 = new_ref(m)
        var2 = new_ref(m)

        # Bind both to same constant
        m.heap[var1] = make_ref(const)
        m.heap[var2] = make_ref(const)

        # Capture state
        heap_snapshot = m.heap[:]

        bind(m, var1, var2)

        # Heap should be unchanged
        assert m.heap == heap_snapshot

    def test_bind_with_dereferencing(self):
        """Bind derefs both arguments first."""
        m = Machine()
        const = new_con(m, "target")
        var1 = new_ref(m)
        var2 = new_ref(m)

        # var1 points to const
        m.heap[var1] = make_ref(const)

        # Bind var2 to var1 (should deref var1 to const)
        bind(m, var2, var1)

        # var2 should now point to const
        assert deref(m, var2) == const

    def test_bind_does_not_trail_in_phase_1(self):
        """With HB=0, bind does not add to trail."""
        m = Machine()
        m.HB = 0

        var = new_ref(m)
        const = new_con(m, "x")

        bind(m, var, const)

        # No trailing should occur
        assert len(m.trail) == 0
        assert m.TR == 0

    def test_bind_trails_old_variable(self):
        """Bind trails variable below HB."""
        m = Machine()

        # Create old variable
        old_var = new_ref(m)
        # Set HB beyond old_var
        m.HB = 10

        # Create new constant
        for _ in range(10):
            new_ref(m)
        const = new_con(m, "new")

        bind(m, old_var, const)

        # old_var (addr 0) < HB (10), should be trailed
        assert old_var in m.trail
        assert m.TR == 1

    def test_bind_does_not_trail_new_variable(self):
        """Bind does not trail variable above HB."""
        m = Machine()
        m.HB = 5

        # Allocate below HB
        for _ in range(5):
            new_ref(m)

        # New variable above HB
        new_var = new_ref(m)
        const = new_con(m, "val")

        bind(m, new_var, const)

        # new_var (addr 5) >= HB (5), should not trail
        assert len(m.trail) == 0
        assert m.TR == 0


class TestUntrail:
    """Test untrail operation."""

    def test_untrail_empty_trail(self):
        """Untrail with empty trail is no-op."""
        m = Machine()
        m.TR = 0

        untrail(m, 0)

        assert m.TR == 0

    def test_untrail_restores_single_binding(self):
        """Untrail restores one binding to unbound."""
        m = Machine()
        var = new_ref(m)
        const = new_con(m, 42)

        # Manually bind and trail
        m.heap[var] = make_ref(const)
        m.trail.append(var)
        m.TR = 1

        # Untrail
        untrail(m, 0)

        # var should be restored to unbound
        assert m.heap[var] == (TAG_REF, var)
        assert m.TR == 0

    def test_untrail_restores_multiple_bindings(self):
        """Untrail restores multiple bindings."""
        m = Machine()
        var1 = new_ref(m)
        var2 = new_ref(m)
        var3 = new_ref(m)
        const = new_con(m, "x")

        # Bind all to const and trail
        m.heap[var1] = make_ref(const)
        m.heap[var2] = make_ref(const)
        m.heap[var3] = make_ref(const)
        m.trail.extend([var1, var2, var3])
        m.TR = 3

        # Untrail all
        untrail(m, 0)

        # All should be unbound
        assert m.heap[var1] == (TAG_REF, var1)
        assert m.heap[var2] == (TAG_REF, var2)
        assert m.heap[var3] == (TAG_REF, var3)
        assert m.TR == 0

    def test_untrail_partial_restore(self):
        """Untrail to intermediate point."""
        m = Machine()
        var1 = new_ref(m)
        var2 = new_ref(m)
        var3 = new_ref(m)
        const = new_con(m, 99)

        # Bind and trail
        m.heap[var1] = make_ref(const)
        m.heap[var2] = make_ref(const)
        m.heap[var3] = make_ref(const)
        m.trail.extend([var1, var2, var3])
        m.TR = 3

        # Untrail to TR=1 (restore var2 and var3)
        untrail(m, 1)

        # var1 still bound, var2 and var3 unbound
        assert m.heap[var1] == (TAG_REF, const)
        assert m.heap[var2] == (TAG_REF, var2)
        assert m.heap[var3] == (TAG_REF, var3)
        assert m.TR == 1

    def test_trail_untrail_round_trip(self):
        """Trail and untrail restore exact state."""
        m = Machine()
        m.HB = 10

        var1 = new_ref(m)
        var2 = new_ref(m)
        const = new_con(m, "test")

        # Save initial state
        initial_heap = m.heap[:]
        initial_TR = m.TR

        # Bind with trailing
        bind(m, var1, const)
        bind(m, var2, const)

        # Verify bindings occurred
        assert deref(m, var1) == const
        assert deref(m, var2) == const

        # Untrail
        untrail(m, initial_TR)

        # State should be restored
        assert m.heap == initial_heap
        assert m.TR == initial_TR


class TestBindEdgeCases:
    """Test edge cases for bind operation."""

    def test_bind_prefers_newer_to_older(self):
        """When binding two unbound vars, newer binds to older."""
        m = Machine()
        old_var = new_ref(m)  # addr 0
        new_var = new_ref(m)  # addr 1

        bind(m, old_var, new_var)

        # new_var should bind to old_var
        assert m.heap[new_var] == (TAG_REF, old_var)
        assert deref(m, new_var) == old_var

    def test_bind_chain_compression(self):
        """Binding with deref compresses chains."""
        m = Machine()
        const = new_con(m, "end")
        var1 = new_ref(m)
        var2 = new_ref(m)
        var3 = new_ref(m)

        # Create chain: var2 -> var1 -> const
        m.heap[var1] = make_ref(const)
        m.heap[var2] = make_ref(var1)

        # Bind var3 to var2 (should deref to const)
        bind(m, var3, var2)

        # var3 should point to const (chain compressed)
        assert deref(m, var3) == const


class TestIntegration:
    """Integration tests combining operations."""

    def test_build_and_traverse_structure(self):
        """Build variable bindings and traverse."""
        m = Machine()

        # Create: X = Y, Y = Z, Z = 42
        x = new_ref(m)
        y = new_ref(m)
        z = new_ref(m)
        const = new_con(m, 42)

        bind(m, x, y)
        bind(m, y, z)
        bind(m, z, const)

        # All should deref to const
        assert deref(m, x) == const
        assert deref(m, y) == const
        assert deref(m, z) == const

    def test_unification_scenario(self):
        """Simulate basic unification with trailing and backtracking."""
        m = Machine()

        # Create old variables
        x = new_ref(m)  # 0
        y = new_ref(m)  # 1

        # Set HB to protect old variables
        m.HB = 2

        # Create new constant
        const = new_con(m, "value")  # 2

        initial_TR = m.TR
        assert initial_TR == 0

        # Bind old variables to constant
        bind(m, x, const)  # x < HB, should trail
        bind(m, y, const)  # y < HB, should trail

        # Check bindings
        assert deref(m, x) == const
        assert deref(m, y) == const

        # Check trailing - both x and y should be trailed
        assert len(m.trail) == 2
        assert x in m.trail
        assert y in m.trail
        assert m.TR == 2

        # Backtrack
        untrail(m, initial_TR)

        # Variables should be unbound again
        assert m.heap[x] == (TAG_REF, x)
        assert m.heap[y] == (TAG_REF, y)
        assert m.TR == 0
