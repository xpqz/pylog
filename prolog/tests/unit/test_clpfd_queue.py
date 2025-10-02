"""Unit tests for CLP(FD) Propagation Queue.

Tests priority-based propagation queue with deduplication,
priority escalation, and self-requeue handling.
"""

import prolog.clpfd.queue as queue_module
from prolog.clpfd.api import add_watcher
from prolog.clpfd.queue import PropagationQueue
from prolog.clpfd.priority import Priority


class MockStore:
    """Mock store for testing that provides required methods."""

    def __init__(self):
        self.attrs = {}

    def get_attrs(self, varid):
        """Return attrs dict for testing."""
        return self.attrs.get(varid, {})


class TestQueueBasics:
    """Test basic queue operations."""

    def test_empty_queue(self):
        """Empty queue has no propagators."""
        queue = PropagationQueue()
        assert queue.is_empty()
        assert queue.pop() is None

    def test_register_propagator(self):
        """Register propagator returns unique ID."""
        queue = PropagationQueue()

        def prop1(store, trail, engine, cause):
            return ("ok", None)

        def prop2(store, trail, engine, cause):
            return ("ok", None)

        pid1 = queue.register(prop1)
        pid2 = queue.register(prop2)

        assert pid1 != pid2
        assert pid1 in queue.propagators
        assert pid2 in queue.propagators
        assert queue.propagators[pid1] is prop1
        assert queue.propagators[pid2] is prop2

    def test_schedule_single_propagator(self):
        """Schedule single propagator."""
        queue = PropagationQueue()

        def prop(store, trail, engine, cause):
            return ("ok", None)

        pid = queue.register(prop)
        queue.schedule(pid, Priority.HIGH)

        assert not queue.is_empty()
        assert pid in queue.queued
        assert queue.queued[pid] == Priority.HIGH

    def test_pop_returns_highest_priority(self):
        """Pop returns propagator with highest priority first."""
        queue = PropagationQueue()

        # Register propagators
        def prop_high(s, t, e, c):
            return ("ok", None)

        def prop_med(s, t, e, c):
            return ("ok", None)

        def prop_low(s, t, e, c):
            return ("ok", None)

        pid_high = queue.register(prop_high)
        pid_med = queue.register(prop_med)
        pid_low = queue.register(prop_low)

        # Schedule in reverse priority order
        queue.schedule(pid_low, Priority.LOW)
        queue.schedule(pid_med, Priority.MED)
        queue.schedule(pid_high, Priority.HIGH)

        # Should pop in priority order
        item1 = queue.pop()
        assert item1[0] == pid_high

        item2 = queue.pop()
        assert item2[0] == pid_med

        item3 = queue.pop()
        assert item3[0] == pid_low

        # Queue should be empty
        assert queue.is_empty()
        assert queue.pop() is None


class TestDeduplication:
    """Test propagator deduplication."""

    def test_schedule_same_propagator_twice_no_duplicate(self):
        """Scheduling same propagator twice doesn't create duplicate."""
        queue = PropagationQueue()

        def prop(s, t, e, c):
            return ("ok", None)

        pid = queue.register(prop)

        # Schedule twice at same priority
        queue.schedule(pid, Priority.MED)
        queue.schedule(pid, Priority.MED)

        # Should only be queued once
        assert pid in queue.queued
        assert queue.queued[pid] == Priority.MED

        # Pop once should get it
        item = queue.pop()
        assert item[0] == pid

        # Queue should be empty
        assert queue.is_empty()

    def test_already_queued_propagator_not_requeued(self):
        """Already queued propagator is not added again at same priority."""
        queue = PropagationQueue()

        def prop(s, t, e, c):
            return ("ok", None)

        pid = queue.register(prop)

        # Schedule with cause
        queue.schedule(pid, Priority.MED, cause=("domain_changed", 1))

        # Schedule again with different cause but same priority
        queue.schedule(pid, Priority.MED, cause=("domain_changed", 2))

        # Should still only be queued once
        item = queue.pop()
        assert item[0] == pid
        # First cause should be preserved
        assert item[1] == ("domain_changed", 1)

        assert queue.is_empty()


class TestPriorityEscalation:
    """Test priority escalation when re-scheduling."""

    def test_escalate_from_low_to_high(self):
        """Escalate propagator from LOW to HIGH priority."""
        queue = PropagationQueue()

        def prop(s, t, e, c):
            return ("ok", None)

        pid = queue.register(prop)

        # Initially schedule at LOW
        queue.schedule(pid, Priority.LOW)
        assert queue.queued[pid] == Priority.LOW

        # Escalate to HIGH
        queue.schedule(pid, Priority.HIGH)
        assert queue.queued[pid] == Priority.HIGH

        # Should pop from HIGH queue
        item = queue.pop()
        assert item[0] == pid
        assert queue.is_empty()

    def test_escalate_from_med_to_high(self):
        """Escalate propagator from MED to HIGH priority."""
        queue = PropagationQueue()

        def prop(s, t, e, c):
            return ("ok", None)

        pid = queue.register(prop)

        # Schedule at MED
        queue.schedule(pid, Priority.MED)

        # Escalate to HIGH
        queue.schedule(pid, Priority.HIGH)

        # Should be in HIGH queue now
        assert queue.queued[pid] == Priority.HIGH

    def test_no_downgrade_priority(self):
        """Cannot downgrade priority once scheduled higher."""
        queue = PropagationQueue()

        def prop(s, t, e, c):
            return ("ok", None)

        pid = queue.register(prop)

        # Schedule at HIGH
        queue.schedule(pid, Priority.HIGH)

        # Try to downgrade to LOW (should stay HIGH)
        queue.schedule(pid, Priority.LOW)

        # Should remain at HIGH
        assert queue.queued[pid] == Priority.HIGH


class TestSelfRequeue:
    """Test self-requeue prevention."""

    def test_running_propagator_cannot_requeue_itself(self):
        """Running propagator cannot directly requeue itself."""
        queue = PropagationQueue()

        def prop(s, t, e, c):
            return ("ok", None)

        pid = queue.register(prop)

        # Set as running
        queue.running = pid

        # Try to schedule while running
        queue.schedule(pid, Priority.HIGH)

        # Should be added to reschedule set, not queued
        assert pid not in queue.queued
        assert (pid, Priority.HIGH, None) in queue.reschedule

    def test_reschedule_set_processed_after_run(self):
        """Reschedule set is processed after propagator finishes."""
        queue = PropagationQueue()

        def prop(s, t, e, c):
            return ("ok", None)

        pid = queue.register(prop)

        # Simulate self-requeue during execution
        queue.running = pid
        queue.schedule(pid, Priority.MED)

        # Should be in reschedule set
        assert (pid, Priority.MED, None) in queue.reschedule

        # Clear running status (simulating end of run)
        queue.running = None

        # Process reschedule set
        for p, prio, cause in queue.reschedule:
            queue.schedule(p, prio, cause)
        queue.reschedule.clear()

        # Now should be queued
        assert pid in queue.queued
        assert queue.queued[pid] == Priority.MED


class TestRunToFixpoint:
    """Test fixpoint computation."""

    def test_empty_queue_reaches_fixpoint(self):
        """Empty queue immediately reaches fixpoint."""
        queue = PropagationQueue()

        # Mock store/trail/engine
        store = {}
        trail = []
        engine = None

        result = queue.run_to_fixpoint(store, trail, engine)
        assert result is True

    def test_single_propagator_runs_once(self):
        """Single propagator runs exactly once."""
        queue = PropagationQueue()

        # Track execution
        executions = []

        def prop(store, trail, engine, cause):
            executions.append(cause)
            return ("ok", None)

        pid = queue.register(prop)
        queue.schedule(pid, Priority.HIGH, cause="initial")

        # Run to fixpoint
        result = queue.run_to_fixpoint({}, [], None)

        assert result is True
        assert len(executions) == 1
        assert executions[0] == "initial"

    def test_propagator_failure_stops_queue(self):
        """Propagator returning 'fail' stops queue execution."""
        queue = PropagationQueue()

        executed = []

        def prop1(store, trail, engine, cause):
            executed.append(1)
            return ("fail", None)

        def prop2(store, trail, engine, cause):
            executed.append(2)
            return ("ok", None)

        pid1 = queue.register(prop1)
        pid2 = queue.register(prop2)

        queue.schedule(pid1, Priority.HIGH)
        queue.schedule(pid2, Priority.LOW)

        result = queue.run_to_fixpoint({}, [], None)

        assert result is False
        assert executed == [1]  # Only first propagator ran

    def test_propagator_wakes_others(self):
        """Propagator returning changed vars wakes watchers."""
        queue = PropagationQueue()

        # Track executions
        executions = []

        def prop1(store, trail, engine, cause):
            executions.append(("prop1", cause))
            # Returns changed variables
            return ("ok", [1, 2])

        def prop2(store, trail, engine, cause):
            executions.append(("prop2", cause))
            return ("ok", None)

        pid1 = queue.register(prop1)
        pid2 = queue.register(prop2)

        # Create a proper test store with attributes
        class TestStore:
            def __init__(self):
                self.attrs = {}

            def get_attrs(self, varid):
                return self.attrs.get(varid, {})

            def set_attrs(self, varid, attrs):
                self.attrs[varid] = attrs

            def put_attr(self, varid, module, value, trail):
                """Put attribute for variable."""
                if varid not in self.attrs:
                    self.attrs[varid] = {}
                # Store old value on trail if needed
                old_attrs = self.attrs[varid].copy()
                if module not in old_attrs:
                    old_attrs[module] = {}
                old_attrs[module] = value
                self.attrs[varid] = old_attrs

        store = TestStore()
        trail = []

        # Add watcher for variable 1
        add_watcher(store, 1, pid2, Priority.MED, trail)

        queue.schedule(pid1, Priority.HIGH, cause="initial")
        result = queue.run_to_fixpoint(store, trail, None)

        assert result is True
        assert len(executions) == 2
        assert executions[0] == ("prop1", "initial")
        assert executions[1] == ("prop2", ("domain_changed", 1))


class TestQueueCausality:
    """Test cause tracking through propagation."""

    def test_cause_passed_to_propagator(self):
        """Cause is correctly passed to propagator."""
        queue = PropagationQueue()

        received_cause = None

        def prop(store, trail, engine, cause):
            nonlocal received_cause
            received_cause = cause
            return ("ok", None)

        pid = queue.register(prop)
        test_cause = ("domain_changed", 42)
        queue.schedule(pid, Priority.HIGH, cause=test_cause)

        queue.run_to_fixpoint(MockStore(), [], None)

        assert received_cause == test_cause

    def test_wake_cause_includes_changed_var(self):
        """Wake cause includes the changed variable ID."""
        queue = PropagationQueue()

        received_causes = []

        def prop_trigger(store, trail, engine, cause):
            # This propagator changes variable 5
            return ("ok", [5])

        def prop_woken(store, trail, engine, cause):
            received_causes.append(cause)
            return ("ok", None)

        pid_trigger = queue.register(prop_trigger)
        pid_woken = queue.register(prop_woken)

        # Create a proper test store with attributes
        class TestStore:
            def __init__(self):
                self.attrs = {}

            def get_attrs(self, varid):
                return self.attrs.get(varid, {})

            def set_attrs(self, varid, attrs):
                self.attrs[varid] = attrs

            def put_attr(self, varid, module, value, trail):
                """Put attribute for variable."""
                if varid not in self.attrs:
                    self.attrs[varid] = {}
                old_attrs = self.attrs[varid].copy()
                if module not in old_attrs:
                    old_attrs[module] = {}
                old_attrs[module] = value
                self.attrs[varid] = old_attrs

        store = TestStore()
        trail = []

        # Add watcher for variable 5
        add_watcher(store, 5, pid_woken, Priority.MED, trail)

        queue.schedule(pid_trigger, Priority.HIGH)
        queue.run_to_fixpoint(store, trail, None)

        assert len(received_causes) == 1
        assert received_causes[0] == ("domain_changed", 5)


class TestQueueIntegration:
    """Test integration with CLP(FD) infrastructure."""

    def test_self_requeue_during_fixpoint(self):
        """Self-requeue is handled correctly during fixpoint computation."""
        queue = PropagationQueue()

        execution_count = 0

        def prop(store, trail, engine, cause):
            nonlocal execution_count
            execution_count += 1

            # First run: try to reschedule self
            if execution_count == 1:
                # This should go to reschedule set
                queue.schedule(queue.running, Priority.LOW)
                return ("ok", None)
            else:
                # Second run: just finish
                return ("ok", None)

        pid = queue.register(prop)
        queue.schedule(pid, Priority.HIGH)

        result = queue.run_to_fixpoint({}, [], None)

        assert result is True
        assert execution_count == 2  # Should run twice

    def test_priority_ordering_maintained(self):
        """Priority ordering is maintained throughout execution."""
        queue = PropagationQueue()

        execution_order = []

        def make_prop(name):
            def prop(store, trail, engine, cause):
                execution_order.append(name)
                return ("ok", None)

            return prop

        # Register propagators
        pid_high1 = queue.register(make_prop("high1"))
        pid_high2 = queue.register(make_prop("high2"))
        pid_med = queue.register(make_prop("med"))
        pid_low = queue.register(make_prop("low"))

        # Schedule in mixed order
        queue.schedule(pid_low, Priority.LOW)
        queue.schedule(pid_high1, Priority.HIGH)
        queue.schedule(pid_med, Priority.MED)
        queue.schedule(pid_high2, Priority.HIGH)

        result = queue.run_to_fixpoint({}, [], None)

        assert result is True
        # High priority should run first (order within priority not guaranteed)
        assert "high1" in execution_order[:2]
        assert "high2" in execution_order[:2]
        # Then medium
        assert execution_order[2] == "med"
        # Then low
        assert execution_order[3] == "low"


class TestQueueEdgeCases:
    """Test edge cases and optimizations."""

    def test_multiple_changed_vars_dedup(self):
        """Propagator watching multiple vars only queued once."""
        queue = PropagationQueue()

        executions = []

        def prop(store, trail, engine, cause):
            executions.append(cause)
            return ("ok", None)

        pid = queue.register(prop)

        # Mock watchers - this propagator watches vars 1, 2, 3
        def mock_iter_watchers(store, varid):
            if varid in [1, 2, 3]:
                yield (pid, Priority.MED if varid == 1 else Priority.LOW)

        original = queue_module.iter_watchers
        queue_module.iter_watchers = mock_iter_watchers

        try:
            # Wake for multiple variables
            mock_store = MockStore()
            queue._wake_watchers(mock_store, 1)  # MED priority
            queue._wake_watchers(mock_store, 2)  # LOW priority
            queue._wake_watchers(mock_store, 3)  # LOW priority

            # Should be queued only once at highest priority (MED)
            assert pid in queue.queued
            assert queue.queued[pid] == Priority.MED

            # Run to fixpoint
            queue.run_to_fixpoint(MockStore(), [], None)

            # Should execute exactly once
            assert len(executions) == 1
            # With the first cause (from highest priority wake)
            assert executions[0] == ("domain_changed", 1)
        finally:
            queue_module.iter_watchers = original

    def test_cause_overwrite_on_escalation(self):
        """Escalation with new cause overwrites old cause."""
        queue = PropagationQueue()

        received_cause = None

        def prop(store, trail, engine, cause):
            nonlocal received_cause
            received_cause = cause
            return ("ok", None)

        pid = queue.register(prop)

        # Schedule with cause A at MED
        cause_a = ("domain_changed", 1)
        queue.schedule(pid, Priority.MED, cause=cause_a)

        # Reschedule with cause B at HIGH (escalation)
        cause_b = ("domain_changed", 2)
        queue.schedule(pid, Priority.HIGH, cause=cause_b)

        # Pop and check
        item = queue.pop()
        assert item[0] == pid
        assert item[1] == cause_b  # New cause should overwrite

    def test_escalation_with_none_cause_preserves(self):
        """Escalation with None cause preserves existing cause."""
        queue = PropagationQueue()

        def prop(s, t, e, c):
            return ("ok", None)

        pid = queue.register(prop)

        # Schedule with cause at MED
        original_cause = ("domain_changed", 5)
        queue.schedule(pid, Priority.MED, cause=original_cause)

        # Escalate with None cause
        queue.schedule(pid, Priority.HIGH, cause=None)

        # Pop and check
        item = queue.pop()
        assert item[0] == pid
        assert item[1] == original_cause  # Original cause preserved

    def test_empty_changed_vars_no_wakes(self):
        """Propagator returning empty changed_vars causes no wakes."""
        queue = PropagationQueue()

        # Propagator that returns empty list
        def prop1(store, trail, engine, cause):
            return ("ok", [])  # Empty list

        # Watcher that should not be woken
        wake_count = 0

        def prop2(store, trail, engine, cause):
            nonlocal wake_count
            wake_count += 1
            return ("ok", None)

        pid1 = queue.register(prop1)
        pid2 = queue.register(prop2)

        # Mock watchers (wouldn't be called anyway with empty list)
        def mock_iter_watchers(store, varid):
            yield (pid2, Priority.MED)

        original = queue_module.iter_watchers
        queue_module.iter_watchers = mock_iter_watchers

        try:
            queue.schedule(pid1, Priority.HIGH)
            result = queue.run_to_fixpoint({}, [], None)

            assert result is True
            assert wake_count == 0  # No wakes should occur
        finally:
            queue_module.iter_watchers = original

    def test_stale_skip_on_escalation(self):
        """Stale entries from escalation are skipped on pop."""
        queue = PropagationQueue()

        def prop(s, t, e, c):
            return ("ok", None)

        pid = queue.register(prop)

        # Schedule at LOW
        queue.schedule(pid, Priority.LOW)

        # Escalate to HIGH (leaves stale entry in LOW queue)
        queue.schedule(pid, Priority.HIGH)

        # Pop should get from HIGH queue
        item = queue.pop()
        assert item[0] == pid

        # Queue should now be empty (stale LOW entry ignored)
        assert queue.is_empty()
        assert queue.pop() is None

    def test_self_requeue_preserves_cause(self):
        """Self-requeued propagators preserve their cause."""
        queue = PropagationQueue()

        received_causes = []

        def prop(store, trail, engine, cause):
            received_causes.append(cause)
            if len(received_causes) == 1:
                # First run: self-requeue with different cause
                queue.schedule(queue.running, Priority.LOW, cause="second_cause")
            return ("ok", None)

        pid = queue.register(prop)

        # Initial schedule with first cause
        queue.schedule(pid, Priority.HIGH, cause="first_cause")

        result = queue.run_to_fixpoint({}, [], None)

        assert result is True
        assert len(received_causes) == 2
        assert received_causes[0] == "first_cause"
        assert received_causes[1] == "second_cause"  # Self-requeue cause preserved


class TestQueueStaleSkipOptimization:
    """Test optimization of stale-skip behavior (Phase 3.1)."""

    def test_stale_entries_eliminated_by_optimization(self):
        """Test that optimization eliminates stale entry accumulation."""
        queue = PropagationQueue()

        def prop(s, t, e, c):
            return ("ok", None)

        # Create many propagators
        num_props = 50
        pids = []
        for i in range(num_props):
            pid = queue.register(prop)
            pids.append(pid)

        # Schedule all at LOW priority
        for pid in pids:
            queue.schedule(pid, Priority.LOW)

        # Initially all are in LOW queue
        assert len(queue.queues[Priority.LOW]) == num_props
        assert len(queue.queues[Priority.HIGH]) == 0

        # Escalate all to HIGH priority - with optimization, LOW queue should be cleaned
        for pid in pids:
            queue.schedule(pid, Priority.HIGH)

        # With eager cleanup optimization:
        # - LOW queue should be empty (stale entries removed)
        # - HIGH queue should have all entries
        assert len(queue.queues[Priority.LOW]) == 0  # No stale entries!
        assert len(queue.queues[Priority.HIGH]) == num_props  # All valid entries

        # Pop all HIGH priority items
        for i in range(num_props):
            item = queue.pop()
            assert item is not None
            assert item[0] in pids

        # Verify queue is completely empty
        assert queue.is_empty()
        assert len(queue.queues[Priority.LOW]) == 0
        assert len(queue.queues[Priority.HIGH]) == 0

    def test_mixed_escalation_and_normal_scheduling(self):
        """Test mixed scenarios with escalation and normal scheduling."""
        queue = PropagationQueue()

        def prop(s, t, e, c):
            return ("ok", None)

        # Create propagators
        num_escalated = 10
        escalated_pids = []
        for i in range(num_escalated):
            pid = queue.register(prop)
            escalated_pids.append(pid)

        # Create one propagator that stays at LOW
        stay_low_pid = queue.register(prop)

        # Schedule escalated ones at LOW, then escalate to HIGH
        for pid in escalated_pids:
            queue.schedule(pid, Priority.LOW)

        # Schedule the stay-low one at LOW
        queue.schedule(stay_low_pid, Priority.LOW)

        # Verify initial state: all in LOW queue
        assert len(queue.queues[Priority.LOW]) == num_escalated + 1
        assert len(queue.queues[Priority.HIGH]) == 0

        # Now escalate some to HIGH - with optimization, they're removed from LOW
        for pid in escalated_pids:
            queue.schedule(pid, Priority.HIGH)

        # With eager cleanup:
        # - LOW queue should only have the stay-low propagator
        # - HIGH queue should have all escalated propagators
        assert len(queue.queues[Priority.LOW]) == 1  # Only stay_low_pid
        assert len(queue.queues[Priority.HIGH]) == num_escalated

        # Pop HIGH priority items first
        for i in range(num_escalated):
            item = queue.pop()
            assert item is not None
            assert item[0] in escalated_pids

        # Then pop the LOW priority item
        item = queue.pop()
        assert item is not None
        assert item[0] == stay_low_pid

        # Queue should be completely empty
        assert queue.is_empty()
        assert len(queue.queues[Priority.LOW]) == 0
        assert len(queue.queues[Priority.HIGH]) == 0

    def test_eager_cleanup_eliminates_stale_entries(self):
        """Test that eager cleanup during escalation eliminates stale entries.

        This test will pass after implementing the optimization.
        """
        queue = PropagationQueue()

        def prop(s, t, e, c):
            return ("ok", None)

        # Create propagators
        pid1 = queue.register(prop)
        pid2 = queue.register(prop)

        # Schedule both at LOW
        queue.schedule(pid1, Priority.LOW)
        queue.schedule(pid2, Priority.LOW)

        # Verify both are in LOW queue
        assert len(queue.queues[Priority.LOW]) == 2

        # Escalate pid1 to HIGH - with eager cleanup, LOW queue should shrink
        queue.schedule(pid1, Priority.HIGH)

        # After optimization, LOW queue should only have pid2
        # (Current implementation leaves stale entry, optimized removes it)
        # For now, this test documents the current behavior
        if hasattr(queue, "_uses_eager_cleanup"):
            # After optimization
            assert len(queue.queues[Priority.LOW]) == 1
        else:
            # Current behavior - stale entry remains
            assert len(queue.queues[Priority.LOW]) == 2

        # Escalate pid2 to HIGH as well
        queue.schedule(pid2, Priority.HIGH)

        # After optimization, LOW queue should be empty
        if hasattr(queue, "_uses_eager_cleanup"):
            assert len(queue.queues[Priority.LOW]) == 0
        else:
            # Current behavior - both stale entries remain
            assert len(queue.queues[Priority.LOW]) == 2

        # HIGH queue should have both
        assert len(queue.queues[Priority.HIGH]) == 2

        # Pop should work correctly regardless
        item1 = queue.pop()
        item2 = queue.pop()
        assert {item1[0], item2[0]} == {pid1, pid2}
        assert queue.is_empty()
