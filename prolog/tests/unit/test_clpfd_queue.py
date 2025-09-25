"""Unit tests for CLP(FD) Propagation Queue.

Tests priority-based propagation queue with deduplication,
priority escalation, and self-requeue handling.
"""

import pytest
from prolog.clpfd.queue import PropagationQueue, Priority


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
            return ('ok', None)

        def prop2(store, trail, engine, cause):
            return ('ok', None)

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
            return ('ok', None)

        pid = queue.register(prop)
        queue.schedule(pid, Priority.HIGH)

        assert not queue.is_empty()
        assert pid in queue.queued
        assert queue.queued[pid] == Priority.HIGH

    def test_pop_returns_highest_priority(self):
        """Pop returns propagator with highest priority first."""
        queue = PropagationQueue()

        # Register propagators
        prop_high = lambda s, t, e, c: ('ok', None)
        prop_med = lambda s, t, e, c: ('ok', None)
        prop_low = lambda s, t, e, c: ('ok', None)

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

        prop = lambda s, t, e, c: ('ok', None)
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

        prop = lambda s, t, e, c: ('ok', None)
        pid = queue.register(prop)

        # Schedule with cause
        queue.schedule(pid, Priority.MED, cause=('domain_changed', 1))

        # Schedule again with different cause but same priority
        queue.schedule(pid, Priority.MED, cause=('domain_changed', 2))

        # Should still only be queued once
        item = queue.pop()
        assert item[0] == pid
        # First cause should be preserved
        assert item[1] == ('domain_changed', 1)

        assert queue.is_empty()


class TestPriorityEscalation:
    """Test priority escalation when re-scheduling."""

    def test_escalate_from_low_to_high(self):
        """Escalate propagator from LOW to HIGH priority."""
        queue = PropagationQueue()

        prop = lambda s, t, e, c: ('ok', None)
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

        prop = lambda s, t, e, c: ('ok', None)
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

        prop = lambda s, t, e, c: ('ok', None)
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

        prop = lambda s, t, e, c: ('ok', None)
        pid = queue.register(prop)

        # Set as running
        queue.running = pid

        # Try to schedule while running
        queue.schedule(pid, Priority.HIGH)

        # Should be added to reschedule set, not queued
        assert pid not in queue.queued
        assert (pid, Priority.HIGH) in queue.reschedule

    def test_reschedule_set_processed_after_run(self):
        """Reschedule set is processed after propagator finishes."""
        queue = PropagationQueue()

        prop = lambda s, t, e, c: ('ok', None)
        pid = queue.register(prop)

        # Simulate self-requeue during execution
        queue.running = pid
        queue.schedule(pid, Priority.MED)

        # Should be in reschedule set
        assert (pid, Priority.MED) in queue.reschedule

        # Clear running status (simulating end of run)
        queue.running = None

        # Process reschedule set
        for p, prio in queue.reschedule:
            queue.schedule(p, prio)
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
            return ('ok', None)

        pid = queue.register(prop)
        queue.schedule(pid, Priority.HIGH, cause='initial')

        # Run to fixpoint
        result = queue.run_to_fixpoint({}, [], None)

        assert result is True
        assert len(executions) == 1
        assert executions[0] == 'initial'

    def test_propagator_failure_stops_queue(self):
        """Propagator returning 'fail' stops queue execution."""
        queue = PropagationQueue()

        executed = []

        def prop1(store, trail, engine, cause):
            executed.append(1)
            return ('fail', None)

        def prop2(store, trail, engine, cause):
            executed.append(2)
            return ('ok', None)

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
            executions.append(('prop1', cause))
            # Returns changed variables
            return ('ok', [1, 2])

        def prop2(store, trail, engine, cause):
            executions.append(('prop2', cause))
            return ('ok', None)

        pid1 = queue.register(prop1)
        pid2 = queue.register(prop2)

        # Set up mock watchers
        def mock_iter_watchers(store, varid):
            if varid == 1:
                yield (pid2, Priority.MED)
            # varid 2 has no watchers

        # Monkey-patch iter_watchers for test
        import prolog.clpfd.api
        original_iter_watchers = getattr(prolog.clpfd.api, 'iter_watchers', None)
        prolog.clpfd.api.iter_watchers = mock_iter_watchers

        try:
            queue.schedule(pid1, Priority.HIGH, cause='initial')
            result = queue.run_to_fixpoint({}, [], None)

            assert result is True
            assert len(executions) == 2
            assert executions[0] == ('prop1', 'initial')
            assert executions[1] == ('prop2', ('domain_changed', 1))
        finally:
            # Restore original
            if original_iter_watchers:
                prolog.clpfd.api.iter_watchers = original_iter_watchers
            elif hasattr(prolog.clpfd.api, 'iter_watchers'):
                delattr(prolog.clpfd.api, 'iter_watchers')


class TestQueueCausality:
    """Test cause tracking through propagation."""

    def test_cause_passed_to_propagator(self):
        """Cause is correctly passed to propagator."""
        queue = PropagationQueue()

        received_cause = None

        def prop(store, trail, engine, cause):
            nonlocal received_cause
            received_cause = cause
            return ('ok', None)

        pid = queue.register(prop)
        test_cause = ('domain_changed', 42)
        queue.schedule(pid, Priority.HIGH, cause=test_cause)

        queue.run_to_fixpoint({}, [], None)

        assert received_cause == test_cause

    def test_wake_cause_includes_changed_var(self):
        """Wake cause includes the changed variable ID."""
        queue = PropagationQueue()

        received_causes = []

        def prop_trigger(store, trail, engine, cause):
            # This propagator changes variable 5
            return ('ok', [5])

        def prop_woken(store, trail, engine, cause):
            received_causes.append(cause)
            return ('ok', None)

        pid_trigger = queue.register(prop_trigger)
        pid_woken = queue.register(prop_woken)

        # Mock watchers - variable 5 watches pid_woken
        def mock_iter_watchers(store, varid):
            if varid == 5:
                yield (pid_woken, Priority.MED)

        import prolog.clpfd.api
        original = getattr(prolog.clpfd.api, 'iter_watchers', None)
        prolog.clpfd.api.iter_watchers = mock_iter_watchers

        try:
            queue.schedule(pid_trigger, Priority.HIGH)
            queue.run_to_fixpoint({}, [], None)

            assert len(received_causes) == 1
            assert received_causes[0] == ('domain_changed', 5)
        finally:
            if original:
                prolog.clpfd.api.iter_watchers = original
            elif hasattr(prolog.clpfd.api, 'iter_watchers'):
                delattr(prolog.clpfd.api, 'iter_watchers')


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
                return ('ok', None)
            else:
                # Second run: just finish
                return ('ok', None)

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
                return ('ok', None)
            return prop

        # Register propagators
        pid_high1 = queue.register(make_prop('high1'))
        pid_high2 = queue.register(make_prop('high2'))
        pid_med = queue.register(make_prop('med'))
        pid_low = queue.register(make_prop('low'))

        # Schedule in mixed order
        queue.schedule(pid_low, Priority.LOW)
        queue.schedule(pid_high1, Priority.HIGH)
        queue.schedule(pid_med, Priority.MED)
        queue.schedule(pid_high2, Priority.HIGH)

        result = queue.run_to_fixpoint({}, [], None)

        assert result is True
        # High priority should run first (order within priority not guaranteed)
        assert 'high1' in execution_order[:2]
        assert 'high2' in execution_order[:2]
        # Then medium
        assert execution_order[2] == 'med'
        # Then low
        assert execution_order[3] == 'low'