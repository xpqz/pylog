"""
Tests for trace filtering and spypoints.
"""

import pytest
from prolog.debug.filters import TraceFilters, should_emit_event
from prolog.debug.tracer import TraceEvent, PortsTracer
from prolog.ast.terms import Atom, Int, Var, Struct, List


# Shared test event factory
def make_event(port="call", pred_id="test/0", frame_depth=0, step_id=1, **kwargs):
    """Create a test TraceEvent."""
    defaults = {
        "version": 1,
        "run_id": "test",
        "step_id": step_id,
        "port": port,
        "goal": Atom("test"),
        "goal_pretty": "test",
        "goal_canonical": "test",
        "frame_depth": frame_depth,
        "cp_depth": 0,
        "goal_height": 1,
        "write_stamp": 1,
        "pred_id": pred_id,
    }
    defaults.update(kwargs)
    return TraceEvent(**defaults)


class TestTraceFilters:
    """Tests for TraceFilters configuration."""

    def test_filters_default_configuration(self):
        """Default filters allow all events."""
        filters = TraceFilters()
        assert filters.ports is None  # All ports
        assert filters.predicates is None  # All predicates
        assert filters.spypoints == set()  # No spypoints
        assert filters.min_depth == 0
        assert filters.max_depth is None
        assert filters.sampling_rate == 1  # Every event
        assert filters.sampling_seed is None  # Non-deterministic
        assert filters.bindings_policy == "none"
        # If these are part of the public contract:
        # assert filters.max_term_depth == 10  # or whatever default
        # assert filters.max_items_per_list == 10  # or whatever default

    def test_filters_port_configuration(self):
        """Can configure port filters."""
        filters = TraceFilters(ports={"call", "exit"})
        assert filters.ports == {"call", "exit"}

    def test_filters_predicate_configuration(self):
        """Can configure predicate filters."""
        filters = TraceFilters(predicates={"member/2", "append/3"})
        assert filters.predicates == {"member/2", "append/3"}

    def test_filters_depth_configuration(self):
        """Can configure depth filters."""
        filters = TraceFilters(min_depth=2, max_depth=10)
        assert filters.min_depth == 2
        assert filters.max_depth == 10

    def test_filters_sampling_configuration(self):
        """Can configure sampling."""
        filters = TraceFilters(sampling_rate=10, sampling_seed=42)
        assert filters.sampling_rate == 10
        assert filters.sampling_seed == 42

    def test_filters_bindings_configuration(self):
        """Can configure bindings policy."""
        filters = TraceFilters(bindings_policy="names_values")
        assert filters.bindings_policy == "names_values"

    def test_spypoint_management(self):
        """Can add and remove spypoints."""
        filters = TraceFilters()

        # Add spypoints
        filters.add_spypoint("member/2")
        filters.add_spypoint("append/3")
        assert filters.spypoints == {"member/2", "append/3"}

        # Remove spypoint
        filters.remove_spypoint("member/2")
        assert filters.spypoints == {"append/3"}

        # Clear all spypoints
        filters.clear_spypoints()
        assert filters.spypoints == set()

    def test_filters_are_mutable(self):
        """Filter settings can be modified after creation."""
        filters = TraceFilters()
        filters.ports = {"call"}
        filters.min_depth = 5
        assert filters.ports == {"call"}
        assert filters.min_depth == 5

    @pytest.mark.parametrize("port", ["call", "exit", "redo", "fail"])
    def test_default_filters_allow_all_ports(self, port):
        """Default filters allow all four ports."""
        assert should_emit_event(make_event(port=port), TraceFilters())


class TestInvalidConfiguration:
    """Tests for invalid configuration rejection."""

    def test_invalid_sampling_rate_rejected(self):
        """Invalid sampling rates are rejected."""
        with pytest.raises(ValueError):
            TraceFilters(sampling_rate=0)
        with pytest.raises(ValueError):
            TraceFilters(sampling_rate=-3)

    def test_invalid_depths_rejected(self):
        """Invalid depth values are rejected."""
        with pytest.raises(ValueError):
            TraceFilters(min_depth=-1)
        with pytest.raises(ValueError):
            TraceFilters(max_depth=-2)
        with pytest.raises(ValueError):
            TraceFilters(min_depth=5, max_depth=3)  # min > max

    def test_invalid_ports_rejected(self):
        """Invalid port names are rejected."""
        with pytest.raises(ValueError):
            TraceFilters(ports={"CALL", "exit"})  # case-sensitive / unknown token
        with pytest.raises(ValueError):
            TraceFilters(ports={"invalid_port"})

    def test_invalid_bindings_policy_rejected(self):
        """Invalid bindings policy is rejected."""
        with pytest.raises(ValueError):
            TraceFilters(bindings_policy="invalid")


class TestFilterPrecedence:
    """Tests for filter precedence rules."""

    def test_port_filter_applies_first(self):
        """Port filter is checked before other filters."""
        filters = TraceFilters(
            ports={"exit"},  # Only exit port
            predicates={"test/0"},  # This predicate is allowed
            min_depth=0,  # Depth is fine
        )

        # Event matches predicate and depth, but wrong port
        event = make_event(port="call", pred_id="test/0", frame_depth=0)
        assert not should_emit_event(event, filters)

        # Event with correct port passes
        event = make_event(port="exit", pred_id="test/0", frame_depth=0)
        assert should_emit_event(event, filters)

    def test_predicate_filter_applies_second(self):
        """Predicate filter is checked after port but before depth."""
        filters = TraceFilters(
            ports={"call"},  # Port matches
            predicates={"member/2"},  # Only member/2
            min_depth=0,  # Depth is fine
        )

        # Event has right port and depth, wrong predicate
        event = make_event(port="call", pred_id="append/3", frame_depth=0)
        assert not should_emit_event(event, filters)

        # Event with correct predicate passes
        event = make_event(port="call", pred_id="member/2", frame_depth=0)
        assert should_emit_event(event, filters)

    def test_depth_filter_applies_third(self):
        """Depth filter is checked after port and predicate."""
        filters = TraceFilters(
            ports={"call"},  # Port matches
            predicates={"test/0"},  # Predicate matches
            min_depth=5,  # Requires depth >= 5
        )

        # Event has right port and predicate, wrong depth
        event = make_event(port="call", pred_id="test/0", frame_depth=2)
        assert not should_emit_event(event, filters)

        # Event with sufficient depth passes
        event = make_event(port="call", pred_id="test/0", frame_depth=5)
        assert should_emit_event(event, filters)

    def test_sampling_applies_last(self):
        """Sampling is applied after all other filters."""
        filters = TraceFilters(
            sampling_rate=2, sampling_seed=42  # Every 2nd event  # Deterministic
        )

        # Generate events and check sampling pattern
        events = [make_event(step_id=i) for i in range(10)]
        results = [should_emit_event(e, filters) for e in events]

        # Should be deterministic pattern (allow some variance)
        assert 4 <= sum(results) <= 6  # Approximately half should pass (rate=2)

        # Same seed should give same pattern
        filters2 = TraceFilters(sampling_rate=2, sampling_seed=42)
        results2 = [should_emit_event(e, filters2) for e in events]
        assert results == results2


class TestSpypointBehaviour:
    """Tests for spypoint functionality."""

    def test_spypoint_overrides_predicate_filter(self):
        """Spypoints override predicate filters."""
        filters = TraceFilters(
            predicates={"member/2"},  # Only member/2 normally
            spypoints={"append/3"},  # But append/3 is a spypoint
        )

        # member/2 passes (in predicates)
        event = make_event(pred_id="member/2")
        assert should_emit_event(event, filters)

        # append/3 passes (is spypoint)
        event = make_event(pred_id="append/3")
        assert should_emit_event(event, filters)

        # other/1 fails (not in predicates or spypoints)
        event = make_event(pred_id="other/1")
        assert not should_emit_event(event, filters)

    def test_spypoints_work_without_predicate_filter(self):
        """Spypoints work when no predicate filter is set."""
        filters = TraceFilters(spypoints={"debug/1"})

        # Spypoint passes
        event = make_event(pred_id="debug/1")
        assert should_emit_event(event, filters)

        # When no predicate filter, only spypoints are traced
        event = make_event(pred_id="other/0")
        assert not should_emit_event(event, filters)

    def test_empty_spypoints_with_no_predicate_filter(self):
        """Empty spypoints with no predicate filter allows all."""
        filters = TraceFilters()  # No predicates, no spypoints

        # All predicates should pass
        event = make_event(pred_id="anything/99")
        assert should_emit_event(event, filters)

    def test_empty_predicates_blocks_all_unless_spypoint(self):
        """Empty predicates set blocks all except spypoints."""
        filters = TraceFilters(predicates=set(), spypoints=set())
        assert not should_emit_event(make_event(pred_id="any/1"), filters)

        filters.spypoints.add("only/1")
        assert should_emit_event(make_event(pred_id="only/1"), filters)
        assert not should_emit_event(make_event(pred_id="other/1"), filters)

    def test_spypoint_does_not_override_port_or_depth(self):
        """Spypoints only affect predicate filtering, not port or depth."""
        filters = TraceFilters(
            ports={"exit"},  # only EXIT allowed
            spypoints={"debug/1"},  # spy this predicate
            min_depth=2,  # depth >= 2
            sampling_rate=1,
        )

        # Spy predicate with wrong port -> blocked by port filter
        e = make_event(port="call", pred_id="debug/1", frame_depth=3)
        assert not should_emit_event(e, filters)

        # Spy predicate with insufficient depth -> blocked by depth
        e = make_event(port="exit", pred_id="debug/1", frame_depth=1)
        assert not should_emit_event(e, filters)

        # Spy predicate with correct port & depth -> allowed
        e = make_event(port="exit", pred_id="debug/1", frame_depth=2)
        assert should_emit_event(e, filters)

    def test_spypoint_still_subject_to_sampling(self):
        """Spypoints are still subject to sampling filter."""
        filters = TraceFilters(spypoints={"trace/1"}, sampling_rate=2, sampling_seed=7)
        events = [make_event(step_id=i, pred_id="trace/1") for i in range(10)]
        results = [should_emit_event(e, filters) for e in events]
        # Not all pass because sampling still applies
        assert 0 < sum(results) < len(results)

    @pytest.mark.parametrize(
        "preds,spies,allowed,blocked",
        [
            (None, set(), ["a/1", "b/2", "c/3"], []),  # allow all
            (None, {"a/1"}, ["a/1"], ["b/2", "c/3"]),  # whitelist by spies
            ({"a/1"}, set(), ["a/1"], ["b/2"]),  # whitelist by predicates
            ({"a/1"}, {"b/2"}, ["a/1", "b/2"], ["c/3"]),  # union of preds and spies
        ],
    )
    def test_predicate_and_spypoint_semantics(self, preds, spies, allowed, blocked):
        """Test predicate and spypoint filter combinations."""
        filters = TraceFilters(predicates=preds, spypoints=spies)
        for pid in allowed:
            assert should_emit_event(make_event(pred_id=pid), filters)
        for pid in blocked:
            assert not should_emit_event(make_event(pred_id=pid), filters)


class TestDepthFiltering:
    """Tests for depth-based filtering."""

    def test_min_depth_filter(self):
        """Minimum depth filter works."""
        filters = TraceFilters(min_depth=3)

        assert not should_emit_event(make_event(frame_depth=0), filters)
        assert not should_emit_event(make_event(frame_depth=2), filters)
        assert should_emit_event(make_event(frame_depth=3), filters)
        assert should_emit_event(make_event(frame_depth=10), filters)

    def test_max_depth_filter(self):
        """Maximum depth filter works."""
        filters = TraceFilters(max_depth=5)

        assert should_emit_event(make_event(frame_depth=0), filters)
        assert should_emit_event(make_event(frame_depth=5), filters)
        assert not should_emit_event(make_event(frame_depth=6), filters)
        assert not should_emit_event(make_event(frame_depth=100), filters)

    def test_depth_range_filter(self):
        """Depth range filtering works."""
        filters = TraceFilters(min_depth=2, max_depth=5)

        assert not should_emit_event(make_event(frame_depth=1), filters)
        assert should_emit_event(make_event(frame_depth=2), filters)
        assert should_emit_event(make_event(frame_depth=4), filters)
        assert should_emit_event(make_event(frame_depth=5), filters)
        assert not should_emit_event(make_event(frame_depth=6), filters)


class TestSamplingBehaviour:
    """Tests for deterministic sampling."""

    def test_sampling_rate_1_allows_all(self):
        """Sampling rate 1 allows all events."""
        filters = TraceFilters(sampling_rate=1)

        for i in range(10):
            event = make_event(step_id=i)
            assert should_emit_event(event, filters)

    def test_sampling_deterministic_with_seed(self):
        """Sampling is deterministic with a seed."""
        filters1 = TraceFilters(sampling_rate=3, sampling_seed=12345)
        filters2 = TraceFilters(sampling_rate=3, sampling_seed=12345)

        events = [make_event(step_id=i) for i in range(20)]

        results1 = [should_emit_event(e, filters1) for e in events]
        results2 = [should_emit_event(e, filters2) for e in events]

        assert results1 == results2  # Same seed gives same results

    def test_sampling_different_seeds_different_patterns(self):
        """Different seeds give different sampling patterns."""
        filters1 = TraceFilters(sampling_rate=3, sampling_seed=100)
        filters2 = TraceFilters(sampling_rate=3, sampling_seed=200)

        events = [make_event(step_id=i) for i in range(20)]

        results1 = [should_emit_event(e, filters1) for e in events]
        results2 = [should_emit_event(e, filters2) for e in events]

        assert results1 != results2  # Different seeds give different results

    def test_sampling_approximately_correct_rate(self):
        """Sampling achieves approximately correct rate."""
        filters = TraceFilters(sampling_rate=4, sampling_seed=999)

        # Generate many events
        events = [make_event(step_id=i) for i in range(1000)]
        passed = sum(should_emit_event(e, filters) for e in events)

        # Should be approximately 1/4 of events
        expected = len(events) // 4
        assert abs(passed - expected) < 50  # Allow some variance


class TestVariableBindings:
    """Tests for variable bindings in filtered events."""

    def test_bindings_none_policy(self):
        """'none' policy omits bindings field entirely."""
        filters = TraceFilters(bindings_policy="none")
        event = make_event()

        # Process event (would be done by tracer)
        processed = filters.process_bindings(event, {})
        assert "bindings" not in processed  # Field should be absent, not None

    def test_bindings_names_policy(self):
        """'names' policy includes variable names only."""
        filters = TraceFilters(bindings_policy="names")
        event = make_event()

        # Simulated bindings
        bindings = {"X": Atom("foo"), "Y": Int(42), "_G1": Struct("f", (Atom("a"),))}

        processed = filters.process_bindings(event, bindings)
        assert "bindings" in processed
        # Should have names but not values
        assert "X" in processed["bindings"]
        assert "Y" in processed["bindings"]
        assert "_G1" in processed["bindings"]
        assert processed["bindings"]["X"] is None  # No value
        assert processed["bindings"]["Y"] is None  # No value
        assert processed["bindings"]["_G1"] is None  # No value

    def test_bindings_names_values_policy(self):
        """'names_values' policy includes names and values."""
        filters = TraceFilters(bindings_policy="names_values")
        event = make_event()

        bindings = {
            "X": Atom("foo"),
            "Y": Int(42),
            "Z": Var(99, "Z"),  # Unbound variable
        }

        processed = filters.process_bindings(event, bindings)
        assert "bindings" in processed
        assert processed["bindings"]["X"] == "foo"
        assert processed["bindings"]["Y"] == 42
        # Unbound var should have a sensible representation
        assert "Z" in processed["bindings"]  # Present
        # Value could be None, '_', or var representation

    def test_bindings_respect_max_depth(self):
        """Bindings respect max_term_depth."""
        filters = TraceFilters(bindings_policy="names_values", max_term_depth=2)
        event = make_event()

        # Deep nested structure
        deep = Struct("f", (Struct("g", (Struct("h", (Atom("deep"),)),)),))
        bindings = {"X": deep}

        processed = filters.process_bindings(event, bindings)
        # Should be truncated at depth 2
        s = str(processed["bindings"]["X"])
        assert "..." in s
        # Verify depth bound is obeyed
        assert s.count("(") <= 2  # no deeper than depth 2

    def test_bindings_truncation_cuts_before_deeper_functor(self):
        """Truncation happens before exceeding depth limit."""
        filters = TraceFilters(bindings_policy="names_values", max_term_depth=2)
        deep = Struct("f", (Struct("g", (Struct("h", (Atom("x"),)),)),))
        s = str(filters.process_bindings(make_event(), {"X": deep})["bindings"]["X"])
        assert "..." in s
        assert "h(" not in s  # deeper functor must be hidden

    def test_bindings_respect_max_items(self):
        """Bindings respect max_items_per_list."""
        filters = TraceFilters(bindings_policy="names_values", max_items_per_list=3)
        event = make_event()

        # Long list
        long_list = List(tuple(Int(i) for i in range(10)))
        bindings = {"L": long_list}

        processed = filters.process_bindings(event, bindings)
        # Should be truncated to show first 3 items
        assert "..." in str(processed["bindings"]["L"])

    def test_bindings_respect_max_items_exactly(self):
        """Max items cap is strictly enforced."""
        filters = TraceFilters(bindings_policy="names_values", max_items_per_list=3)
        event = make_event()
        long_list = List(tuple(Int(i) for i in range(10)))
        processed = filters.process_bindings(event, {"L": long_list})
        s = str(processed["bindings"]["L"])
        # Expect exactly first 3 items visible
        # Note: Adjust for actual list formatting
        assert "[0, 1, 2" in s or "[0,1,2" in s  # first 3 items
        # Item 3 (0-indexed) should not appear before ellipsis
        # Handle both ASCII and Unicode ellipsis
        ellipsis = "..." if "..." in s else "…" if "…" in s else None
        if ellipsis:
            before_ellipsis = s.split(ellipsis, 1)[0]
        else:
            before_ellipsis = s
        assert "3" not in before_ellipsis


class TestStepIdPolicy:
    """Tests for step_id increment policy."""

    def test_step_id_increments_only_for_emitted_events_integration(self):
        """Integration test: step_id increments only for events that pass filters."""

        # Use a collecting sink to capture emitted events
        class _CollectingSink:
            def __init__(self):
                self.events = []

            def write_event(self, e):
                self.events.append(e)
                return True

            def flush(self):
                return True

            def close(self):
                pass

        # Filters block CALL, but allow EXIT
        filters = TraceFilters(ports={"exit"})
        tracer = PortsTracer(engine=None, filters=filters)
        sink = _CollectingSink()
        tracer.sinks = [sink]

        # Use tracer's own path so step_id is assigned post-filter
        tracer.emit_event(make_event(port="call", step_id=0))  # filtered out
        tracer.emit_event(make_event(port="exit", step_id=0))  # emitted

        # Only exit event should be in sink with step_id = 1
        assert [e.step_id for e in sink.events] == [1]


class TestFilterComposition:
    """Tests for filter composition and combination."""

    def test_multiple_filters_combine_correctly(self):
        """Multiple filters combine with AND logic."""
        filters = TraceFilters(
            ports={"call", "exit"}, predicates={"member/2"}, min_depth=1, max_depth=5
        )

        # Event must pass ALL filters
        event = make_event(port="call", pred_id="member/2", frame_depth=3)
        assert should_emit_event(event, filters)

        # Fails port filter
        event = make_event(port="redo", pred_id="member/2", frame_depth=3)
        assert not should_emit_event(event, filters)

        # Fails predicate filter
        event = make_event(port="call", pred_id="append/3", frame_depth=3)
        assert not should_emit_event(event, filters)

        # Fails depth filter
        event = make_event(port="call", pred_id="member/2", frame_depth=0)
        assert not should_emit_event(event, filters)


class TestDeterministicVariableNaming:
    """Tests for deterministic variable naming."""

    def test_fresh_variables_get_g_prefix(self):
        """Fresh variables get _G prefix with deterministic numbering."""
        filters = TraceFilters(bindings_policy="names_values")

        # Simulate fresh variable map from engine
        fresh_var_map = {100: "_G1", 101: "_G2", 102: "_G3"}

        event = make_event()
        bindings = {"_G1": Atom("a"), "_G2": Int(42), "_G3": Var(102, "")}  # Unbound

        processed = filters.process_bindings(event, bindings, fresh_var_map)
        assert "_G1" in processed["bindings"]
        assert "_G2" in processed["bindings"]
        assert "_G3" in processed["bindings"]

    def test_named_variables_keep_names(self):
        """Named variables keep their original names."""
        filters = TraceFilters(bindings_policy="names_values")

        event = make_event()
        bindings = {
            "X": Atom("value"),
            "Result": List((Int(1), Int(2))),
            "Acc": Var(200, "Acc"),  # Unbound but named
        }

        processed = filters.process_bindings(event, bindings)
        assert "X" in processed["bindings"]
        assert "Result" in processed["bindings"]
        assert "Acc" in processed["bindings"]


class TestFilterLaziness:
    """Tests for filter short-circuiting and laziness."""

    def test_sampling_rng_not_advanced_when_port_filter_blocks(self):
        """Sampling PRNG not consumed when earlier filter blocks."""
        filters = TraceFilters(ports={"exit"}, sampling_rate=2, sampling_seed=123)

        # Try to access internal RNG if it exists
        rng = getattr(filters, "_rng", None)
        if rng is None:
            pytest.skip("filters PRNG not accessible; using monkeypatch test instead")

        state_before = rng.getstate() if hasattr(rng, "getstate") else str(rng)

        # Event fails at the first check (wrong port)
        assert not should_emit_event(make_event(port="call", step_id=0), filters)

        # PRNG state must be unchanged (sampling not reached)
        state_after = rng.getstate() if hasattr(rng, "getstate") else str(rng)
        assert state_after == state_before

    def test_check_order_short_circuits(self, monkeypatch):
        """Filter checks short-circuit on first failure."""
        filters = TraceFilters(
            ports={"exit"},
            predicates={"test/0"},
            min_depth=1,
            sampling_rate=2,
            sampling_seed=123,
        )

        # Track which checks are called
        calls = []

        # Mock the internal check methods if they exist
        # Otherwise we'll test via side effects
        if hasattr(filters, "_check_port"):

            def mock_check_port(e):
                calls.append("port")
                return False  # Fail port check

            def mock_check_predicate(e):
                calls.append("pred")
                return True

            def mock_check_depth(e):
                calls.append("depth")
                return True

            def mock_check_sampling(e):
                calls.append("sampling")
                return True

            monkeypatch.setattr(filters, "_check_port", mock_check_port)
            monkeypatch.setattr(filters, "_check_predicate", mock_check_predicate)
            monkeypatch.setattr(filters, "_check_depth", mock_check_depth)
            monkeypatch.setattr(filters, "_check_sampling", mock_check_sampling)

            assert not should_emit_event(make_event(port="call"), filters)
            # Only the port check should run
            assert calls == ["port"]
        else:
            # Alternative: test that a complex filter setup with wrong port
            # doesn't affect sampling state (proving it wasn't reached)
            event = make_event(port="call", pred_id="test/0", frame_depth=5)

            # This should fail on port check
            assert not should_emit_event(event, filters)

            # Now test with correct port - sampling should be deterministic
            # from the same starting point
            events = [
                make_event(port="exit", pred_id="test/0", frame_depth=5, step_id=i)
                for i in range(10)
            ]
            results1 = [should_emit_event(e, filters) for e in events]

            # Reset and try again - should get same pattern
            filters2 = TraceFilters(
                ports={"exit"},
                predicates={"test/0"},
                min_depth=1,
                sampling_rate=2,
                sampling_seed=123,
            )
            results2 = [should_emit_event(e, filters2) for e in events]
            assert results1 == results2  # Sampling deterministic
