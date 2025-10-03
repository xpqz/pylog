"""
Test JSONL encoding for trace and internal events.

Tests that JSONL output has correct type tags and structure.
"""

import json
import tempfile
from pathlib import Path


from prolog.parser import parser
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine
from prolog.debug.sinks import JSONLTraceSink


class TestJSONLEncoding:
    """Test JSONL encoding for trace and internal events."""

    def test_trace_event_json_structure(self):
        """Test that TraceEvent is encoded with correct JSON structure."""
        # Create a simple program
        clauses = parser.parse_program(
            """
            test(a).
            test(b).
        """
        )
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Use temporary file for JSONL output
        with tempfile.NamedTemporaryFile(mode="w", suffix=".jsonl", delete=False) as f:
            temp_file = f.name

        try:
            # Open file and create sink
            output_file = open(temp_file, "w")
            sink = JSONLTraceSink(output_file)
            engine.tracer.add_sink(sink)

            # Run a query
            goals = parser.parse_query("?- test(X).")
            solutions = list(engine.run(goals))

            # Flush and close the sink
            sink.close()
            output_file.close()

            # Read and parse JSONL
            with open(temp_file, "r") as f:
                lines = f.readlines()

            assert len(lines) > 0, "Should have JSONL output"

            for line in lines:
                if line.strip():
                    event = json.loads(line)

                    # All events should have 'type' field
                    assert "type" in event, f"Missing 'type' field in: {event}"

                    if event["type"] == "trace":
                        # Trace events should have required fields
                        assert "rid" in event, "Missing run_id"
                        assert "sid" in event, "Missing step_id"
                        assert "p" in event, "Missing port"
                        assert "pid" in event, "Missing pred_id"

                        # Port should be 0-3 (call, exit, redo, fail)
                        assert event["p"] in [0, 1, 2, 3], f"Invalid port: {event['p']}"

        finally:
            Path(temp_file).unlink(missing_ok=True)

    def test_internal_event_json_structure(self):
        """Test that InternalEvent is encoded with correct JSON structure."""
        # Create a program with if-then-else to generate internal events
        clauses = parser.parse_program(
            """
            test(X) :- (X = 1 -> true ; true).
        """
        )
        engine = Engine(Program(tuple(clauses)), trace=True)
        engine.tracer.enable_internal_events = True

        # Use temporary file for JSONL output
        with tempfile.NamedTemporaryFile(mode="w", suffix=".jsonl", delete=False) as f:
            temp_file = f.name

        try:
            # Open file and create sink
            output_file = open(temp_file, "w")
            sink = JSONLTraceSink(output_file)
            engine.tracer.add_sink(sink)

            # Run a query
            goals = parser.parse_query("?- test(1).")
            solutions = list(engine.run(goals))

            # Flush and close the sink
            sink.close()
            output_file.close()

            # Read and parse JSONL
            with open(temp_file, "r") as f:
                lines = f.readlines()

            internal_events_found = False

            for line in lines:
                if line.strip():
                    event = json.loads(line)

                    # All events should have 'type' field
                    assert "type" in event

                    if event["type"] == "internal":
                        internal_events_found = True
                        # Internal events should have required fields
                        assert "sid" in event, "Missing step_id"
                        assert "kind" in event, "Missing kind"
                        assert "details" in event, "Missing details"

            assert internal_events_found, "Should have found internal events"

        finally:
            Path(temp_file).unlink(missing_ok=True)

    def test_json_type_tags(self):
        """Test that every JSON line includes 'type' with only 'trace' or 'internal'."""
        # Create a program that generates both types
        clauses = parser.parse_program(
            """
            test(X) :- (X = 1 -> Y = a ; Y = b), Y = a.
        """
        )
        engine = Engine(Program(tuple(clauses)), trace=True)
        engine.tracer.enable_internal_events = True

        # Use temporary file for JSONL output
        with tempfile.NamedTemporaryFile(mode="w", suffix=".jsonl", delete=False) as f:
            temp_file = f.name

        try:
            # Open file and create sink
            output_file = open(temp_file, "w")
            sink = JSONLTraceSink(output_file)
            engine.tracer.add_sink(sink)

            # Run a query
            goals = parser.parse_query("?- test(1).")
            solutions = list(engine.run(goals))

            # Flush and close the sink
            sink.close()
            output_file.close()

            # Read and parse JSONL
            with open(temp_file, "r") as f:
                lines = f.readlines()

            types_seen = set()

            for line in lines:
                if line.strip():
                    event = json.loads(line)

                    # Must have 'type' field
                    assert "type" in event, f"Missing 'type' field in: {event}"

                    # Type must be only 'trace' or 'internal'
                    assert event["type"] in [
                        "trace",
                        "internal",
                    ], f"Invalid type: {event['type']}"

                    types_seen.add(event["type"])

            # We should see both types in this test
            assert "trace" in types_seen, "Should have trace events"
            # Internal events might not be present if not implemented
            # So we don't assert on 'internal' being present

        finally:
            Path(temp_file).unlink(missing_ok=True)

    def test_step_id_monotonicity(self):
        """Test that step_id strictly increases over 4-port events."""
        # Create a simple program
        clauses = parser.parse_program(
            """
            test(1).
            test(2).
            test(3).
        """
        )
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Use temporary file for JSONL output
        with tempfile.NamedTemporaryFile(mode="w", suffix=".jsonl", delete=False) as f:
            temp_file = f.name

        try:
            # Open file and create sink
            output_file = open(temp_file, "w")
            sink = JSONLTraceSink(output_file)
            engine.tracer.add_sink(sink)

            # Run a query that generates multiple solutions
            goals = parser.parse_query("?- test(X).")
            solutions = list(engine.run(goals))

            assert len(solutions) == 3  # Should find all three

            # Flush and close the sink
            sink.close()
            output_file.close()

            # Read and parse JSONL
            with open(temp_file, "r") as f:
                lines = f.readlines()

            # Extract step_ids from trace events
            step_ids = []
            for line in lines:
                if line.strip():
                    event = json.loads(line)
                    if event.get("type") == "trace":
                        step_ids.append(event["sid"])

            # Step IDs should be monotonically increasing
            for i in range(1, len(step_ids)):
                assert (
                    step_ids[i] > step_ids[i - 1]
                ), f"Step IDs not monotonic: {step_ids[i-1]} -> {step_ids[i]}"

        finally:
            Path(temp_file).unlink(missing_ok=True)

    def test_port_encoding(self):
        """Test that ports are encoded as integers 0-3."""
        # Create a program that will generate all port types
        clauses = parser.parse_program(
            """
            test(X) :- member(X, [1,2,3]).
            member(X, [X|_]).
            member(X, [_|T]) :- member(X, T).
        """
        )
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Use temporary file for JSONL output
        with tempfile.NamedTemporaryFile(mode="w", suffix=".jsonl", delete=False) as f:
            temp_file = f.name

        try:
            # Open file and create sink
            output_file = open(temp_file, "w")
            sink = JSONLTraceSink(output_file)
            engine.tracer.add_sink(sink)

            # Run a query
            goals = parser.parse_query("?- test(X).")
            solutions = list(engine.run(goals))

            # Flush and close the sink
            sink.close()
            output_file.close()

            # Read and parse JSONL
            with open(temp_file, "r") as f:
                lines = f.readlines()

            ports_seen = set()

            for line in lines:
                if line.strip():
                    event = json.loads(line)
                    if event.get("type") == "trace":
                        port = event["p"]
                        assert isinstance(
                            port, int
                        ), f"Port should be int, got {type(port)}"
                        assert 0 <= port <= 3, f"Port out of range: {port}"
                        ports_seen.add(port)

            # We should see at least CALL (0) and EXIT (1) or FAIL (3)
            assert 0 in ports_seen, "Should have CALL port"
            assert 1 in ports_seen or 3 in ports_seen, "Should have EXIT or FAIL"

        finally:
            Path(temp_file).unlink(missing_ok=True)

    def test_compact_keys(self):
        """Test that JSONL uses compact keys as specified."""
        # Create a simple program
        clauses = parser.parse_program(
            """
            test(a).
        """
        )
        engine = Engine(Program(tuple(clauses)), trace=True)

        # Use temporary file for JSONL output
        with tempfile.NamedTemporaryFile(mode="w", suffix=".jsonl", delete=False) as f:
            temp_file = f.name

        try:
            # Open file and create sink
            output_file = open(temp_file, "w")
            sink = JSONLTraceSink(output_file)
            engine.tracer.add_sink(sink)

            # Run a query
            goals = parser.parse_query("?- test(X).")
            solutions = list(engine.run(goals))

            # Flush and close the sink
            sink.close()
            output_file.close()

            # Read and parse JSONL
            with open(temp_file, "r") as f:
                lines = f.readlines()

            # Check that compact keys are used
            for line in lines:
                if line.strip():
                    event = json.loads(line)
                    if event.get("type") == "trace":
                        # Should use compact keys: sid, p, pid, rid, etc.
                        # Should NOT use long keys like step_id, port, pred_id
                        assert "step_id" not in event, "Should use 'sid' not 'step_id'"
                        assert "port" not in event, "Should use 'p' not 'port'"
                        assert "pred_id" not in event, "Should use 'pid' not 'pred_id'"

        finally:
            Path(temp_file).unlink(missing_ok=True)
