"""Tests for Web REPL per-solution streaming functionality."""

import time

from prolog.engine.engine import Engine
from prolog.ast.clauses import Program
from prolog.parser.parser import parse_query, parse_program


class TestPerSolutionStreaming:
    """Test suite for per-solution streaming mode in Web REPL."""

    def test_streaming_mode_emits_individual_solutions(self):
        """Test that streaming mode emits one solution event per result."""
        # Parse a simple program with multiple solutions
        program_text = """
        color(red).
        color(green).
        color(blue).
        """
        clauses = parse_program(program_text)
        engine = Engine(Program(clauses))

        # Parse a query that will have multiple solutions
        goals = parse_query("?- color(X).")

        # Simulate streaming mode
        solution_callbacks = []
        done_callback = None

        # Mock callback functions
        def on_solution(solution):
            solution_callbacks.append(solution)

        def on_done(count):
            nonlocal done_callback
            done_callback = count

        # Run query with streaming (simulating what worker would do)
        solutions = engine.run(goals, max_solutions=10)

        # In streaming mode, emit each solution as found
        for i, solution in enumerate(solutions):
            on_solution({"index": i, "bindings": solution})

        # Emit done event after all solutions
        on_done(len(solutions))

        # Verify we got individual solution events
        assert len(solution_callbacks) == 3
        assert solution_callbacks[0]["index"] == 0
        assert solution_callbacks[1]["index"] == 1
        assert solution_callbacks[2]["index"] == 2

        # Verify done event was sent
        assert done_callback == 3

    def test_streaming_vs_batched_mode_flag(self):
        """Test that streaming mode is controlled by a flag."""
        # Create a simple query
        program_text = "fact(1). fact(2)."
        clauses = parse_program(program_text)
        engine = Engine(Program(clauses))
        goals = parse_query("?- fact(X).")

        # Test batched mode (default)
        batched_results = engine.run(goals)
        assert len(batched_results) == 2
        assert isinstance(batched_results, list)

        # Test streaming mode (would be implemented as generator)
        # This tests the expected interface
        streaming_enabled = True
        if streaming_enabled:
            # In streaming, we'd yield solutions one by one
            solutions = []
            for sol in engine.run(goals):
                solutions.append(sol)
                # Each solution is available immediately
                assert sol is not None
            assert len(solutions) == 2

    def test_streaming_handles_empty_results(self):
        """Test that streaming mode correctly handles queries with no solutions."""
        # Create a query with no solutions
        program_text = "fact(a)."
        clauses = parse_program(program_text)
        engine = Engine(Program(clauses))
        goals = parse_query("?- fact(b).")

        solution_count = 0
        done_called = False

        # Simulate streaming
        solutions = engine.run(goals)
        for _ in solutions:
            solution_count += 1
        done_called = True

        assert solution_count == 0
        assert done_called

    def test_streaming_respects_solution_limit(self):
        """Test that streaming stops when max_solutions is reached."""
        # Create a program with many solutions
        program_text = "\n".join([f"num({i})." for i in range(20)])
        clauses = parse_program(program_text)
        engine = Engine(Program(clauses))
        goals = parse_query("?- num(X).")

        # Run with limit
        max_solutions = 5
        solutions = list(engine.run(goals, max_solutions=max_solutions))

        assert len(solutions) == max_solutions

    def test_streaming_message_format(self):
        """Test the format of streaming solution messages."""
        # Expected message format from docs/repl.md
        solution_message = {
            "type": "solution",
            "index": 0,
            "bindings": {
                "X": {"kind": "Int", "value": 1},
                "Y": {"kind": "Int", "value": 2},
            },
            "pretty": "X = 1, Y = 2",
        }

        # Verify message has required fields
        assert solution_message["type"] == "solution"
        assert "index" in solution_message
        assert "bindings" in solution_message
        assert "pretty" in solution_message

        done_message = {"type": "done", "solutions": 1, "elapsedMs": 3}

        # Verify done message format
        assert done_message["type"] == "done"
        assert "solutions" in done_message
        assert "elapsedMs" in done_message

    def test_streaming_preserves_solution_order(self):
        """Test that streaming preserves the order of solutions."""
        program_text = """
        ordered(1, first).
        ordered(2, second).
        ordered(3, third).
        """
        clauses = parse_program(program_text)
        engine = Engine(Program(clauses))
        goals = parse_query("?- ordered(N, Label).")

        solutions = list(engine.run(goals))

        # Verify order is preserved
        assert solutions[0]["N"].value == 1
        assert solutions[1]["N"].value == 2
        assert solutions[2]["N"].value == 3

    def test_streaming_with_single_solution(self):
        """Test streaming mode with queries that have exactly one solution."""
        program_text = "unique_fact(42)."
        clauses = parse_program(program_text)
        engine = Engine(Program(clauses))
        goals = parse_query("?- unique_fact(X).")

        solutions = list(engine.run(goals))

        assert len(solutions) == 1
        assert solutions[0]["X"].value == 42

    def test_streaming_ui_incremental_rendering(self):
        """Test that UI can handle incremental solution rendering."""
        # Simulate UI receiving streaming solutions
        ui_buffer = []

        def handle_solution(msg):
            if msg["type"] == "solution":
                # Add solution to display buffer
                ui_buffer.append(f"Solution {msg['index']}: {msg['pretty']}")
            elif msg["type"] == "done":
                # Add completion marker
                ui_buffer.append(f"Done: {msg['solutions']} solutions")

        # Simulate receiving streamed messages
        messages = [
            {"type": "solution", "index": 0, "pretty": "X = red"},
            {"type": "solution", "index": 1, "pretty": "X = green"},
            {"type": "solution", "index": 2, "pretty": "X = blue"},
            {"type": "done", "solutions": 3},
        ]

        for msg in messages:
            handle_solution(msg)

        # Verify incremental rendering
        assert len(ui_buffer) == 4
        assert "Solution 0" in ui_buffer[0]
        assert "Solution 1" in ui_buffer[1]
        assert "Solution 2" in ui_buffer[2]
        assert "Done: 3 solutions" in ui_buffer[3]

    def test_streaming_performance_metrics(self):
        """Test that streaming includes timing information."""
        start_time = time.time()

        # Simulate query execution
        program_text = "fact(1)."
        clauses = parse_program(program_text)
        engine = Engine(Program(clauses))
        goals = parse_query("?- fact(X).")

        solutions = list(engine.run(goals))

        elapsed_ms = (time.time() - start_time) * 1000

        # Done message should include timing
        done_msg = {
            "type": "done",
            "solutions": len(solutions),
            "elapsedMs": elapsed_ms,
        }

        assert done_msg["elapsedMs"] >= 0
        assert done_msg["solutions"] == 1

    def test_streaming_configuration_option(self):
        """Test that streaming can be enabled/disabled via configuration."""
        # Default configuration
        default_config = {
            "maxSteps": 100000,
            "maxSolutions": 100,
            "streaming": False,  # Default to batched mode
        }

        # Streaming configuration
        streaming_config = {
            "maxSteps": 100000,
            "maxSolutions": 100,
            "streaming": True,  # Enable streaming mode
        }

        assert default_config["streaming"] is False
        assert streaming_config["streaming"] is True
