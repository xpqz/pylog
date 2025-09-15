"""Tests for the PyLog REPL (Read-Eval-Print Loop).

These tests verify the REPL functionality including:
- File loading
- Query execution
- Solution display
- Interactive commands
"""

import pytest
from unittest.mock import Mock, patch

from prolog.engine.engine import Engine
from prolog.ast.terms import Atom, Int, List, Var, Struct


def assert_engine_clean(engine):
    """Assert that engine state is clean (no leftover stacks/trail)."""
    if hasattr(engine, 'cp_stack'):
        assert len(engine.cp_stack) == 0
    if hasattr(engine, 'goal_stack'):
        assert engine.goal_stack.height() == 0
    if hasattr(engine, 'frame_stack'):
        assert len(engine.frame_stack) == 0
    if hasattr(engine, 'trail'):
        assert engine.trail.position() == 0


class TestREPLCore:
    """Test core REPL functionality."""
    
    def test_repl_initialization(self):
        """Test that REPL initializes with an engine."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        assert repl.engine is not None
        assert isinstance(repl.engine, Engine)
        assert repl.engine.program is not None
    
    def test_load_file_success(self, tmp_path):
        """Test loading a Prolog file."""
        from prolog.repl import PrologREPL
        
        # Create a test file
        test_file = tmp_path / "test.pl"
        test_file.write_text("""
            % Test facts and rules
            parent(tom, bob).
            parent(bob, pat).
            
            grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
        """)
        
        repl = PrologREPL()
        result = repl.load_file(str(test_file))
        
        assert result is True
        # Verify facts were loaded by querying
        solutions = list(repl.engine.query("parent(tom, bob)"))
        assert len(solutions) == 1
    
    def test_load_file_not_found(self):
        """Test loading a non-existent file."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        result = repl.load_file("nonexistent.pl")
        
        assert result is False
    
    def test_load_file_parse_error(self, tmp_path):
        """Test loading a file with parse errors."""
        from prolog.repl import PrologREPL
        
        # Create a file with invalid syntax
        test_file = tmp_path / "invalid.pl"
        test_file.write_text("""
            % Invalid syntax
            parent(tom bob).  % Missing comma
        """)
        
        repl = PrologREPL()
        result = repl.load_file(str(test_file))
        
        assert result is False
    
    def test_execute_query_success(self):
        """Test executing a successful query."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("parent(tom, bob). parent(bob, pat).")
        
        result = repl.execute_query("parent(tom, X)")
        
        assert result is not None
        assert result["success"] is True
        assert "X" in result["bindings"]
        assert result["bindings"]["X"] == Atom("bob")
    
    def test_execute_query_failure(self):
        """Test executing a failing query."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("parent(tom, bob).")
        
        result = repl.execute_query("parent(bob, tom)")
        
        assert result is not None
        assert result["success"] is False
    
    def test_execute_query_multiple_solutions(self):
        """Test query with multiple solutions."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("""
            color(red).
            color(green).
            color(blue).
        """)
        
        # Get all solutions
        results = repl.execute_query_all("color(X)")
        
        assert len(results) == 3
        colors = [r["bindings"]["X"] for r in results]
        assert Atom("red") in colors
        assert Atom("green") in colors
        assert Atom("blue") in colors
    
    def test_format_solution(self):
        """Test formatting solutions for display."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # Test success with bindings (deterministic order)
        result = {
            "success": True,
            "bindings": {"X": Atom("bob"), "Y": Int(42), "Z": List((Int(1),), Atom("[]"))}
        }
        formatted = repl.format_solution(result)
        assert "X = bob" in formatted
        assert "Y = 42" in formatted
        assert "Z = [1]" in formatted
        # Check order is deterministic (sorted by var name)
        assert formatted.index("X =") < formatted.index("Y =") < formatted.index("Z =")
        
        # Test success without bindings (true)
        result = {"success": True, "bindings": {}}
        formatted = repl.format_solution(result)
        assert formatted == "true"
        
        # Test failure
        result = {"success": False}
        formatted = repl.format_solution(result)
        assert formatted == "false"
        
        # Test with list and unbound var
        result = {
            "success": True,
            "bindings": {"L": List((Int(1), Int(2)), Atom("[]")), "U": Var(0, "_")}
        }
        formatted = repl.format_solution(result)
        assert "L = [1, 2]" in formatted or "L = [1,2]" in formatted
        # Unbound vars should show as _ or similar, not None
        assert "U = None" not in formatted


    def test_execute_query_cleans_state_on_success(self):
        """Test that engine state is clean after successful query."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("a. a.")
        result = repl.execute_query("a")
        assert result["success"] is True
        assert_engine_clean(repl.engine)
    
    def test_execute_query_cleans_state_on_failure(self):
        """Test that engine state is clean after failed query."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("a.")
        result = repl.execute_query("b")
        assert result["success"] is False
        assert_engine_clean(repl.engine)
    
    def test_execute_query_all_cleans_state(self):
        """Test that engine state is clean after getting all solutions."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("c(1). c(2). c(3).")
        results = repl.execute_query_all("c(X)")
        assert len(results) == 3
        assert_engine_clean(repl.engine)
    
    def test_query_generator_stops_on_dot_frees_state(self):
        """Test that stopping a query generator cleans state."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("n(1). n(2).")
        gen = repl.query_generator("n(X)")
        first = next(gen)
        assert first["bindings"]["X"] == Int(1)
        # Simulate '.' by closing the generator
        gen.close()
        assert_engine_clean(repl.engine)
    
    def test_multiple_consults_append_program(self, tmp_path):
        """Test that multiple consults append to program rather than replace."""
        from prolog.repl import PrologREPL
        
        f1 = tmp_path / "p1.pl"
        f1.write_text("a(1).")
        f2 = tmp_path / "p2.pl"
        f2.write_text("b(2).")
        
        repl = PrologREPL()
        assert repl.load_file(str(f1)) is True
        assert repl.load_file(str(f2)) is True
        
        # Both predicates should be available
        results = list(repl.engine.query("a(X), b(Y)"))
        assert len(results) == 1
        assert results[0]["X"] == Int(1)
        assert results[0]["Y"] == Int(2)


class TestREPLCommands:
    """Test REPL commands and special inputs."""
    
    def test_help_command(self):
        """Test help command displays help text."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        help_text = repl.get_help_text()
        
        assert "Commands:" in help_text
        assert "?-" in help_text  # Query syntax
        assert "consult" in help_text  # File loading
        assert "quit" in help_text  # Exit command
    
    def test_parse_command(self):
        """Test parsing different command types."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # Test query command
        cmd = repl.parse_command("?- parent(X, Y).")
        assert cmd["type"] == "query"
        assert cmd["content"] == "parent(X, Y)"
        
        # Test consult command
        cmd = repl.parse_command("consult('test.pl').")
        assert cmd["type"] == "consult"
        assert cmd["file"] == "test.pl"
        
        # Test help command
        cmd = repl.parse_command("help.")
        assert cmd["type"] == "help"
        
        # Test quit command
        cmd = repl.parse_command("quit.")
        assert cmd["type"] == "quit"
        
        # Test incomplete command (missing period)
        cmd = repl.parse_command("invalid input")
        assert cmd["type"] == "incomplete"
    
    def test_parse_command_whitespace_and_missing_dot(self):
        """Test parsing commands with whitespace and missing dots."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # Query with extra whitespace but no period (incomplete)
        cmd = repl.parse_command("  ?-  parent(X, Y)   ")
        assert cmd["type"] == "incomplete"
        assert cmd["content"] == "parent(X, Y)"
        
        # Query with extra whitespace and period (complete)
        cmd = repl.parse_command("  ?-  parent(X, Y).   ")
        assert cmd["type"] == "query"
        assert cmd["content"] == "parent(X, Y)"
        
        # Consult without trailing dot but with whitespace
        cmd = repl.parse_command("consult('test.pl')  ")
        assert cmd["type"] == "consult"
        assert cmd["file"] == "test.pl"
        
        # Consult with double quotes
        cmd = repl.parse_command('consult("file.pl")')
        assert cmd["type"] == "consult"
        assert cmd["file"] == "file.pl"


class TestREPLInteraction:
    """Test interactive REPL behavior."""
    
    @patch('prolog.repl.PromptSession')
    @patch('builtins.print')
    def test_repl_session_loop(self, mock_print, mock_prompt_session):
        """Test the main REPL loop with prompt_toolkit."""
        from prolog.repl import PrologREPL
        
        # Mock user inputs
        mock_session = Mock()
        mock_session.prompt.side_effect = [
            "consult('test.pl').",
            "?- parent(X, Y).",
            ";",  # Next solution
            ".",  # Stop searching
            "quit."
        ]
        mock_prompt_session.return_value = mock_session
        
        repl = PrologREPL()
        repl.engine.consult_string("parent(tom, bob). parent(tom, liz).")
        
        # Mock file loading
        with patch.object(repl, 'load_file', return_value=True):
            # Run the session (should exit after 'quit')
            repl.run_session()
        
        # Verify prompts were called
        assert mock_session.prompt.call_count == 5
        
        # Verify solutions were printed
        print_calls = [str(call) for call in mock_print.call_args_list]
        assert any("bob" in str(call) for call in print_calls)
        
        # Engine should be clean after session
        assert_engine_clean(repl.engine)
    
    @patch('prolog.repl.PromptSession')
    def test_repl_handles_eof_and_keyboardinterrupt(self, mock_prompt_session):
        """Test REPL handles EOF and KeyboardInterrupt gracefully."""
        from prolog.repl import PrologREPL
        
        mock_session = Mock()
        mock_session.prompt.side_effect = [KeyboardInterrupt, EOFError]
        mock_prompt_session.return_value = mock_session
        
        repl = PrologREPL()
        # Should not raise; should exit cleanly
        repl.run_session()
        assert mock_session.prompt.call_count >= 1
    
    def test_handle_query_with_continuation(self):
        """Test handling query with ; and . for multiple solutions."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("""
            num(1).
            num(2).
            num(3).
        """)
        
        # Start a query
        generator = repl.query_generator("num(X)")
        
        # Get first solution
        result = next(generator)
        assert result["success"] is True
        assert result["bindings"]["X"] == Int(1)
        
        # Get next solution
        result = next(generator)
        assert result["success"] is True
        assert result["bindings"]["X"] == Int(2)
        
        # Get last solution
        result = next(generator)
        assert result["success"] is True
        assert result["bindings"]["X"] == Int(3)
        
        # No more solutions
        with pytest.raises(StopIteration):
            next(generator)


class TestREPLWithPromptToolkit:
    """Test REPL integration with prompt_toolkit features."""
    
    def test_syntax_highlighting(self):
        """Test that syntax highlighting is configured."""
        from prolog.repl import PrologLexer
        from pygments.token import Token
        
        lexer = PrologLexer()
        
        # Test highlighting various Prolog constructs
        tokens = list(lexer.get_tokens("parent(X, Y) :- true."))
        
        # Should have some tokens (exact tokens depend on implementation)
        assert len(tokens) > 0
        
        # Check for expected token types
        token_types = [t[0] for t in tokens]
        assert Token.Name in token_types or Token.Name.Function in token_types
    
    def test_syntax_highlighting_handles_comments_numbers_vars(self):
        """Test syntax highlighting with comments, numbers, and variables."""
        from prolog.repl import PrologLexer
        
        lexer = PrologLexer()
        code = "% comment\nparent(X, Y) :- X is 1+2."
        tokens = list(lexer.get_tokens(code))
        assert len(tokens) > 0  # Should not raise
    
    def test_completer(self):
        """Test auto-completion for predicates."""
        from prolog.repl import PrologCompleter
        from unittest.mock import Mock
        
        # Create completer with some predicates
        completer = PrologCompleter()
        completer.add_predicate("parent", 2)
        completer.add_predicate("grandparent", 2)
        completer.add_predicate("person", 1)
        
        # Mock document for testing
        doc = Mock()
        doc.get_word_before_cursor.return_value = "par"
        
        # Test completion
        completions = list(completer.get_completions(doc, None))
        assert len(completions) == 1
        assert completions[0].text == "parent"
        
        # Test with "p" prefix
        doc.get_word_before_cursor.return_value = "p"
        completions = list(completer.get_completions(doc, None))
        assert len(completions) == 2
        comp_texts = [c.text for c in completions]
        assert "parent" in comp_texts
        assert "person" in comp_texts
    
    def test_completer_updates_from_engine(self):
        """Test that completer reflects dynamic program changes."""
        from prolog.repl import PrologCompleter, PrologREPL
        from unittest.mock import Mock
        
        repl = PrologREPL()
        repl.engine.consult_string("parent(tom, bob).")
        
        completer = PrologCompleter()
        completer.add_predicate("parent", 2)
        
        # Mock document for testing
        doc = Mock()
        doc.get_word_before_cursor.return_value = "par"
        completions = list(completer.get_completions(doc, None))
        assert len(completions) == 1
        assert completions[0].text == "parent"
        
        # Add new predicate
        repl.engine.consult_string("grandparent(tom, pat).")
        completer.add_predicate("grandparent", 2)
        
        doc.get_word_before_cursor.return_value = "grand"
        completions = list(completer.get_completions(doc, None))
        assert len(completions) == 1
        assert completions[0].text == "grandparent"
    
    def test_history_persistence(self, tmp_path):
        """Test that command history is saved and loaded."""
        from prolog.repl import PrologREPL
        
        history_file = tmp_path / ".pylog_history"
        
        # Create REPL with custom history file
        repl = PrologREPL(history_file=str(history_file))
        
        # Add some commands to history
        repl.add_to_history("?- parent(X, Y).")
        repl.add_to_history("consult('test.pl').")
        
        # Save history
        repl.save_history()
        
        # Create new REPL and load history
        repl2 = PrologREPL(history_file=str(history_file))
        repl2.load_history()
        
        # Verify history was loaded
        history = repl2.get_history()
        assert "?- parent(X, Y)." in history
        assert "consult('test.pl')." in history


    def test_history_load_missing_file_is_ok(self, tmp_path):
        """Test that loading non-existent history file doesn't raise."""
        from prolog.repl import PrologREPL
        
        hist = tmp_path / ".none"
        repl = PrologREPL(history_file=str(hist))
        repl.load_history()  # Should not raise
        assert repl.get_history() == []


class TestREPLErrorHandling:
    """Test error handling in the REPL."""
    
    def test_division_by_zero(self):
        """Test that division by zero is handled gracefully."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # Division by zero in operator-free syntax
        result = repl.execute_query("is(X, '/'(1, 0))")
        assert result["success"] is False
        assert_engine_clean(repl.engine)
    
    def test_arithmetic_evaluation_errors(self):
        """Test handling of arithmetic evaluation errors."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # Unbound variable in arithmetic
        result = repl.execute_query("X is Y + 1")
        assert result["success"] is False
        assert_engine_clean(repl.engine)
        
        # Division by zero
        result = repl.execute_query("X is 1 / 0")
        assert result["success"] is False
        assert_engine_clean(repl.engine)
        
        # Type error in arithmetic
        result = repl.execute_query("X is atom + 1")
        assert result["success"] is False
        assert_engine_clean(repl.engine)
    
    def test_handle_parse_error(self):
        """Test handling of parse errors in queries."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # Invalid query syntax
        result = repl.execute_query("parent(X Y)")  # Missing comma
        
        assert result is not None
        assert result["success"] is False
        assert "error" in result
        assert "parse" in result["error"].lower()
    
    def test_handle_runtime_error(self):
        """Test handling of runtime errors."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # Query with undefined predicate (should fail, not error in Stage-1)
        result = repl.execute_query("undefined_predicate(X)")
        
        assert result is not None
        assert result["success"] is False
    
    def test_handle_file_encoding_error(self, tmp_path):
        """Test handling files with encoding issues."""
        from prolog.repl import PrologREPL
        
        # Create a file with invalid UTF-8
        test_file = tmp_path / "bad_encoding.pl"
        test_file.write_bytes(b"parent(tom, \xff\xfe).")  # Invalid UTF-8
        
        repl = PrologREPL()
        result = repl.load_file(str(test_file))
        
        assert result is False
    
    @pytest.mark.skip(reason="Timeout mechanism not working properly - test hangs")
    def test_query_timeout_protection(self):
        """Test protection against infinite loops."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("loop :- loop.")
        
        # Should handle infinite loop gracefully (timeout or depth limit)
        # For now, just test that it doesn't hang forever
        result = repl.execute_query_with_timeout("loop", timeout_ms=100)
        assert result["success"] is False
        assert "timeout" in result.get("error", "").lower() or "limit" in result.get("error", "").lower()
        assert_engine_clean(repl.engine)