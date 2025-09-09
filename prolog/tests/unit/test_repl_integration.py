"""Integration tests for the PyLog REPL.

These tests verify end-to-end REPL functionality including:
- Loading library files
- Running complex queries
- Interactive sessions
"""

import pytest
from pathlib import Path
from unittest.mock import Mock, patch
import tempfile

from prolog.ast.terms import Atom, Int, List, Var


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


class TestREPLLibraryIntegration:
    """Test REPL with library predicates."""
    
    def test_load_library_file(self):
        """Test loading the standard library."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # Load the lists library
        lib_path = Path("prolog/lib/lists.pl")
        if not lib_path.exists():
            pytest.skip("lists.pl not yet created")
        
        result = repl.load_file(str(lib_path))
        assert result is True
        
        # Test that library predicates work
        result = repl.execute_query("append([1,2], [3,4], X)")
        assert result["success"] is True
        expected = List((Int(1), Int(2), Int(3), Int(4)), Atom("[]"))
        assert result["bindings"]["X"] == expected
        
        # Verify state is clean after query
        assert_engine_clean(repl.engine)
    
    def test_multiple_file_loading(self, tmp_path):
        """Test loading multiple files in sequence."""
        from prolog.repl import PrologREPL
        
        # Create test files
        file1 = tmp_path / "facts.pl"
        file1.write_text("""
            person(alice).
            person(bob).
        """)
        
        file2 = tmp_path / "rules.pl"
        file2.write_text("""
            friend(alice, bob).
            friend(bob, charlie).
            
            knows(X, Y) :- friend(X, Y).
            knows(X, Y) :- friend(Y, X).
        """)
        
        repl = PrologREPL()
        
        # Load both files
        assert repl.load_file(str(file1)) is True
        assert repl.load_file(str(file2)) is True
        
        # Test that facts from both files are available
        result = repl.execute_query("person(alice)")
        assert result["success"] is True
        assert_engine_clean(repl.engine)
        
        result = repl.execute_query("knows(charlie, bob)")
        assert result["success"] is True
        assert_engine_clean(repl.engine)
    
    def test_repl_with_recursive_rules(self):
        """Test REPL with recursive predicates."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("""
            % Factorial using successor arithmetic
            factorial(0, s(0)).
            factorial(s(N), F) :- 
                factorial(N, F1),
                multiply(s(N), F1, F).
            
            % Simplified multiply for testing
            multiply(_, 0, 0).
            multiply(0, _, 0).
            multiply(s(0), X, X).
        """)
        
        # Test factorial(0, X)
        result = repl.execute_query("factorial(0, X)")
        assert result["success"] is True
        # X should be s(0) (representing 1)
        from prolog.ast.terms import Struct
        assert isinstance(result["bindings"]["X"], Struct)
        assert result["bindings"]["X"].functor == "s"
        assert_engine_clean(repl.engine)


class TestREPLSessionScenarios:
    """Test complete REPL session scenarios."""
    
    @patch('prolog.repl.PromptSession')
    def test_family_tree_session(self, mock_prompt_session, tmp_path):
        """Test a complete family tree interaction session."""
        from prolog.repl import PrologREPL
        
        # Create a family tree file
        family_file = tmp_path / "family.pl"
        family_file.write_text("""
            % Family relationships
            parent(tom, bob).
            parent(tom, liz).
            parent(bob, ann).
            parent(bob, pat).
            parent(pat, jim).
            
            % Rules
            grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
            sibling(X, Y) :- parent(P, X), parent(P, Y), \\=(X, Y).
        """)
        
        # Mock user inputs for a session
        mock_session = Mock()
        mock_session.prompt.side_effect = [
            f"consult('{family_file}').",
            "?- parent(tom, X).",
            ";",  # Get next solution
            ".",  # Stop
            "?- grandparent(tom, X).",
            ";",  # Get next solution
            ".",
            "?- sibling(bob, X).",
            ".",
            "quit."
        ]
        mock_prompt_session.return_value = mock_session
        
        repl = PrologREPL()
        
        # Capture output
        with patch('builtins.print') as mock_print:
            repl.run_session()
        
        # Verify the session executed correctly
        print_calls = [str(call) for call in mock_print.call_args_list]
        
        # Should show successful file load
        loaded_found = any("loaded" in str(call).lower() or "success" in str(call).lower() 
                          for call in print_calls)
        assert loaded_found, f"No 'loaded' message found in {print_calls}"
        
        # Should show parent results (bob and liz)
        bob_found = any("bob" in str(call) for call in print_calls)
        assert bob_found, f"'bob' not found in {print_calls}"
        
        liz_found = any("liz" in str(call) for call in print_calls)
        assert liz_found, f"'liz' not found in {print_calls}"
        
        # Should show grandparent results (ann and pat)
        ann_found = any("ann" in str(call) for call in print_calls)
        assert ann_found, f"'ann' not found in {print_calls}"
        
        pat_found = any("pat" in str(call) for call in print_calls)
        assert pat_found, f"'pat' not found in {print_calls}"
    
    def test_arithmetic_session(self):
        """Test REPL with arithmetic operations."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # Test basic arithmetic with 'is'
        result = repl.execute_query("X is 2 + 3")
        assert result["success"] is True
        assert result["bindings"]["X"] == Int(5)
        assert_engine_clean(repl.engine)
        
        # Test comparison
        result = repl.execute_query("5 > 3")
        assert result["success"] is True
        assert_engine_clean(repl.engine)
        
        result = repl.execute_query("2 > 3")
        assert result["success"] is False
        assert_engine_clean(repl.engine)
        
        # Test arithmetic errors
        result = repl.execute_query("X is Y + 1")  # Y unbound
        assert result["success"] is False
        assert_engine_clean(repl.engine)
        
        result = repl.execute_query("X is 1 / 0")  # Division by zero
        assert result["success"] is False
        assert_engine_clean(repl.engine)


class TestREPLDisplay:
    """Test solution display and formatting."""
    
    def test_pretty_print_terms(self):
        """Test pretty printing of various term types."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # Test atom
        assert repl.pretty_print(Atom("hello")) == "hello"
        
        # Test integer
        assert repl.pretty_print(Int(42)) == "42"
        
        # Test list
        lst = List((Int(1), Int(2), Int(3)), Atom("[]"))
        assert repl.pretty_print(lst) == "[1, 2, 3]"
        
        # Test nested list
        nested = List((List((Int(1), Int(2)), Atom("[]")), 
                      List((Int(3),), Atom("[]"))), Atom("[]"))
        result = repl.pretty_print(nested)
        assert result in ["[[1, 2], [3]]", "[[1,2],[3]]"]
        
        # Test structure
        from prolog.ast.terms import Struct
        struct = Struct("foo", (Atom("bar"), Int(42)))
        assert repl.pretty_print(struct) == "foo(bar, 42)"
        
        # Test variable (unbound)
        var = Var(0, "X")
        result = repl.pretty_print(var)
        assert result in ["X", "_G0", "_"]  # Various representations OK
        
        # Test anonymous variable
        anon = Var(1, "_")
        result = repl.pretty_print(anon)
        assert result == "_"
    
    def test_format_multiple_solutions(self):
        """Test formatting when multiple solutions exist."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("""
            color(red).
            color(green).
            color(blue).
        """)
        
        # Get all solutions at once
        results = repl.execute_query_all("color(X)")
        formatted = repl.format_all_solutions(results)
        
        # Should show all solutions clearly
        assert "X = red" in formatted or "red" in formatted
        assert "X = green" in formatted or "green" in formatted
        assert "X = blue" in formatted or "blue" in formatted
        
        # Should indicate number of solutions (various formats OK)
        assert ("3 solution" in formatted.lower() or 
                "3 result" in formatted.lower() or
                formatted.count("X =") == 3)
    
    def test_format_no_solutions(self):
        """Test formatting when no solutions exist."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        results = repl.execute_query_all("undefined(X)")
        formatted = repl.format_all_solutions(results)
        
        assert formatted == "false"


class TestREPLRobustness:
    """Test REPL robustness and edge cases."""
    
    def test_empty_input(self):
        """Test handling of empty input."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        cmd = repl.parse_command("")
        
        assert cmd["type"] == "empty"
    
    def test_whitespace_input(self):
        """Test handling of whitespace-only input."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        cmd = repl.parse_command("   \n  \t  ")
        
        assert cmd["type"] == "empty"
    
    def test_incomplete_query(self):
        """Test handling of incomplete queries."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # Query without closing period
        cmd = repl.parse_command("?- parent(X, Y)")
        
        # Should either handle gracefully or prompt for more
        assert cmd["type"] in ["incomplete", "error", "query"]
    
    def test_very_long_query(self):
        """Test handling of very long queries."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # Create a very long query
        long_query = "?- " + ", ".join([f"p{i}(X{i})" for i in range(100)]) + "."
        
        cmd = repl.parse_command(long_query)
        assert cmd["type"] == "query"
    
    def test_special_characters_in_atoms(self):
        """Test handling of special characters."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("""
            'special-atom'(123).
            'with spaces'(ok).
        """)
        
        # Test querying with special atoms
        result = repl.execute_query("'special-atom'(X)")
        assert result["success"] is True
        assert result["bindings"]["X"] == Int(123)
    
    def test_unicode_support(self):
        """Test Unicode support in the REPL."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        repl.engine.consult_string("""
            greeting('你好').
            greeting('Здравствуйте').
            greeting('مرحبا').
        """)
        
        results = repl.execute_query_all("greeting(X)")
        assert len(results) == 3
        
        # Check that Unicode is preserved
        greetings = [r["bindings"]["X"].name for r in results]
        assert '你好' in greetings
        assert 'Здравствуйте' in greetings
        assert 'مرحبا' in greetings
        
        # Verify clean state after Unicode query
        assert_engine_clean(repl.engine)
    
    def test_error_recovery(self):
        """Test REPL recovers gracefully from errors."""
        from prolog.repl import PrologREPL
        
        repl = PrologREPL()
        
        # First, a parse error
        result = repl.execute_query("bad syntax (")
        assert result["success"] is False
        assert_engine_clean(repl.engine)
        
        # Then a valid query to ensure recovery
        repl.engine.consult_string("ok.")
        result = repl.execute_query("ok")
        assert result["success"] is True
        assert_engine_clean(repl.engine)
        
        # Runtime error (undefined predicate)
        result = repl.execute_query("undefined(X)")
        assert result["success"] is False
        assert_engine_clean(repl.engine)
        
        # Another valid query
        result = repl.execute_query("ok")
        assert result["success"] is True
        assert_engine_clean(repl.engine)