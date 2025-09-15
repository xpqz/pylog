"""PyLog REPL (Read-Eval-Print Loop) implementation.

This module provides an interactive Prolog query interface using prompt_toolkit
for enhanced user experience with syntax highlighting, auto-completion, and
command history.

Requirements:
- Python 3.10+
- prompt_toolkit
- pygments
"""

from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import Any, Optional, Generator

from prompt_toolkit import PromptSession
from prompt_toolkit.history import FileHistory
from prompt_toolkit.lexers import PygmentsLexer
from prompt_toolkit.completion import Completer, Completion
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory
from pygments.lexer import RegexLexer
from pygments.token import (
    Comment, Keyword, Name, Number, Operator, String, Text, Whitespace
)

from prolog.engine.engine import Engine
from prolog.ast.terms import Atom, Int, Var, Struct, List, Term
from prolog.ast.clauses import Program
from prolog.ast.pretty import pretty


class PrologLexer(RegexLexer):
    """Pygments lexer for Prolog syntax highlighting."""
    
    name = 'Prolog'
    aliases = ['prolog']
    filenames = ['*.pl', '*.pro']
    
    tokens = {
        'root': [
            # Comments
            (r'%.*$', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment'),
            
            # Keywords and operators
            (r'\b(true|fail|false)\b', Keyword.Constant),
            (r':-|-->|\?-', Operator),
            (r'[!;,|]', Operator),
            
            # Numbers
            (r'\b\d+\b', Number.Integer),
            (r'\b\d+\.\d+\b', Number.Float),
            
            # Variables (uppercase or underscore)
            (r'\b[A-Z_][a-zA-Z0-9_]*\b', Name.Variable),
            
            # Atoms and functors
            (r'\'[^\']*\'', String.Symbol),  # Quoted atoms
            (r'\b[a-z][a-zA-Z0-9_]*\b', Name.Function),
            
            # Strings
            (r'"[^"]*"', String.Double),
            
            # Brackets and parentheses
            (r'[\[\](){}]', Operator),
            
            # Operators
            (r'[=<>\\+\-*/^]', Operator),
            (r'\s+', Whitespace),
            
            # Everything else
            (r'.', Text),
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
    }


class PrologCompleter(Completer):
    """Auto-completion for Prolog predicates."""
    
    def __init__(self):
        self.predicates = set()
        self._add_builtins()
    
    def _add_builtins(self):
        """Add built-in predicates."""
        builtins = [
            'true', 'fail', 'call', 'is', 'var', 'nonvar', 'atom', 'integer',
            'append', 'member', 'reverse', 'length', 'between',
            'consult', 'help', 'quit', 'trace', 'notrace'
        ]
        for pred in builtins:
            self.predicates.add(pred)
    
    def add_predicate(self, functor: str, arity: int):
        """Add a predicate to the completion list."""
        self.predicates.add(functor)
    
    def get_completions(self, document, complete_event):
        """Prompt_toolkit completion interface.
        
        Args:
            document: Document object with the current text
            complete_event: CompleteEvent with completion context
            
        Yields:
            Completion objects
        """
        word = document.get_word_before_cursor()
        if not word:
            return
        
        for pred in sorted(self.predicates):
            if pred.startswith(word):
                yield Completion(pred, start_position=-len(word))


class PrologREPL:
    """Interactive Prolog REPL with prompt_toolkit."""
    
    def __init__(self, history_file: Optional[str] = None):
        """Initialize the REPL.
        
        Args:
            history_file: Path to command history file
        """
        # Create empty program and engine
        self.program = Program([])
        self.engine = Engine(self.program)
        self.completer = PrologCompleter()
        self.history_file = history_file or str(Path.home() / '.pylog_history')
        
        # Load standard library
        self._load_standard_library()
        self.session = None
        self._init_session()
        self._query_generator = None
    
    def _init_session(self):
        """Initialize the prompt_toolkit session."""
        try:
            history = FileHistory(self.history_file)
        except:
            # If history file is inaccessible, use in-memory history
            history = None
        
        self.session = PromptSession(
            history=history,
            lexer=PygmentsLexer(PrologLexer),
            completer=self.completer,
            auto_suggest=AutoSuggestFromHistory(),
            multiline=False,
            complete_while_typing=True,
        )
    
    def _load_standard_library(self):
        """Load the standard library predicates."""
        from prolog.parser import parser
        
        # Find the lib directory relative to this file
        lib_dir = Path(__file__).parent / 'lib'
        lists_file = lib_dir / 'lists.pl'
        
        if lists_file.exists():
            try:
                with open(lists_file, 'r') as f:
                    program_text = f.read()
                
                # Parse and add clauses to the program
                clauses = parser.parse_program(program_text)
                
                # Create new program with library clauses
                all_clauses = list(self.program.clauses) + list(clauses)
                self.program = Program(tuple(all_clauses))
                self.engine = Engine(self.program)
                
                # Update completer with library predicates
                self._update_completer()
                
            except Exception as e:
                print(f"Warning: Could not load standard library: {e}")
    
    def load_file(self, filepath: str) -> bool:
        """Load a Prolog file.
        
        Opens file with UTF-8 encoding and appends to current program.
        
        Args:
            filepath: Path to the Prolog file
            
        Returns:
            True if successful, False otherwise
        """
        try:
            path = Path(filepath)
            if not path.exists():
                print(f"Error: File not found: {filepath}")
                return False
            
            # Try UTF-8 first, with explicit error handling
            try:
                with open(path, 'r', encoding='utf-8') as f:
                    content = f.read()
            except UnicodeDecodeError as e:
                print(f"Error: Unable to decode file '{filepath}' as UTF-8.")
                print(f"  Details: {e}")
                print(f"  Try converting the file to UTF-8 encoding.")
                return False
            
            # Append to program (not replace)
            self.engine.consult_string(content)
            print(f"Loaded: {filepath}")
            
            # Update completer with predicates from the file
            self._update_completer()
            return True
            
        except Exception as e:
            print(f"Error loading file: {e}")
            return False
    
    def _update_completer(self):
        """Update completer with predicates from the loaded program."""
        if hasattr(self.engine, 'program') and self.engine.program:
            for clause in self.engine.program.clauses:
                if hasattr(clause.head, 'functor'):
                    functor = clause.head.functor
                    arity = len(clause.head.args) if hasattr(clause.head, 'args') else 0
                    self.completer.add_predicate(functor, arity)
    
    def execute_query(self, query_text: str) -> dict[str, Any]:
        """Execute a single query and return the first solution.
        
        Args:
            query_text: The Prolog query to execute
            
        Returns:
            Dictionary with 'success' and optional 'bindings' or 'error'
        """
        try:
            # Clean up any previous query state
            self._cleanup_query_state()
            
            # Execute the query
            solutions = self.engine.query(query_text)
            
            if solutions:
                return {
                    'success': True,
                    'bindings': solutions[0]
                }
            else:
                return {'success': False}
                
        except Exception as e:
            # Handle parse errors and other exceptions
            error_msg = str(e)
            if 'parse' in error_msg.lower() or 'syntax' in error_msg.lower():
                # Check for common operator mistakes in Stage 1
                if ' is ' in query_text or '=' in query_text and '=(' not in query_text:
                    return {
                        'success': False,
                        'error': f"Parse error: {error_msg}\n" +
                                "Note: Stage 1 uses operator-free syntax. Examples:\n" +
                                "  - Use: is(X, '+'(2, 3))  instead of: X is 2 + 3\n" +
                                "  - Use: '>'(5, 3)         instead of: 5 > 3\n" +
                                "  - Use: '='(X, Y)         instead of: X = Y"
                    }
                return {
                    'success': False,
                    'error': f"Parse error: {error_msg}"
                }
            return {
                'success': False,
                'error': str(e)
            }
        finally:
            # Ensure state is clean
            self._cleanup_query_state()
    
    def execute_query_all(self, query_text: str) -> list[dict[str, Any]]:
        """Execute a query and return all solutions.
        
        Args:
            query_text: The Prolog query to execute
            
        Returns:
            List of solution dictionaries
        """
        try:
            self._cleanup_query_state()
            solutions = self.engine.query(query_text)
            
            results = []
            for solution in solutions:
                results.append({
                    'success': True,
                    'bindings': solution
                })
            
            return results
            
        except Exception as e:
            return [{
                'success': False,
                'error': str(e)
            }]
        finally:
            self._cleanup_query_state()
    
    def execute_query_with_timeout(self, query_text: str, timeout_ms: int) -> dict[str, Any]:
        """Execute a query with a timeout using step limits.
        
        Uses the engine's built-in max_steps parameter to prevent infinite loops.
        Estimates steps based on timeout: ~10000 steps per 100ms.
        
        Args:
            query_text: The Prolog query to execute
            timeout_ms: Timeout in milliseconds
            
        Returns:
            Dictionary with 'success' and optional 'bindings' or 'error'
        """
        # Save current max_steps setting
        old_max_steps = self.engine.max_steps
        
        # Set temporary step limit based on timeout
        # Rough estimate: 10000 steps per 100ms
        self.engine.max_steps = (timeout_ms * 100)
        self.engine._steps_taken = 0  # Reset step counter
        
        try:
            self._cleanup_query_state()
            solutions = list(self.engine.query(query_text))
            
            # Check if we hit the step limit (>= because counter increments before check)
            if self.engine._steps_taken >= self.engine.max_steps:
                return {
                    'success': False,
                    'error': 'Query timeout: exceeded step limit'
                }
            
            if solutions:
                return {
                    'success': True,
                    'bindings': solutions[0]
                }
            else:
                return {'success': False}
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
        finally:
            # Restore original max_steps setting
            self.engine.max_steps = old_max_steps
            self._cleanup_query_state()
    
    
    def query_generator(self, query_text: str) -> Generator[dict[str, Any], None, None]:
        """Create a generator for interactive query results.
        
        Args:
            query_text: The Prolog query to execute
            
        Yields:
            Solution dictionaries
        """
        try:
            self._cleanup_query_state()
            
            try:
                # Get all solutions at once
                solutions = self.engine.query(query_text)
                
                # Yield them one by one for interactive display
                for solution in solutions:
                    yield {
                        'success': True,
                        'bindings': solution
                    }
                    
                # If no solutions, yield false
                if not solutions:
                    yield {'success': False}
                    
            except GeneratorExit:
                # User stopped with '.' or generator was closed
                pass
            except Exception as e:
                yield {
                    'success': False,
                    'error': str(e)
                }
        finally:
            # Always cleanup, even if generator is closed early
            self._cleanup_query_state()
    
    def _cleanup_query_state(self):
        """Clean up engine state after query execution."""
        # Reset engine stacks and trail
        if hasattr(self.engine, 'cp_stack'):
            self.engine.cp_stack.clear()
        if hasattr(self.engine, 'goal_stack'):
            while self.engine.goal_stack.height() > 0:
                self.engine.goal_stack.pop()
        if hasattr(self.engine, 'frame_stack'):
            self.engine.frame_stack.clear()
        if hasattr(self.engine, 'trail'):
            self.engine.trail.unwind_to(0, self.engine.store)
    
    def format_solution(self, result: dict[str, Any]) -> str:
        """Format a solution for display.
        
        Args:
            result: Solution dictionary
            
        Returns:
            Formatted string
        """
        if not result['success']:
            return 'false'
        
        bindings = result.get('bindings', {})
        if not bindings:
            return 'true'
        
        # Sort variable names for deterministic output
        sorted_vars = sorted(bindings.keys())
        parts = []
        for var in sorted_vars:
            value = bindings[var]
            formatted_value = self.pretty_print(value)
            parts.append(f"{var} = {formatted_value}")
        
        return '\n'.join(parts)
    
    def format_all_solutions(self, results: list[dict[str, Any]]) -> str:
        """Format all solutions for display.
        
        Args:
            results: List of solution dictionaries
            
        Returns:
            Formatted string
        """
        if not results:
            return 'false'
        
        if all(not r['success'] for r in results):
            return 'false'
        
        formatted = []
        for i, result in enumerate(results):
            if result['success']:
                solution = self.format_solution(result)
                formatted.append(f"Solution {i+1}:\n{solution}")
        
        if not formatted:
            return 'false'
        
        output = '\n\n'.join(formatted)
        count = len(formatted)
        output += f"\n\n{count} solution{'s' if count != 1 else ''} found."
        return output
    
    def pretty_print(self, term: Term) -> str:
        """Pretty print a Prolog term.
        
        Args:
            term: The term to print
            
        Returns:
            Formatted string
        """
        if isinstance(term, Atom):
            return term.name
        elif isinstance(term, Int):
            return str(term.value)
        elif isinstance(term, Var):
            if term.hint == '_':
                return '_'
            elif term.hint:
                return term.hint
            else:
                return f"_G{term.id}"
        elif isinstance(term, List):
            if term.items:
                items_str = ', '.join(self.pretty_print(item) for item in term.items)
                if term.tail != Atom('[]'):
                    return f"[{items_str}|{self.pretty_print(term.tail)}]"
                return f"[{items_str}]"
            return '[]'
        elif isinstance(term, Struct):
            if term.args:
                args_str = ', '.join(self.pretty_print(arg) for arg in term.args)
                return f"{term.functor}({args_str})"
            return term.functor
        else:
            # Fallback to the pretty function from ast.pretty if available
            try:
                return pretty(term)
            except:
                return str(term)
    
    def parse_command(self, input_text: str) -> dict[str, str]:
        """Parse user input to determine command type.
        
        Args:
            input_text: Raw user input
            
        Returns:
            Dictionary with 'type' and relevant data
        """
        input_text = input_text.strip()
        
        if not input_text:
            return {'type': 'empty'}
        
        # Check for help
        if input_text.rstrip('.').lower() == 'help':
            return {'type': 'help'}
        
        # Check for quit
        if input_text.rstrip('.').lower() in ['quit', 'exit', 'halt']:
            return {'type': 'quit'}
        
        # Check for consult
        consult_match = re.match(r'consult\s*\(\s*[\'"]([^\'"]*)[\'"]\s*\)\s*\.?', input_text)
        if consult_match:
            return {'type': 'consult', 'file': consult_match.group(1)}
        
        # Check for query (with or without ?- prefix)
        query_text = input_text
        if input_text.startswith('?-'):
            query_text = input_text[2:].strip()
        
        # Check if query is complete (has trailing period)
        if not query_text.endswith('.'):
            return {'type': 'incomplete', 'content': query_text}
        
        # Remove trailing period and treat as query
        query = query_text[:-1].strip()
        return {'type': 'query', 'content': query}
    
    def get_help_text(self) -> str:
        """Get help text for the REPL."""
        return """
PyLog REPL Commands:
    ?- <query>.          Execute a Prolog query
    consult('file.pl').  Load a Prolog file
    help.                Show this help message
    quit.                Exit the REPL
    
During query results:
    ;                    Show next solution
    .                    Stop searching for solutions
    
Examples:
    ?- parent(X, Y).     Find parent relationships
    ?- member(2, [1,2,3]). Check list membership
"""
    
    def run_session(self):
        """Run the interactive REPL session."""
        print("Welcome to PyLog REPL")
        print("Type 'help.' for commands or 'quit.' to exit\n")
        
        incomplete_query = ""
        
        while True:
            try:
                # Get user input
                if incomplete_query:
                    prompt = '|    '  # Continuation prompt
                else:
                    prompt = '?- '
                
                user_input = self.session.prompt(prompt)
                
                # Handle incomplete queries
                if incomplete_query:
                    user_input = incomplete_query + " " + user_input
                    incomplete_query = ""
                
                # Parse command
                cmd = self.parse_command(user_input)
                
                if cmd['type'] == 'empty':
                    continue
                    
                elif cmd['type'] == 'incomplete':
                    # Save for continuation
                    incomplete_query = '?- ' + cmd['content']
                    continue
                    
                elif cmd['type'] == 'quit':
                    print("Goodbye!")
                    break
                    
                elif cmd['type'] == 'help':
                    print(self.get_help_text())
                    
                elif cmd['type'] == 'consult':
                    self.load_file(cmd['file'])
                    
                elif cmd['type'] == 'query':
                    self._handle_query(cmd['content'])
                    
                else:
                    print(f"Error: {cmd.get('message', 'Unknown command')}")
                    
            except EOFError:
                # Ctrl-D pressed
                print("\nGoodbye!")
                break
            except KeyboardInterrupt:
                # Ctrl-C pressed
                print("^C")
                incomplete_query = ""  # Reset any incomplete query
                continue
            except Exception as e:
                print(f"Error: {e}")
                incomplete_query = ""  # Reset on error
    
    def _handle_query(self, query_text: str):
        """Handle an interactive query with ; and . support.
        
        Args:
            query_text: The query to execute
        """
        try:
            generator = self.query_generator(query_text)
            solution_count = 0
            
            for i, result in enumerate(generator):
                if result['success']:
                    print(self.format_solution(result))
                    solution_count += 1
                    
                    # Check if there might be more solutions
                    try:
                        # Peek at next solution
                        next_result = next(generator)
                        
                        # Ask user for continuation
                        try:
                            cont = self.session.prompt(' ', default=';')
                            if cont.strip() == '.':
                                generator.close()
                                print(".")  # Indicate stop
                                break
                            # Show the peeked solution
                            print(self.format_solution(next_result))
                            solution_count += 1
                        except (EOFError, KeyboardInterrupt):
                            generator.close()
                            print("\n.")  # Indicate stop
                            break
                    except StopIteration:
                        # No more solutions
                        if solution_count > 0:
                            print(".")  # Final solution marker
                        break
                else:
                    if 'error' in result:
                        print(f"Error: {result['error']}")
                    else:
                        print('false.')
                    break
                    
        except Exception as e:
            print(f"Error: {e}")
        finally:
            self._cleanup_query_state()
    
    def add_to_history(self, command: str):
        """Add a command to history.
        
        Args:
            command: Command to add
        """
        if self.session and self.session.history:
            # Use store_string to persist immediately to disk
            if hasattr(self.session.history, 'store_string'):
                self.session.history.store_string(command)
            else:
                self.session.history.append_string(command)
    
    def save_history(self):
        """Save command history to file."""
        # History is saved when using store_string in add_to_history
        pass
    
    def load_history(self):
        """Load command history from file."""
        # History is automatically loaded by FileHistory on init
        pass
    
    def get_history(self) -> list[str]:
        """Get command history.
        
        Returns:
            List of commands
        """
        if self.session and self.session.history:
            # For FileHistory, we need to load from disk
            if hasattr(self.session.history, 'load_history_strings'):
                # Load and reverse (FileHistory returns newest first)
                strings = list(self.session.history.load_history_strings())
                strings.reverse()
                return strings
            else:
                # For in-memory history
                return list(self.session.history.get_strings())
        return []


def main():
    """Main entry point for the REPL."""
    repl = PrologREPL()
    repl.run_session()


if __name__ == '__main__':
    main()