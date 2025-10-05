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
from pathlib import Path
from typing import Any, Optional, Generator
import argparse
import sys

from prompt_toolkit import PromptSession
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.history import FileHistory
from prompt_toolkit.lexers import PygmentsLexer
from prompt_toolkit.completion import Completer, Completion
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory
from pygments.lexer import RegexLexer
from pygments.token import (
    Comment,
    Keyword,
    Name,
    Number,
    Operator,
    String,
    Text,
    Whitespace,
)

from prolog.engine.engine import Engine
from prolog.ast.terms import Atom, Int, Var, Struct, List, Term
from prolog.ast.clauses import Program
from prolog.ast.pretty import pretty, pretty_clause
from prolog.parser import parser
from prolog.parser.parser import parse_program
from prolog.debug.sinks import PrettyTraceSink, JSONLTraceSink, CollectorSink
from prolog.debug.filters import TraceFilters


class PrologLexer(RegexLexer):
    """Pygments lexer for Prolog syntax highlighting."""

    name = "Prolog"
    aliases = ["prolog"]
    filenames = ["*.pl", "*.pro"]

    tokens = {
        "root": [
            # Comments
            (r"%.*$", Comment.Single),
            (r"/\*", Comment.Multiline, "comment"),
            # Keywords and operators
            (r"\b(true|fail|false)\b", Keyword.Constant),
            (r":-|-->|\?-", Operator),
            (r"[!;,|]", Operator),
            # Numbers
            (r"\b\d+\b", Number.Integer),
            (r"\b\d+\.\d+\b", Number.Float),
            # Variables (uppercase or underscore)
            (r"\b[A-Z_][a-zA-Z0-9_]*\b", Name.Variable),
            # Atoms and functors
            (r"\'[^\']*\'", String.Symbol),  # Quoted atoms
            (r"\b[a-z][a-zA-Z0-9_]*\b", Name.Function),
            # Strings
            (r'"[^"]*"', String.Double),
            # Brackets and parentheses
            (r"[\[\](){}]", Operator),
            # Operators
            (r"[=<>\\+\-*/^]", Operator),
            (r"\s+", Whitespace),
            # Everything else
            (r".", Text),
        ],
        "comment": [
            (r"[^*/]+", Comment.Multiline),
            (r"/\*", Comment.Multiline, "#push"),
            (r"\*/", Comment.Multiline, "#pop"),
            (r"[*/]", Comment.Multiline),
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
            "true",
            "fail",
            "call",
            "is",
            "var",
            "nonvar",
            "atom",
            "integer",
            "append",
            "member",
            "reverse",
            "length",
            "between",
            "consult",
            "help",
            "quit",
            "trace",
            "notrace",
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
        self.history_file = history_file or str(Path.home() / ".pylog_history")

        # Debug/trace state (stubs for TDD)
        self.trace_enabled = False
        self.trace_mode = "pretty"  # 'pretty', 'json', 'collector'
        self.trace_file = None
        self.trace_sample_rate = 1
        self.spypoints = set()
        self.collector_sink = None
        self.metrics_enabled = False

        # Load standard library
        self._load_standard_library()
        self.session = None
        self._init_session()
        self._query_generator = None

    def _init_session(self):
        """Initialize the prompt_toolkit session."""
        try:
            history = FileHistory(self.history_file)
        except Exception:
            # If history file is inaccessible, use in-memory history
            history = None

        # Key bindings: make Tab accept inline autosuggestions; otherwise, complete next
        kb = KeyBindings()

        @kb.add("tab")
        def _(event):  # type: ignore
            buff = event.app.current_buffer
            # If there's an inline suggestion (from history), accept it
            if getattr(buff, "suggestion", None):
                buff.insert_text(buff.suggestion.text)
            else:
                # Fall back to normal completion behavior
                buff.complete_next()

        self.session = PromptSession(
            history=history,
            lexer=PygmentsLexer(PrologLexer),
            completer=self.completer,
            auto_suggest=AutoSuggestFromHistory(),
            multiline=False,
            complete_while_typing=True,
            key_bindings=kb,
        )

    def _load_standard_library(self):
        """Load the standard library predicates."""

        # Find the lib directory relative to this file
        lib_dir = Path(__file__).parent / "lib"
        lists_file = lib_dir / "lists.pl"

        if lists_file.exists():
            try:
                with open(lists_file, "r") as f:
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
                with open(path, "r", encoding="utf-8") as f:
                    content = f.read()
            except UnicodeDecodeError as e:
                print(f"Error: Unable to decode file '{filepath}' as UTF-8.")
                print(f"  Details: {e}")
                print("  Try converting the file to UTF-8 encoding.")
                return False

            # Append to program (not replace)
            self.engine.consult_string(content)

            # CRITICAL: Sync self.program with the updated engine.program
            # Otherwise _recreate_engine will use stale program
            self.program = self.engine.program

            print(f"Loaded: {filepath}")

            # Update completer with predicates from the file
            self._update_completer()
            return True

        except Exception as e:
            print(f"Error loading file: {e}")
            return False

    def _update_completer(self):
        """Update completer with predicates from the loaded program."""
        if hasattr(self.engine, "program") and self.engine.program:
            for clause in self.engine.program.clauses:
                if hasattr(clause.head, "functor"):
                    functor = clause.head.functor
                    arity = len(clause.head.args) if hasattr(clause.head, "args") else 0
                    self.completer.add_predicate(functor, arity)

    def execute_query(self, query_text: str) -> dict[str, Any]:
        """Execute a single query and return the first solution.

        Args:
            query_text: The Prolog query to execute

        Returns:
            Dictionary with 'success' and optional 'bindings' or 'error'
        """
        try:
            # Ensure engine is properly configured if trace properties were set directly
            if self.trace_enabled and (
                not hasattr(self, "engine")
                or not self.engine
                or (self.trace_mode == "collector" and not self.collector_sink)
            ):
                self._recreate_engine()

            # Clean up any previous query state
            self._cleanup_query_state()

            # Execute the query
            solutions = self.engine.query(query_text)

            if solutions:
                return {"success": True, "bindings": solutions[0]}
            else:
                return {"success": False}

        except Exception as e:
            # Handle parse errors and other exceptions
            error_msg = str(e)
            if "parse" in error_msg.lower() or "syntax" in error_msg.lower():
                # Check for common operator mistakes in Stage 1
                if " is " in query_text or "=" in query_text and "=(" not in query_text:
                    return {
                        "success": False,
                        "error": f"Parse error: {error_msg}\n"
                        + "Note: Stage 1 uses operator-free syntax. Examples:\n"
                        + "  - Use: is(X, '+'(2, 3))  instead of: X is 2 + 3\n"
                        + "  - Use: '>'(5, 3)         instead of: 5 > 3\n"
                        + "  - Use: '='(X, Y)         instead of: X = Y",
                    }
                return {"success": False, "error": f"Parse error: {error_msg}"}
            return {"success": False, "error": str(e)}
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
                results.append({"success": True, "bindings": solution})

            return results

        except Exception as e:
            return [{"success": False, "error": str(e)}]
        finally:
            self._cleanup_query_state()

    def execute_query_with_timeout(
        self, query_text: str, timeout_ms: int
    ) -> dict[str, Any]:
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
        self.engine.max_steps = timeout_ms * 100
        self.engine._steps_taken = 0  # Reset step counter

        try:
            self._cleanup_query_state()
            solutions = list(self.engine.query(query_text))

            # Check if we hit the step limit (>= because counter increments before check)
            if self.engine._steps_taken >= self.engine.max_steps:
                return {"success": False, "error": "Query timeout: exceeded step limit"}

            if solutions:
                return {"success": True, "bindings": solutions[0]}
            else:
                return {"success": False}
        except Exception as e:
            return {"success": False, "error": str(e)}
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
                    yield {"success": True, "bindings": solution}

                # If no solutions, yield false
                if not solutions:
                    yield {"success": False}

            except GeneratorExit:
                # User stopped with '.' or generator was closed
                pass
            except Exception as e:
                yield {"success": False, "error": str(e)}
        finally:
            # Always cleanup, even if generator is closed early
            self._cleanup_query_state()

    def _cleanup_query_state(self):
        """Clean up engine state after query execution."""
        # Reset engine stacks and trail
        if hasattr(self.engine, "cp_stack"):
            self.engine.cp_stack.clear()
        if hasattr(self.engine, "goal_stack"):
            while self.engine.goal_stack.height() > 0:
                self.engine.goal_stack.pop()
        if hasattr(self.engine, "frame_stack"):
            self.engine.frame_stack.clear()
        if hasattr(self.engine, "trail"):
            self.engine.trail.unwind_to(0, self.engine.store)

    def format_solution(self, result: dict[str, Any]) -> str:
        """Format a solution for display.

        Args:
            result: Solution dictionary

        Returns:
            Formatted string
        """
        if not result["success"]:
            return "false"

        bindings = result.get("bindings", {})
        if not bindings:
            return "true"

        # Sort variable names for deterministic output
        sorted_vars = sorted(bindings.keys())
        parts = []
        for var in sorted_vars:
            value = bindings[var]
            formatted_value = self.pretty_print(value)
            parts.append(f"{var} = {formatted_value}")

        return "\n".join(parts)

    def format_all_solutions(self, results: list[dict[str, Any]]) -> str:
        """Format all solutions for display.

        Args:
            results: List of solution dictionaries

        Returns:
            Formatted string
        """
        if not results:
            return "false"

        if all(not r["success"] for r in results):
            return "false"

        formatted = []
        for i, result in enumerate(results):
            if result["success"]:
                solution = self.format_solution(result)
                formatted.append(f"Solution {i+1}:\n{solution}")

        if not formatted:
            return "false"

        output = "\n\n".join(formatted)
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
            if term.hint == "_":
                return "_"
            elif term.hint:
                return term.hint
            else:
                return f"_G{term.id}"
        elif isinstance(term, List):
            # Flatten list spines so we always show [a, b, c] for proper lists
            items, tail = self._flatten_list(term)

            # Heuristic: pretty-print rectangular nested lists (matrices) across lines
            matrix_rows = self._detect_matrix_rows(items)
            if matrix_rows is not None and tail == Atom("[]"):
                return self._format_matrix(matrix_rows)

            if items:
                items_str = ", ".join(self.pretty_print(item) for item in items)
                if tail != Atom("[]"):
                    return f"[{items_str}|{self.pretty_print(tail)}]"
                return f"[{items_str}]"
            return "[]"
        elif isinstance(term, Struct):
            if term.args:
                args_str = ", ".join(self.pretty_print(arg) for arg in term.args)
                return f"{term.functor}({args_str})"
            return term.functor
        else:
            # Fallback to the pretty function from ast.pretty if available
            try:
                return pretty(term)
            except Exception:
                return str(term)

    def _flatten_list(self, lst: List) -> tuple[list[Term], Term]:
        """Flatten a possibly spined Prolog list into items and final tail.

        This turns nested cons-like representations into a flat item list so
        REPL output shows canonical [a, b, c] rather than [a|[b|[c|[]]]].
        """
        items: list[Term] = []
        current: Term = lst
        while isinstance(current, List):
            if current.items:
                items.extend(current.items)
            if isinstance(current.tail, List):
                current = current.tail
            else:
                return items, current.tail
        # If input wasn't a List (defensive), return as-is
        return items, current

    def _detect_matrix_rows(self, items: list[Term]) -> Optional[list[list[Term]]]:
        """Detect rectangular nested lists suitable for multi-line matrix formatting.

        Returns a list of rows (each a list of Terms) if and only if:
        - All top-level items are proper lists with empty tails
        - All rows have equal length (rectangular)
        - Dimensions are at least 3x3 (to avoid changing small nested list formatting)
        Otherwise returns None.
        """
        if not items:
            return None

        rows: list[list[Term]] = []
        expected_len: Optional[int] = None
        for it in items:
            if not isinstance(it, List):
                return None
            row_items, row_tail = self._flatten_list(it)
            if row_tail != Atom("[]"):
                return None
            if expected_len is None:
                expected_len = len(row_items)
                if expected_len == 0:
                    return None
            elif len(row_items) != expected_len:
                return None
            rows.append(row_items)

        # Only switch to matrix formatting for reasonably large grids
        if len(rows) >= 3 and (expected_len or 0) >= 3:
            return rows
        return None

    def _format_matrix(self, rows: list[list[Term]]) -> str:
        """Format a rectangular nested list as a multi-line matrix for readability."""
        formatted_rows = []
        for row in rows:
            row_str = ", ".join(self.pretty_print(cell) for cell in row)
            formatted_rows.append(f"  [{row_str}]")
        return "[\n" + ",\n".join(formatted_rows) + "\n]"

    def parse_command(self, input_text: str) -> dict[str, str]:
        """Parse user input to determine command type.

        Args:
            input_text: Raw user input

        Returns:
            Dictionary with 'type' and relevant data
        """
        # Keep original to check for trailing period later
        original_input = input_text.strip()
        # Remove period for command checking
        input_text = original_input.rstrip(".")

        if not input_text:
            return {"type": "empty"}

        # Normalize for case-insensitive matching
        input_lower = input_text.lower()

        # Check for help
        if input_lower == "help":
            return {"type": "help"}

        # Check for quit
        if input_lower in ["quit", "exit", "halt"]:
            return {"type": "quit"}

        # Check for trace commands
        if input_lower.startswith("trace "):
            parts = input_text.split(None, 2)
            if len(parts) >= 2:
                action = parts[1].lower()
                if action == "on":
                    return {"type": "trace", "action": "on"}
                elif action == "off":
                    return {"type": "trace", "action": "off"}
                elif action == "sample" and len(parts) == 3:
                    try:
                        rate = int(parts[2])
                        return {"type": "trace", "action": "sample", "rate": rate}
                    except ValueError:
                        pass
                elif action == "collector":
                    return {"type": "trace", "action": "collector"}
                elif action in ["json", "pretty"] and len(parts) == 3:
                    return {"type": "trace", "action": action, "file": parts[2]}

        # Check for spy commands (handle tabs as well as spaces)
        if input_lower == "spys":
            return {"type": "spy", "action": "list"}
        elif input_lower == "untrace":
            return {"type": "spy", "action": "clear"}
        elif (
            input_lower.startswith("spy")
            and len(input_text) > 3
            and input_text[3].isspace()
        ):
            pred = input_text[3:].strip()
            return {"type": "spy", "action": "add", "predicate": pred}
        elif (
            input_lower.startswith("unspy")
            and len(input_text) > 5
            and input_text[5].isspace()
        ):
            pred = input_text[5:].strip()
            return {"type": "spy", "action": "remove", "predicate": pred}

        # Check for debug commands
        if input_lower == "snapshot":
            return {"type": "debug", "action": "snapshot"}
        elif input_lower == "metrics on":
            return {"type": "debug", "action": "metrics_on"}
        elif input_lower == "metrics off":
            return {"type": "debug", "action": "metrics_off"}
        elif input_lower == "metrics reset":
            return {"type": "debug", "action": "metrics_reset"}
        elif input_lower == "metrics":
            return {"type": "debug", "action": "metrics"}

        # Check for consult(user) interactive mode
        if re.match(r"^consult\s*\(\s*user\s*\)\s*\.?$", input_lower):
            return {"type": "consult_user"}

        # Check for consult
        consult_match = re.match(
            r'consult\s*\(\s*[\'"]([^\'"]*)[\'"]\s*\)\s*\.?', original_input
        )
        if consult_match:
            return {"type": "consult", "file": consult_match.group(1)}

        # Program listing: listing. or listing(Name/Arity). Also accept 'list'.
        list_match = re.match(
            r"^(?:listing|list)\s*(?:\(\s*([a-z][a-zA-Z0-9_]*)\s*/\s*(\d+)\s*\))?\s*$",
            input_text,
        )
        if list_match:
            pred_name = list_match.group(1)
            pred_arity_str = list_match.group(2)
            pred_arity = int(pred_arity_str) if pred_arity_str else None
            return {"type": "listing", "name": pred_name, "arity": pred_arity}

        # Check for query (with or without ?- prefix)
        # Allow trailing comments after the period by stripping comments first
        query_text = self._strip_comments(original_input)
        if query_text.startswith("?-"):
            query_text = query_text[2:].strip()

        # Check if query is complete (has trailing period)
        if not query_text.endswith("."):
            return {"type": "incomplete", "content": query_text}

        # Remove trailing period and treat as query
        query = query_text[:-1].strip()
        return {"type": "query", "content": query}

    def _strip_comments(self, s: str) -> str:
        """Strip Prolog line and block comments from a single REPL input line.

        - Removes '%' line comments (outside quoted atoms and blocks)
        - Removes '/* ... */' block comments (non-nested) across the string
        - Preserves content inside quoted atoms
        """
        out = []
        i = 0
        n = len(s)
        in_quote = False
        in_block = False
        while i < n:
            ch = s[i]
            nxt = s[i + 1] if i + 1 < n else ""

            if in_block:
                # Look for block comment end */
                if ch == "*" and nxt == "/":
                    in_block = False
                    i += 2
                    continue
                i += 1
                continue

            if in_quote:
                out.append(ch)
                if ch == "\\":
                    # Escape next character inside quotes
                    if i + 1 < n:
                        out.append(s[i + 1])
                        i += 2
                        continue
                elif ch == "'":
                    in_quote = False
                i += 1
                continue

            # Not in quote or block comment
            if ch == "'":
                in_quote = True
                out.append(ch)
                i += 1
                continue

            # Start of block comment
            if ch == "/" and nxt == "*":
                in_block = True
                i += 2
                continue

            # Line comment starts with % (outside quotes/blocks) -> stop here
            if ch == "%":
                break

            out.append(ch)
            i += 1

        return "".join(out).strip()

    def _consult_user(self) -> bool:
        """Interactive clause entry mode: consult(user).

        Reads clauses from the user until EOF (Ctrl-D). Appends them to the
        current program using engine.consult_string without recreating the engine,
        so tracing and spypoints persist.
        """
        print("Entering consult(user). Type clauses, end with Ctrl-D (EOF).")
        lines: list[str] = []
        while True:
            try:
                line = self.session.prompt("|: ")
            except EOFError:
                # Finish on Ctrl-D
                print("")
                break
            # Allow a single '.' on its own line to finish as well (quality of life)
            if line.strip() == ".":
                break
            lines.append(line)

        text = "\n".join(lines).strip()
        if not text:
            print("true.")
            return True

        try:
            self.engine.consult_string(text)
            # Keep self.program in sync (engine persists)
            self.program = self.engine.program
            # Update completer with new predicates
            self._update_completer()
            print("true.")
            return True
        except Exception as e:
            print(f"Error: {e}")
            return False

    def execute_trace_command(self, cmd: dict) -> bool:
        """Execute a trace command.

        Args:
            cmd: Parsed command with 'action' and optional parameters

        Returns:
            True if successful
        """
        action = cmd.get("action")

        if action == "on":
            self.trace_enabled = True
            self.trace_mode = "pretty"
            self._recreate_engine()
            print("Tracing enabled (pretty format to stdout)")
            return True
        elif action == "off":
            self.trace_enabled = False
            self.trace_file = None  # Clear file reference when disabling
            if hasattr(self, "_trace_file_handle"):
                self._trace_file_handle.close()
                del self._trace_file_handle
            self._recreate_engine()
            print("Tracing disabled")
            return True
        elif action == "json":
            try:
                file_path = cmd.get("file")
                # Open file for JSON output
                self._trace_file_handle = open(file_path, "w")
                self.trace_enabled = True
                self.trace_mode = "json"
                self.trace_file = file_path
                self._recreate_engine()
                print(f"Tracing enabled (JSON format to {file_path})")
                return True
            except IOError as e:
                print(f"Error opening trace file: {e}")
                return False
        elif action == "pretty":
            try:
                file_path = cmd.get("file")
                # Open file for pretty output
                self._trace_file_handle = open(file_path, "w")
                self.trace_enabled = True
                self.trace_mode = "pretty"
                self.trace_file = file_path
                self._recreate_engine()
                print(f"Tracing enabled (pretty format to {file_path})")
                return True
            except IOError as e:
                print(f"Error opening trace file: {e}")
                return False
        elif action == "sample":
            self.trace_enabled = True
            self.trace_sample_rate = cmd.get("rate", 1)
            self._recreate_engine()
            print(f"Trace sampling enabled (1 in {self.trace_sample_rate} events)")
            return True
        elif action == "collector":
            self.trace_enabled = True
            self.trace_mode = "collector"
            self._recreate_engine()
            print("Tracing enabled (collector mode for testing)")
            return True

        return False

    def _recreate_engine(self):
        """Recreate the engine with current trace/debug settings."""

        # Sync with engine if it has additional clauses from direct consult_string calls
        # Only sync if engine has more clauses than REPL program (avoid overwriting REPL updates)
        if hasattr(self.engine, "program") and len(self.engine.program.clauses) > len(
            self.program.clauses
        ):
            self.program = self.engine.program

        # Create new engine with trace settings
        self.engine = Engine(
            self.program, trace=self.trace_enabled, metrics=self.metrics_enabled
        )

        if self.trace_enabled and self.engine.tracer:
            # Create and configure TraceFilters
            filters = TraceFilters(
                spypoints=self.spypoints, sampling_rate=self.trace_sample_rate
            )

            # Set filters on the engine tracer
            self.engine.set_trace_filters(filters)

            # Add appropriate sink
            if self.trace_mode == "json" and hasattr(self, "_trace_file_handle"):
                sink = JSONLTraceSink(output=self._trace_file_handle, batch_size=1)
                self.engine.tracer.add_sink(sink)
            elif (
                self.trace_mode == "pretty"
                and self.trace_file
                and hasattr(self, "_trace_file_handle")
            ):
                sink = PrettyTraceSink(output=self._trace_file_handle, batch_size=1)
                self.engine.tracer.add_sink(sink)
            elif self.trace_mode == "pretty":
                # Default to stdout with immediate output for REPL
                sink = PrettyTraceSink(batch_size=1)
                self.engine.tracer.add_sink(sink)
            elif self.trace_mode == "collector":
                # For testing
                self.collector_sink = CollectorSink()
                self.engine.tracer.add_sink(self.collector_sink)

    def execute_spy_command(self, cmd: dict) -> bool:
        """Execute a spy command.

        Args:
            cmd: Parsed command with 'action' and optional parameters

        Returns:
            True if successful
        """
        action = cmd.get("action")

        if action == "add":
            pred = cmd.get("predicate")
            if pred and "/" in pred:
                self.spypoints.add(pred)
                if self.trace_enabled:
                    self._recreate_engine()  # Update filtering
                print(f"Spypoint added: {pred}")
                return True
            else:
                print(
                    f"Error: Invalid predicate format '{pred}'. Use name/arity format."
                )
                return False
        elif action == "remove":
            pred = cmd.get("predicate")
            if pred in self.spypoints:
                self.spypoints.discard(pred)
                if self.trace_enabled:
                    self._recreate_engine()  # Update filtering
                print(f"Spypoint removed: {pred}")
            else:
                print(f"Spypoint not found: {pred}")
            return True
        elif action == "list":
            if self.spypoints:
                print("Active spypoints:")
                for sp in sorted(self.spypoints):
                    print(f"  {sp}")
            else:
                print("No active spypoints")
            return True
        elif action == "clear":
            self.spypoints.clear()
            if self.trace_enabled:
                self._recreate_engine()  # Update filtering
            print("All spypoints cleared")
            return True

        return False

    def execute_debug_command(self, cmd: dict) -> bool:
        """Execute a debug command.

        Args:
            cmd: Parsed command with 'action' and optional parameters

        Returns:
            True if successful
        """
        action = cmd.get("action")

        if action == "snapshot":
            print("Engine State Snapshot")
            print("-" * 40)
            if hasattr(self.engine, "store"):
                print(f"Store size: {self.engine.store.size()}")
                # Could add more detail here if needed
                if hasattr(self.engine.store, "cells"):
                    # cells is a list, not a dict
                    bound_count = sum(
                        1 for cell in self.engine.store.cells if cell.term is not None
                    )
                    print(f"Bound variables: {bound_count}")
            if hasattr(self.engine, "trail"):
                trail_size = (
                    self.engine.trail.size()
                    if hasattr(self.engine.trail, "size")
                    else 0
                )
                print(f"Trail size: {trail_size}")
            if hasattr(self.engine, "goal_stack"):
                print(f"Goal stack height: {self.engine.goal_stack.height()}")
            if hasattr(self.engine, "cp_stack"):
                print(f"Choicepoint stack size: {len(self.engine.cp_stack)}")
            print("-" * 40)
            return True
        elif action == "metrics":
            if (
                self.metrics_enabled
                and hasattr(self.engine, "metrics")
                and self.engine.metrics
            ):
                print("Engine Metrics")
                print("-" * 40)
                metrics_dict = self.engine.metrics.to_dict()

                # Display global metrics
                if "global" in metrics_dict:
                    print("Global counters:")
                    for name, value in metrics_dict["global"].items():
                        print(f"  {name}: {value}")

                # Display per-predicate metrics
                if "predicates" in metrics_dict and metrics_dict["predicates"]:
                    print("\nPer-predicate statistics:")
                    for pred_id, stats in metrics_dict["predicates"].items():
                        print(f"  {pred_id}:")
                        for stat_name, value in stats.items():
                            print(f"    {stat_name}: {value}")
                print("-" * 40)
            else:
                print("Metrics not enabled. Use 'metrics on' to enable.")
            return True
        elif action == "metrics_reset":
            if hasattr(self.engine, "metrics") and self.engine.metrics:
                self.engine.metrics.reset()
                print("Metrics reset")
            else:
                print("Metrics not available")
            return True
        elif action == "metrics_on":
            self.metrics_enabled = True
            self._recreate_engine()
            print("Metrics enabled")
            return True
        elif action == "metrics_off":
            self.metrics_enabled = False
            self._recreate_engine()
            print("Metrics disabled")
            return True

        return False

    def load_string(self, program_text: str):
        """Load a Prolog program from string.

        Args:
            program_text: Prolog program text
        """

        try:
            new_clauses = parse_program(program_text)

            # Add to existing program - need to create new tuple
            existing_clauses = (
                list(self.program.clauses) if self.program.clauses else []
            )
            existing_clauses.extend(new_clauses)
            self.program = Program(tuple(existing_clauses))

            # Recreate engine with updated program
            self._recreate_engine()

        except Exception as e:
            print(f"Error loading program: {e}")

    def get_help_text(self) -> str:
        """Get help text for the REPL."""
        return """
PyLog REPL Commands:
    ?- <query>.          Execute a Prolog query
    consult('file.pl').  Load a Prolog file
    consult(user).       Enter interactive clause input (Ctrl-D to finish)
    listing.             List all loaded clauses
    listing(name/N).     List clauses for name/N
    help.                Show this help message
    quit.                Exit the REPL

Trace Commands:
    trace on             Enable tracing
    trace off            Disable tracing
    trace json FILE      Output JSON trace to file
    trace pretty FILE    Output pretty trace to file
    trace sample N       Sample 1 in N events

Spy Commands:
    spy pred/N           Add spypoint for predicate
    unspy pred/N         Remove spypoint
    spys                 List active spypoints
    untrace              Clear all spypoints

Debug Commands:
    snapshot             Display engine state
    metrics              Show performance metrics
    metrics reset        Clear metrics

During query results:
    ;                    Show next solution
    .                    Stop searching for solutions

Examples:
    ?- parent(X, Y).     Find parent relationships
    ?- member(2, [1,2,3]). Check list membership

Runtime Database:
    dynamic(Name/Arity)      Declare predicate dynamic
    assertz(Clause)          Append a clause (fact or Head :- Body)
    asserta(Clause)          Prepend a clause
    retract(Clause)          Remove one matching clause (variables bind)
    retractall(HeadOrClause) Remove all matching clauses (no bindings)
    abolish(Name/Arity)      Remove all clauses for a predicate
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
                    prompt = "|    "  # Continuation prompt
                else:
                    prompt = "?- "

                user_input = self.session.prompt(prompt)

                # Handle incomplete queries
                if incomplete_query:
                    user_input = incomplete_query + " " + user_input
                    incomplete_query = ""

                # Parse command
                cmd = self.parse_command(user_input)

                if cmd["type"] == "empty":
                    continue

                elif cmd["type"] == "incomplete":
                    # Save for continuation
                    incomplete_query = "?- " + cmd["content"]
                    continue

                elif cmd["type"] == "quit":
                    print("Goodbye!")
                    break

                elif cmd["type"] == "help":
                    print(self.get_help_text())

                elif cmd["type"] == "consult":
                    self.load_file(cmd["file"])
                elif cmd["type"] == "consult_user":
                    self._consult_user()

                elif cmd["type"] == "trace":
                    self.execute_trace_command(cmd)

                elif cmd["type"] == "spy":
                    self.execute_spy_command(cmd)

                elif cmd["type"] == "debug":
                    self.execute_debug_command(cmd)

                elif cmd["type"] == "listing":
                    self.execute_listing_command(cmd)

                elif cmd["type"] == "query":
                    self._handle_query(cmd["content"])

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
                if result["success"]:
                    print(self.format_solution(result))
                    solution_count += 1

                    # Check if there might be more solutions
                    try:
                        # Peek at next solution
                        next_result = next(generator)

                        # Ask user for continuation
                        try:
                            cont = self.session.prompt(" ", default=";")
                            if cont.strip() == ".":
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
                    if "error" in result:
                        print(f"Error: {result['error']}")
                    else:
                        print("false.")
                    break

        except Exception as e:
            print(f"Error: {e}")
        finally:
            self._cleanup_query_state()

    def execute_listing_command(self, cmd: dict) -> bool:
        """List loaded clauses, optionally for a specific predicate.

        Args:
            cmd: Dict with optional keys 'name' and 'arity'.

        Returns:
            True if executed.
        """
        try:
            program = getattr(self, "program", None) or getattr(
                self.engine, "program", None
            )
            clauses = list(program.clauses) if program and program.clauses else []

            if not clauses:
                print("% No user-defined clauses loaded.")
                return True

            name = cmd.get("name")
            arity = cmd.get("arity")

            def head_id(cl):
                h = cl.head
                if isinstance(h, Atom):
                    return (h.name, 0)
                elif isinstance(h, Struct):
                    return (h.functor, len(h.args))
                return (None, None)

            if name is not None and arity is not None:
                clauses = [cl for cl in clauses if head_id(cl) == (name, arity)]
                if not clauses:
                    print(f"% No clauses for {name}/{arity}.")
                    return True

            for cl in clauses:
                print(pretty_clause(cl))
            return True
        except Exception as e:
            print(f"Error: {e}")
            return False

    def add_to_history(self, command: str):
        """Add a command to history.

        Args:
            command: Command to add
        """
        if self.session and self.session.history:
            # Use store_string to persist immediately to disk
            if hasattr(self.session.history, "store_string"):
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
            if hasattr(self.session.history, "load_history_strings"):
                # Load and reverse (FileHistory returns newest first)
                strings = list(self.session.history.load_history_strings())
                strings.reverse()
                return strings
            else:
                # For in-memory history
                return list(self.session.history.get_strings())
        return []


def main():
    """Main entry point for the REPL and simple CLI.

    Usage examples:
      - Interactive REPL:              pylog
      - Consult file then REPL:        pylog path/to/file.pl
      - Run a goal and exit:           pylog path/to/file.pl -g "member(X,[1,2])" --once --noninteractive
      - Run with tracing to stdout:    pylog -g "append([1],[2],X)" --once --trace --noninteractive
    """
    parser = argparse.ArgumentParser(prog="pylog", add_help=True)
    parser.add_argument(
        "files", nargs="*", help="Prolog files to consult before starting"
    )
    parser.add_argument(
        "-g",
        "--goal",
        dest="goal",
        help="Query to run (omit ?- and trailing .)",
    )
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--once", action="store_true", help="Return only the first solution"
    )
    group.add_argument("--all", action="store_true", help="Print all solutions")
    parser.add_argument(
        "--noninteractive",
        action="store_true",
        help="Do not start the REPL after running the goal",
    )
    parser.add_argument(
        "--trace", action="store_true", help="Enable pretty tracing to stdout"
    )

    # If no CLI args other than the program name, drop into REPL
    if len(sys.argv) == 1:
        repl = PrologREPL()
        repl.run_session()
        return

    args = parser.parse_args()

    repl = PrologREPL()

    # Optional tracing
    if args.trace:
        try:
            repl.execute_trace_command({"action": "on"})
        except Exception:
            pass

    # Consult files (if any)
    for file_path in args.files:
        repl.load_file(file_path)

    # Execute goal if provided
    if args.goal:
        if args.all:
            results = repl.execute_query_all(args.goal)
            printed_any = False
            for res in results:
                if res.get("success"):
                    bindings = res.get("bindings", {})
                    if bindings:
                        line = ", ".join(
                            f"{k} = {pretty(v)}" for k, v in bindings.items()
                        )
                        print(line)
                    else:
                        print("true.")
                    printed_any = True
                else:
                    print(f"Error: {res.get('error', 'unknown error')}")
                    printed_any = True
            if not printed_any:
                print("false.")
        else:
            # Default to once if --all not set
            res = repl.execute_query(args.goal)
            if res.get("success"):
                bindings = res.get("bindings", {})
                if bindings:
                    print(", ".join(f"{k} = {pretty(v)}" for k, v in bindings.items()))
                else:
                    print("true.")
            else:
                err = res.get("error")
                if err:
                    print(f"Error: {err}")
                else:
                    print("false.")

        if args.noninteractive:
            return

    # Fall back to interactive session
    repl.run_session()


if __name__ == "__main__":
    main()
