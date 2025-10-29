"""Unit tests for ISO Prolog stream I/O predicates.

Tests the implementation of ISO-compliant stream operations including:
- open/3, close/1-2
- read/1-2, write/1-2
- nl/0-1
- current_output/1
- stream_property/2
"""

from __future__ import annotations

import pytest

from prolog.ast.terms import Atom, Int, Var, Struct, List
from prolog.ast.clauses import Program
from prolog.engine.engine import Engine
from prolog.engine.errors import PrologThrow
from prolog.engine.builtins.streams import StreamManager, VirtualFS


@pytest.mark.iso
class TestVirtualFS:
    """Test the virtual filesystem for deterministic testing."""

    def test_virtual_fs_initialization(self):
        """Virtual FS should contain required test files."""
        vfs = VirtualFS()

        assert "nowrite" in vfs.files
        assert "empty" in vfs.files
        assert "hello" in vfs.files
        assert "scowen" in vfs.files

    def test_nowrite_permissions(self):
        """nowrite file should be read-only."""
        vfs = VirtualFS()
        content, perms = vfs.files["nowrite"]

        # Check it's read-only (0o444 = r--r--r--)
        assert perms == 0o444
        assert content == b"content"

    def test_empty_file(self):
        """empty file should be empty but writable."""
        vfs = VirtualFS()
        content, perms = vfs.files["empty"]

        assert content == b""
        assert perms == 0o644  # rw-r--r--

    def test_hello_file(self):
        """hello file should contain 'hello\\n'."""
        vfs = VirtualFS()
        content, perms = vfs.files["hello"]

        assert content == b"hello\\n"
        assert perms == 0o644


@pytest.mark.iso
class TestStreamManager:
    """Test stream management and lifecycle."""

    def test_stream_manager_initialization(self):
        """StreamManager should initialize with standard streams."""
        sm = StreamManager()

        assert "user_input" in sm.aliases
        assert "user_output" in sm.aliases
        assert "user_error" in sm.aliases

        assert sm.current_input == "user_input"
        assert sm.current_output == "user_output"

    def test_unique_stream_ids(self):
        """Each stream should get a unique ID."""
        sm = StreamManager()
        vfs = VirtualFS()

        stream1 = sm.open_stream("test1.txt", "write", vfs)
        stream2 = sm.open_stream("test2.txt", "write", vfs)

        assert stream1 != stream2
        assert stream1 in sm.streams
        assert stream2 in sm.streams


@pytest.mark.iso
class TestOpenClose:
    """Test open/3 and close/1-2 predicates."""

    def test_open_write_mode(self):
        """open/3 should create writable stream."""
        engine = Engine(Program(()))

        # open('test.txt', write, S)
        solutions = engine.run(
            [Struct("open", (Atom("test.txt"), Atom("write"), Var(0, "S")))]
        )

        assert len(solutions) >= 1
        assert "S" in solutions[0]
        stream = solutions[0]["S"]
        assert stream is not None

    def test_open_read_mode_existing(self):
        """open/3 should open existing file for reading."""
        engine = Engine(Program(()))

        # open('hello', read, S) - hello exists in VirtualFS
        solutions = engine.run(
            [Struct("open", (Atom("hello"), Atom("read"), Var(0, "S")))]
        )

        assert len(solutions) >= 1
        assert "S" in solutions[0]
        stream = solutions[0]["S"]
        assert stream is not None

    def test_open_read_mode_nonexistent(self):
        """open/3 should throw existence_error for missing file in read mode."""
        engine = Engine(Program(()))

        with pytest.raises(PrologThrow) as exc_info:
            engine.run(
                [Struct("open", (Atom("nonexistent.txt"), Atom("read"), Var(0, "S")))]
            )

        ball = exc_info.value.ball
        assert isinstance(ball, Struct) and ball.functor == "error"
        error = ball.args[0]
        assert isinstance(error, Struct) and error.functor == "existence_error"
        assert error.args[0] == Atom("source_sink")
        assert error.args[1] == Atom("nonexistent.txt")

    def test_open_invalid_mode(self):
        """open/3 should throw domain_error for invalid mode."""
        engine = Engine(Program(()))

        with pytest.raises(PrologThrow) as exc_info:
            engine.run(
                [Struct("open", (Atom("test.txt"), Atom("invalid"), Var(0, "S")))]
            )

        ball = exc_info.value.ball
        assert isinstance(ball, Struct) and ball.functor == "error"
        error = ball.args[0]
        assert isinstance(error, Struct) and error.functor == "domain_error"
        assert error.args[0] == Atom("io_mode")
        assert error.args[1] == Atom("invalid")

    def test_open_instantiation_error(self):
        """open/3 should throw instantiation_error if File or Mode unbound."""
        engine = Engine(Program(()))

        # open(_, read, S) - unbound file
        with pytest.raises(PrologThrow) as exc_info:
            engine.run([Struct("open", (Var(0, "_"), Atom("read"), Var(1, "S")))])

        ball = exc_info.value.ball
        assert isinstance(ball, Struct) and ball.functor == "error"
        error = ball.args[0]
        assert error == Atom("instantiation_error")

    def test_open_type_error(self):
        """open/3 should throw type_error if File not atom."""
        engine = Engine(Program(()))

        # open(123, read, S) - integer instead of atom
        with pytest.raises(PrologThrow) as exc_info:
            engine.run([Struct("open", (Int(123), Atom("read"), Var(0, "S")))])

        ball = exc_info.value.ball
        assert isinstance(ball, Struct) and ball.functor == "error"
        error = ball.args[0]
        assert isinstance(error, Struct) and error.functor == "type_error"
        assert error.args[0] == Atom("atom")
        assert error.args[1] == Int(123)

    def test_open_permission_error(self):
        """open/3 should throw permission_error for read-only file in write mode."""
        engine = Engine(Program(()))

        # open('nowrite', write, S) - nowrite is read-only
        with pytest.raises(PrologThrow) as exc_info:
            engine.run([Struct("open", (Atom("nowrite"), Atom("write"), Var(0, "S")))])

        ball = exc_info.value.ball
        assert isinstance(ball, Struct) and ball.functor == "error"
        error = ball.args[0]
        assert isinstance(error, Struct) and error.functor == "permission_error"
        assert error.args[0] == Atom("open")
        assert error.args[1] == Atom("source_sink")
        assert error.args[2] == Atom("nowrite")

    def test_close_stream(self):
        """close/1 should close an open stream."""
        engine = Engine(Program(()))

        # open('test.txt', write, S), close(S)
        solutions = engine.run(
            [
                Struct(
                    ",",
                    (
                        Struct("open", (Atom("test.txt"), Atom("write"), Var(0, "S"))),
                        Struct("close", (Var(0, "S"),)),
                    ),
                )
            ]
        )

        assert len(solutions) >= 1

    def test_close_nonexistent_stream(self):
        """close/1 should throw existence_error for non-existent stream."""
        engine = Engine(Program(()))

        with pytest.raises(PrologThrow) as exc_info:
            engine.run([Struct("close", (Atom("fake_stream"),))])

        ball = exc_info.value.ball
        assert isinstance(ball, Struct) and ball.functor == "error"
        error = ball.args[0]
        assert isinstance(error, Struct) and error.functor == "existence_error"
        assert error.args[0] == Atom("stream")


@pytest.mark.iso
class TestReadWrite:
    """Test read/1-2 and write/1-2 predicates."""

    def test_write_read_roundtrip(self):
        """Writing then reading should preserve terms."""
        engine = Engine(Program(()))

        # open('test.txt', write, S), write(S, hello(world)), close(S),
        # open('test.txt', read, S2), read(S2, Term), close(S2), Term = hello(world)
        query = Struct(
            ",",
            (
                Struct("open", (Atom("test.txt"), Atom("write"), Var(0, "S"))),
                Struct(
                    ",",
                    (
                        Struct(
                            "write", (Var(0, "S"), Struct("hello", (Atom("world"),)))
                        ),
                        Struct(
                            ",",
                            (
                                Struct("close", (Var(0, "S"),)),
                                Struct(
                                    ",",
                                    (
                                        Struct(
                                            "open",
                                            (
                                                Atom("test.txt"),
                                                Atom("read"),
                                                Var(1, "S2"),
                                            ),
                                        ),
                                        Struct(
                                            ",",
                                            (
                                                Struct(
                                                    "read",
                                                    (Var(1, "S2"), Var(2, "Term")),
                                                ),
                                                Struct(
                                                    ",",
                                                    (
                                                        Struct(
                                                            "close", (Var(1, "S2"),)
                                                        ),
                                                        Struct(
                                                            "=",
                                                            (
                                                                Var(2, "Term"),
                                                                Struct(
                                                                    "hello",
                                                                    (Atom("world"),),
                                                                ),
                                                            ),
                                                        ),
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1

    def test_read_end_of_file(self):
        """read/2 should return end_of_file at EOF."""
        engine = Engine(Program(()))

        # open('empty', read, S), read(S, Term), close(S), Term = end_of_file
        query = Struct(
            ",",
            (
                Struct("open", (Atom("empty"), Atom("read"), Var(0, "S"))),
                Struct(
                    ",",
                    (
                        Struct("read", (Var(0, "S"), Var(1, "Term"))),
                        Struct(
                            ",",
                            (
                                Struct("close", (Var(0, "S"),)),
                                Struct("=", (Var(1, "Term"), Atom("end_of_file"))),
                            ),
                        ),
                    ),
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1

    def test_write_to_current_output(self):
        """write/1 should write to current output stream."""
        engine = Engine(Program(()))

        # write(hello) - should succeed without specifying stream
        solutions = engine.run([Struct("write", (Atom("hello"),))])

        assert len(solutions) >= 1

    def test_read_permission_error(self):
        """read/2 should throw permission_error on write-only stream."""
        engine = Engine(Program(()))

        # open('test.txt', write, S), read(S, Term)
        with pytest.raises(PrologThrow) as exc_info:
            engine.run(
                [
                    Struct(
                        ",",
                        (
                            Struct(
                                "open", (Atom("test.txt"), Atom("write"), Var(0, "S"))
                            ),
                            Struct("read", (Var(0, "S"), Var(1, "Term"))),
                        ),
                    )
                ]
            )

        ball = exc_info.value.ball
        assert isinstance(ball, Struct) and ball.functor == "error"
        error = ball.args[0]
        assert isinstance(error, Struct) and error.functor == "permission_error"
        assert error.args[0] == Atom("input")
        assert error.args[1] == Atom("stream")

    def test_write_permission_error(self):
        """write/2 should throw permission_error on read-only stream."""
        engine = Engine(Program(()))

        # open('hello', read, S), write(S, test)
        with pytest.raises(PrologThrow) as exc_info:
            engine.run(
                [
                    Struct(
                        ",",
                        (
                            Struct("open", (Atom("hello"), Atom("read"), Var(0, "S"))),
                            Struct("write", (Var(0, "S"), Atom("test"))),
                        ),
                    )
                ]
            )

        ball = exc_info.value.ball
        assert isinstance(ball, Struct) and ball.functor == "error"
        error = ball.args[0]
        assert isinstance(error, Struct) and error.functor == "permission_error"
        assert error.args[0] == Atom("output")
        assert error.args[1] == Atom("stream")


@pytest.mark.iso
class TestNewline:
    """Test nl/0-1 predicate."""

    def test_nl_current_output(self):
        """nl/0 should write newline to current output."""
        engine = Engine(Program(()))

        solutions = engine.run([Atom("nl")])
        assert len(solutions) >= 1

    def test_nl_specific_stream(self):
        """nl/1 should write newline to specific stream."""
        engine = Engine(Program(()))

        # open('test.txt', write, S), nl(S), close(S)
        query = Struct(
            ",",
            (
                Struct("open", (Atom("test.txt"), Atom("write"), Var(0, "S"))),
                Struct(
                    ",", (Struct("nl", (Var(0, "S"),)), Struct("close", (Var(0, "S"),)))
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1


@pytest.mark.iso
class TestCurrentOutput:
    """Test current_output/1 predicate."""

    def test_current_output_unifies(self):
        """current_output/1 should unify with current output stream."""
        engine = Engine(Program(()))

        solutions = engine.run([Struct("current_output", (Var(0, "S"),))])

        assert len(solutions) >= 1
        assert "S" in solutions[0]
        stream = solutions[0]["S"]
        assert stream is not None

    def test_current_output_alias(self):
        """current_output stream should have user_output alias."""
        engine = Engine(Program(()))

        # current_output(S), stream_property(S, alias(user_output))
        query = Struct(
            ",",
            (
                Struct("current_output", (Var(0, "S"),)),
                Struct(
                    "stream_property",
                    (Var(0, "S"), Struct("alias", (Atom("user_output"),))),
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1


@pytest.mark.iso
class TestStreamProperty:
    """Test stream_property/2 predicate."""

    def test_stream_property_file_name(self):
        """stream_property/2 should report file_name for file streams."""
        engine = Engine(Program(()))

        # open('test.txt', write, S), stream_property(S, file_name('test.txt'))
        query = Struct(
            ",",
            (
                Struct("open", (Atom("test.txt"), Atom("write"), Var(0, "S"))),
                Struct(
                    "stream_property",
                    (Var(0, "S"), Struct("file_name", (Atom("test.txt"),))),
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1

    def test_stream_property_mode(self):
        """stream_property/2 should report mode."""
        engine = Engine(Program(()))

        # open('test.txt', write, S), stream_property(S, mode(write))
        query = Struct(
            ",",
            (
                Struct("open", (Atom("test.txt"), Atom("write"), Var(0, "S"))),
                Struct(
                    "stream_property", (Var(0, "S"), Struct("mode", (Atom("write"),)))
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1

    def test_stream_property_alias_standard(self):
        """stream_property/2 should report aliases for standard streams."""
        engine = Engine(Program(()))

        # current_output(S), stream_property(S, alias(user_output))
        query = Struct(
            ",",
            (
                Struct("current_output", (Var(0, "S"),)),
                Struct(
                    "stream_property",
                    (Var(0, "S"), Struct("alias", (Atom("user_output"),))),
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1

    def test_stream_property_end_of_stream(self):
        """stream_property/2 should report end_of_stream status."""
        engine = Engine(Program(()))

        # open('empty', read, S), read(S, _), stream_property(S, end_of_stream(at))
        query = Struct(
            ",",
            (
                Struct("open", (Atom("empty"), Atom("read"), Var(0, "S"))),
                Struct(
                    ",",
                    (
                        Struct("read", (Var(0, "S"), Var(1, "_"))),
                        Struct(
                            "stream_property",
                            (Var(0, "S"), Struct("end_of_stream", (Atom("at"),))),
                        ),
                    ),
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1

    def test_stream_property_backtrack(self):
        """stream_property/2 should backtrack over all properties."""
        engine = Engine(Program(()))

        # open('test.txt', write, S), findall(P, stream_property(S, P), Props)
        query = Struct(
            ",",
            (
                Struct("open", (Atom("test.txt"), Atom("write"), Var(0, "S"))),
                Struct(
                    "findall",
                    (
                        Var(1, "P"),
                        Struct("stream_property", (Var(0, "S"), Var(1, "P"))),
                        Var(2, "Props"),
                    ),
                ),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1

        props = solutions[0]["Props"]
        # Should have at least file_name and mode properties
        assert isinstance(props, List)
        assert len(props.items) >= 2


@pytest.mark.iso
class TestBacktracking:
    """Test stream behavior during backtracking."""

    def test_stream_closed_on_backtrack(self):
        """Streams opened in failed branch should be closed on backtrack."""
        engine = Engine(Program(()))

        # (open('test.txt', write, S), fail) ; true
        # After backtracking, stream should be closed
        query = Struct(
            ";",
            (
                Struct(
                    ",",
                    (
                        Struct("open", (Atom("test.txt"), Atom("write"), Var(0, "S"))),
                        Atom("fail"),
                    ),
                ),
                Atom("true"),
            ),
        )

        solutions = engine.run([query])
        assert len(solutions) >= 1
        # Stream S should not be bound after backtracking
        assert "S" not in solutions[0]
