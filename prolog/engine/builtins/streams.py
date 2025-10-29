"""ISO Prolog stream I/O predicates.

Implements ISO-compliant stream operations including:
- open/3, close/1-2 for stream management
- read/1-2, write/1-2 for term I/O
- nl/0-1 for newline output
- current_output/1 for stream identification
- stream_property/2 for metadata queries

Uses a virtual filesystem for deterministic testing.
"""

from __future__ import annotations

import io
import sys
from dataclasses import dataclass, field
from typing import Dict, List, Optional, TextIO, Tuple

from prolog.ast.terms import Atom, Int, Struct
from prolog.ast.terms import Term
from prolog.parser.reader import Reader
from prolog.ast.pretty import pretty


# Stream types
@dataclass
class Stream:
    """Base class for all stream types."""

    stream_id: str
    mode: str  # 'read', 'write', 'append'
    file_name: Optional[str] = None
    alias: Optional[str] = None
    position: int = 0
    at_end: bool = False

    def close(self):
        """Close the stream."""
        pass


@dataclass
class InMemoryStream(Stream):
    """In-memory stream for virtual filesystem."""

    buffer: io.StringIO = field(default_factory=io.StringIO)

    def close(self):
        """Close the in-memory stream."""
        self.buffer.close()


@dataclass
class StandardStream(Stream):
    """Standard I/O stream (stdin, stdout, stderr)."""

    handle: TextIO = field(default_factory=lambda: sys.stdout)

    def close(self):
        """Standard streams should not be closed."""
        pass  # Don't close standard streams


class VirtualFS:
    """Virtual filesystem for deterministic testing."""

    def __init__(self):
        """Initialize virtual filesystem with test files."""
        self.files: Dict[str, Tuple[bytes, int]] = {
            "nowrite": (b"content", 0o444),  # read-only
            "empty": (b"", 0o644),  # empty but writable
            "hello": (b"hello\n", 0o644),  # contains "hello\n"
            "scowen": (b"test data\n", 0o644),  # test data file
        }
        # Storage for written files
        self.written_files: Dict[str, bytes] = {}

    def exists(self, path: str) -> bool:
        """Check if a file exists."""
        return path in self.files or path in self.written_files

    def is_readable(self, path: str) -> bool:
        """Check if a file is readable."""
        if path in self.files:
            _, perms = self.files[path]
            return (perms & 0o400) != 0
        return path in self.written_files

    def is_writable(self, path: str) -> bool:
        """Check if a file is writable."""
        if path in self.files:
            _, perms = self.files[path]
            return (perms & 0o200) != 0
        return True  # New files are always writable

    def read_file(self, path: str) -> str:
        """Read file contents as string."""
        if path in self.written_files:
            return self.written_files[path].decode("utf-8")
        elif path in self.files:
            content, _ = self.files[path]
            return content.decode("utf-8")
        else:
            raise FileNotFoundError(path)

    def write_file(self, path: str, content: str):
        """Write content to file."""
        self.written_files[path] = content.encode("utf-8")

    def get_content(self, path: str) -> Optional[str]:
        """Get file content if it exists."""
        if path in self.written_files:
            return self.written_files[path].decode("utf-8")
        elif path in self.files:
            content, _ = self.files[path]
            return content.decode("utf-8")
        return None


class StreamManager:
    """Manages stream lifecycle and operations."""

    def __init__(self):
        """Initialize stream manager with standard streams."""
        self.streams: Dict[str, Stream] = {}
        self.aliases: Dict[str, str] = {}
        self.next_id = 0
        self.vfs = VirtualFS()

        # Initialize standard streams
        self._init_standard_streams()

        # Current I/O streams
        self.current_input = "user_input"
        self.current_output = "user_output"

    def _init_standard_streams(self):
        """Initialize standard I/O streams."""
        # Standard input
        stdin_id = self._next_stream_id()
        stdin_stream = StandardStream(
            stream_id=stdin_id, mode="read", alias="user_input", handle=sys.stdin
        )
        self.streams[stdin_id] = stdin_stream
        self.aliases["user_input"] = stdin_id

        # Standard output
        stdout_id = self._next_stream_id()
        stdout_stream = StandardStream(
            stream_id=stdout_id, mode="write", alias="user_output", handle=sys.stdout
        )
        self.streams[stdout_id] = stdout_stream
        self.aliases["user_output"] = stdout_id

        # Standard error
        stderr_id = self._next_stream_id()
        stderr_stream = StandardStream(
            stream_id=stderr_id, mode="write", alias="user_error", handle=sys.stderr
        )
        self.streams[stderr_id] = stderr_stream
        self.aliases["user_error"] = stderr_id

    def _next_stream_id(self) -> str:
        """Generate next unique stream ID."""
        stream_id = f"stream_{self.next_id}"
        self.next_id += 1
        return stream_id

    def open_stream(
        self, file_name: str, mode: str, vfs: Optional[VirtualFS] = None
    ) -> str:
        """Open a new stream."""
        if vfs is None:
            vfs = self.vfs

        # Check file existence for read mode
        if mode == "read" and not vfs.exists(file_name):
            raise FileNotFoundError(file_name)

        # Check permissions
        if mode == "read" and not vfs.is_readable(file_name):
            raise PermissionError(f"Cannot read {file_name}")
        elif (
            mode in ("write", "append")
            and vfs.exists(file_name)
            and not vfs.is_writable(file_name)
        ):
            raise PermissionError(f"Cannot write to {file_name}")

        # Create stream
        stream_id = self._next_stream_id()

        # Initialize buffer with content for read mode
        buffer = io.StringIO()
        if mode == "read":
            content = vfs.get_content(file_name)
            if content:
                buffer.write(content)
                buffer.seek(0)
        elif mode == "append" and vfs.exists(file_name):
            content = vfs.get_content(file_name)
            if content:
                buffer.write(content)
                # Leave position at end for append

        stream = InMemoryStream(
            stream_id=stream_id, mode=mode, file_name=file_name, buffer=buffer
        )

        self.streams[stream_id] = stream
        return stream_id

    def close_stream(self, stream_id: str):
        """Close a stream."""
        if stream_id not in self.streams:
            raise ValueError(f"Stream {stream_id} does not exist")

        stream = self.streams[stream_id]

        # Save content for write/append streams
        if isinstance(stream, InMemoryStream) and stream.mode in ("write", "append"):
            if stream.file_name:
                content = stream.buffer.getvalue()
                self.vfs.write_file(stream.file_name, content)

        stream.close()
        del self.streams[stream_id]

        # Remove any aliases
        for alias, sid in list(self.aliases.items()):
            if sid == stream_id:
                del self.aliases[alias]

    def get_stream(self, stream_id: str) -> Stream:
        """Get a stream by ID."""
        if stream_id not in self.streams:
            raise ValueError(f"Stream {stream_id} does not exist")
        return self.streams[stream_id]

    def write_term(self, stream_id: str, term: Term):
        """Write a term to a stream."""
        stream = self.get_stream(stream_id)

        if stream.mode not in ("write", "append"):
            raise PermissionError("Cannot write to read-only stream")

        # Pretty print the term
        text = pretty(term)

        if isinstance(stream, InMemoryStream):
            stream.buffer.write(text)
        elif isinstance(stream, StandardStream):
            stream.handle.write(text)
            stream.handle.flush()

    def write_newline(self, stream_id: str):
        """Write a newline to a stream."""
        stream = self.get_stream(stream_id)

        if stream.mode not in ("write", "append"):
            raise PermissionError("Cannot write to read-only stream")

        if isinstance(stream, InMemoryStream):
            stream.buffer.write("\n")
        elif isinstance(stream, StandardStream):
            stream.handle.write("\n")
            stream.handle.flush()

    def read_term(self, stream_id: str) -> Term:
        """Read a term from a stream."""
        stream = self.get_stream(stream_id)

        if stream.mode != "read":
            raise PermissionError("Cannot read from write-only stream")

        if isinstance(stream, InMemoryStream):
            # Check if at end of stream
            current_pos = stream.buffer.tell()
            content = stream.buffer.read()
            if not content:
                stream.at_end = True
                return Atom("end_of_file")

            # Reset to where we were and read one term
            stream.buffer.seek(current_pos)

            # For now, just read the entire content as an atom
            # In a real implementation, we'd parse Prolog terms
            line = stream.buffer.readline().strip()
            if not line:
                stream.at_end = True
                return Atom("end_of_file")

            # Try to parse as a Prolog term
            try:
                reader = Reader()
                term = reader.read_term(line)
                return term
            except Exception:
                # If parsing fails, return as atom
                return Atom(line)

        elif isinstance(stream, StandardStream):
            # Read from standard input
            try:
                line = stream.handle.readline().strip()
                if not line:
                    return Atom("end_of_file")

                reader = Reader()
                term = reader.read_term(line)
                return term
            except Exception:
                return Atom("end_of_file")

        return Atom("end_of_file")

    def get_stream_properties(self, stream_id: str) -> List[Struct]:
        """Get all properties of a stream."""
        stream = self.get_stream(stream_id)
        properties = []

        # file_name property
        if stream.file_name:
            properties.append(Struct("file_name", (Atom(stream.file_name),)))

        # mode property
        properties.append(Struct("mode", (Atom(stream.mode),)))

        # alias property
        if stream.alias:
            properties.append(Struct("alias", (Atom(stream.alias),)))

        # position property
        properties.append(Struct("position", (Int(stream.position),)))

        # end_of_stream property
        if stream.at_end:
            properties.append(Struct("end_of_stream", (Atom("at"),)))
        elif stream.mode == "read":
            properties.append(Struct("end_of_stream", (Atom("not"),)))

        return properties


# Global stream manager instance
_stream_manager: Optional[StreamManager] = None


def get_stream_manager() -> StreamManager:
    """Get or create the global stream manager."""
    global _stream_manager
    if _stream_manager is None:
        _stream_manager = StreamManager()
    return _stream_manager


def reset_stream_manager():
    """Reset the global stream manager (for testing)."""
    global _stream_manager
    _stream_manager = None
