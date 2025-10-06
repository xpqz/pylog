"""
Stream abstraction for Prolog I/O operations.

This module provides a simple stream abstraction for JSON and other I/O
builtins to handle both file streams and string streams uniformly.
"""

from io import StringIO
from typing import Union, TextIO


class PrologStream:
    """Simple stream abstraction for builtin I/O operations.

    Handles file streams, StringIO objects, and other text I/O uniformly
    for use by JSON and other I/O builtins.
    """

    def __init__(self, stream: Union[TextIO, StringIO, str]):
        """Initialize PrologStream with a stream or file path.

        Args:
            stream: A text stream (file, StringIO) or file path string

        Raises:
            ValueError: If stream type is not supported
            IOError: If file path cannot be opened
        """
        if isinstance(stream, str):
            # String argument - treat as file path
            try:
                self._stream = open(stream, "r", encoding="utf-8")
                self._owns_stream = True
            except IOError as e:
                raise IOError(f"Cannot open file '{stream}': {e}")
        elif hasattr(stream, "read") and hasattr(stream, "write"):
            # Stream-like object (file, StringIO, etc.)
            self._stream = stream
            self._owns_stream = False
        else:
            raise ValueError(f"Unsupported stream type: {type(stream)}")

    def read_text(self) -> str:
        """Read all text from the stream.

        Returns:
            Complete text content from the stream

        Raises:
            IOError: If stream cannot be read
        """
        try:
            # Save current position and read from beginning if possible
            if hasattr(self._stream, "seek"):
                current_pos = self._stream.tell()
                self._stream.seek(0)
                content = self._stream.read()
                self._stream.seek(current_pos)
                return content
            else:
                # Stream doesn't support seeking, read as-is
                return self._stream.read()
        except Exception as e:
            raise IOError(f"Cannot read from stream: {e}")

    def write_text(self, text: str) -> None:
        """Write text to the stream.

        Args:
            text: Text content to write

        Raises:
            IOError: If stream cannot be written to
        """
        try:
            self._stream.write(text)
            # Flush if supported to ensure data is written
            if hasattr(self._stream, "flush"):
                self._stream.flush()
        except Exception as e:
            raise IOError(f"Cannot write to stream: {e}")

    def close(self) -> None:
        """Close the stream if we own it."""
        if self._owns_stream and hasattr(self._stream, "close"):
            self._stream.close()

    def __enter__(self):
        """Context manager support."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager cleanup."""
        self.close()

    @property
    def raw_stream(self) -> Union[TextIO, StringIO]:
        """Access to the underlying stream object.

        Returns:
            The wrapped stream object
        """
        return self._stream


def is_stream_like(obj) -> bool:
    """Check if an object is stream-like (has read/write methods).

    Args:
        obj: Object to check

    Returns:
        True if object appears to be a stream
    """
    return hasattr(obj, "read") or hasattr(obj, "write")


def ensure_prolog_stream(stream_arg) -> PrologStream:
    """Ensure argument is a PrologStream, converting if necessary.

    Args:
        stream_arg: Stream argument (PrologStream, file-like, or path string)

    Returns:
        PrologStream instance

    Raises:
        ValueError: If argument cannot be converted to a stream
    """
    if isinstance(stream_arg, PrologStream):
        return stream_arg
    elif is_stream_like(stream_arg) or isinstance(stream_arg, str):
        return PrologStream(stream_arg)
    else:
        raise ValueError(f"Cannot convert {type(stream_arg)} to PrologStream")
