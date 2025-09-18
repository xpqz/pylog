"""
Cursor implementations for clause selection.

Provides streaming and materialized cursor implementations that present
the same interface for iterating over clause candidates while supporting
backtracking.
"""

import itertools
from typing import Iterator, List, Any, Optional


class StreamingClauseCursor:
    """
    Streaming cursor that lazily yields clauses from an iterator.

    Maintains a 1-item lookahead buffer for peek() and tracks consumed
    items to support backtracking via clone().

    Compatible with ClauseCursor interface:
    - has_more(): Check if more clauses available
    - peek(): Look at next clause without consuming
    - take(): Consume and return next clause
    - clone(): Create cursor at same logical position
    - functor: Predicate functor name
    - arity: Predicate arity

    Note: Clones should not be advanced concurrently. The behavior is
    undefined if you interleave take() calls between original and clone.
    """

    __slots__ = ('functor', 'arity', '_it', '_buffer', '_consumed', '_exhausted', '_exception')

    def __init__(self, functor: str, arity: int, it: Iterator[Any]):
        """
        Initialize streaming cursor.

        Args:
            functor: Predicate functor name
            arity: Predicate arity
            it: Iterator yielding clause indices
        """
        self.functor = functor
        self.arity = arity
        self._it = it
        self._buffer: Optional[Any] = None  # 1-item lookahead
        self._consumed: List[Any] = []  # For clone support
        self._exhausted = False
        self._exception: Optional[Exception] = None

    def has_more(self) -> bool:
        """Check if more clauses are available."""
        if self._exhausted or self._exception:
            if self._exception:
                # Re-raise stored exception
                raise self._exception
            return False

        if self._buffer is not None:
            return True

        # Try to fill buffer
        try:
            self._buffer = next(self._it)
            return True
        except StopIteration:
            self._exhausted = True
            return False
        except Exception as e:
            # Store exception for consistent surfacing
            self._exception = e
            raise e

    def peek(self) -> Any:
        """
        Return next clause without consuming it.

        Raises:
            StopIteration: If no more clauses available
        """
        if self._exception:
            raise self._exception

        if self._buffer is not None:
            return self._buffer

        if self._exhausted:
            raise StopIteration("No more clauses")

        # Fill buffer
        try:
            self._buffer = next(self._it)
            return self._buffer
        except StopIteration:
            self._exhausted = True
            raise StopIteration("No more clauses")
        except Exception as e:
            self._exception = e
            raise e

    def take(self) -> Any:
        """
        Consume and return next clause.

        Raises:
            StopIteration: If no more clauses available
        """
        if self._exception:
            raise self._exception

        if self._buffer is not None:
            # Consume buffered item
            result = self._buffer
            self._buffer = None
            self._consumed.append(result)
            return result

        if self._exhausted:
            raise StopIteration("No more clauses")

        # Get next directly
        try:
            result = next(self._it)
            self._consumed.append(result)
            return result
        except StopIteration:
            self._exhausted = True
            raise StopIteration("No more clauses")
        except Exception as e:
            self._exception = e
            raise e

    def clone(self) -> 'StreamingClauseCursor':
        """
        Create a cursor at the same logical position.

        The clone will yield the same remaining sequence as this cursor.
        Consumed items are shared to support backtracking.

        Warning:
        - Clones should not be advanced concurrently.
        - itertools.tee may retain memory for unconsumed items between
          the original and cloned iterators. This is usually not an issue
          as clones are rare and short-lived in backtracking scenarios.
        """
        # Collect remaining items for the clone
        # We need to buffer the current item (if any) separately

        # Create iterator that yields remaining items
        if self._buffer is not None:
            # Chain the buffered item with the rest of the iterator
            remaining_items = itertools.chain([self._buffer], self._it)
        else:
            # Just use the original iterator
            remaining_items = self._it

        # Important: We need to split the iterator for both cursors
        # This uses tee to create two independent iterators
        self._it, clone_it = itertools.tee(remaining_items, 2)

        # Restore buffer if it was there
        if self._buffer is not None:
            # Consume one item from self._it to skip the buffered one we added
            try:
                next(self._it)
            except StopIteration:
                pass

        # Create new cursor with cloned iterator
        cloned = StreamingClauseCursor(self.functor, self.arity, clone_it)
        cloned._consumed = self._consumed.copy()  # Share consumed history
        cloned._exhausted = self._exhausted
        cloned._exception = self._exception

        return cloned