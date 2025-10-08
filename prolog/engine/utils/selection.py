"""Clause selection and cursor management utilities.

This module provides utilities for clause selection and cursor management,
extracted from the engine's _dispatch_predicate method to reduce complexity
and improve maintainability.
"""

from dataclasses import dataclass
from typing import Tuple, Optional, Union, Iterator, Protocol
from prolog.ast.terms import Term, Atom, Struct
from prolog.ast.clauses import ClauseCursor
from prolog.engine.cursors import StreamingClauseCursor
from prolog.engine.indexed_program import IndexedProgram
from prolog.unify.store import Store


class MetricsCollector(Protocol):
    """Protocol for metrics collectors."""

    def record_candidates(self, considered: int, yielded: int) -> None:
        """Record candidate clause statistics."""
        ...


class Tracer(Protocol):
    """Protocol for trace systems."""

    def emit_internal_event(self, event_type: str, data: dict) -> None:
        """Emit an internal tracing event."""
        ...


class Program(Protocol):
    """Protocol for Prolog programs."""

    def clauses_for(self, functor: str, arity: int) -> list:
        """Get clauses for a predicate."""
        ...


class IndexedProgramProtocol(Program, Protocol):
    """Protocol for indexed Prolog programs."""

    def select(
        self, pred_key: Tuple[str, int], goal_term: Term, store: Store
    ) -> Iterator:
        """Select matching clauses using indexing."""
        ...


def extract_predicate_key(goal_term: Term) -> Optional[Tuple[str, int]]:
    """Extract functor and arity from a goal term.

    Args:
        goal_term: The goal term to extract predicate key from.

    Returns:
        Tuple of (functor, arity) or None if not a valid predicate call.
    """
    if isinstance(goal_term, Atom):
        return (goal_term.name, 0)
    elif isinstance(goal_term, Struct):
        return (goal_term.functor, len(goal_term.args))
    else:
        # Can't match a variable or other term
        return None


def should_use_streaming(
    use_streaming: bool,
    debug: bool,
    metrics: Optional[MetricsCollector],
    tracer: Optional[Tracer],
) -> bool:
    """Determine if streaming cursor should be used.

    Streaming is disabled when debug or metrics are enabled because:
    - Debug mode needs to count candidates_considered for instrumentation
    - Metrics need to count candidates_yielded for performance tracking
    - Tracer needs complete event sequences for debugging
    All of these require materialized lists, not streaming iterators.

    Args:
        use_streaming: Whether streaming is enabled in general.
        debug: Whether debug mode is enabled.
        metrics: Metrics collector instance (None if disabled).
        tracer: Tracer instance (None if disabled).

    Returns:
        True if streaming should be used, False otherwise.
    """
    return (
        use_streaming
        and not debug
        and not metrics
        and not tracer  # Also disable with tracer to preserve debug behavior
    )


@dataclass
class SelectionContext:
    """Context parameters for clause selection.

    Groups related selection parameters to reduce function parameter count
    and improve maintainability.
    """

    use_indexing: bool
    use_streaming: bool
    debug: bool
    metrics: Optional[MetricsCollector]
    tracer: Optional[Tracer]
    trace: bool


@dataclass
class ClauseSelection:
    """Clause selection result containing cursor and metadata."""

    cursor: Union[ClauseCursor, StreamingClauseCursor]
    candidates_considered: int = 0
    candidates_yielded: int = 0
    total_clauses: int = 0
    used_streaming: bool = False


def select_clauses(
    program: Union[Program, IndexedProgramProtocol],
    goal_term: Term,
    store: Store,
    context: SelectionContext,
) -> ClauseSelection:
    """Select clauses for a predicate goal and create appropriate cursor.

    This function wraps the clause selection logic, handling both indexed
    and non-indexed programs, streaming vs materialized cursors, and
    debug/metrics accounting.

    Args:
        program: The program to select clauses from.
        goal_term: The goal term to match clauses against.
        store: The unification store.
        context: Selection context containing configuration and debugging options.

    Returns:
        ClauseSelection containing cursor and metadata.
    """
    pred_key = extract_predicate_key(goal_term)
    if pred_key is None:
        # Create empty cursor for non-callable terms
        return ClauseSelection(
            cursor=ClauseCursor(matches=[], functor="", arity=0),
            candidates_considered=0,
            candidates_yielded=0,
            total_clauses=0,
            used_streaming=False,
        )

    functor, arity = pred_key

    # Get matching clauses - use indexing if available
    if context.use_indexing and hasattr(program, "select"):
        return _select_with_indexing(
            program, pred_key, goal_term, store, functor, arity, context
        )
    else:
        return _select_without_indexing(program, functor, arity, context)


def _select_with_indexing(
    program: IndexedProgramProtocol,
    pred_key: Tuple[str, int],
    goal_term: Term,
    store: Store,
    functor: str,
    arity: int,
    context: SelectionContext,
) -> ClauseSelection:
    """Select clauses using indexed program."""
    # Determine if we should use streaming
    stream = should_use_streaming(
        context.use_streaming, context.debug, context.metrics, context.tracer
    )

    if stream:
        # Use streaming cursor for memory efficiency
        clause_iterator = program.select(pred_key, goal_term, store)
        cursor = StreamingClauseCursor(functor=functor, arity=arity, it=clause_iterator)
        return ClauseSelection(
            cursor=cursor,
            candidates_considered=0,  # Can't count with streaming
            candidates_yielded=0,
            total_clauses=0,
            used_streaming=True,
        )
    else:
        # Materialize list for debug/metrics compatibility
        matches = list(program.select(pred_key, goal_term, store))
        cursor = ClauseCursor(matches=matches, functor=functor, arity=arity)

        candidates_considered = len(matches)
        candidates_yielded = 0
        total_clauses = 0

        # Track candidates in debug mode (only when materialized)
        if context.debug:
            if context.metrics:
                # Count how many are actually yielded (have potential to match)
                candidates_yielded = len([m for m in matches if m is not None])

            # Log detailed info if trace is enabled too
            if context.trace:
                # Optimize total clause count for IndexedProgram
                if isinstance(program, IndexedProgram):
                    pred_idx = program._index.preds.get((functor, arity))
                    total_clauses = len(pred_idx.order) if pred_idx else 0
                else:
                    total_clauses = len(program.clauses_for(functor, arity))

        return ClauseSelection(
            cursor=cursor,
            candidates_considered=candidates_considered,
            candidates_yielded=candidates_yielded,
            total_clauses=total_clauses,
            used_streaming=False,
        )


def _select_without_indexing(
    program: Program, functor: str, arity: int, context: SelectionContext
) -> ClauseSelection:
    """Select clauses using standard program interface."""
    # Fall back to standard clause selection
    matches = program.clauses_for(functor, arity)
    cursor = ClauseCursor(matches=matches, functor=functor, arity=arity)

    candidates_considered = 0
    candidates_yielded = 0

    # Track all candidates in debug mode (no filtering)
    if context.debug:
        candidates_considered = len(matches)
        if context.metrics:
            # All clauses are yielded (no filtering without indexing)
            candidates_yielded = len(matches)

    return ClauseSelection(
        cursor=cursor,
        candidates_considered=candidates_considered,
        candidates_yielded=candidates_yielded,
        total_clauses=len(matches),
        used_streaming=False,
    )
