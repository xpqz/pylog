"""Unit tests for prolog.engine.utils.selection module."""

from unittest.mock import Mock
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.clauses import ClauseCursor
from prolog.engine.cursors import StreamingClauseCursor
from prolog.engine.indexed_program import IndexedProgram
from prolog.unify.store import Store
from prolog.engine.utils.selection import (
    extract_predicate_key,
    should_use_streaming,
    ClauseSelection,
    SelectionContext,
    select_clauses,
)


class TestExtractPredicateKey:
    """Test extract_predicate_key function."""

    def test_extract_from_atom(self):
        """Test extracting predicate key from atom goal."""
        goal = Atom("test")
        result = extract_predicate_key(goal)
        assert result == ("test", 0)

    def test_extract_from_struct(self):
        """Test extracting predicate key from struct goal."""
        goal = Struct("foo", (Atom("a"), Int(1)))
        result = extract_predicate_key(goal)
        assert result == ("foo", 2)

    def test_extract_from_variable_returns_none(self):
        """Test extracting from variable returns None."""
        goal = Var(1, "X")
        result = extract_predicate_key(goal)
        assert result is None

    def test_extract_from_integer_returns_none(self):
        """Test extracting from integer returns None."""
        goal = Int(42)
        result = extract_predicate_key(goal)
        assert result is None


class TestShouldUseStreaming:
    """Test should_use_streaming function."""

    def test_streaming_enabled_no_debug_no_metrics_no_tracer(self):
        """Test streaming is used when enabled and no debug/metrics/tracer."""
        result = should_use_streaming(
            use_streaming=True, debug=False, metrics=None, tracer=None
        )
        assert result is True

    def test_streaming_disabled(self):
        """Test streaming not used when disabled."""
        result = should_use_streaming(
            use_streaming=False, debug=False, metrics=None, tracer=None
        )
        assert result is False

    def test_streaming_disabled_with_debug(self):
        """Test streaming not used when debug is enabled."""
        result = should_use_streaming(
            use_streaming=True, debug=True, metrics=None, tracer=None
        )
        assert result is False

    def test_streaming_disabled_with_metrics(self):
        """Test streaming not used when metrics are enabled."""
        mock_metrics = Mock()
        result = should_use_streaming(
            use_streaming=True, debug=False, metrics=mock_metrics, tracer=None
        )
        assert result is False

    def test_streaming_disabled_with_tracer(self):
        """Test streaming not used when tracer is enabled."""
        mock_tracer = Mock()
        result = should_use_streaming(
            use_streaming=True, debug=False, metrics=None, tracer=mock_tracer
        )
        assert result is False


class TestClauseSelection:
    """Test ClauseSelection class."""

    def test_clause_selection_initialization(self):
        """Test ClauseSelection initialization with default values."""
        mock_cursor = Mock()
        selection = ClauseSelection(cursor=mock_cursor)

        assert selection.cursor is mock_cursor
        assert selection.candidates_considered == 0
        assert selection.candidates_yielded == 0
        assert selection.total_clauses == 0
        assert selection.used_streaming is False

    def test_clause_selection_with_custom_values(self):
        """Test ClauseSelection initialization with custom values."""
        mock_cursor = Mock()
        selection = ClauseSelection(
            cursor=mock_cursor,
            candidates_considered=10,
            candidates_yielded=5,
            total_clauses=20,
            used_streaming=True,
        )

        assert selection.cursor is mock_cursor
        assert selection.candidates_considered == 10
        assert selection.candidates_yielded == 5
        assert selection.total_clauses == 20
        assert selection.used_streaming is True


def _make_context(
    use_indexing=False,
    use_streaming=False,
    debug=False,
    metrics=None,
    tracer=None,
    trace=False,
):
    """Helper to create SelectionContext for tests."""
    return SelectionContext(
        use_indexing=use_indexing,
        use_streaming=use_streaming,
        debug=debug,
        metrics=metrics,
        tracer=tracer,
        trace=trace,
    )


class TestSelectClauses:
    """Test select_clauses function."""

    def test_select_clauses_with_invalid_goal(self):
        """Test selecting clauses with invalid goal returns empty cursor."""
        mock_program = Mock()
        goal = Var(1, "X")  # Invalid goal
        store = Store()

        context = _make_context()
        result = select_clauses(
            program=mock_program, goal_term=goal, store=store, context=context
        )

        assert isinstance(result.cursor, ClauseCursor)
        assert result.cursor.functor == ""
        assert result.cursor.arity == 0
        assert not result.cursor.has_more()
        assert result.candidates_considered == 0
        assert result.used_streaming is False

    def test_select_clauses_without_indexing(self):
        """Test selecting clauses without indexing."""
        # Create a mock program
        mock_program = Mock()
        mock_clauses = [Mock(), Mock(), Mock()]  # 3 mock clauses
        mock_program.clauses_for.return_value = mock_clauses

        goal = Atom("test")
        store = Store()

        context = _make_context(
            use_indexing=False,
            use_streaming=False,
            debug=False,
            metrics=None,
            tracer=None,
            trace=False,
        )
        result = select_clauses(
            program=mock_program, goal_term=goal, store=store, context=context
        )

        # Verify program.clauses_for was called
        mock_program.clauses_for.assert_called_once_with("test", 0)

        # Verify result
        assert isinstance(result.cursor, ClauseCursor)
        assert result.cursor.functor == "test"
        assert result.cursor.arity == 0
        assert result.candidates_considered == 0  # Only counted in debug mode
        assert result.used_streaming is False

    def test_select_clauses_without_indexing_with_debug(self):
        """Test selecting clauses without indexing with debug enabled."""
        # Create a mock program
        mock_program = Mock()
        mock_clauses = [Mock(), Mock(), Mock()]  # 3 mock clauses
        mock_program.clauses_for.return_value = mock_clauses

        # Create mock metrics
        mock_metrics = Mock()

        goal = Atom("test")
        store = Store()

        context = _make_context(
            use_indexing=False,
            use_streaming=False,
            debug=True,
            metrics=mock_metrics,
            tracer=None,
            trace=False,
        )
        result = select_clauses(
            program=mock_program, goal_term=goal, store=store, context=context
        )

        # Verify result has debug metrics
        assert result.candidates_considered == 3
        assert result.candidates_yielded == 3  # All clauses yielded without indexing

    def test_select_clauses_with_indexing_no_streaming(self):
        """Test selecting clauses with indexing but no streaming."""
        # Create a mock indexed program
        mock_program = Mock()
        mock_program.select = Mock()

        # Mock the select method to return an iterator
        mock_clauses = [Mock(), Mock()]
        mock_program.select.return_value = iter(mock_clauses)

        goal = Struct("foo", (Atom("a"),))
        store = Store()

        context = _make_context(
            use_indexing=True,
            use_streaming=False,  # Force no streaming
            debug=False,
            metrics=None,
            tracer=None,
            trace=False,
        )
        result = select_clauses(
            program=mock_program, goal_term=goal, store=store, context=context
        )

        # Verify indexed select was called
        mock_program.select.assert_called_once_with(("foo", 1), goal, store)

        # Verify result
        assert isinstance(result.cursor, ClauseCursor)
        assert result.cursor.functor == "foo"
        assert result.cursor.arity == 1
        assert result.used_streaming is False

    def test_select_clauses_with_indexing_and_streaming(self):
        """Test selecting clauses with indexing and streaming enabled."""
        # Create a mock indexed program
        mock_program = Mock()
        mock_program.select = Mock()

        # Mock the select method to return an iterator
        mock_iterator = iter([Mock(), Mock()])
        mock_program.select.return_value = mock_iterator

        goal = Struct("foo", (Atom("a"),))
        store = Store()

        context = _make_context(
            use_indexing=True,
            use_streaming=True,
            debug=False,
            metrics=None,
            tracer=None,
            trace=False,
        )
        result = select_clauses(
            program=mock_program, goal_term=goal, store=store, context=context
        )

        # Verify indexed select was called
        mock_program.select.assert_called_once_with(("foo", 1), goal, store)

        # Verify result uses streaming cursor
        assert isinstance(result.cursor, StreamingClauseCursor)
        assert result.cursor.functor == "foo"
        assert result.cursor.arity == 1
        assert result.used_streaming is True
        assert result.candidates_considered == 0  # Can't count with streaming

    def test_select_clauses_with_indexing_debug_and_metrics(self):
        """Test selecting clauses with indexing, debug, and metrics."""
        # Create a mock indexed program
        mock_program = Mock()
        mock_program.select = Mock()

        # Mock the select method to return matches with None filtering
        mock_clauses = [Mock(), None, Mock()]  # Include None for filtering test
        mock_program.select.return_value = iter(mock_clauses)

        # Create mock metrics
        mock_metrics = Mock()

        goal = Struct("foo", (Atom("a"),))
        store = Store()

        context = _make_context(
            use_indexing=True,
            use_streaming=False,  # Disabled due to debug/metrics
            debug=True,
            metrics=mock_metrics,
            tracer=None,
            trace=False,
        )
        result = select_clauses(
            program=mock_program, goal_term=goal, store=store, context=context
        )

        # Verify result has debug metrics
        assert result.candidates_considered == 3
        assert result.candidates_yielded == 2  # Excludes None entries
        assert result.used_streaming is False

    def test_select_clauses_with_indexing_debug_and_trace(self):
        """Test selecting clauses with indexing, debug, and trace logging."""
        # Create a mock indexed program
        mock_program = Mock(spec=IndexedProgram)
        mock_program.select = Mock()

        # Mock the _index and preds structure
        mock_pred_idx = Mock()
        mock_pred_idx.order = [1, 2, 3, 4]  # 4 total clauses
        mock_program._index = Mock()
        mock_program._index.preds = {("foo", 1): mock_pred_idx}

        # Mock the select method
        mock_clauses = [Mock(), Mock()]
        mock_program.select.return_value = iter(mock_clauses)

        goal = Struct("foo", (Atom("a"),))
        store = Store()

        context = _make_context(
            use_indexing=True,
            use_streaming=False,
            debug=True,
            metrics=None,
            tracer=None,
            trace=True,
        )
        result = select_clauses(
            program=mock_program, goal_term=goal, store=store, context=context
        )

        # Verify result includes total clauses count
        assert result.total_clauses == 4
        assert result.candidates_considered == 2

    def test_select_clauses_streaming_disabled_by_debug(self):
        """Test that streaming is disabled when debug is enabled."""
        mock_program = Mock()
        mock_program.select = Mock()
        mock_program.select.return_value = iter([Mock()])

        goal = Atom("test")
        store = Store()

        context = _make_context(
            use_indexing=True,
            use_streaming=True,  # Requested but should be disabled
            debug=True,  # This disables streaming
            metrics=None,
            tracer=None,
            trace=False,
        )
        result = select_clauses(
            program=mock_program, goal_term=goal, store=store, context=context
        )

        # Should use ClauseCursor not StreamingClauseCursor
        assert isinstance(result.cursor, ClauseCursor)
        assert result.used_streaming is False

    def test_select_clauses_streaming_disabled_by_metrics(self):
        """Test that streaming is disabled when metrics are enabled."""
        mock_program = Mock()
        mock_program.select = Mock()
        mock_program.select.return_value = iter([Mock()])

        mock_metrics = Mock()

        goal = Atom("test")
        store = Store()

        context = _make_context(
            use_indexing=True,
            use_streaming=True,  # Requested but should be disabled
            debug=False,
            metrics=mock_metrics,  # This disables streaming
            tracer=None,
            trace=False,
        )
        result = select_clauses(
            program=mock_program, goal_term=goal, store=store, context=context
        )

        # Should use ClauseCursor not StreamingClauseCursor
        assert isinstance(result.cursor, ClauseCursor)
        assert result.used_streaming is False

    def test_select_clauses_streaming_disabled_by_tracer(self):
        """Test that streaming is disabled when tracer is enabled."""
        mock_program = Mock()
        mock_program.select = Mock()
        mock_program.select.return_value = iter([Mock()])

        mock_tracer = Mock()

        goal = Atom("test")
        store = Store()

        context = _make_context(
            use_indexing=True,
            use_streaming=True,  # Requested but should be disabled
            debug=False,
            metrics=None,
            tracer=mock_tracer,  # This disables streaming
            trace=False,
        )
        result = select_clauses(
            program=mock_program, goal_term=goal, store=store, context=context
        )

        # Should use ClauseCursor not StreamingClauseCursor
        assert isinstance(result.cursor, ClauseCursor)
        assert result.used_streaming is False
