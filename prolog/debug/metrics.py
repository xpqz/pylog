"""
Performance and behavior metrics collection for PyLog.

Provides lightweight metrics tracking with zero overhead when disabled.
Tracks both global engine counters and per-predicate statistics.
"""

from dataclasses import dataclass, field
from typing import Dict, Any


@dataclass(frozen=True, slots=True)
class PredMetrics:
    """
    Per-predicate performance metrics.

    Immutable dataclass with slots for memory efficiency.
    All counters default to 0 if not specified.
    """
    pred_id: str
    calls: int = 0
    exits: int = 0
    fails: int = 0
    redos: int = 0
    unifications: int = 0
    backtracks: int = 0

    def to_dict(self) -> Dict[str, Any]:
        """Export metrics as a dictionary."""
        return {
            "pred_id": self.pred_id,
            "calls": self.calls,
            "exits": self.exits,
            "fails": self.fails,
            "redos": self.redos,
            "unifications": self.unifications,
            "backtracks": self.backtracks
        }


class EngineMetrics:
    """
    Global engine metrics and per-predicate tracking.

    Provides methods to record various engine events with
    automatic accumulation and validation.
    """

    def __init__(self):
        """Initialize all counters to 0."""
        # Global counters
        self.unifications_attempted = 0
        self.unifications_succeeded = 0
        self.backtracks_taken = 0
        self.cuts_executed = 0
        self.alternatives_pruned = 0
        self.exceptions_thrown = 0
        self.exceptions_caught = 0
        self.candidates_considered = 0
        self.candidates_yielded = 0

        # Per-predicate metrics (pred_id -> mutable counters)
        # We use a dict of dicts for mutability during collection
        self._pred_metrics: Dict[str, Dict[str, int]] = {}

    def reset(self):
        """Reset all metrics to initial state."""
        self.unifications_attempted = 0
        self.unifications_succeeded = 0
        self.backtracks_taken = 0
        self.cuts_executed = 0
        self.alternatives_pruned = 0
        self.exceptions_thrown = 0
        self.exceptions_caught = 0
        self.candidates_considered = 0
        self.candidates_yielded = 0
        self._pred_metrics.clear()

    # Global counter methods

    def record_unification_attempt(self):
        """Record a unification attempt."""
        self.unifications_attempted += 1

    def record_unification_success(self):
        """Record a successful unification."""
        self.unifications_succeeded += 1

    def record_backtrack(self):
        """Record a backtrack operation."""
        self.backtracks_taken += 1

    def record_cut(self):
        """Record a cut execution."""
        self.cuts_executed += 1

    def record_alternatives_pruned(self, count: int):
        """Record number of alternatives pruned by a cut."""
        if count < 0:
            raise ValueError(f"Cannot prune negative alternatives: {count}")
        self.alternatives_pruned += count

    def record_exception_thrown(self):
        """Record an exception being thrown."""
        self.exceptions_thrown += 1

    def record_exception_caught(self):
        """Record an exception being caught."""
        self.exceptions_caught += 1

    def record_candidates(self, considered: int, yielded: int):
        """Record candidate clauses from indexing (cumulative)."""
        if considered < 0:
            raise ValueError(f"Cannot consider negative candidates: {considered}")
        if yielded < 0:
            raise ValueError(f"Cannot yield negative candidates: {yielded}")
        self.candidates_considered += considered
        self.candidates_yielded += yielded

    # Per-predicate methods

    def _ensure_predicate(self, pred_id: str):
        """Ensure a predicate entry exists."""
        if pred_id not in self._pred_metrics:
            self._pred_metrics[pred_id] = {
                "calls": 0,
                "exits": 0,
                "fails": 0,
                "redos": 0,
                "unifications": 0,
                "backtracks": 0
            }

    def record_call(self, pred_id: str):
        """Record a CALL port for a predicate."""
        self._ensure_predicate(pred_id)
        self._pred_metrics[pred_id]["calls"] += 1

    def record_exit(self, pred_id: str):
        """Record an EXIT port for a predicate."""
        self._ensure_predicate(pred_id)
        self._pred_metrics[pred_id]["exits"] += 1

    def record_fail(self, pred_id: str):
        """Record a FAIL port for a predicate."""
        self._ensure_predicate(pred_id)
        self._pred_metrics[pred_id]["fails"] += 1

    def record_redo(self, pred_id: str):
        """Record a REDO port for a predicate."""
        self._ensure_predicate(pred_id)
        self._pred_metrics[pred_id]["redos"] += 1

    def record_predicate_unification(self, pred_id: str, attempted: int, succeeded: int = 0):
        """Record unifications for a specific predicate."""
        if attempted < 0:
            raise ValueError(f"Negative attempted unifications: {attempted}")
        if succeeded < 0:
            raise ValueError(f"Negative succeeded unifications: {succeeded}")
        if succeeded > attempted:
            raise ValueError(f"succeeded ({succeeded}) > attempted ({attempted})")

        self._ensure_predicate(pred_id)
        # Store attempted count (not succeeded for per-predicate)
        self._pred_metrics[pred_id]["unifications"] += attempted
        # Also update global counters
        self.unifications_attempted += attempted
        self.unifications_succeeded += succeeded

    def record_predicate_backtrack(self, pred_id: str):
        """Record a backtrack for a specific predicate."""
        self._ensure_predicate(pred_id)
        self._pred_metrics[pred_id]["backtracks"] += 1
        # Also update global counter
        self.backtracks_taken += 1

    def get_predicate_metrics(self, pred_id: str) -> PredMetrics:
        """Get metrics for a specific predicate."""
        if pred_id in self._pred_metrics:
            data = self._pred_metrics[pred_id]
            return PredMetrics(
                pred_id=pred_id,
                calls=data["calls"],
                exits=data["exits"],
                fails=data["fails"],
                redos=data["redos"],
                unifications=data["unifications"],
                backtracks=data["backtracks"]
            )
        else:
            # Return zeros for unknown predicates
            return PredMetrics(pred_id=pred_id)

    def to_dict(self) -> Dict[str, Any]:
        """
        Export all metrics as a dictionary.

        Returns a dict with 'global' and 'predicates' sections.
        """
        result = {
            "global": {
                "unifications_attempted": self.unifications_attempted,
                "unifications_succeeded": self.unifications_succeeded,
                "backtracks_taken": self.backtracks_taken,
                "cuts_executed": self.cuts_executed,
                "alternatives_pruned": self.alternatives_pruned,
                "exceptions_thrown": self.exceptions_thrown,
                "exceptions_caught": self.exceptions_caught,
                "candidates_considered": self.candidates_considered,
                "candidates_yielded": self.candidates_yielded
            },
            "predicates": {}
        }

        # Add per-predicate metrics
        for pred_id in self._pred_metrics:
            metrics = self.get_predicate_metrics(pred_id)
            result["predicates"][pred_id] = metrics.to_dict()
            # Remove redundant pred_id from nested dict
            del result["predicates"][pred_id]["pred_id"]

        return result
