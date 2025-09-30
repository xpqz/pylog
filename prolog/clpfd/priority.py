"""Priority levels for CLP(FD) propagation queue."""

from enum import IntEnum


class Priority(IntEnum):
    """Propagator priorities (lower value = higher priority)."""

    HIGH = 0  # Equality propagators
    MED = 1  # Inequality propagators
    LOW = 2  # Labeling/search
