"""Builtin predicate registration system.

This module provides a centralized registration system for all builtin predicates.
Each builtin module implements a register(registry) function that adds its predicates
to the registry.
"""

from typing import Dict, Tuple, Callable

# Import all builtin modules at the top
from prolog.engine.builtins.types import register as register_types


def register_all(registry: Dict[Tuple[str, int], Callable]) -> None:
    """Register all builtin predicates with the given registry.

    Args:
        registry: Dictionary mapping (predicate_name, arity) -> callable
    """
    # Register type checking predicates
    register_types(registry)
