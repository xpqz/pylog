"""Builtin predicate registration system.

This module provides a centralized registration system for all builtin predicates.
Each builtin module implements a register(registry) function that adds its predicates
to the registry.

## Builtin Groups

The following builtin groups are currently implemented:

- **types**: Type-checking predicates (var/1, nonvar/1, atom/1, integer/1, float/1,
  number/1, atomic/1, compound/1, callable/1, ground/1)
- **arithmetic**: Arithmetic predicates (is/2, =:=/2, =\\=/2, >/2, </2, >=/2, =</2)

## Future Groups

The following groups are planned for subsequent phases:
- **terms**: Term construction/inspection (=../2, functor/3, arg/3, ==/2, \\==/2)
- **solutions**: All-solutions predicates (findall/3, bagof/3, setof/3)
- **exceptions**: Exception handling (throw/1, catch/3)
- **dynamic_db**: Dynamic database operations (dynamic/1, assert*/1, retract*/1, abolish/1)
"""

from typing import Dict, Tuple, Callable

# Import all builtin modules at the top
from prolog.engine.builtins.types import register as register_types
from prolog.engine.builtins.arithmetic import register as register_arithmetic


def register_all(registry: Dict[Tuple[str, int], Callable]) -> None:
    """Register all builtin predicates with the given registry.

    This function is idempotent - calling it multiple times will simply
    overwrite existing registry entries with the same values.

    Args:
        registry: Dictionary mapping (predicate_name, arity) -> callable
    """
    # Register type checking predicates
    register_types(registry)

    # Register arithmetic predicates
    register_arithmetic(registry)
