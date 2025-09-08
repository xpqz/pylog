"""Exception types for the Prolog engine."""

from dataclasses import dataclass
from typing import Any


@dataclass
class PrologThrow(Exception):
    """Exception raised by throw/1 builtin.
    
    Attributes:
        ball: The Prolog term that was thrown
    """
    ball: Any
    
    def __str__(self):
        return f"Uncaught exception: {self.ball}"