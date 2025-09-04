"""Test helper functions for Prolog engine tests."""

import sys
from contextlib import contextmanager
from typing import List, Dict, Any
from prolog.ast.terms import Term, Atom, Var, Struct
from prolog.ast.clauses import Clause, Program
from prolog.engine.engine import Engine


def mk_fact(functor: str, *args: Term) -> Clause:
    """Create a fact clause.
    
    Args:
        functor: The predicate name.
        *args: The arguments to the predicate.
        
    Returns:
        A Clause with the given head and empty body.
    """
    if len(args) == 0:
        # Zero-arity predicate - just an atom
        head = Atom(functor)
    elif len(args) == 1 and not isinstance(args[0], tuple):
        # Single argument - could be atom or struct
        head = Struct(functor, (args[0],))
    else:
        # Multiple arguments
        head = Struct(functor, args)
    
    return Clause(head=head, body=())


def mk_rule(functor: str, head_args: tuple, *body_terms: Term) -> Clause:
    """Create a rule clause.
    
    Args:
        functor: The head predicate name.
        head_args: Tuple of arguments for the head.
        *body_terms: The body goals.
        
    Returns:
        A Clause with the given head and body.
    """
    if len(head_args) == 0:
        # Zero-arity predicate
        head = Atom(functor)
    else:
        # One or more arguments
        head = Struct(functor, head_args)
    
    return Clause(head=head, body=body_terms)


def program(*clauses: Clause) -> Program:
    """Create a Program from clauses.
    
    Args:
        *clauses: The clauses to include in the program.
        
    Returns:
        A Program containing the given clauses.
    """
    return Program(clauses=clauses)


def run_query(engine: Engine, *goals: Term) -> List[Dict[str, Any]]:
    """Run a query on an engine.
    
    Args:
        engine: The engine to run on.
        *goals: The goal terms to prove.
        
    Returns:
        List of solution dictionaries.
    """
    return engine.run(list(goals))


@contextmanager
def assert_no_recursion(limit: int = 100):
    """Context manager to assert no Python recursion approaching limit.
    
    Args:
        limit: Maximum recursion depth to allow (default 100).
        
    Raises:
        AssertionError: If recursion depth exceeds limit.
    """
    # Get current recursion depth
    frame = sys._getframe()
    depth = 0
    while frame is not None:
        depth += 1
        frame = frame.f_back
    
    initial_depth = depth
    
    # Track maximum depth during execution
    class DepthTracker:
        max_depth = initial_depth
    
    original_getframe = sys._getframe
    
    def tracking_getframe(depth=0):
        """Track recursion depth while getting frame."""
        result = original_getframe(depth + 1)  # +1 for this wrapper
        
        # Count current depth
        frame = result
        current_depth = 0
        while frame is not None:
            current_depth += 1
            frame = frame.f_back
        
        DepthTracker.max_depth = max(DepthTracker.max_depth, current_depth)
        
        # Check limit
        if current_depth - initial_depth > limit:
            raise AssertionError(
                f"Python recursion depth exceeded limit: "
                f"{current_depth - initial_depth} > {limit}"
            )
        
        return result
    
    # Monkey patch for duration of context
    sys._getframe = tracking_getframe
    
    try:
        yield
    finally:
        # Restore original
        sys._getframe = original_getframe
        
        # Final check
        max_recursion = DepthTracker.max_depth - initial_depth
        if max_recursion > limit:
            raise AssertionError(
                f"Maximum Python recursion depth exceeded limit: "
                f"{max_recursion} > {limit}"
            )