"""Main unification algorithm.

Unification is the pattern-matching heart of Prolog. It attempts to make
two terms equal by binding variables. The algorithm is iterative (using
an explicit stack) to avoid Python recursion limits.

Key invariants:
- All mutations go through the trail for backtracking
- Failed unification undoes all changes made during the attempt
- Unification is symmetric: unify(A, B) == unify(B, A)
"""

from typing import Any, List, Tuple

from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.unify.store import Store
from prolog.unify.unify_helpers import union_vars, bind_root_to_term, deref_term
from prolog.unify.occurs import occurs


def unify(t1: Any, t2: Any, store: Store, trail: List, occurs_check: bool = False) -> bool:
    """Unify two terms, making them equal by binding variables.
    
    Args:
        t1: First term
        t2: Second term
        store: Variable store
        trail: Trail for recording changes
        occurs_check: Whether to perform occurs check (default False)
        
    Returns:
        True if unification succeeds, False otherwise
        
    The algorithm uses an explicit stack to avoid recursion. Each stack
    entry is a pair of terms to unify. The algorithm processes pairs
    iteratively until the stack is empty (success) or a pair fails.
    
    On failure, all changes made during this unification attempt are
    undone via the trail.
    """
    # Mark trail position for potential undo
    mark = len(trail)
    
    # Stack of term pairs to unify
    stack = [(t1, t2)]
    
    while stack:
        term1, term2 = stack.pop()
        
        # Dereference both terms
        tag1, val1 = deref_term(term1, store)
        tag2, val2 = deref_term(term2, store)
        
        # Case 1: Both are variables
        if tag1 == "VAR" and tag2 == "VAR":
            if val1 != val2:  # Different variables
                try:
                    union_vars(val1, val2, trail, store)
                except ValueError:
                    # One is bound (shouldn't happen after deref, but be safe)
                    _undo_and_fail(mark, trail, store)
                    return False
            # Same variable: no-op, continue
            
        # Case 2: One is a variable
        elif tag1 == "VAR":
            # val1 is unbound var ID, val2 is the term
            if occurs_check and occurs(val1, val2, store):
                _undo_and_fail(mark, trail, store)
                return False
            bind_root_to_term(val1, val2, trail, store)
            
        elif tag2 == "VAR":
            # val2 is unbound var ID, val1 is the term
            if occurs_check and occurs(val2, val1, store):
                _undo_and_fail(mark, trail, store)
                return False
            bind_root_to_term(val2, val1, trail, store)
            
        # Case 3: Both are non-variables
        else:
            # Now val1 and val2 are the actual terms
            if not _unify_nonvars(val1, val2, stack):
                _undo_and_fail(mark, trail, store)
                return False
    
    # Stack empty: unification succeeded
    return True


def _unify_nonvars(t1: Any, t2: Any, stack: List[Tuple[Any, Any]]) -> bool:
    """Unify two non-variable terms.
    
    Args:
        t1: First non-variable term
        t2: Second non-variable term
        stack: Stack to push subterm pairs onto
        
    Returns:
        True if terms can potentially unify, False if definitely incompatible
        
    This function handles the structural cases: atoms, integers, structs, lists.
    For compound terms, it pushes subterm pairs onto the stack for later processing.
    """
    # Atoms
    if isinstance(t1, Atom) and isinstance(t2, Atom):
        return t1.name == t2.name
    
    # Integers
    if isinstance(t1, Int) and isinstance(t2, Int):
        return t1.value == t2.value
    
    # Structs
    if isinstance(t1, Struct) and isinstance(t2, Struct):
        if t1.functor != t2.functor:
            return False
        if len(t1.args) != len(t2.args):
            return False
        # Push argument pairs onto stack in reverse order
        # so they're processed left-to-right
        for a1, a2 in reversed(list(zip(t1.args, t2.args))):
            stack.append((a1, a2))
        return True
    
    # Atom with zero-arity Struct (equivalent in Prolog)
    if isinstance(t1, Atom) and isinstance(t2, Struct) and len(t2.args) == 0:
        return t1.name == t2.functor
    if isinstance(t2, Atom) and isinstance(t1, Struct) and len(t1.args) == 0:
        return t2.name == t1.functor
    
    # Lists - handle both PrologList and '.'/2 cons representation
    # Try list interop first
    list_result = _try_unify_list_interop(t1, t2, stack)
    if list_result is not None:
        return list_result
    
    # Standard PrologList unification
    if isinstance(t1, PrologList) and isinstance(t2, PrologList):
        return _unify_lists(t1, t2, stack)
    
    # Type mismatch
    return False


def _unify_lists(l1: PrologList, l2: PrologList, stack: List[Tuple[Any, Any]]) -> bool:
    """Unify two lists, handling tails properly.
    
    Args:
        l1: First list
        l2: Second list
        stack: Stack to push element pairs onto
        
    Returns:
        True if lists can potentially unify, False otherwise
    
    Prolog lists can have custom tails (e.g., [H|T]). The algorithm:
    1. Match elements from the front as long as both have elements
    2. When one runs out of elements, unify remaining with the other's tail
    """
    items1 = list(l1.items)
    items2 = list(l2.items)
    tail1 = l1.tail
    tail2 = l2.tail
    
    # Match items from the front
    while items1 and items2:
        stack.append((items1.pop(0), items2.pop(0)))
    
    # Now at least one items list is empty
    if items1:
        # l1 has remaining items, l2 is exhausted
        # Unify remaining items1 + tail1 with tail2
        remaining = PrologList(tuple(items1), tail=tail1)
        stack.append((remaining, tail2))
    elif items2:
        # l2 has remaining items, l1 is exhausted
        # Unify tail1 with remaining items2 + tail2
        remaining = PrologList(tuple(items2), tail=tail2)
        stack.append((tail1, remaining))
    else:
        # Both exhausted, unify tails
        stack.append((tail1, tail2))
    
    return True


def _try_unify_list_interop(t1: Any, t2: Any, stack: List[Tuple[Any, Any]]) -> Any:
    """Try to unify terms that might involve list representations.
    
    Handles unification between PrologList and '.'/2 cons representations
    without building intermediate structures.
    
    Args:
        t1: First term
        t2: Second term  
        stack: Stack to push pairs onto
        
    Returns:
        True if can unify, False if incompatible, None if not list-related
    """
    def step(term):
        """Return ('nil'), ('cons', head, tail), or ('other', term)."""
        if isinstance(term, PrologList):
            if not term.items and term.tail == Atom('[]'):
                return ('nil',)
            elif term.items:
                head = term.items[0]
                if len(term.items) == 1:
                    tail = term.tail
                else:
                    tail = PrologList(term.items[1:], tail=term.tail)
                return ('cons', head, tail)
            else:
                # Empty items but non-[] tail
                return ('other', term.tail)
        elif isinstance(term, Atom) and term.name == '[]':
            return ('nil',)
        elif isinstance(term, Struct) and term.functor == '.' and len(term.args) == 2:
            return ('cons', term.args[0], term.args[1])
        else:
            return ('other', term)
    
    # Get the first step for both terms
    s1 = step(t1)
    s2 = step(t2)
    
    # If neither is list-related, return None
    if s1[0] == 'other' and s2[0] == 'other':
        return None
    
    # Build pairs to unify iteratively
    pairs = [(t1, t2)]
    while pairs:
        term1, term2 = pairs.pop()
        
        s1 = step(term1)
        s2 = step(term2)
        
        if s1[0] == 'nil' and s2[0] == 'nil':
            # Both nil - continue
            continue
        elif s1[0] == 'cons' and s2[0] == 'cons':
            # Both cons - unify heads and continue with tails
            stack.append((s1[1], s2[1]))  # heads
            pairs.append((s1[2], s2[2]))  # tails
        elif s1[0] == 'other' and s2[0] == 'other':
            # Both non-list - regular unification
            stack.append((s1[1], s2[1]))
        elif s1[0] == 'other':
            # t1 is non-list, t2 is list-like
            # If step extracted the same term AND it's not a Var, we have incompatible types
            # Variables can unify with anything, including lists
            from prolog.ast.terms import Var
            if s1[1] is term1 and not isinstance(term1, Var):
                # term1 is definitely not list-like and not a var - incompatible with list
                return False
            # Otherwise unify the extracted term with the list term
            stack.append((s1[1], term2))
        elif s2[0] == 'other':
            # t2 is non-list, t1 is list-like
            # If step extracted the same term AND it's not a Var, we have incompatible types
            # Variables can unify with anything, including lists
            from prolog.ast.terms import Var
            if s2[1] is term2 and not isinstance(term2, Var):
                # term2 is definitely not list-like and not a var - incompatible with list
                return False
            # Otherwise unify the list term with the extracted term
            stack.append((term1, s2[1]))
        else:
            # Mismatch (nil vs cons)
            return False
    
    return True


def _undo_and_fail(mark: int, trail: List, store: Store) -> None:
    """Undo trail entries back to mark on unification failure.
    
    Args:
        mark: Trail position to restore to
        trail: Trail with entries to undo
        store: Store to restore
    """
    from prolog.unify.trail import undo_to
    undo_to(mark, trail, store)


