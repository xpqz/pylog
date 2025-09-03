"""Variable Renaming for Clause Isolation (Stage 0)."""

from typing import Dict, Optional
from prolog.ast.terms import Term, Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause
from prolog.unify.store import Store


class VarRenamer:
    """Renames variables in terms to fresh variables from a Store.
    
    Each VarRenamer instance maintains a mapping for consistent renaming
    within a single clause, but each new renamer gets fresh variables.
    This ensures proper variable isolation between clause uses.
    """
    
    def __init__(self, store: Store):
        """Initialize with a Store for allocating fresh variables.
        
        Args:
            store: The Store to allocate fresh variables from.
        """
        self._store = store
        self._mapping: Dict[int, int] = {}  # Original var ID -> fresh var ID
    
    def rename_term(self, term: Term) -> Term:
        """Rename all variables in a term to fresh variables.
        
        Variables with the same ID in the input will map to the same
        fresh variable in the output (consistency within a clause).
        
        Args:
            term: The term to rename.
            
        Returns:
            A new term with all variables renamed.
        """
        if isinstance(term, Var):
            # Check if we've seen this variable before
            if term.id not in self._mapping:
                # Allocate a fresh variable
                fresh_id = self._store.new_var(hint=term.hint)
                self._mapping[term.id] = fresh_id
            
            # Return the mapped variable (preserving hint)
            return Var(self._mapping[term.id], term.hint)
        
        elif isinstance(term, (Atom, Int)):
            # Atoms and integers are unchanged
            return term
        
        elif isinstance(term, Struct):
            # Recursively rename arguments
            renamed_args = tuple(self.rename_term(arg) for arg in term.args)
            return Struct(term.functor, renamed_args)
        
        elif isinstance(term, PrologList):
            # Rename items and tail
            renamed_items = tuple(self.rename_term(item) for item in term.items)
            renamed_tail = self.rename_term(term.tail)
            return PrologList(renamed_items, tail=renamed_tail)
        
        else:
            raise TypeError(f"Unknown term type: {type(term)}")
    
    def rename_clause(self, clause: Clause) -> Clause:
        """Rename all variables in a clause to fresh variables.
        
        All occurrences of the same variable in the clause (head and body)
        will be renamed to the same fresh variable.
        
        Args:
            clause: The clause to rename.
            
        Returns:
            A new clause with all variables renamed.
        """
        # Rename head
        renamed_head = self.rename_term(clause.head)
        
        # Rename body goals (if any)
        renamed_body = tuple(self.rename_term(goal) for goal in clause.body)
        
        return Clause(head=renamed_head, body=renamed_body)