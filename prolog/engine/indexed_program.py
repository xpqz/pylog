"""
IndexedProgram wrapper for Engine integration with indexing.

This module provides a bridge between ClauseIndex and the Engine's
expected Program interface, enabling transparent indexing integration.
"""

from typing import List, Iterator, Optional, Tuple
from prolog.ast.terms import Term, Struct, Atom
from prolog.ast.clauses import Clause, Program
from prolog.engine.indexing import ClauseIndex, build_from_clauses
from prolog.unify.store import Store


class IndexedProgram:
    """
    Wrapper that provides Program-like interface over an indexed clause set.
    
    This allows the Engine to use indexing transparently by replacing
    the Program with an IndexedProgram when use_indexing=True.
    """
    
    def __init__(self, clauses, index: Optional[ClauseIndex] = None):
        """
        Initialize IndexedProgram with clauses and optional pre-built index.
        
        Args:
            clauses: List or tuple of clauses
            index: Optional pre-built ClauseIndex, will build if not provided
        """
        self.clauses = tuple(clauses) if not isinstance(clauses, tuple) else clauses
        self._index = index if index is not None else build_from_clauses(list(clauses))
    
    @classmethod
    def from_clauses(cls, clauses: List[Clause]) -> "IndexedProgram":
        """
        Create IndexedProgram from a list of clauses.
        
        Args:
            clauses: List of clauses to index
            
        Returns:
            New IndexedProgram with built index
        """
        return cls(tuple(clauses))
    
    @classmethod
    def from_program(cls, program: Program) -> "IndexedProgram":
        """
        Convert an existing Program to IndexedProgram.
        
        Args:
            program: Program to convert
            
        Returns:
            New IndexedProgram with same clauses
        """
        return cls(program.clauses)
    
    def clauses_for(self, functor: str, arity: int) -> List[int]:
        """
        Return indices of clauses matching functor/arity.
        
        This method maintains compatibility with Program.clauses_for()
        by returning clause indices in source order.
        
        Args:
            functor: The predicate name
            arity: The number of arguments
            
        Returns:
            List of clause indices in source order
        """
        # Build list of matching clause indices
        pred_key = (functor, arity)
        
        # For compatibility, we need to return global clause indices
        # We'll iterate through all clauses and collect matching indices
        result = []
        for i, clause in enumerate(self.clauses):
            head = clause.head
            if isinstance(head, Atom):
                if head.name == functor and arity == 0:
                    result.append(i)
            elif isinstance(head, Struct):
                if head.functor == functor and len(head.args) == arity:
                    result.append(i)
        
        return result
    
    def select(self, pred_key: Tuple[str, int], goal: Term, store: Store) -> Iterator[int]:
        """
        Select clause indices using indexing.
        
        This method uses the ClauseIndex to efficiently select matching
        clauses and returns their indices in source order.
        
        Args:
            pred_key: (predicate_name, arity) tuple
            goal: The goal term to match
            store: Store for dereferencing variables
            
        Yields:
            Clause indices that match the goal
        """
        # Use the index to get matching clauses
        matching_clauses = self._index.select(pred_key, goal, store)
        
        # We need to convert clauses back to indices
        # Build a clause-to-index map for efficiency
        clause_to_idx = {clause: i for i, clause in enumerate(self.clauses)}
        
        for clause in matching_clauses:
            if clause in clause_to_idx:
                yield clause_to_idx[clause]