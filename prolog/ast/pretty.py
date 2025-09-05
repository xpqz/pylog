"""Pretty printer for Prolog terms and programs.

Provides functions to convert AST nodes back to readable Prolog syntax.
Supports round-trip property: parse(pretty(term)) == term.
"""

from typing import Dict, Optional, List, Tuple, Union
import re
from prolog.ast.terms import Term, Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause


def _needs_quotes(atom_name: str) -> bool:
    """Check if an atom needs quotes.
    
    Args:
        atom_name: The atom name to check
        
    Returns:
        True if the atom needs quotes, False otherwise
    """
    # Empty string needs quotes
    if not atom_name:
        return True
    
    # Special atoms that need quotes
    if atom_name in ['[]', '!', ':-', ',', ';', '|', '{}']:
        # ! doesn't need quotes as cut
        if atom_name == '!':
            return False
        # All others need quotes (including '[]')
        return True
    
    # Check if it's a valid unquoted atom
    # Must start with lowercase letter
    if not atom_name[0].islower():
        return True
    
    # Rest must be alphanumeric or underscore
    for char in atom_name[1:]:
        if not (char.isalnum() or char == '_'):
            return True
    
    return False


def _escape_atom(atom_name: str) -> str:
    """Escape special characters in an atom for quoted output.
    
    Args:
        atom_name: The atom name to escape
        
    Returns:
        The escaped atom name
    """
    # Escape backslashes first
    result = atom_name.replace('\\', '\\\\')
    # Escape single quotes
    result = result.replace("'", "\\'")
    # Escape newlines
    result = result.replace('\n', '\\n')
    # Escape tabs
    result = result.replace('\t', '\\t')
    return result


def pretty(term: Term, var_names: Optional[Dict[int, str]] = None) -> str:
    """Convert a term to its string representation.
    
    Args:
        term: The term to pretty print
        var_names: Optional mapping of variable IDs to names
        
    Returns:
        String representation of the term
    """
    if var_names is None:
        var_names = {}
        
    if isinstance(term, Atom):
        if _needs_quotes(term.name):
            return f"'{_escape_atom(term.name)}'"
        return term.name
        
    elif isinstance(term, Int):
        return str(term.value)
        
    elif isinstance(term, Var):
        if term.id in var_names:
            return var_names[term.id]
        else:
            # Generate a name for this variable
            if term.hint:
                name = term.hint
            else:
                name = f"_G{term.id}"
            var_names[term.id] = name
            return name
            
    elif isinstance(term, Struct):
        if len(term.args) == 0:
            # Zero-arity structure is just the functor
            if _needs_quotes(term.functor):
                return f"'{_escape_atom(term.functor)}'"
            return term.functor
        else:
            # Format with arguments
            functor_str = term.functor
            if _needs_quotes(term.functor):
                functor_str = f"'{_escape_atom(term.functor)}'"
            args_str = ", ".join(pretty(arg, var_names) for arg in term.args)
            return f"{functor_str}({args_str})"
            
    elif isinstance(term, PrologList):
        if not term.items and isinstance(term.tail, Atom) and term.tail.name == '[]':
            # Empty list
            return "[]"
        elif term.tail is not None and not (isinstance(term.tail, Atom) and term.tail.name == '[]'):
            # List with non-empty tail [H|T]
            if term.items:
                items_str = ", ".join(pretty(item, var_names) for item in term.items)
                tail_str = pretty(term.tail, var_names)
                return f"[{items_str}|{tail_str}]"
            else:
                # Shouldn't happen but handle it
                return f"[|{pretty(term.tail, var_names)}]"
        else:
            # Normal list [1, 2, 3]
            items_str = ", ".join(pretty(item, var_names) for item in term.items)
            return f"[{items_str}]"
            
    else:
        raise ValueError(f"Unknown term type: {type(term)}")


def pretty_clause(clause: Clause, var_names: Optional[Dict[int, str]] = None) -> str:
    """Convert a clause to its string representation.
    
    Args:
        clause: The clause to pretty print
        var_names: Optional mapping of variable IDs to names
        
    Returns:
        String representation of the clause
    """
    if var_names is None:
        var_names = {}
        
    head_str = pretty(clause.head, var_names)
    
    if not clause.body:
        # Fact
        return f"{head_str}."
    else:
        # Rule
        body_strs = [pretty(goal, var_names) for goal in clause.body]
        body_str = ", ".join(body_strs)
        return f"{head_str} :- {body_str}."


def pretty_query(goals: Union[Tuple[Term, ...], List[Term]], var_names: Optional[Dict[int, str]] = None) -> str:
    """Convert a query (list of goals) to its string representation.
    
    Args:
        goals: The goals to pretty print
        var_names: Optional mapping of variable IDs to names
        
    Returns:
        String representation of the query
    """
    if var_names is None:
        var_names = {}
        
    if not goals:
        return "?- true."
        
    goal_strs = [pretty(goal, var_names) for goal in goals]
    goals_str = ", ".join(goal_strs)
    return f"?- {goals_str}."


def pretty_solution(bindings: Dict[str, Term]) -> str:
    """Convert solution bindings to their string representation.
    
    Args:
        bindings: Dictionary mapping variable names to terms
        
    Returns:
        String representation of the solution
    """
    if not bindings:
        return "true"
        
    var_names = {}
    binding_strs = []
    for var_name, value in sorted(bindings.items()):
        value_str = pretty(value, var_names)
        binding_strs.append(f"{var_name} = {value_str}")
        
    return ", ".join(binding_strs)


def pretty_program(clauses: List[Clause]) -> str:
    """Convert a program (list of clauses) to its string representation.
    
    Args:
        clauses: The clauses to pretty print
        
    Returns:
        String representation of the program
    """
    if not clauses:
        return ""
        
    # Use a shared var_names dict for the whole program
    # to ensure consistent variable naming
    var_names = {}
    clause_strs = []
    
    for clause in clauses:
        # Each clause gets its own variable namespace
        clause_var_names = {}
        clause_strs.append(pretty_clause(clause, clause_var_names))
        
    return "\n".join(clause_strs)