"""Pretty printer for Prolog terms and programs.

Provides functions to convert AST nodes back to readable Prolog syntax.
Supports round-trip property: parse(pretty(term)) == term.

Quoting rules:
- Atoms starting with lowercase letter followed by alphanumeric/underscore: unquoted
- Reserved tokens ([], :-, ,, |, etc.): quoted
- Everything else (spaces, punctuation, uppercase start, numeric): quoted
"""

from typing import Dict, Optional, List, Tuple, Union
from prolog.ast.terms import Term, Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause

# Reserved tokens that always need quotes (except special cases)
_RESERVED = {'[]', ':-', ',', '|', ';', '{}'}  # '!' is special - doesn't need quotes as cut


def _needs_quotes(s: str) -> bool:
    """Check if an atom/functor name needs quotes.
    
    Args:
        s: The atom/functor name to check
        
    Returns:
        True if quotes are needed, False otherwise
    """
    # Empty string needs quotes
    if not s:
        return True
    
    # Special case: ! (cut) doesn't need quotes
    if s == '!':
        return False
    
    # Reserved tokens need quotes
    if s in _RESERVED:
        return True
    
    # Check if it's a valid unquoted atom:
    # - Must start with lowercase letter
    # - Rest must be alphanumeric or underscore
    if s[0].islower() and all(c.isalnum() or c == '_' for c in s):
        return False
    
    return True


def _escape_atom(s: str) -> str:
    """Escape special characters in an atom for quoted output.
    
    Escapes in order: backslash, single quote, newline, tab.
    This ensures idempotent escaping.
    
    Args:
        s: The string to escape
        
    Returns:
        The escaped string
    """
    # Order matters: backslash must be first to avoid double-escaping
    s = s.replace('\\', '\\\\')
    s = s.replace("'", "\\'")
    s = s.replace('\n', '\\n')
    s = s.replace('\t', '\\t')
    return s


def pretty_atom(name: str) -> str:
    """Format an atom name with quotes if needed.
    
    Args:
        name: The atom name
        
    Returns:
        The formatted atom (quoted and escaped if necessary)
    """
    if _needs_quotes(name):
        return f"'{_escape_atom(name)}'"
    return name


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
        return pretty_atom(term.name)
        
    elif isinstance(term, Int):
        return str(term.value)
        
    elif isinstance(term, Var):
        if term.id in var_names:
            return var_names[term.id]
        else:
            # Generate a stable name for this variable
            if term.hint == "_":
                # Anonymous variables always print as _
                # Don't store in var_names to keep them distinct
                return "_"
            elif term.hint:
                name = term.hint
            else:
                # Generate a stable name within this pretty call
                name = f"_G{term.id}"
            var_names[term.id] = name
            return name
            
    elif isinstance(term, Struct):
        if len(term.args) == 0:
            # Zero-arity structure prints as atom (no parentheses)
            return pretty_atom(term.functor)
        else:
            # Format with arguments
            functor_str = pretty_atom(term.functor)
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