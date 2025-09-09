"""
Stage 1.5: Static operator table for Prolog operators.
This module provides the single source of truth for operator precedence and associativity.
"""

from typing import Dict, Tuple, Optional

# Type alias for operator info: (precedence, type, canonical_form)
OperatorInfo = Tuple[int, str, str]

# The operator table: maps (operator, position) to (precedence, type, canonical)
# Position is 'infix', 'prefix', or 'postfix'
# Type is one of: fx, fy, xf, yf, xfx, xfy, yfx, yfy
OPERATOR_TABLE: Dict[Tuple[str, str], OperatorInfo] = {
    # Control flow operators
    (',', 'infix'): (1000, 'xfy', "','"),      # Conjunction (right-associative)
    (';', 'infix'): (1100, 'xfy', "';'"),      # Disjunction (right-associative)
    ('->', 'infix'): (1050, 'xfy', "'->'"),    # If-then (right-associative)
    
    # Equality and disequality
    ('=', 'infix'): (700, 'xfx', "'='"),       # Unification
    ('\\=', 'infix'): (700, 'xfx', "'\\\\='"),   # Not unifiable
    
    # Structural comparison
    ('==', 'infix'): (700, 'xfx', "'=='"),     # Structural equality
    ('\\==', 'infix'): (700, 'xfx', "'\\\\=='"), # Structural inequality
    
    # Term order
    ('@<', 'infix'): (700, 'xfx', "'@<'"),     # Term less than
    ('@>', 'infix'): (700, 'xfx', "'@>'"),     # Term greater than
    ('@=<', 'infix'): (700, 'xfx', "'@=<'"),   # Term less or equal
    ('@>=', 'infix'): (700, 'xfx', "'@>='"),   # Term greater or equal
    
    # Arithmetic comparison
    ('<', 'infix'): (700, 'xfx', "'<'"),       # Less than
    ('>', 'infix'): (700, 'xfx', "'>'"),       # Greater than
    ('=<', 'infix'): (700, 'xfx', "'=<'"),     # Less or equal
    ('>=', 'infix'): (700, 'xfx', "'>='"),     # Greater or equal
    ('=:=', 'infix'): (700, 'xfx', "'=:='"),   # Arithmetic equality
    ('=\\=', 'infix'): (700, 'xfx', "'=\\\\='"),  # Arithmetic inequality
    
    # Arithmetic operators
    ('+', 'infix'): (500, 'yfx', "'+'"),       # Addition (left-associative)
    ('-', 'infix'): (500, 'yfx', "'-'"),       # Subtraction (left-associative)
    ('*', 'infix'): (400, 'yfx', "'*'"),       # Multiplication (left-associative)
    ('/', 'infix'): (400, 'yfx', "'/'"),       # Division (left-associative)
    ('//', 'infix'): (400, 'yfx', "'//'"),     # Integer division (left-associative)
    ('mod', 'infix'): (400, 'yfx', "'mod'"),   # Modulo (left-associative)
    ('**', 'infix'): (200, 'xfy', "'**'"),     # Power (RIGHT-associative!)
    
    # Unary operators
    ('-', 'prefix'): (200, 'fy', "'-'"),       # Unary minus
    ('+', 'prefix'): (200, 'fy', "'+'"),       # Unary plus
    ('\\+', 'prefix'): (900, 'fy', "'\\\\+'"),   # Negation as failure
    
    # Other standard operators
    ('is', 'infix'): (700, 'xfx', "'is'"),     # Arithmetic evaluation
}

# Set of operators that are not yet supported in Stage 1 runtime
UNSUPPORTED_IN_STAGE1 = {
    ('//', 'infix'),
    ('mod', 'infix'),
    ('**', 'infix'),
}


def get_operator_info(operator: str, position: str) -> Optional[OperatorInfo]:
    """
    Get operator information from the table.
    
    Args:
        operator: The operator symbol (e.g., '+', '=<', 'mod')
        position: The position ('infix', 'prefix', or 'postfix')
    
    Returns:
        Tuple of (precedence: int, type: str, canonical_form: str) or None if not found.
        - precedence: Integer precedence value (200-1100)
        - type: Associativity type ('xfx', 'xfy', 'yfx', 'yfy', 'fx', 'fy', 'xf', 'yf')
        - canonical_form: Quoted atom form for the operator (e.g., "'+'", "'=<'")
        Returns None for unknown operators (never raises).
    """
    return OPERATOR_TABLE.get((operator, position))


def is_stage1_supported(operator: str, position: str) -> bool:
    """
    Check if an operator is supported in Stage 1 runtime.
    
    Args:
        operator: The operator symbol (e.g., '+', '//', 'mod')
        position: The position ('infix', 'prefix', or 'postfix')
    
    Returns:
        True if the operator is supported in Stage 1, False otherwise.
        Returns False for unknown operators (not in table) and for operators
        explicitly marked as unsupported in UNSUPPORTED_IN_STAGE1.
    """
    # Unknown operators are not supported
    if (operator, position) not in OPERATOR_TABLE:
        return False
    # Check if explicitly unsupported
    return (operator, position) not in UNSUPPORTED_IN_STAGE1


def get_all_operators() -> Dict[Tuple[str, str], OperatorInfo]:
    """
    Get the complete operator table.
    
    Returns:
        Dictionary mapping (operator, position) to (precedence, type, canonical)
    """
    return OPERATOR_TABLE.copy()


def is_operator(symbol: str) -> bool:
    """
    Check if a symbol is an operator (in any position).
    
    Args:
        symbol: The symbol to check
    
    Returns:
        True if the symbol is an operator in some position
    """
    return any(op == symbol for op, _ in OPERATOR_TABLE.keys())


def get_operator_positions(symbol: str) -> list[str]:
    """
    Get all positions where a symbol is defined as an operator.
    
    Args:
        symbol: The operator symbol
    
    Returns:
        List of positions ('infix', 'prefix', 'postfix') where the symbol is defined
    """
    return [pos for (op, pos) in OPERATOR_TABLE.keys() if op == symbol]


def is_xfx_operator(operator: str) -> bool:
    """
    Check if an operator is non-chainable (xfx type).
    
    Args:
        operator: The operator symbol
    
    Returns:
        True if the operator is xfx (non-chainable) in infix position
    """
    info = get_operator_info(operator, 'infix')
    return info is not None and info[1] == 'xfx'