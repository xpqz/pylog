"""Pretty printer for Prolog terms and programs.

Provides functions to convert AST nodes back to readable Prolog syntax.
Supports round-trip property: parse(pretty(term)) == term.

Quoting rules:
- Atoms starting with lowercase letter followed by alphanumeric/underscore: unquoted
- Reserved tokens ([], :-, ,, |, etc.): quoted
- Everything else (spaces, punctuation, uppercase start, numeric): quoted

Operator mode:
- When operator_mode=True, recognized operator structures print as operators
- Parenthesization follows precedence and associativity rules
- When operator_mode=False (default), canonical form is used
"""

from typing import Dict, Optional, List, Tuple, Union
from prolog.ast.terms import Term, Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause
from prolog.parser.operators import get_operator_info

# Reserved tokens that always need quotes (except special cases)
_RESERVED = {
    "[]",
    ":-",
    ",",
    "|",
    ";",
    "{}",
}  # '!' is special - doesn't need quotes as cut


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
    if s == "!":
        return False

    # Reserved tokens need quotes
    if s in _RESERVED:
        return True

    # Check if it's a valid unquoted atom:
    # - Must start with lowercase letter
    # - Rest must be alphanumeric or underscore
    if s[0].islower() and all(c.isalnum() or c == "_" for c in s):
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
    s = s.replace("\\", "\\\\")
    s = s.replace("'", "\\'")
    s = s.replace("\n", "\\n")
    s = s.replace("\t", "\\t")
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


def _is_operator_struct(term: Struct) -> Optional[Tuple[str, int, str, str]]:
    """Check if a Struct represents an operator in canonical form.

    Args:
        term: The Struct to check

    Returns:
        Tuple of (position, precedence, type, canonical) if operator, None otherwise
    """
    # Check unary operators (arity 1)
    if len(term.args) == 1:
        info = get_operator_info(term.functor, "prefix")
        if info:
            return ("prefix", info[0], info[1], info[2])

    # Check binary operators (arity 2)
    elif len(term.args) == 2:
        info = get_operator_info(term.functor, "infix")
        if info:
            return ("infix", info[0], info[1], info[2])

    return None


def _needs_parens(
    parent_op: Optional[Tuple[str, int, str, str]],
    child_op: Optional[Tuple[str, int, str, str]],
    child_position: str,
) -> bool:
    """Determine if parentheses are needed around a child expression.

    Args:
        parent_op: Parent operator info (position, precedence, type, canonical) or None
        child_op: Child operator info or None
        child_position: 'left' or 'right' - position of child in parent

    Returns:
        True if parentheses are needed
    """
    if not parent_op or not child_op:
        return False

    parent_pos, parent_prec, parent_type, _ = parent_op
    child_pos, child_prec, child_type, _ = child_op

    # Lower precedence always needs parens
    if child_prec > parent_prec:  # Higher number = lower precedence in Prolog
        return True

    # Same precedence - check associativity
    if child_prec == parent_prec:
        # For xfx operators, any nesting needs parens
        if "xfx" in parent_type:
            return True

        # Check associativity conflicts
        if parent_pos == "infix" and child_pos == "infix":
            # Left-associative (yfx): right child needs parens
            if "yfx" in parent_type and child_position == "right":
                return True
            # Right-associative (xfy): left child needs parens
            if "xfy" in parent_type and child_position == "left":
                return True

    return False


def pretty(
    term: Term,
    var_names: Optional[Dict[int, str]] = None,
    operator_mode: bool = False,
    parent_op: Optional[Tuple[str, int, str, str]] = None,
) -> str:
    """Convert a term to its string representation.

    Supports two modes:
    - Canonical mode (default): Prints terms in canonical form with quoted functors
    - Operator mode: Prints operators using infix/prefix notation with minimal parentheses

    In operator mode, the pretty printer consults the operator table for precedence
    and associativity, printing the minimal parentheses required to preserve parse.
    The output is a valid Prolog term that round-trips via the Reader.

    Args:
        term: The term to pretty print
        var_names: Optional mapping of variable IDs to names. Applied after formatting
                  and does not interfere with operator spacing.
        operator_mode: If True, print operators in operator syntax. Default False
                      maintains backward compatibility with canonical mode.
        parent_op: Parent operator info for parenthesization (internal use only)

    Returns:
        String representation of the term. In operator mode, returns a valid Prolog
        term that satisfies: Reader().read_term(pretty(t, operator_mode=True)) == t
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

        # Check if this is an operator and we're in operator mode
        if operator_mode:
            op_info = _is_operator_struct(term)
            if op_info:
                position, precedence, op_type, _ = op_info

                if position == "prefix" and len(term.args) == 1:
                    # Unary operator
                    arg_str = pretty(term.args[0], var_names, operator_mode, op_info)

                    # Check if arg needs parens
                    arg_op = None
                    if isinstance(term.args[0], Struct):
                        arg_op = _is_operator_struct(term.args[0])

                    # Special case: unary operators need parens around binary operators
                    needs_parens = False
                    if arg_op:
                        arg_pos = arg_op[0]
                        arg_prec = arg_op[1]
                        my_prec = op_info[1]
                        # Always need parens around infix operators for clarity
                        # e.g., -(X + Y) not -X + Y
                        if arg_pos == "infix":
                            needs_parens = True
                        # For other prefix operators, need parens if same or higher precedence
                        elif arg_prec <= my_prec:
                            needs_parens = True

                    if needs_parens:
                        arg_str = f"({arg_str})"

                    # Special handling for negation operator spacing
                    if term.functor == "\\+":
                        return f"\\+{arg_str}"
                    else:
                        return f"{term.functor}{arg_str}"

                elif position == "infix" and len(term.args) == 2:
                    # Binary operator
                    left_str = pretty(term.args[0], var_names, operator_mode, op_info)
                    right_str = pretty(term.args[1], var_names, operator_mode, op_info)

                    # Check if children need parens
                    left_op = None
                    right_op = None
                    if isinstance(term.args[0], Struct):
                        left_op = _is_operator_struct(term.args[0])
                    if isinstance(term.args[1], Struct):
                        right_op = _is_operator_struct(term.args[1])

                    if _needs_parens(op_info, left_op, "left"):
                        left_str = f"({left_str})"
                    if _needs_parens(op_info, right_op, "right"):
                        right_str = f"({right_str})"

                    # Special case: if-then-else pattern (A -> B ; C) always needs outer parens
                    result = None
                    if term.functor == ";" and isinstance(term.args[0], Struct):
                        left_struct = term.args[0]
                        if left_struct.functor == "->" and len(left_struct.args) == 2:
                            # This is if-then-else, needs parens
                            if term.functor.isalpha():
                                result = f"({left_str} {term.functor} {right_str})"
                            else:
                                result = f"({left_str} {term.functor} {right_str})"

                    if result:
                        return result

                    # Handle spacing for different operators
                    if term.functor == ",":
                        # Comma has no spaces
                        return f"{left_str}, {right_str}"
                    elif term.functor.isalpha():
                        # Word operators like 'mod' need spaces
                        return f"{left_str} {term.functor} {right_str}"
                    else:
                        # Other operators have spaces
                        return f"{left_str} {term.functor} {right_str}"

        # Fall back to canonical form
        functor_str = pretty_atom(term.functor)
        args_str = ", ".join(pretty(arg, var_names, operator_mode) for arg in term.args)
        return f"{functor_str}({args_str})"

    elif isinstance(term, PrologList):
        if not term.items and isinstance(term.tail, Atom) and term.tail.name == "[]":
            # Empty list
            return "[]"
        elif term.tail is not None and not (
            isinstance(term.tail, Atom) and term.tail.name == "[]"
        ):
            # List with non-empty tail [H|T]
            if term.items:
                items_str = ", ".join(
                    pretty(item, var_names, operator_mode) for item in term.items
                )
                tail_str = pretty(term.tail, var_names, operator_mode)
                return f"[{items_str}|{tail_str}]"
            else:
                # Shouldn't happen but handle it
                return f"[|{pretty(term.tail, var_names, operator_mode)}]"
        else:
            # Normal list [1, 2, 3]
            items_str = ", ".join(
                pretty(item, var_names, operator_mode) for item in term.items
            )
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


def pretty_query(
    goals: Union[Tuple[Term, ...], List[Term]],
    var_names: Optional[Dict[int, str]] = None,
) -> str:
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
