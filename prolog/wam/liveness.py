"""Variable liveness analysis for WAM compilation.

Classifies variables as temporary (X) or permanent (Y) based on lifetime:
- Temporary: Dies before next call, single occurrence
- Permanent: Lives across calls, multiple occurrences

The classification determines register allocation:
- X registers: Temporary variables (not preserved across calls)
- Y registers: Permanent variables (preserved in environment frame)
"""

from typing import Set
from prolog.ast.clauses import Clause
from prolog.ast.terms import Term, Var, Struct, List, Atom, Int, Float


def extract_vars(term: Term) -> Set[int]:
    """Extract all variable IDs from a term recursively.

    Args:
        term: Any Prolog term

    Returns:
        Set of variable IDs found in term
    """
    if isinstance(term, Var):
        return {term.id}
    elif isinstance(term, Struct):
        vars_set = set()
        for arg in term.args:
            vars_set.update(extract_vars(arg))
        return vars_set
    elif isinstance(term, List):
        vars_set = set()
        for item in term.items:
            vars_set.update(extract_vars(item))
        vars_set.update(extract_vars(term.tail))
        return vars_set
    elif isinstance(term, (Atom, Int, Float)):
        return set()
    else:
        # Unknown term type - conservatively return empty set
        return set()


def classify_vars(clause: Clause) -> tuple[Set[int], Set[int]]:
    """Classify clause variables as temporary or permanent.

    Algorithm:
    1. Precompute first/last goal index for each var (head = -1)
    2. For each goal i:
       a) Classic "appears after call": if i>0, vars seen before → permanent
       b) Generator-input preservation: if i<n-1 and goal produces outputs
          used later, promote all inputs to permanent

    Args:
        clause: AST Clause node with head and body

    Returns:
        (temp_vars, perm_vars): Two sets of variable IDs

    Examples:
        p(X, Y) :- q(X), r(Y).
        -> X temporary (no generator outputs), Y permanent (appears after call)

        p(X) :- q(X, Y), r(Y, Z).
        -> X permanent (generator input), Y permanent (spans calls), Z temporary
    """
    perm_vars = set()

    # Collect head vars
    head_vars = extract_vars(clause.head)

    # Collect vars per goal
    goal_vars_list = [extract_vars(goal) for goal in clause.body]

    # Compute first and last occurrence for each var
    first_occurrence = {}
    last_occurrence = {}

    # Head vars have first occurrence at -1
    for var in head_vars:
        first_occurrence[var] = -1
        last_occurrence[var] = -1

    # Scan body goals
    for i, goal_vars in enumerate(goal_vars_list):
        for var in goal_vars:
            if var not in first_occurrence:
                first_occurrence[var] = i
            last_occurrence[var] = i

    # Process each goal
    n = len(goal_vars_list)
    for i, goal_vars in enumerate(goal_vars_list):
        # Rule 1: Classic "appears after call" promotion
        if i > 0:
            for var in goal_vars:
                if first_occurrence[var] < i:
                    perm_vars.add(var)

        # Rule 2: Generator-input preservation
        if i < n - 1:  # Non-last goal
            # Find outputs: vars first appearing in this goal and used later
            outputs = {
                v
                for v in goal_vars
                if first_occurrence[v] == i and last_occurrence[v] > i
            }

            if outputs:
                # This goal is a generator - promote all its inputs
                inputs = {v for v in goal_vars if first_occurrence[v] < i}
                perm_vars.update(inputs)

    # All vars not marked permanent are temporary
    all_vars = set(first_occurrence.keys())
    temp_vars = all_vars - perm_vars

    return temp_vars, perm_vars


def debug_liveness(clause: Clause, temp_vars: Set[int], perm_vars: Set[int]) -> None:
    """Pretty-print liveness analysis results.

    Args:
        clause: The analyzed clause
        temp_vars: Set of temporary variable IDs
        perm_vars: Set of permanent variable IDs
    """
    print(f"Clause: {clause.head}")
    print(f"Temporary (X): {sorted(temp_vars)}")
    print(f"Permanent (Y): {sorted(perm_vars)}")
    print(f"Allocate: {len(perm_vars)}")


__all__ = ["classify_vars", "debug_liveness", "extract_vars"]
