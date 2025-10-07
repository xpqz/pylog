"""Variable Renaming for Clause Isolation (Stage 0)."""

from typing import Dict
from prolog.ast.terms import Term, Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause
from prolog.unify.store import Store


class VarRenamer:
    """Renames variables in terms to fresh variables from a Store.

    Each rename_clause call gets a fresh mapping to ensure proper
    variable isolation between clause instances.
    """

    def __init__(self, store: Store):
        """Initialize with a Store for allocating fresh variables.

        Args:
            store: The Store to allocate fresh variables from.
        """
        self._store = store

    def rename_term(self, term: Term, mapping: Dict[int, int]) -> Term:
        """Rename all variables in a term to fresh variables (iterative).

        Variables with the same ID in the input will map to the same
        fresh variable in the output (consistency within a clause).

        Args:
            term: The term to rename.
            mapping: Variable mapping for this renaming operation.

        Returns:
            A new term with all variables renamed.
        """
        # Use iterative approach to avoid stack overflow on deep structures
        # We'll do a two-pass approach: first collect all terms, then build bottom-up

        # First pass: collect all terms in depth-first order
        all_terms = []
        visited = set()
        stack = [term]

        while stack:
            current = stack.pop()
            if id(current) in visited:
                continue
            visited.add(id(current))
            all_terms.append(current)

            if isinstance(current, Struct):
                # Add args in reverse for depth-first order
                for arg in reversed(current.args):
                    stack.append(arg)
            elif isinstance(current, PrologList):
                stack.append(current.tail)
                for item in reversed(current.items):
                    stack.append(item)

        # Second pass: rename bottom-up
        renamed = {}  # id(original) -> renamed term

        for current in reversed(all_terms):
            if isinstance(current, Var):
                # Check if we've seen this variable before
                if current.id not in mapping:
                    # Allocate a fresh variable
                    fresh_id = self._store.new_var(hint=current.hint)
                    mapping[current.id] = fresh_id

                # Create renamed variable
                renamed[id(current)] = Var(mapping[current.id], current.hint)

            elif isinstance(current, (Atom, Int)):
                # Atoms and integers are unchanged
                renamed[id(current)] = current

            elif isinstance(current, Struct):
                # All args should already be renamed
                renamed_args = tuple(renamed[id(arg)] for arg in current.args)
                renamed[id(current)] = Struct(current.functor, renamed_args)

            elif isinstance(current, PrologList):
                # All items and tail should already be renamed
                renamed_items = tuple(renamed[id(item)] for item in current.items)
                renamed_tail = renamed[id(current.tail)]
                renamed[id(current)] = PrologList(renamed_items, tail=renamed_tail)
            else:
                raise TypeError(f"Unknown term type: {type(current)}")

        return renamed[id(term)]

    def rename_clause(self, clause: Clause) -> Clause:
        """Rename all variables in a clause to fresh variables.

        All occurrences of the same variable in the clause (head and body)
        will be renamed to the same fresh variable. Each call to this method
        creates a fresh mapping to ensure isolation between clause instances.

        Args:
            clause: The clause to rename.

        Returns:
            A new clause with all variables renamed.
        """
        # Create a fresh mapping for this clause instance
        mapping = {}  # Original var ID -> fresh var ID

        # Rename head
        renamed_head = self.rename_term(clause.head, mapping)

        # Rename body goals iteratively to avoid recursion
        renamed_body = []
        for goal in clause.body:
            renamed_goal = self.rename_term(goal, mapping)
            renamed_body.append(renamed_goal)

        # Debug assertion: verify sharing is preserved
        if __debug__:
            # Check that repeated variables in head are properly shared
            head_vars = self._collect_vars(clause.head)
            for orig_id in head_vars:
                occurrences = [v for v in head_vars[orig_id]]
                if len(occurrences) > 1:
                    # This variable appears multiple times
                    renamed_ids = set()
                    for orig_var in occurrences:
                        # Find corresponding renamed var
                        if orig_var.id in mapping:
                            renamed_ids.add(mapping[orig_var.id])
                    assert (
                        len(renamed_ids) == 1
                    ), f"Variable sharing not preserved: {orig_id} mapped to {renamed_ids}"

        return Clause(head=renamed_head, body=tuple(renamed_body))

    def _collect_vars(self, term: Term) -> Dict[int, list]:
        """Collect all variables in a term, grouped by ID."""
        vars_by_id = {}
        stack = [term]

        while stack:
            current = stack.pop()
            if isinstance(current, Var):
                if current.id not in vars_by_id:
                    vars_by_id[current.id] = []
                vars_by_id[current.id].append(current)
            elif isinstance(current, Struct):
                stack.extend(current.args)
            elif isinstance(current, PrologList):
                stack.extend(current.items)
                stack.append(current.tail)

        return vars_by_id
