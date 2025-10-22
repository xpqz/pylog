"""WAM meta-call builtins.

Implements ISO Prolog meta-call predicates:
- call/1: Dynamically execute a goal

Meta-call builtins enable higher-order programming by treating
goals as data that can be manipulated and invoked dynamically.
"""

from __future__ import annotations

from prolog.ast.terms import Atom, Int
from prolog.wam.errors import InstantiationError, TypeError
from prolog.wam.heap import TAG_CON, TAG_STR, is_ref
from prolog.wam.unify import deref

__all__ = [
    "builtin_call",
    "register_meta_builtins",
]


def builtin_call(machine) -> bool:
    """call(Goal): Dynamically execute Goal.

    Args:
        machine: Machine with X[0] containing goal address

    Returns:
        True if goal succeeds, False if goal fails

    Raises:
        InstantiationError: If Goal is unbound variable
        TypeError: If Goal is not callable (not atom or structure)

    Examples:
        ?- call(true).              % succeeds
        ?- call(fail).              % fails
        ?- call(append([1],[2],X)). % X=[1,2]
        ?- call(X).                 % error: instantiation_error
        ?- call(123).               % error: type_error(callable, 123)

    Implementation:
        1. Deref Goal from X[0]
        2. Check if bound (raise instantiation_error if unbound)
        3. Extract functor name and arity
        4. Build predicate symbol "module:name/arity"
        5. Look up predicate in predicate_table
        6. If atom (0-arity): call directly
        7. If structure: load arguments into X registers, then call
        8. Transfer control by setting P to predicate address
    """
    # Get goal from X[0]
    goal_addr = machine.X[0]
    goal_addr = deref(machine, goal_addr)
    goal_cell = machine.heap[goal_addr]

    # Check if unbound variable
    if is_ref(goal_cell) and goal_cell[1] == goal_addr:
        raise InstantiationError()

    # Extract functor and arity
    tag = goal_cell[0]

    if tag == TAG_CON:
        # Atom: 0-arity predicate
        functor = goal_cell[1]

        # Validate callable type
        if not isinstance(functor, str):
            # Convert non-callable to AST term for error
            if isinstance(functor, int):
                culprit = Int(functor)
            elif isinstance(functor, bool):
                culprit = Atom(str(functor).lower())
            else:
                culprit = Atom(str(functor))
            raise TypeError("callable", culprit)

        # Build predicate symbol: use "user" as default module
        pred_symbol = f"user:{functor}/0"

        # Look up predicate (will raise RuntimeError if undefined)
        try:
            entry_point = machine.resolve_predicate(pred_symbol)
        except RuntimeError:
            # Predicate not found: fail
            return False

        # Transfer control: save return address and jump to predicate
        # Note: OP_CALL_BUILTIN will do P += 1 after we return True,
        # so we set P to entry_point - 1 to compensate
        machine.CP = machine.P + 1
        machine.P = entry_point - 1
        return True

    elif tag == TAG_STR:
        # Structure: multi-arity predicate
        functor_addr = goal_cell[1]
        functor_cell = machine.heap[functor_addr]

        # Functor cell format: (TAG_CON, (name, arity))
        if functor_cell[0] != TAG_CON:
            # Malformed structure - use placeholder term
            raise TypeError("callable", Atom("?"))

        functor_data = functor_cell[1]
        if not isinstance(functor_data, tuple) or len(functor_data) != 2:
            # Malformed functor - use placeholder term
            raise TypeError("callable", Atom("?"))

        name, arity = functor_data

        # Validate functor name
        if not isinstance(name, str):
            # Invalid functor name - use placeholder term
            raise TypeError("callable", Atom("?"))

        # Build predicate symbol
        pred_symbol = f"user:{name}/{arity}"

        # Look up predicate
        try:
            entry_point = machine.resolve_predicate(pred_symbol)
        except RuntimeError:
            # Predicate not found: fail
            return False

        # Load arguments into X registers
        # Arguments are at functor_addr + 1 through functor_addr + arity
        machine.X = []
        for i in range(arity):
            arg_addr = functor_addr + 1 + i
            machine.X.append(arg_addr)

        # Transfer control
        # Note: OP_CALL_BUILTIN will do P += 1 after we return True,
        # so we set P to entry_point - 1 to compensate
        machine.CP = machine.P + 1
        machine.P = entry_point - 1
        return True

    else:
        # Invalid goal type (LIST or unknown tag) - use placeholder term
        raise TypeError("callable", Atom("?"))


def register_meta_builtins(registry: dict) -> None:
    """Register all meta-call builtins in the given registry.

    Args:
        registry: Builtin registry dict to populate

    Registers:
        - system:call/1
    """
    registry["system:call/1"] = builtin_call
