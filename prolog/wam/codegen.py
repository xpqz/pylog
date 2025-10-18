"""WAM code generation from Prolog clauses.

This module provides functions to compile Prolog clauses into WAM instruction sequences.
The compilation process uses liveness analysis and register allocation to generate
efficient get/put/unify sequences for pattern matching and term construction.
"""

from prolog.ast.terms import Atom, Int, Float, Var, Struct, List
from prolog.wam.instructions import (
    OP_ALLOCATE,
    OP_DEALLOCATE,
    OP_GET_VARIABLE,
    OP_GET_VALUE,
    OP_GET_CONSTANT,
    OP_GET_STRUCTURE,
    OP_UNIFY_VARIABLE,
    OP_UNIFY_VALUE,
    OP_UNIFY_CONSTANT,
    OP_PUT_VARIABLE,
    OP_PUT_VALUE,
    OP_PUT_CONSTANT,
    OP_PUT_STRUCTURE,
    OP_CALL,
    OP_EXECUTE,
    OP_PROCEED,
)
from prolog.wam.liveness import extract_vars

__all__ = ["compile_head", "build_list_struct", "compile_body", "finalize_clause"]


def _get_constant_value(term):
    """Extract raw constant value from AST term.

    Args:
        term: AST term (Atom, Int, Float, or raw value)

    Returns:
        Raw value suitable for WAM constant instructions (str, int, float)

    Examples:
        >>> _get_constant_value(Atom("foo"))
        "foo"
        >>> _get_constant_value(Int(42))
        42
        >>> _get_constant_value(Float(3.14))
        3.14
    """
    if isinstance(term, Atom):
        return term.name
    elif isinstance(term, Int):
        return term.value
    elif isinstance(term, Float):
        return term.value
    else:
        return term


def build_list_struct(list_term):
    """Transform a List AST node into nested ./2 Struct nodes.

    Prolog lists are syntactic sugar for nested pairs using the dot functor:
    - [H|T] becomes .(H, T)
    - [a, b|T] becomes .(a, .(b, T))
    - [X, Y, Z] becomes .(X, .(Y, .(Z, [])))

    The transformation builds nested structures from right to left, starting
    with the tail and wrapping each item in a ./2 structure.

    Args:
        list_term: List AST node with items tuple and optional tail

    Returns:
        Struct node representing nested ./2 pairs, or Atom("[]") for empty list

    Examples:
        >>> build_list_struct(List(items=(Atom("a"),), tail=Var(1, "T")))
        Struct(".", (Atom("a"), Var(1, "T")))

        >>> build_list_struct(List(items=(Atom("a"), Atom("b")), tail=Atom("[]")))
        Struct(".", (Atom("a"), Struct(".", (Atom("b"), Atom("[]")))))
    """
    if not list_term.items:
        # Empty list: []
        return Atom("[]")

    # Build nested pairs from right to left
    # Start with tail (or [] if no tail specified)
    result = list_term.tail if list_term.tail else Atom("[]")

    # Wrap each item in ./2, processing right-to-left
    for item in reversed(list_term.items):
        result = Struct(".", (item, result))

    return result


def compile_head(clause, register_map, perm_count):
    """Compile clause head to WAM get/unify instruction sequence.

    Args:
        clause: Clause with head to compile
        register_map: dict mapping var_id -> ("X"|"Y", index)
        perm_count: Number of permanent variables (for allocate K)

    Returns:
        List of instruction tuples

    Notes:
        - Emits allocate K iff perm_count > 0
        - Argument register indices (Aj) are 0-based
        - Constants emitted as raw values (int/float/string), not AST terms
        - Tracks variable first/subsequent occurrences across contexts
        - Lists compiled as ./2 structures per Prolog semantics
    """
    instructions = []
    seen_vars = set()  # Track which variables have been seen

    # Find max X register index for temp allocation
    max_x = -1
    for vid, (bank, idx) in register_map.items():
        if bank == "X" and idx > max_x:
            max_x = idx
    next_temp_x = max_x + 1  # Counter for temp X allocation

    # Step 1: Emit allocate K if permanent variables present
    if perm_count > 0:
        instructions.append((OP_ALLOCATE, perm_count))

    # Helper to compile term in unify context (inside structure)
    def compile_unify_term(term):
        nonlocal seen_vars, next_temp_x

        if isinstance(term, Var):
            reg = register_map[term.id]
            if term.id in seen_vars:
                # Subsequent occurrence -> unify_value
                instructions.append((OP_UNIFY_VALUE, reg))
            else:
                # First occurrence -> unify_variable
                instructions.append((OP_UNIFY_VARIABLE, reg))
                seen_vars.add(term.id)

        elif isinstance(term, (Atom, Int, Float)):
            # Constant -> unify_constant
            instructions.append((OP_UNIFY_CONSTANT, _get_constant_value(term)))

        elif isinstance(term, Struct):
            # Nested structure: allocate temp register, emit get_structure, recurse
            temp_idx = next_temp_x
            next_temp_x += 1
            temp_reg = ("X", temp_idx)

            # Emit unify_variable for temp register to hold structure
            instructions.append((OP_UNIFY_VARIABLE, temp_reg))

            # Emit get_structure for nested structure with int index
            instructions.append(
                (OP_GET_STRUCTURE, (term.functor, len(term.args)), temp_idx)
            )

            # Compile arguments
            for arg in term.args:
                compile_unify_term(arg)

        elif isinstance(term, List):
            # Transform list to nested ./2 structures and compile
            list_struct = build_list_struct(term)
            if isinstance(list_struct, Atom):
                # Empty list -> constant
                instructions.append((OP_UNIFY_CONSTANT, "[]"))
            else:
                # Nested pairs -> compile as structure
                compile_unify_term(list_struct)

    # Step 2: Compile head arguments
    head = clause.head

    # Handle atom predicates (no arguments)
    if isinstance(head, Atom):
        return instructions

    # Process each argument position
    for aj_idx, arg in enumerate(head.args):

        if isinstance(arg, Var):
            # Variable at top level
            reg = register_map[arg.id]
            if arg.id in seen_vars:
                # Subsequent occurrence -> get_value
                instructions.append((OP_GET_VALUE, reg, aj_idx))
            else:
                # First occurrence -> get_variable
                instructions.append((OP_GET_VARIABLE, reg, aj_idx))
                seen_vars.add(arg.id)

        elif isinstance(arg, (Atom, Int, Float)):
            # Constant at top level -> get_constant
            instructions.append((OP_GET_CONSTANT, _get_constant_value(arg), aj_idx))

        elif isinstance(arg, Struct):
            # Structure at top level
            instructions.append(
                (OP_GET_STRUCTURE, (arg.functor, len(arg.args)), aj_idx)
            )

            # Compile structure arguments
            for struct_arg in arg.args:
                compile_unify_term(struct_arg)

        elif isinstance(arg, List):
            # Transform list to nested ./2 structures and compile
            list_struct = build_list_struct(arg)
            if isinstance(list_struct, Atom):
                # Empty list -> get_constant
                instructions.append((OP_GET_CONSTANT, "[]", aj_idx))
            else:
                # Nested pairs -> get_structure for outermost pair
                instructions.append((OP_GET_STRUCTURE, (".", 2), aj_idx))
                compile_unify_term(list_struct.args[0])  # First item
                compile_unify_term(list_struct.args[1])  # Nested tail

    return instructions


def compile_body(clause, register_map):
    """Compile clause body to WAM put/call instruction sequence.

    Args:
        clause: Clause with body to compile
        register_map: dict mapping var_id -> ("X"|"Y", index)

    Returns:
        List of instruction tuples

    Notes:
        - Emits put_* instructions for goal arguments
        - Emits call instructions for each body goal
        - Tracks variable first/subsequent occurrences across entire body
        - Empty body returns empty list
        - Lists compiled as ./2 structures per Prolog semantics
        - Call targets formatted as "user:{functor}/{arity}"
    """
    instructions = []

    # Handle empty body
    if not clause.body:
        return []

    # Single seen set across entire body for first/subsequent occurrence tracking
    # Pre-populate with head variables (they're already bound from head matching)
    seen_vars = set()
    head_vars = extract_vars(clause.head)
    seen_vars.update(head_vars)

    # Find max X register index for temp allocation
    max_x = -1
    for vid, (bank, idx) in register_map.items():
        if bank == "X" and idx > max_x:
            max_x = idx
    next_temp_x = max_x + 1  # Counter for temp X allocation

    # Helper to compile term in unify context (inside structure being built)
    def compile_unify_term_body(term):
        nonlocal seen_vars, next_temp_x

        if isinstance(term, Var):
            reg = register_map[term.id]
            if term.id in seen_vars:
                # Subsequent occurrence -> unify_value
                instructions.append((OP_UNIFY_VALUE, reg))
            else:
                # First occurrence -> unify_variable
                instructions.append((OP_UNIFY_VARIABLE, reg))
                seen_vars.add(term.id)

        elif isinstance(term, (Atom, Int, Float)):
            # Constant -> unify_constant
            instructions.append((OP_UNIFY_CONSTANT, _get_constant_value(term)))

        elif isinstance(term, Struct):
            # Nested structure: allocate temp register, emit get_structure, recurse
            temp_idx = next_temp_x
            next_temp_x += 1
            temp_reg = ("X", temp_idx)

            # Emit unify_variable for temp register to hold nested structure
            instructions.append((OP_UNIFY_VARIABLE, temp_reg))

            # Emit get_structure for nested structure with int index
            # This matches head semantics: unify_variable creates a REF,
            # get_structure binds that REF to the allocated structure
            instructions.append(
                (OP_GET_STRUCTURE, (term.functor, len(term.args)), temp_idx)
            )

            # Compile arguments
            for arg in term.args:
                compile_unify_term_body(arg)

        elif isinstance(term, List):
            # Transform list to nested ./2 structures and compile
            list_struct = build_list_struct(term)
            if isinstance(list_struct, Atom):
                # Empty list -> constant
                instructions.append((OP_UNIFY_CONSTANT, "[]"))
            else:
                # Nested pairs -> compile as structure
                compile_unify_term_body(list_struct)

    # Helper to compile a single goal argument term
    def compile_put_term(term, aj_idx):
        nonlocal seen_vars, next_temp_x

        if isinstance(term, Var):
            # Variable
            reg = register_map[term.id]
            if term.id in seen_vars:
                # Subsequent occurrence -> put_value
                instructions.append((OP_PUT_VALUE, reg, aj_idx))
            else:
                # First occurrence -> put_variable
                instructions.append((OP_PUT_VARIABLE, reg, aj_idx))
                seen_vars.add(term.id)

        elif isinstance(term, (Atom, Int, Float)):
            # Constant -> put_constant
            instructions.append((OP_PUT_CONSTANT, _get_constant_value(term), aj_idx))

        elif isinstance(term, Struct):
            # Structure: build into temp Xk, then put_value
            temp_idx = next_temp_x
            next_temp_x += 1
            temp_reg = ("X", temp_idx)

            # Emit put_structure into temp register
            instructions.append(
                (OP_PUT_STRUCTURE, (term.functor, len(term.args)), temp_idx)
            )

            # Compile structure arguments
            for arg in term.args:
                compile_unify_term_body(arg)

            # Put the built structure into target argument register
            instructions.append((OP_PUT_VALUE, temp_reg, aj_idx))

        elif isinstance(term, List):
            # Transform list to nested ./2 structures and compile
            list_struct = build_list_struct(term)
            if isinstance(list_struct, Atom):
                # Empty list -> put_constant
                instructions.append((OP_PUT_CONSTANT, "[]", aj_idx))
            else:
                # Nested pairs: build structure into temp, then put_value
                temp_idx = next_temp_x
                next_temp_x += 1
                temp_reg = ("X", temp_idx)

                # Build outermost ./2 structure
                instructions.append((OP_PUT_STRUCTURE, (".", 2), temp_idx))
                compile_unify_term_body(list_struct.args[0])  # First item
                compile_unify_term_body(list_struct.args[1])  # Nested tail

                # Put the built list into target argument register
                instructions.append((OP_PUT_VALUE, temp_reg, aj_idx))

    # Process each goal in body
    for goal in clause.body:

        if isinstance(goal, Atom):
            # Atom goal (0-arity predicate)
            target = f"user:{goal.name}/0"
            instructions.append((OP_CALL, target))

        elif isinstance(goal, Struct):
            # Structure goal (N-arity predicate)

            # Emit put sequences for each argument
            for aj_idx, arg in enumerate(goal.args):
                compile_put_term(arg, aj_idx)

            # Emit call
            target = f"user:{goal.functor}/{len(goal.args)}"
            instructions.append((OP_CALL, target))

    return instructions


def finalize_clause(head_instructions, body_instructions, perm_count):
    """Add frame management and return instructions to complete clause.

    Combines head and body instruction sequences and adds proper frame management
    (allocate/deallocate) and return instructions (proceed/execute). Implements
    last call optimization (LCO) by replacing the final CALL with EXECUTE.

    Args:
        head_instructions: Instructions from compile_head()
        body_instructions: Instructions from compile_body()
        perm_count: Number of permanent variables (Y registers)

    Returns:
        Complete instruction sequence with allocate/deallocate/proceed or execute

    Notes:
        - allocate K already emitted by compile_head() if perm_count > 0
        - LCO: Replace last CALL with EXECUTE (tail call optimization)
        - deallocate before return if perm_count > 0
        - proceed for non-LCO returns
        - LCO applies to any last call, including fail/0 (true WAM semantics)

    Examples:
        Fact (no body):
            [get_..., proceed]

        Single goal, no permanents:
            [get_..., put_..., execute user:q/1]

        Multi-goal, with permanents:
            [allocate 1, get_..., put_..., call user:q/1, deallocate, execute user:r/1]
    """
    # Combine head and body instructions
    code = [*head_instructions, *body_instructions]

    # Empty body (fact): append proceed
    if not body_instructions:
        # For facts, deallocate before proceed if perm_count > 0
        # (In practice, facts shouldn't have permanent vars, but guard anyway)
        if perm_count > 0:
            code.append((OP_DEALLOCATE,))
        code.append((OP_PROCEED,))
        return code

    # Non-empty body: check if last instruction is CALL (LCO candidate)
    last_instr = code[-1]
    last_op = last_instr[0]

    if last_op == OP_CALL:
        # LCO: Replace last CALL with EXECUTE
        target = last_instr[1]

        # If permanent variables, insert deallocate before the execute
        if perm_count > 0:
            code.insert(len(code) - 1, (OP_DEALLOCATE,))

        # Replace CALL with EXECUTE
        code[-1] = (OP_EXECUTE, target)
    elif last_op == OP_EXECUTE or last_op == OP_PROCEED:
        # Defensive: already finalized (shouldn't happen in normal compilation)
        # Return as-is to avoid appending duplicate returns
        return code
    else:
        # Non-LCO case: append deallocate (if needed) and proceed
        # (Shouldn't happen in normal code since compile_body always ends with CALL,
        #  but handle gracefully for future compiler phases)
        if perm_count > 0:
            code.append((OP_DEALLOCATE,))
        code.append((OP_PROCEED,))

    return code
