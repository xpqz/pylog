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
    OP_TRY_ME_ELSE,
    OP_RETRY_ME_ELSE,
    OP_TRUST_ME,
)
from prolog.wam.liveness import extract_vars

__all__ = [
    "compile_head",
    "build_list_struct",
    "compile_body",
    "compile_disjunction",
    "finalize_clause",
]

# Global counter for generating unique labels
_label_counter = 0


def _gen_label():
    """Generate unique label for choicepoint branches.

    Returns:
        Unique label string (e.g., "L1", "L2", ...)
    """
    global _label_counter
    _label_counter += 1
    return f"L{_label_counter}"


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


def compile_disjunction(branches, register_map, is_last_goal, module="user"):
    """Compile disjunction (;/2) into choicepoint instruction sequence.

    Implements the WAM choicepoint pattern for disjunction:
    - try_me_else for first N-1 branches
    - retry_me_else between middle branches
    - trust_me before final branch
    - Each branch gets a label for backtracking

    Args:
        branches: List of goal terms (AST nodes) representing disjunction branches
        register_map: dict mapping var_id -> ("X"|"Y", index)
        is_last_goal: Whether this disjunction is the last goal in the clause
        module: Current module context (default "user")

    Returns:
        List of instruction tuples including choicepoint instructions and labels

    Example:
        (a ; b ; c) compiles to:
            try_me_else L1
            <code for a>
            retry_me_else L2
        L1:
            <code for b>
            trust_me
        L2:
            <code for c>

    Notes:
        - Single branch: compiles without choicepoint overhead
        - Labels generated using _gen_label() for uniqueness
        - Each branch compiled as if it were a standalone goal
        - Uses execute for last goal (LCO), call otherwise
    """
    instructions = []

    # Edge case: single branch (not really a disjunction)
    if len(branches) == 1:
        # Just compile the goal without choicepoint overhead
        goal = branches[0]
        goal_instrs = _compile_single_goal(
            goal, register_map, is_last_goal, module, set()
        )
        return goal_instrs

    # Multiple branches: generate choicepoint sequence
    num_branches = len(branches)

    # Generate labels: one for each branch after the first
    branch_labels = [_gen_label() for _ in range(num_branches - 1)]

    for i, branch in enumerate(branches):
        # Before each branch (except first), place label
        if i > 0:
            instructions.append(("LABEL", branch_labels[i - 1]))

        # Emit choicepoint instruction before branch code
        if i == 0:
            # First branch: try_me_else to next branch
            instructions.append((OP_TRY_ME_ELSE, branch_labels[0]))
        elif i < num_branches - 1:
            # Middle branches: retry_me_else to next branch
            instructions.append((OP_RETRY_ME_ELSE, branch_labels[i]))
        else:
            # Last branch: trust_me (no label arg)
            instructions.append((OP_TRUST_ME,))

        # Compile the branch goal
        branch_instrs = _compile_single_goal(
            branch, register_map, is_last_goal, module, set()
        )
        instructions.extend(branch_instrs)

    return instructions


def _compile_single_goal(goal, register_map, is_last, module, seen_vars):
    """Compile a single goal term into put/call or put/execute sequence.

    Args:
        goal: AST term representing the goal
        register_map: dict mapping var_id -> ("X"|"Y", index)
        is_last: Whether this is the last goal (for LCO)
        module: Module context for unqualified goals
        seen_vars: Set of var_ids already seen (for first/subsequent occurrence)

    Returns:
        List of instruction tuples
    """
    instructions = []

    # Handle module qualification
    if isinstance(goal, Struct) and goal.functor == ":" and len(goal.args) == 2:
        module_term, actual_goal = goal.args
        if isinstance(module_term, Atom):
            call_module = module_term.name
            goal = actual_goal
        else:
            call_module = module
    else:
        call_module = module

    # Compile based on goal type
    if isinstance(goal, Atom):
        # 0-arity predicate
        target = f"{call_module}:{goal.name}/0"
        if is_last:
            instructions.append((OP_EXECUTE, target))
        else:
            instructions.append((OP_CALL, target))

    elif isinstance(goal, Struct):
        # N-arity predicate: emit put instructions for arguments
        # (We need a helper to compile arguments - for now, simplified)
        for aj_idx, arg in enumerate(goal.args):
            if isinstance(arg, Var):
                reg = register_map.get(arg.id, ("X", 0))  # Default to X0 if not in map
                if arg.id in seen_vars:
                    instructions.append((OP_PUT_VALUE, reg, aj_idx))
                else:
                    instructions.append((OP_PUT_VARIABLE, reg, aj_idx))
                    seen_vars.add(arg.id)
            elif isinstance(arg, (Atom, Int, Float)):
                instructions.append((OP_PUT_CONSTANT, _get_constant_value(arg), aj_idx))
            # TODO: Handle Struct and List arguments

        target = f"{call_module}:{goal.functor}/{len(goal.args)}"
        if is_last:
            instructions.append((OP_EXECUTE, target))
        else:
            instructions.append((OP_CALL, target))

    return instructions


def compile_body(clause, register_map, module="user"):
    """Compile clause body to WAM put/call instruction sequence.

    Args:
        clause: Clause with body to compile
        register_map: dict mapping var_id -> ("X"|"Y", index)
        module: Current module context for unqualified calls (default "user")

    Returns:
        List of instruction tuples

    Notes:
        - Emits put_* instructions for goal arguments
        - Emits OP_CALL for non-last goals, OP_EXECUTE for last goal (LCO)
        - Tracks variable first/subsequent occurrences across entire body
        - Empty body returns empty list
        - Lists compiled as ./2 structures per Prolog semantics
        - Unqualified calls use module parameter: "module:functor/arity"
        - Qualified calls (M:Goal) use explicit module M
        - Symbol format always "module:functor/arity"
    """
    instructions = []

    # Handle empty body
    if not clause.body:
        return []

    # Helper to resolve module qualification
    def resolve_goal(goal):
        """Extract (module, actual_goal) from potentially qualified goal.

        Args:
            goal: Either unqualified (Atom/Struct) or qualified Struct(":", (Atom(m), Goal))

        Returns:
            (module_name, actual_goal) tuple
        """
        if isinstance(goal, Struct) and goal.functor == ":" and len(goal.args) == 2:
            # Qualified: M:Goal
            module_term, actual_goal = goal.args
            if isinstance(module_term, Atom):
                return (module_term.name, actual_goal)
            else:
                # Malformed qualification (e.g., variable module)
                # Treat as calling functor ":" with arity 2
                return (module, goal)
        else:
            # Unqualified: use current module
            return (module, goal)

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
    num_goals = len(clause.body)
    for goal_idx, goal in enumerate(clause.body):
        is_last = goal_idx == num_goals - 1

        # Resolve module qualification
        call_module, actual_goal = resolve_goal(goal)

        if isinstance(actual_goal, Atom):
            # Atom goal (0-arity predicate)
            target = f"{call_module}:{actual_goal.name}/0"
            if is_last:
                instructions.append((OP_EXECUTE, target))
            else:
                instructions.append((OP_CALL, target))

        elif isinstance(actual_goal, Struct):
            # Structure goal (N-arity predicate)

            # Emit put sequences for each argument
            for aj_idx, arg in enumerate(actual_goal.args):
                compile_put_term(arg, aj_idx)

            # Emit call or execute
            target = f"{call_module}:{actual_goal.functor}/{len(actual_goal.args)}"
            if is_last:
                instructions.append((OP_EXECUTE, target))
            else:
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

    # Non-empty body: check if last instruction is CALL or EXECUTE
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
    elif last_op == OP_EXECUTE:
        # compile_body() already emitted EXECUTE for last goal
        # If permanent variables, insert deallocate before the execute
        if perm_count > 0:
            code.insert(len(code) - 1, (OP_DEALLOCATE,))
    elif last_op == OP_PROCEED:
        # Defensive: already has proceed (shouldn't happen in normal compilation)
        # Return as-is to avoid appending duplicate returns
        return code
    else:
        # Non-LCO case: append deallocate (if needed) and proceed
        # (Shouldn't happen in normal code since compile_body always ends with EXECUTE,
        #  but handle gracefully for future compiler phases)
        if perm_count > 0:
            code.append((OP_DEALLOCATE,))
        code.append((OP_PROCEED,))

    return code
