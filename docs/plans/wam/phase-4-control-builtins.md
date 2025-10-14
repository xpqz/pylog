# Phase 4 — Full control features + core builtins

## Objective

Add disjunction and if-then-else compilation; implement essential builtins natively in WAM (no bridge to tree-walker).

## Outcomes

- `(;)/2` and `->/2 ;/2` compile patterns
- Core builtins reimplemented natively: arithmetic, type checks, meta-call
- Exception handling interoperates with disjunction and cut
- Unwind rules work across call/execute boundaries

## Non-Goals (Phase 4)

- All-solutions predicates (`findall/3`, `bagof/3`, `setof/3`) — Phase 6
- List utilities beyond basics — Phase 5
- I/O operations — Phase 7
- CLP(FD) integration — Phase 5

## Scope

**In:**
- Compile disjunction via choicepoints
- Compile if-then-else via commit pattern
- Native WAM builtins: `is/2`, `=:=/2`, `</2`, `>/2`, `=</2`, `>=/2`
- Native WAM builtins: `var/1`, `nonvar/1`, `atom/1`, `integer/1`, `number/1`
- Native WAM builtins: `call/1`
- Error propagation with proper Prolog error terms

**Out:**
- Bridge to tree-walker builtins (rejected; see rationale below)
- Complex meta-predicates beyond `call/1`
- Reflection predicates (`functor/3`, `arg/3`, `=../2`) — Phase 5

## Dependencies

- Phase 3 compiler and Phase 2 control working
- Phase 3.5 exception handling in place

## Design

### Disjunction Compilation

Emit `try_me_else` chains per branch; join after `trust_me`.

```
(A ; B ; C) compiles to:
  try_me_else Lb
    <code for A>
    trust_me Lend
  Lb:
    try_me_else Lc
    <code for B>
    trust_me Lend
  Lc:
    <code for C>
  Lend:
```

### If-Then-Else Compilation

Compile `Cond -> Then ; Else` with temp choicepoint and commit:

```
(Cond -> Then ; Else) compiles to:
  get_level Y0           # Save current B
  try_me_else Lelse
    <code for Cond>
    cut_to Y0            # Commit: remove choice
    <code for Then>
    trust_me Lend
  Lelse:
    <code for Else>
  Lend:
```

### Native Builtin Implementation

**No bridge**. All builtins reimplemented to operate directly on WAM heap.

**Rationale for no bridge:**
1. Marshalling overhead on every builtin call defeats WAM performance
2. Dependency on tree-walker Store/Trail complicates lifecycle
3. Cleaner to port once than maintain bridge code forever
4. Core builtins (arithmetic, type checks) are small and straightforward

#### Builtin Signature

```python
def builtin_is(machine) -> bool:
    """X is Expr: Evaluate arithmetic.

    Args:
        machine: WAM machine (args in X[0], X[1])

    Returns:
        True if success (unifies), False if fails

    Side effects:
        May modify heap, trail
    """
    lhs_addr = deref(machine.X[0], machine.heap)
    rhs_addr = deref(machine.X[1], machine.heap)

    # Evaluate arithmetic expression
    try:
        value = eval_arithmetic(rhs_addr, machine.heap)
    except ArithmeticError as e:
        raise PrologError("evaluation_error", str(e))

    # Allocate result on heap
    result_addr = machine.allocate_con(value)

    # Unify LHS with result
    return unify(lhs_addr, result_addr, machine)
```

#### Arithmetic Evaluation

```python
def eval_arithmetic(addr, heap):
    """Evaluate arithmetic expression recursively."""
    addr = deref(addr, heap)
    cell = heap[addr]

    if cell[0] == TAG_CON:
        value = cell[1]
        if isinstance(value, (int, float)):
            return value
        raise PrologError("type_error", "number expected")

    elif cell[0] == TAG_STR:
        functor_addr = cell[1]
        name, arity = heap[functor_addr][1]

        if (name, arity) == ("+", 2):
            left = eval_arithmetic(functor_addr + 1, heap)
            right = eval_arithmetic(functor_addr + 2, heap)
            return left + right

        elif (name, arity) == ("-", 2):
            left = eval_arithmetic(functor_addr + 1, heap)
            right = eval_arithmetic(functor_addr + 2, heap)
            return left - right

        elif (name, arity) == ("*", 2):
            left = eval_arithmetic(functor_addr + 1, heap)
            right = eval_arithmetic(functor_addr + 2, heap)
            return left * right

        elif (name, arity) == ("//", 2):
            left = eval_arithmetic(functor_addr + 1, heap)
            right = eval_arithmetic(functor_addr + 2, heap)
            if right == 0:
                raise PrologError("evaluation_error", "zero_divisor")
            return left // right

        elif (name, arity) == ("mod", 2):
            left = eval_arithmetic(functor_addr + 1, heap)
            right = eval_arithmetic(functor_addr + 2, heap)
            if right == 0:
                raise PrologError("evaluation_error", "zero_divisor")
            return left % right

        elif (name, arity) == ("-", 1):
            arg = eval_arithmetic(functor_addr + 1, heap)
            return -arg

        else:
            raise PrologError("type_error", f"unknown operator {name}/{arity}")

    elif cell[0] == TAG_REF:
        raise PrologError("instantiation_error", "unbound variable in arithmetic")

    else:
        raise PrologError("type_error", "number expected")
```

#### Type Check Builtins

```python
def builtin_var(machine) -> bool:
    """var(X): Succeeds if X is unbound."""
    addr = deref(machine.X[0], machine.heap)
    cell = machine.heap[addr]
    return cell[0] == TAG_REF and cell[1] == addr  # Self-ref = unbound

def builtin_nonvar(machine) -> bool:
    """nonvar(X): Succeeds if X is bound."""
    return not builtin_var(machine)

def builtin_atom(machine) -> bool:
    """atom(X): Succeeds if X is an atom."""
    addr = deref(machine.X[0], machine.heap)
    cell = machine.heap[addr]
    if cell[0] == TAG_CON:
        value = cell[1]
        return isinstance(value, str)
    return False

def builtin_integer(machine) -> bool:
    """integer(X): Succeeds if X is an integer."""
    addr = deref(machine.X[0], machine.heap)
    cell = machine.heap[addr]
    if cell[0] == TAG_CON:
        value = cell[1]
        return isinstance(value, int) and not isinstance(value, bool)
    return False
```

#### Meta-call: call/1

```python
def builtin_call(machine) -> bool:
    """call(Goal): Meta-call predicate.

    Looks up Goal in code area and executes it.
    """
    goal_addr = deref(machine.X[0], machine.heap)
    cell = machine.heap[goal_addr]

    # Extract predicate name/arity
    if cell[0] == TAG_CON:
        # Atom: 0-arity predicate
        name = cell[1]
        arity = 0
    elif cell[0] == TAG_STR:
        functor_addr = cell[1]
        name, arity = machine.heap[functor_addr][1]
        # Move args to X registers
        for i in range(arity):
            machine.X[i] = functor_addr + 1 + i
    else:
        raise PrologError("type_error", "callable expected")

    # Look up in code area
    key = (name, arity)
    if key in machine.code:
        # Save CP and call
        machine.CP = machine.P + 1
        machine.P = machine.code[key]
        return True
    else:
        raise PrologError("existence_error", f"undefined predicate {name}/{arity}")
```

### Error Mapping

Raise Python exceptions that map to Prolog error terms:

```python
class PrologError(Exception):
    """WAM-level Prolog error."""
    def __init__(self, error_type, message):
        self.error_type = error_type  # e.g., "type_error", "instantiation_error"
        self.message = message
        super().__init__(f"{error_type}: {message}")
```

Map to standard error terms on `throw/1`:
- `instantiation_error`
- `type_error(Expected, Culprit)`
- `domain_error(Domain, Culprit)`
- `evaluation_error(Error)`

## Implementation Tasks

### Compiler Extensions

- [ ] Implement disjunction compilation (try/retry/trust chains)
- [ ] Implement if-then-else compilation (get_level + cut_to)
- [ ] Update assembler to handle new patterns
- [ ] Update loader with label resolution for branches

### Native Builtins

- [ ] Implement arithmetic evaluation (`eval_arithmetic`)
- [ ] Implement `is/2` builtin
- [ ] Implement comparison builtins: `=:=/2`, `</2`, `>/2`, `=</2`, `>=/2`
- [ ] Implement type check builtins: `var/1`, `nonvar/1`, `atom/1`, `integer/1`, `number/1`
- [ ] Implement `call/1` meta-predicate
- [ ] Register all builtins in `WAM_BUILTINS` registry

### Error Handling

- [ ] Define `PrologError` exception class
- [ ] Map error types to ISO error terms
- [ ] Integrate with `throw/1` instruction (Phase 3.5)
- [ ] Add error position tracking where available

### Testing

- [ ] Disjunction: `p(X) :- (X=a ; X=b ; X=c).` yields 3 solutions in order
- [ ] ITE: `p(X) :- (X=1 -> Y=yes ; Y=no).` commits on success
- [ ] Arithmetic: `X is 2 + 3 * 4` evaluates to 14
- [ ] Comparisons: `5 > 3` succeeds, `2 > 5` fails
- [ ] Type checks: `var(X)` succeeds on unbound, fails on bound
- [ ] Meta-call: `call(append([1],[2],X))` succeeds
- [ ] Errors: arithmetic with unbound variable raises `instantiation_error`
- [ ] Errors: division by zero raises `evaluation_error`

## Acceptance Criteria

- All tests pass
- WAM can execute programs with:
  - Disjunction and if-then-else
  - Arithmetic evaluation
  - Type checking
  - Meta-call via `call/1`
- No dependency on tree-walker builtins
- Error handling produces standard Prolog error terms

## Risks

### Risk 1: Builtin Implementation Effort

**Mitigation:** Core builtins are small and well-specified. Start with minimal set (10-15 builtins), expand in later phases.

### Risk 2: Arithmetic Edge Cases

**Mitigation:** Use differential testing against tree-walker to verify correctness. Property tests for commutativity, associativity.

### Risk 3: call/1 Complexity

**Mitigation:** Start with simple version (no DCG expansion, no module qualification). Add features incrementally.

## Rollout

- Feature flag: `PYLOG_ENGINE=wam` required
- No changes to user entrypoints
- Differential tests validate parity with tree-walker

## Follow-up Phases

- **Phase 5**: Structure reflection (`functor/3`, `arg/3`, `=../2`) and CLP(FD) basics
- **Phase 6**: All-solutions predicates
- **Phase 7**: I/O operations (stubbed for web)

## Implementation Checklist

### Compiler
- [ ] Emit disjunction patterns (`try_me_else` chains)
- [ ] Emit if-then-else patterns (`get_level`, `cut_to`)
- [ ] Update assembler for new instruction sequences
- [ ] Update loader for branch label resolution

### Builtins
- [ ] Arithmetic evaluation engine
- [ ] `is/2`
- [ ] `=:=/2`, `</2`, `>/2`, `=</2`, `>=/2`
- [ ] `var/1`, `nonvar/1`
- [ ] `atom/1`, `integer/1`, `number/1`
- [ ] `call/1`
- [ ] Builtin registry and dispatcher

### Error Handling
- [ ] `PrologError` exception class
- [ ] Error type constants
- [ ] Integration with `throw/1`
- [ ] Position tracking

### Tests
- [ ] Disjunction tests
- [ ] If-then-else tests
- [ ] Arithmetic tests
- [ ] Comparison tests
- [ ] Type check tests
- [ ] Meta-call tests
- [ ] Error tests (instantiation, type, evaluation)
- [ ] Differential tests vs tree-walker

### Documentation
- [ ] Update builtin reference
- [ ] Document error types
- [ ] Add code examples
