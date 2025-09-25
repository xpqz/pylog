# Single-Loop VM Design (Final)

## Overview

The PyLog engine uses a single iterative loop with explicit stacks, eliminating all Python recursion from the control flow. This design ensures the engine can handle arbitrarily deep Prolog structures and large clause databases without hitting Python's recursion limit.

## VM State

```python
from typing import Dict, Tuple
from dataclasses import dataclass

@dataclass
class VMState:
    goal_stack: GoalStack           # Goals to be proven
    frame_stack: FrameStack         # Call frames for cut barriers
    choicepoint_stack: ChoiceStack  # Backtracking points
    trail: Trail                    # Single source of truth for undoing
    store: Store                    # Variable bindings (union-find)
    solutions: List[Dict]           # Collected solutions
    config: VMConfig                # max_solutions, trace flags, etc.
    step_counter: int               # Monotonic step count for debugging
    pred_table: Dict[Tuple[str,int], int]  # (functor,arity) -> pred_id
    builtin_table: Dict[Tuple[str,int], int]  # (functor,arity) -> builtin_id
```

## Core Types

### Enums for Readability

```python
from enum import IntEnum

class CPKind(IntEnum):
    PRED = 0        # Predicate with multiple clauses
    DISJ = 1        # Disjunction (;/2)
    BUILTIN = 2     # Non-deterministic builtin
    CONTROL = 3     # Control construct (if-then-else)

class GoalKind(IntEnum):
    CALL = 0        # Regular predicate/builtin call
    POP_FRAME = 1   # Sentinel to pop frame
    CONTROL = 2     # Internal control operation

class TrailOp(IntEnum):
    BIND = 0        # Variable binding
    PARENT = 1      # Union-find parent change
    ATTR = 2        # Attribute change
    FD_DOMAIN = 3   # FD domain change

class Port(IntEnum):
    CALL = 0
    EXIT = 1
    REDO = 2
    FAIL = 3
```

### Frame

```python
@dataclass(frozen=True, slots=True)
class Frame:
    frame_id: int         # Monotonic ID for debugging
    pred_id: int          # Interned predicate identifier
    cut_barrier: int      # CP stack height on entry (for cut)
```

### Choicepoint

```python
@dataclass(slots=True)
class Choicepoint:
    cp_id: int           # Monotonic ID for debugging
    kind: CPKind         # Type of choicepoint
    trail_top: int       # Trail position for unwinding
    goal_top: int        # Goal stack height to restore
    frame_top: int       # Frame stack height to restore
    # Payload (interpretation depends on kind):
    k1: int              # PRED: pred_id, DISJ: unused, BUILTIN: builtin_id, CONTROL: op
    k2: int              # PRED: next_clause_idx, BUILTIN: state, others: unused
    k3: object           # PRED: original_goal, DISJ: right_goal, BUILTIN: term, CONTROL: args
    k4: object           # Reserved for future use
```

### Goal

```python
@dataclass(frozen=True, slots=True)
class Goal:
    kind: GoalKind
    arg1: object         # Term for CALL, frame_id for POP_FRAME, op for CONTROL
    arg2: object         # Additional args for CONTROL ops
```

### Trail with Write Stamps

```python
@dataclass(slots=True)
class Trail:
    entries: List[Tuple[TrailOp, int, object]]  # (op, varid, old_value)
    stamp: int                                   # Current write stamp
    
    def mark(self) -> int:
        """Return current position."""
        return len(self.entries)
    
    def push(self, op: TrailOp, varid: int, old_value: object):
        """Add entry to trail."""
        self.entries.append((op, varid, old_value))
    
    def unwind_to(self, mark: int, store: Store):
        """Restore state to mark."""
        while len(self.entries) > mark:
            op, varid, old_value = self.entries.pop()
            if op == TrailOp.BIND:
                store.cells[varid] = old_value
            elif op == TrailOp.PARENT:
                store.parents[varid] = old_value
            # ... other ops handled by engine
    
    def next_stamp(self) -> int:
        """Increment and return stamp for new CP window."""
        self.stamp += 1
        return self.stamp
```

## Main Loop

```python
class StepResult(IntEnum):
    CONTINUE = 0    # Continue to next step
    SOLUTION = 1    # Solution found
    DONE = 2        # No more work
    ERROR = 3       # Error occurred

def run(self, goals: List[Term]) -> List[Dict]:
    # Initialize state
    self.goal_stack.clear()
    self.frame_stack.clear()
    self.choicepoint_stack.clear()
    self.trail.clear()
    self.solutions.clear()
    self.step_counter = 0
    
    # Push initial goals (reversed for LIFO)
    for goal in reversed(goals):
        self.goal_stack.push(Goal(GoalKind.CALL, goal, None))
    
    # Main loop - NO RECURSION
    while self.step_counter < self.config.step_limit:
        self.step_counter += 1
        result = self.step()
        
        if result == StepResult.SOLUTION:
            self.record_solution()
            if len(self.solutions) >= self.config.max_solutions:
                return self.solutions
            # Backtrack for more solutions
            result = self.backtrack()
            if result == StepResult.DONE:
                return self.solutions
                
        elif result == StepResult.DONE:
            return self.solutions
            
        elif result == StepResult.ERROR:
            raise self.last_error
    
    raise EngineError(f"Step limit {self.config.step_limit} exceeded")

def step(self) -> StepResult:
    # Pop next goal
    goal = self.goal_stack.pop()
    
    if goal is None:
        # No more goals - solution found
        return StepResult.SOLUTION
    
    # Unified dispatch
    if goal.kind == GoalKind.CALL:
        term = goal.arg1
        
        # Check control constructs first
        if self.is_control(term):
            return self.execute_control_construct(term)
        
        # Check builtins
        builtin_key = (term.functor, len(term.args)) if isinstance(term, Struct) else (term.name, 0)
        if builtin_key in self.builtin_table:
            return self.execute_builtin(term)
        
        # Regular predicate call
        return self.call_predicate(term)
        
    elif goal.kind == GoalKind.POP_FRAME:
        return self.pop_frame(goal.arg1)
        
    elif goal.kind == GoalKind.CONTROL:
        return self.execute_control_op(goal.arg1, goal.arg2)
        

## Note: CLP(FD) Labeling and Disjunctions

Labeling in CLP(FD) is implemented entirely within the single‑loop VM without Python recursion by pushing explicit goals that encode the search tree.

Key choice: continuations are embedded per alternative. Instead of scheduling a separate continuation goal after a value assignment (e.g., push(X=V), then later push(label(Vars))), the engine builds disjunctions whose branches include the continuation:

- (X = V1, label(Vars)) ; (X = V2, label(Vars)) ; ...

This avoids a fragile “empty goal stack window” between assignment and continuation that can appear when backtracking from a prior branch. With the continuation inside each branch, the VM restores a consistent state on redo, and all solutions are found. For the single‑value case, we push the unify goal on top and place label(Vars) immediately below it on the goal stack so the assignment executes first and the continuation follows deterministically — still with no Python recursion.

This pattern is robust across backtracking and integrates with the existing disjunction choicepoint machinery without requiring special cases in the engine.

    else:
        return self.backtrack()
```

## Predicate Calls

```python
def call_predicate(self, term: Term) -> StepResult:
    # Get predicate ID
    pred_key = (term.functor, len(term.args)) if isinstance(term, Struct) else (term.name, 0)
    pred_id = self.pred_table.get(pred_key)
    
    if pred_id is None:
        return self.backtrack()
    
    # Get matching clauses
    clauses = self.program.clauses_for_id(pred_id)
    
    if not clauses:
        return self.backtrack()
    
    # Emit CALL port
    self.trace_port(Port.CALL, term, self.next_frame_id())
    
    # Compute cut barrier BEFORE any CP creation
    cut_barrier = len(self.choicepoint_stack)
    
    # Create choicepoint if multiple clauses
    if len(clauses) > 1:
        cp = Choicepoint(
            cp_id=self.next_cp_id(),
            kind=CPKind.PRED,
            trail_top=self.trail.mark(),
            goal_top=len(self.goal_stack),
            frame_top=len(self.frame_stack),
            k1=pred_id,
            k2=1,  # Next clause index
            k3=term,  # Original goal
            k4=None
        )
        self.choicepoint_stack.push(cp)
        self.trail.next_stamp()  # New CP window
    
    # Push frame ONCE with pre-computed barrier
    frame = Frame(
        frame_id=self.next_frame_id(),
        pred_id=pred_id,
        cut_barrier=cut_barrier
    )
    self.frame_stack.push(frame)
    
    # Try first clause
    return self.try_clause(clauses[0], term, frame.frame_id)

def try_clause(self, clause: Clause, goal_term: Term, frame_id: int) -> StepResult:
    """Try to unify with clause head and schedule body. NO RECURSION."""
    # Rename clause with fresh variables
    renamer = VarRenamer(self.store)
    renamed_clause = renamer.rename_clause(clause)
    
    # Try to unify with clause head
    if not unify(renamed_clause.head, goal_term, self.store, self.trail):
        # Unification failed - emit FAIL port and backtrack
        self.trace_port(Port.FAIL, goal_term, frame_id)
        return self.backtrack()
    
    # Unification succeeded - schedule body execution
    # First push frame sentinel
    self.goal_stack.push(Goal(GoalKind.POP_FRAME, frame_id, None))
    
    # Then push body goals in reverse order
    for goal in reversed(renamed_clause.body):
        self.goal_stack.push(Goal(GoalKind.CALL, goal, None))
    
    return StepResult.CONTINUE

def pop_frame(self, frame_id: int) -> StepResult:
    """Pop frame when clause body completes."""
    tf = self.frame_stack.peek()
    
    if self.config.debug:
        assert tf and tf.frame_id == frame_id, f"Frame mismatch: expected {frame_id}, got {tf.frame_id if tf else None}"
    
    if tf and tf.frame_id == frame_id:
        self.frame_stack.pop()
        # Emit EXIT port on successful completion
        self.trace_port(Port.EXIT, None, frame_id)
    
    return StepResult.CONTINUE
```

## Builtin Execution

```python
class BuiltinDeterminism(IntEnum):
    DET = 0       # Deterministic - exactly one solution
    SEMIDET = 1   # Semi-deterministic - at most one solution
    NONDET = 2    # Non-deterministic - multiple solutions

# Builtin contract:
# - Det/Semidet: execute(engine, term) -> bool
#   Must NOT push CPs, must NOT modify state on failure
# - Nondet: start(engine, term) -> (bool, state)
#           redo(engine, term, state) -> (bool, new_state)
#   Engine handles trail marks and unwinding

def execute_builtin(self, term: Term) -> StepResult:
    builtin_key = (term.functor, len(term.args)) if isinstance(term, Struct) else (term.name, 0)
    builtin_id = self.builtin_table.get(builtin_key)
    
    if builtin_id is None:
        return self.backtrack()
    
    builtin = self.builtins[builtin_id]
    
    if builtin.determinism == BuiltinDeterminism.NONDET:
        # Non-deterministic: capture state first
        trail_mark = self.trail.mark()
        ok, state = builtin.start(self, term)
        
        if not ok:
            return self.backtrack()
        
        # Create CP for remaining solutions
        cp = Choicepoint(
            cp_id=self.next_cp_id(),
            kind=CPKind.BUILTIN,
            trail_top=trail_mark,
            goal_top=len(self.goal_stack),
            frame_top=len(self.frame_stack),
            k1=builtin_id,
            k2=state,  # Builtin state (int/tuple, not iterator)
            k3=term,
            k4=None
        )
        self.choicepoint_stack.push(cp)
        self.trail.next_stamp()
        return StepResult.CONTINUE
    else:
        # Deterministic/semi-det
        if builtin.execute(self, term):
            return StepResult.CONTINUE
        else:
            return self.backtrack()
```

## Control Constructs

```python
def is_control(self, term: Term) -> bool:
    """Check if term is a control construct."""
    if isinstance(term, Atom):
        return term.name == '!'
    if isinstance(term, Struct):
        return term.functor in (';', '->')
    return False

def execute_control_construct(self, term: Term) -> StepResult:
    """Handle control constructs at top level."""
    if isinstance(term, Atom) and term.name == '!':
        return self.execute_cut()
    
    if isinstance(term, Struct):
        if term.functor == ';' and len(term.args) == 2:
            return self.execute_disjunction(term.args[0], term.args[1])
        
        if term.functor == '->' and len(term.args) == 2:
            # (Cond -> Then)
            return self.execute_if_then(term.args[0], term.args[1])
    
    return self.backtrack()

def execute_cut(self) -> StepResult:
    """Execute cut (!). Remove choicepoints up to current frame's barrier."""
    frame = self.frame_stack.peek()
    if frame:
        if self.config.debug:
            assert frame.cut_barrier <= len(self.choicepoint_stack), \
                   f"Invalid cut barrier: {frame.cut_barrier} > {len(self.choicepoint_stack)}"
        
        # Remove all choicepoints created after entering this predicate
        while len(self.choicepoint_stack) > frame.cut_barrier:
            self.choicepoint_stack.pop()
    
    return StepResult.CONTINUE

def execute_disjunction(self, left: Term, right: Term) -> StepResult:
    """Execute (A ; B) disjunction."""
    # Create choicepoint for right branch
    cp = Choicepoint(
        cp_id=self.next_cp_id(),
        kind=CPKind.DISJ,
        trail_top=self.trail.mark(),
        goal_top=len(self.goal_stack),
        frame_top=len(self.frame_stack),
        k1=0, k2=0,
        k3=right,  # Right branch goal
        k4=None
    )
    self.choicepoint_stack.push(cp)
    self.trail.next_stamp()
    
    # Schedule left branch
    self.goal_stack.push(Goal(GoalKind.CALL, left, None))
    return StepResult.CONTINUE

def execute_if_then(self, cond: Term, then_branch: Term) -> StepResult:
    """Execute (Cond -> Then). Equivalent to (Cond -> Then ; fail)."""
    return self.execute_if_then_else(cond, then_branch, Atom('fail'))

def execute_if_then_else(self, cond: Term, then_branch: Term, else_branch: Term) -> StepResult:
    """Execute (Cond -> Then ; Else) with proper cut semantics."""
    tmp_barrier = len(self.choicepoint_stack)
    
    # Controller CP to run Else only if Cond fails exhaustively
    cp = Choicepoint(
        cp_id=self.next_cp_id(),
        kind=CPKind.CONTROL,
        trail_top=self.trail.mark(),
        goal_top=len(self.goal_stack),
        frame_top=len(self.frame_stack),
        k1=0,  # ITE_ELSE operation
        k2=tmp_barrier,
        k3=else_branch,
        k4=None
    )
    self.choicepoint_stack.push(cp)
    self.trail.next_stamp()
    
    # Schedule: when Cond succeeds, commit (prune to tmp_barrier) and run Then
    self.goal_stack.push(Goal(GoalKind.CONTROL, 1, (then_branch, tmp_barrier)))  # ITE_THEN
    self.goal_stack.push(Goal(GoalKind.CALL, cond, None))
    
    return StepResult.CONTINUE

def execute_control_op(self, op: int, args: object) -> StepResult:
    """Execute internal control operations."""
    if op == 1:  # ITE_THEN
        # Cond succeeded - commit by pruning to barrier, then run Then
        then_branch, tmp_barrier = args
        while len(self.choicepoint_stack) > tmp_barrier:
            self.choicepoint_stack.pop()
        self.goal_stack.push(Goal(GoalKind.CALL, then_branch, None))
        return StepResult.CONTINUE
    
    return self.backtrack()
```

## Backtracking

```python
def backtrack(self) -> StepResult:
    """Restore state and try next alternative. NO RECURSION."""
    while self.choicepoint_stack:
        cp = self.choicepoint_stack.pop()
        
        # Restore state
        self.trail.unwind_to(cp.trail_top, self.store)
        self.goal_stack.truncate_to(cp.goal_top)
        self.frame_stack.truncate_to(cp.frame_top)
        
        if self.config.debug:
            # Verify restoration
            assert len(self.goal_stack) == cp.goal_top
            assert len(self.frame_stack) == cp.frame_top
        
        # Emit REDO port
        self.trace_port(Port.REDO, None, cp.cp_id)
        
        # Resume based on kind
        if self.resume_choicepoint(cp):
            return StepResult.CONTINUE
    
    return StepResult.DONE  # No more choicepoints

def resume_choicepoint(self, cp: Choicepoint) -> bool:
    """Resume from choicepoint. Returns True if alternative found."""
    
    if cp.kind == CPKind.PRED:
        # Get clauses for predicate
        clauses = self.program.clauses_for_id(cp.k1)
        next_idx = cp.k2
        
        if next_idx >= len(clauses):
            return False
        
        # Compute cut barrier BEFORE creating next CP
        cut_barrier = len(self.choicepoint_stack)
        
        # Push updated CP if more alternatives remain
        if next_idx + 1 < len(clauses):
            cp.k2 = next_idx + 1
            self.choicepoint_stack.push(cp)
            self.trail.next_stamp()
        
        # Re-push frame with correct barrier
        frame = Frame(
            frame_id=self.next_frame_id(),
            pred_id=cp.k1,
            cut_barrier=cut_barrier
        )
        self.frame_stack.push(frame)
        
        # Try next clause
        result = self.try_clause(clauses[next_idx], cp.k3, frame.frame_id)
        return result == StepResult.CONTINUE
        
    elif cp.kind == CPKind.DISJ:
        # Schedule right branch
        self.goal_stack.push(Goal(GoalKind.CALL, cp.k3, None))
        return True
        
    elif cp.kind == CPKind.BUILTIN:
        # Resume builtin - engine already restored state
        builtin = self.builtins[cp.k1]
        ok, new_state = builtin.redo(self, cp.k3, cp.k2)
        
        if ok:
            # Update state and re-push CP
            cp.k2 = new_state
            self.choicepoint_stack.push(cp)
            self.trail.next_stamp()
        
        return ok
        
    elif cp.kind == CPKind.CONTROL:
        if cp.k1 == 0:  # ITE_ELSE
            # Cond failed exhaustively - run Else
            self.goal_stack.push(Goal(GoalKind.CALL, cp.k3, None))
            return True
    
    return False
```

## Solution Recording

```python
def record_solution(self):
    """Record current solution. NO RECURSION, NO MUTATIONS."""
    solution = {}
    
    # Project only query variables in original order
    for varid, var_name in self.query_vars:
        # Follow bindings iteratively
        value = self.deref_iterative(varid)
        # Map structures iteratively
        solution[var_name] = self.project_term_iterative(value)
    
    self.solutions.append(solution)

def deref_iterative(self, varid: int) -> Term:
    """Follow binding chain without recursion."""
    # Implementation uses explicit stack
    ...

def project_term_iterative(self, term: Term) -> Term:
    """Project term to user representation without recursion."""
    # Implementation uses explicit stack, no new Var allocations
    ...
```

## Invariants & Assertions

1. **No Recursion**: No VM function calls another VM function recursively
2. **Trail Authority**: Trail is the single source of truth for undoing state
3. **Frame Lifecycle**: One frame per predicate call, popped via sentinel
4. **Cut Barrier Rule**: `frame.cut_barrier <= len(choicepoint_stack)` always
5. **Restoration Exactness**: After backtrack, stack heights match CP fields exactly
6. **Determinism Contract**: 
   - Det/Semidet builtins never push CPs, never mutate on failure
   - Nondet builtins: engine handles trail/restoration, builtin just returns state
7. **Write Stamps**: Each CP window has unique stamp to prevent redundant trailing

## Testing Strategy

### Basic Sanity Tests
```prolog
% Top-level builtin
?- true.  % succeeds
?- fail.  % fails

% Cut commits clause
a(1). a(2).
p(X) :- a(X), !, q.
q.
% ?- p(X). yields X=1 only, no backtracking into a/1

% If-then-else commit
a(1). a(2).
r(X) :- (a(X) -> ! ; true), X=1.
% Cut only affects then-branch

% Disjunction isolation
s(X) :- (X=1 ; X=2), var(X).
% Bindings from left don't leak to right

% Backtrack restore
% Deep goal/frame/CP heights restored exactly (verify with debug hooks)
```

### Stress Tests
1. **No Recursion**: 5000+ clauses, 10000+ deep structures
2. **Frame Lifecycle**: Verify one frame per call via monotonic IDs
3. **Trail Discipline**: No redundant entries within same CP window
4. **Cut Semantics**: Proper CP removal without affecting frames
5. **Solution Order**: All solutions in correct left-to-right, depth-first order
