# Stage 0: Core Shapes & Explicit Stacks - Detailed Implementation Plan

## Overview
Eliminate Python recursion from execution by implementing an explicit goal stack and choicepoint system. This stage establishes the core execution model that will survive all subsequent stages unchanged.

## Core Components

### 1. Clause Representation (`prolog/ast/clauses.py`)

#### Data Structures
```python
@dataclass(frozen=True)
class Clause:
    head: Term                    # Atom or Struct
    body: Tuple[Term, ...]       # Conjunction of goals (empty for facts)
    
@dataclass(frozen=True) 
class Program:
    clauses: Tuple[Clause, ...]   # All clauses in order
    
    def clauses_for(self, functor: str) -> List[Clause]:
        """Return clauses matching functor/arity."""
```

#### Clause Cursor
```python
@dataclass
class ClauseCursor:
    functor: str                  # e.g., "append"
    arity: int                    # e.g., 3
    matches: List[int]            # Indices into Program.clauses
    pos: int = 0                  # Current position
    
    def has_more(self) -> bool:
        """Check if more clauses available."""
        return self.pos < len(self.matches)
        
    def peek(self) -> Optional[int]:
        """Look at next clause index without advancing."""
        return self.matches[self.pos] if self.has_more() else None
        
    def take(self) -> Optional[int]:
        """Get next clause index and advance."""
        if not self.has_more():
            return None
        i = self.matches[self.pos]
        self.pos += 1
        return i
        
    def clone(self) -> "ClauseCursor":
        """Create copy of cursor at current position."""
        return ClauseCursor(self.functor, self.arity, self.matches, self.pos)
```

### 2. Goal Stack (`prolog/engine/goals.py`)

#### Goal Types
```python
@dataclass(frozen=True)
class Goal:
    term: Term                    # The goal to solve
    
class GoalStack:
    _stack: List[Goal]            # Stack of goals to solve
    
    def push(self, goal: Goal):
        """Push goal onto stack."""
        self._stack.append(goal)
        
    def pop(self) -> Optional[Goal]:
        """Pop next goal, None if empty."""
        return self._stack.pop() if self._stack else None
        
    def push_body(self, body: Tuple[Term, ...]):
        """Push clause body goals in reverse order."""
        for goal in reversed(body):
            self._stack.append(Goal(goal))
        
    def snapshot(self) -> Tuple[Goal, ...]:
        """Create immutable snapshot for choicepoint."""
        return tuple(self._stack)
        
    def restore(self, snapshot: Tuple[Goal, ...]):
        """Restore from snapshot."""
        self._stack = list(snapshot)
```

### 3. Choicepoint System (`prolog/engine/choicepoint.py`)

#### Choicepoint Structure
```python
@dataclass
class Choicepoint:
    id: int                       # Unique identifier for cut
    goals: Tuple[Goal, ...]       # Snapshot of goal stack
    cursor: ClauseCursor          # Next clause to try
    trail_mark: int               # Trail position for backtracking
    cut_barrier: Optional[int]    # Parent choicepoint ID (for cut scope)
    store_size: int               # Number of variables at creation
    
class ChoiceStack:
    stack: List[Choicepoint]     # Stack of choicepoints
    next_id: int                  # Next choicepoint ID
    
    def push(self, cp: Choicepoint) -> int:
        """Push choicepoint, return its ID."""
        
    def pop(self) -> Optional[Choicepoint]:
        """Pop most recent choicepoint."""
        
    def cut_to(self, barrier: int):
        """Remove all choicepoints newer than barrier."""
        
    def find(self, cp_id: int) -> Optional[Choicepoint]:
        """Find choicepoint by ID (for cut)."""
```

### 4. Main Engine (`prolog/engine/engine.py`)

#### Engine State
```python
@dataclass
class Engine:
    program: Program              # Clauses to execute
    store: Store                  # From Stage -1
    trail: List                   # From Stage -1
    goals: GoalStack              # Current goals
    choices: ChoiceStack          # Choicepoints for backtracking
    solutions: List[Dict]         # Accumulated solutions
    max_solutions: Optional[int]  # Limit on solutions
    trace: bool = False           # Enable execution tracing
    
    # Runtime state
    _initial_var_cutoff: int      # Variables before query execution
    _cut_barrier: Optional[int]   # Current cut scope
    _query_vars: List[Tuple[int, str]]  # Query variable mappings
```

#### Core Run Loop
```python
def run(self, initial_goals: List[Term]) -> List[Dict]:
    """Execute goals, return all solutions (or up to max_solutions)."""
    
    # Initialize
    self.goals = GoalStack()
    for goal in reversed(initial_goals):
        self.goals.push(Goal(goal))
    self.solutions = []
    self._initial_var_cutoff = len(self.store.cells)  # After query vars allocated
    self._cut_barrier = None
    
    while True:
        # Pop next goal
        goal = self.goals.pop()
        if goal is None:
            # Success: record solution
            self._record_solution()
            if len(self.solutions) >= (self.max_solutions or float('inf')):
                break
            if not self._backtrack():
                break
            continue
            
        # Special handling for builtins
        if self._is_builtin(goal.term):
            if not self._execute_builtin(goal.term):
                if not self._backtrack():
                    break
            continue
            
        # Snapshot goals BEFORE consuming goal for choicepoint
        pre_goals = self.goals.snapshot()
        
        # Find matching clauses
        cursor = self._make_cursor(goal.term)
        if not cursor.has_more():
            # No clauses match
            if not self._backtrack():
                break
            continue
            
        # Check if we need a choicepoint (more than one clause matches)
        clause_index = cursor.take()
        if cursor.has_more():
            # Create choicepoint for remaining clauses
            cp = Choicepoint(
                id=self.choices.next_id,
                goals=pre_goals,  # Goal still on top in snapshot
                cursor=cursor.clone(),  # Cursor at next clause
                trail_mark=len(self.trail),
                cut_barrier=self._cut_barrier,
                store_size=len(self.store.cells)
            )
            self.choices.push(cp)
            
        # Try the clause
        if not self._try_clause(goal.term, clause_index):
            if not self._backtrack():
                break
    
    return self.solutions
```

#### Helper Methods
```python
def _try_clause(self, goal_term: Term, clause_index: int) -> bool:
    """Try to unify goal with clause and push body on success."""
    clause = self.program.clauses[clause_index]
    renamer = VarRenamer(self.store)
    renamed_clause = renamer.rename_clause(clause)
    mark = len(self.trail)
    
    if unify(goal_term, renamed_clause.head, self.store, self.trail, occurs_check=False):
        # Entering clause: install cut barrier
        self._cut_barrier = self.choices.top_id() if self.choices.stack else None
        # Push body goals (left-to-right = reverse for stack)
        self.goals.push_body(renamed_clause.body)
        return True
    else:
        undo_to(mark, self.trail, self.store)
        return False

def _backtrack(self) -> bool:
    """Restore state from most recent choicepoint."""
    cp = self.choices.pop()
    if cp is None:
        return False
        
    # Restore store state
    undo_to(cp.trail_mark, self.trail, self.store)
    del self.store.cells[cp.store_size:]  # Shrink store
    
    # Restore goals snapshot (goal on top again)
    self.goals.restore(cp.goals)
    self._cut_barrier = cp.cut_barrier
    
    # Re-select goal and try next clause
    goal = self.goals.pop()
    assert goal is not None, "Choicepoint should have goal on top"
    
    clause_index = cp.cursor.take()
    if clause_index is None:
        # No more clauses for this goal
        return self._backtrack()  # Try earlier choicepoint
        
    # Check if we need another choicepoint for remaining clauses
    if cp.cursor.has_more():
        self.choices.push(cp)  # Re-push for next alternative
        
    return self._try_clause(goal.term, clause_index)

def _record_solution(self):
    """Record current variable bindings as solution."""
    solution = {}
    for vid, name in self._query_vars:
        # Only include variables from original query
        if vid < self._initial_var_cutoff:
            solution[name] = self._reify_var(vid)
    self.solutions.append(solution)
```

### 5. Built-in Predicates (`prolog/engine/builtins.py`)

#### Core Builtins (Stage 0)
```python
def builtin_true(engine: Engine, goal_term: Term) -> bool:
    """true/0 - Always succeeds."""
    return True
    
def builtin_fail(engine: Engine, goal_term: Term) -> bool:
    """fail/0 - Always fails."""  
    return False
    
def builtin_cut(engine: Engine, goal_term: Term) -> bool:
    """!/0 - Remove choicepoints up to cut barrier."""
    if engine._cut_barrier is not None:
        engine.choices.cut_to(engine._cut_barrier)
    return True
    
def builtin_call(engine: Engine, goal_term: Term) -> bool:
    """call/1 - Execute term as goal."""
    if not isinstance(goal_term, Struct) or len(goal_term.args) != 1:
        return False
    
    # Deref the argument (might be var bound to goal)
    arg = goal_term.args[0]
    tag, val = deref_term(arg, engine.store)
    if tag == "VAR":
        return False  # Unbound variable
    
    # Validate callable (Atom or Struct for Stage 0)
    if not isinstance(val, (Atom, Struct)):
        return False
        
    # Push as new goal
    engine.goals.push(Goal(val))
    return True
```

### 6. Variable Renaming (`prolog/engine/rename.py`)

#### Clause Renaming
```python
class VarRenamer:
    """Rename variables in clauses to avoid conflicts."""
    
    def __init__(self, store: Store):
        self.store = store
        self.var_map: Dict[int, int] = {}
        
    def rename_term(self, term: Term) -> Term:
        """Recursively rename all variables in term."""
        if isinstance(term, Var):
            if term.id not in self.var_map:
                new_id = self.store.new_var(term.hint)
                self.var_map[term.id] = new_id
            return Var(self.var_map[term.id], term.hint)
        elif isinstance(term, Struct):
            new_args = tuple(self.rename_term(arg) for arg in term.args)
            return Struct(term.functor, new_args)
        elif isinstance(term, List):
            new_items = tuple(self.rename_term(item) for item in term.items)
            new_tail = self.rename_term(term.tail)
            return List(new_items, new_tail)
        else:
            return term  # Atom, Int unchanged
            
    def rename_clause(self, clause: Clause) -> Clause:
        """Rename all variables in a clause."""
        new_head = self.rename_term(clause.head)
        new_body = tuple(self.rename_term(g) for g in clause.body)
        return Clause(new_head, new_body)
```

### 7. Exception Handling (`prolog/engine/exceptions.py`)

#### Throw/Catch Skeleton
```python
@dataclass
class CatchFrame:
    """Frame for catch/3 error handling."""
    catcher: Term                 # Pattern to match thrown term
    recovery: Term                # Goal to execute on match
    choicepoint_id: int          # Choicepoint to restore to
    trail_mark: int              # Trail position
    
class ExceptionStack:
    """Stack of catch frames."""
    frames: List[CatchFrame] = []
    
    def push_catch(self, catcher: Term, recovery: Term, 
                   cp_id: int, mark: int):
        """Install exception handler."""
        
    def find_handler(self, thrown: Term, store: Store) -> Optional[CatchFrame]:
        """Find matching handler for thrown term."""
        
def builtin_throw(engine: Engine, goal: Goal) -> bool:
    """throw/1 - Throw exception term."""
    if not isinstance(goal.term, Struct) or len(goal.term.args) != 1:
        return False
    thrown = goal.term.args[0]
    
    handler = engine.exceptions.find_handler(thrown, engine.store)
    if handler:
        # Restore to catch point
        undo_to(handler.trail_mark, engine.trail, engine.store)
        # Execute recovery goal
        engine.goals.push(Goal(handler.recovery, {}))
        return True
    else:
        # Uncaught exception
        raise PrologException(f"Uncaught exception: {thrown}")
        
def builtin_catch(engine: Engine, goal_term: Term) -> bool:
    """catch/3 - Install exception handler (skeleton for Stage 0)."""
    # Defer full implementation to Stage 1
    # For now, just execute the protected goal
    if not isinstance(goal_term, Struct) or len(goal_term.args) != 3:
        return False
    protected = goal_term.args[0]
    engine.goals.push(Goal(protected))
    return True
```

## Testing Strategy

### Unit Tests

1. **Clause and Program Tests** (`test_clauses.py`)
   - Clause creation with head and body
   - Program indexing by functor/arity
   - Clause cursor advancement
   - Empty body for facts

2. **Goal Stack Tests** (`test_goals.py`)
   - Push/pop operations
   - Body goal pushing (reverse order)
   - Snapshot creation
   - Empty stack handling

3. **Choicepoint Tests** (`test_choicepoint.py`)
   - Choicepoint creation and storage
   - Cut barrier tracking
   - Cut operation (remove newer choicepoints)
   - Trail mark restoration

4. **Renaming Tests** (`test_rename.py`)
   - Variable renaming with fresh IDs
   - Structure preservation
   - List handling
   - Mapping consistency

5. **Engine Tests** (`test_engine.py`)
   - Fact retrieval
   - Rule expansion
   - Backtracking over multiple solutions
   - Cut behavior (green and red cut tests)
   - Variable bindings in solutions
   - Solution scope: only query variables included
   - `call/1`: `call(f(X))` behaves like `f(X)`
   - `call/1`: deref variable bound to goal

### Integration Tests

1. **Deep Recursion Test** (`test_deep_recursion.py`)
   ```prolog
   deep(zero).
   deep(s(N)) :- deep(N).
   
   ?- deep(s(s(s(...s(zero)...)))).  % 5000 levels - Should succeed without stack overflow
   ```

2. **Backtracking Test** (`test_backtracking.py`)
   ```prolog
   choice(1). choice(2). choice(3).
   pair(X, Y) :- choice(X), choice(Y).
   
   ?- pair(X, Y).  % Should produce 9 solutions
   ```

3. **Cut Tests** (`test_cut.py`)
   ```prolog
   % Green cut (doesn't change semantics)
   max(X, Y, X) :- X >= Y, !.
   max(X, Y, Y).
   
   % Red cut (changes semantics)
   det_member(X, [X|_]) :- !.
   det_member(X, [_|T]) :- det_member(X, T).
   ```

### Property Tests

1. **Solution Completeness**
   - All valid solutions are found
   - No duplicate solutions
   - Order matches left-to-right, depth-first search
   - Solutions follow source clause order

2. **State Isolation**
   - Failed goals don't affect subsequent attempts
   - Backtracking fully restores state
   - No variable leaks between solutions
   - Renamed clause variables don't appear in solutions

3. **Cut Correctness**
   - Cut removes exactly the right choicepoints
   - Cut barrier properly scoped
   - Nested cuts work correctly
   - Cut inside body: `(!, fail ; true)` only prunes pre-cut alternatives

4. **Goal Retry Correctness**
   - A goal with N matching clauses produces N solutions in order
   - Backtracking restores the same goal each time
   - Store/Trail/GoalStack match saved snapshot exactly after backtrack

### Stress Tests

1. **Large Clause Databases**
   - 10000+ clauses
   - Deep inheritance hierarchies
   - Many alternatives per predicate

2. **Deep Goal Stacks**
   - 1000+ pending goals
   - Deeply nested conjunctions
   - Many choicepoints

3. **Memory Stability**
   - Run for extended time
   - Monitor memory usage
   - Verify cleanup after backtracking

## Implementation Order

1. **Phase 1: Basic Structures**
   - Clause and Program representations
   - Goal stack implementation
   - Basic tests

2. **Phase 2: Core Engine Loop**
   - Engine state management
   - Main run loop (without builtins)
   - Clause cursor and matching
   - Variable renaming
   - Fact tests

3. **Phase 3: Backtracking**
   - Choicepoint creation and restoration
   - Multiple solution collection
   - Backtracking tests

4. **Phase 4: Cut Implementation**
   - Cut barrier tracking
   - Cut builtin
   - Green and red cut tests

5. **Phase 5: Exception Skeleton**
   - CatchFrame and ExceptionStack
   - throw/catch builtins (basic)
   - Exception propagation tests

6. **Phase 6: Integration and Stress**
   - Deep recursion tests
   - Complex backtracking scenarios
   - Stress tests
   - Performance baseline

## Success Metrics

1. **Correctness**
   - All unit tests pass
   - Integration tests produce expected solutions
   - No Python recursion limit errors
   - Cut behaves correctly (both green and red cuts)
   - Backtracking: For any goal with N matching clauses, engine yields N solutions in source order
   - Cut inside Kth clause removes remaining N-K alternatives for that goal only
   - State restoration: After each backtrack, Store/Trail/GoalStack/_cut_barrier match snapshot exactly

2. **Performance Baseline**
   - Establish baseline for future optimization
   - Track metrics: goals/second, choicepoints created, backtracks
   - Memory usage stable during long runs
   - Deterministic behavior with consistent variable ID allocation

3. **Robustness**
   - Handle deep recursion (5000+ levels)
   - Support large clause databases (10000+ clauses)
   - Clean state management through backtracking
   - Engine.reset() allows safe reuse of instance

## Design Decisions & Rationale

1. **Explicit Goal Stack**
   - Eliminates Python recursion
   - Clear execution model
   - Easy to instrument and debug
   - Supports arbitrary goal depth

2. **Immutable Snapshots**
   - Choicepoints store immutable goal snapshots
   - Prevents subtle bugs from shared state
   - Clear semantics for backtracking
   - Slight memory overhead acceptable for correctness

3. **Clause Cursor Abstraction**
   - Encapsulates clause iteration logic
   - Preparation for Stage 2 indexing
   - Clean interface for choicepoints
   - Supports different iteration strategies

4. **Cut Barrier Design**
   - Each choicepoint knows its parent
   - Cut removes choicepoints up to barrier
   - Proper scoping for nested cuts
   - Standard Prolog semantics

5. **Variable Renaming per Clause**
   - Fresh variables for each clause use
   - Prevents variable capture bugs
   - Standard Prolog behavior
   - Uses store from Stage -1

6. **Exception Skeleton Only**
   - Basic throw/catch for Stage 0
   - Full implementation can wait
   - Tests exception path early
   - Establishes handler stack pattern

## Key Invariants

1. **No Python Recursion**: All execution uses explicit stacks
2. **State Restoration**: Backtracking exactly restores previous state
3. **Variable Isolation**: Renamed variables don't leak between clauses
4. **Cut Scope**: Cut only affects choicepoints in its scope
5. **Solution Independence**: Each solution is independent of others

## Interface Stability

The following interfaces are designed to remain stable through all stages:

1. **Engine.run(goals)** - Main entry point
2. **Choicepoint structure** - Core fields won't change
3. **Goal representation** - Term plus environment
4. **Builtin signature** - `(Engine, Goal) -> bool`
5. **Solution format** - Dictionary of variable bindings

## Dependencies on Stage -1

- Store and variable management
- Unification algorithm
- Trail and undo operations
- Term representations

## Preparation for Future Stages

- **Stage 1**: Builtin predicates plug into existing framework
- **Stage 2**: Indexing replaces simple clause iteration
- **Stage 3**: Tracer hooks into goal push/pop
- **Stage 4**: Attributed variables work with existing store
- **Stage 5+**: CLP(FD) uses attributed variable mechanism

## Open Questions to Resolve During Implementation

1. **Solution Representation**: How to present variable bindings to user?
2. **Trace Format**: What information to include in traces?
3. **Error Messages**: How to format and report errors?
4. **Builtin Registration**: Static dictionary or dynamic registration?
5. **Goal Expansion**: When to expand goal aliases (e.g., lists to conjunctions)?

These will be resolved based on what emerges during implementation, following the principle of discovering the right design through building.