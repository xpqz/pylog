# T0.3 Test Plan: Engine.run() Single Iterative Loop

## Test Coverage Overview

The test suite for T0.3 covers the following key aspects of the single-loop VM:

### 1. Basic Execution (`TestBasicExecution`)
- **Trivial queries**: Facts with and without variables
- **Conjunctions**: Left-to-right execution order
- **Empty queries**: Should succeed once
- **Builtins**: `true/0` and `fail/0`
- **Rule bodies**: Proper goal scheduling
- **No recursion**: Deep chains execute without Python recursion

### 2. Backtracking (`TestBacktracking`)
- **Multiple facts**: Enumerate all matching clauses
- **Conjunction backtracking**: Generate all combinations
- **Disjunction**: Try both alternatives
- **Variable restoration**: Proper trailing and unwinding
- **Solution limits**: Respect `max_solutions`

### 3. Control Constructs (`TestControlConstructs`)
- **Cut (`!`)**: Prevent backtracking past cut point
- **Cut in conjunction**: Correct scope limitation
- **If-then-else**: Proper branch selection
- **Nested structures**: Correct execution order

### 4. Frame and Choicepoint Management (`TestFrameAndChoicepoint`)
- **Frame lifecycle**: One frame per predicate call
- **State restoration**: Choicepoints restore properly
- **Deep backtracking**: Through multiple call levels

### 5. Error Handling (`TestErrorHandling`)
- **Undefined predicates**: Should fail gracefully
- **Infinite loops**: Respect solution limits
- **Variable isolation**: Between clause instances
- **Occurs check**: Prevent infinite structures

### 6. Integration (`TestIntegration`)
- **List operations**: `member/2` and `append/3`
- **Arithmetic**: Factorial with `is/2` evaluation

## Key Requirements Verified

1. **No Python Recursion**: The test with 1000-deep rule chain verifies the engine uses explicit stacks
2. **Correct Execution Order**: Conjunctions execute left-to-right, backtracking is depth-first
3. **Proper State Management**: Variables are correctly bound/unbound during backtracking
4. **Control Flow**: Cut and if-then-else work as specified
5. **Isolation**: Each clause instance gets fresh variables

## Implementation Notes

The Engine.run() implementation should:

1. Use the runtime types from T0.2:
   - `GoalStack` for pending goals
   - `Frame` stack for activation records
   - `Choicepoint` stack for backtracking
   - `Trail` for state restoration

2. Main loop structure:
   ```python
   while goal := goal_stack.pop():
       dispatch(goal)
   ```

3. Dispatch based on `goal.type`:
   - `PREDICATE`: Dispatch to predicate handler
   - `BUILTIN`: Dispatch to builtin handler (separate from predicate)
   - `CONJUNCTION`: Push goals in reverse order
   - `DISJUNCTION`: Create choicepoint for alternative
   - `IF_THEN_ELSE`: Evaluate condition and choose branch
   - `CUT`: Remove choicepoints up to cut barrier

4. Backtracking:
   - When a goal fails and choicepoints exist
   - Restore goal_stack to cp.goal_top
   - Restore frame_stack to cp.frame_top  
   - Restore state via trail.unwind_to(cp.trail_top)
   - Resume from choicepoint

5. Solution recording:
   - When goal stack empty and no failure
   - Project user query vars in declared order
   - No allocations during reification
   - Use iterative traversal only
   - Continue via backtracking for more solutions

## Critical Invariants to Maintain

1. **No Python Recursion**: All operations must be iterative
2. **Stack Height Consistency**: After backtracking, stack heights must match CP records
3. **Frame Lifecycle**: Exactly one frame per predicate call, popped once
4. **Trail Discipline**: Variables trailed at most once per choice region
5. **Variable Isolation**: Each clause instance gets fresh variables
6. **Cut Scope**: Cut only affects choicepoints within its predicate
7. **Solution Projection**: Only query variables in solutions, stable order