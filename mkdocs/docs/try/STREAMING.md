# Web REPL Streaming Mode

## Overview

The Web REPL supports an optional per-solution streaming mode that emits individual solution events as they are found, providing a more responsive user experience for queries with multiple solutions.

## Current Implementation

### Features
- **Toggle Command**: Use `streaming on` or `streaming off` to switch modes
- **Individual Events**: Each solution is sent as a separate `solution` message
- **Completion Event**: A final `done` message includes total count and timing
- **Backward Compatible**: Default remains batched mode

### Message Format

**Solution Event**:
```javascript
{
  type: 'solution',
  index: 0,           // 0-based solution index
  bindings: {         // Structured variable bindings
    X: { kind: 'Atom', value: 'red' }
  },
  pretty: 'X = red'   // Pre-formatted output
}
```

**Done Event**:
```javascript
{
  type: 'done',
  solutions: 3,       // Total solution count
  elapsedMs: 150,     // Elapsed time in milliseconds
  stepCount: 42       // Engine steps taken
}
```

## Current Limitations

### Not True Incremental Streaming
The current implementation still calls `pylogEngine.run()` and collects all solutions before posting individual events. This means:
- No reduction in time-to-first-solution for long-running queries
- All solutions are computed before any are displayed
- Memory usage is similar to batched mode during computation

This satisfies the message protocol requirements and provides the UI experience of streaming, but doesn't yet provide the performance benefits of true incremental solution generation.

### Future Improvements

To achieve true incremental streaming with reduced time-to-first-solution:
1. **Engine API Changes**: Modify the Python engine to support generator/iterator interfaces
2. **Pyodide Bridge**: Implement async iteration over Python generators in JavaScript
3. **Worker Protocol**: Support partial result batching and pause/resume semantics

## Usage

### Enable Streaming
```prolog
?- streaming on.
Streaming mode enabled.
Solutions will be displayed as they are found.
```

### Disable Streaming
```prolog
?- streaming off.
Streaming mode disabled.
Solutions will be displayed all at once.
```

### Check Status
```prolog
?- limits.
Current safety limits:
  Max steps: 100,000
  Max solutions: 100
  Timeout: 10s
  Configurable: Yes
  Streaming: Enabled
```

## Implementation Notes

### Memory Optimization
In streaming mode, the worker:
- Doesn't build the full results array
- Uses a counter instead of array.push()
- Reduces memory footprint for queries with many solutions

### UI Behavior
The UI in streaming mode:
- Displays each solution immediately upon receipt
- Shows semicolons between solutions
- Replaces the final semicolon with a period
- Displays metadata after all solutions

### Error Handling
Streaming mode maintains the same error handling:
- Parse errors are reported immediately
- Step/solution limits are enforced
- Timeout protection remains active