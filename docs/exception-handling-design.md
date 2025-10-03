# Exception Handling Design Choices

## Ball Copy Semantics

### Design Choice: Reification vs Copy Semantics

PyLog uses **reification at throw time** instead of ISO Prolog's copy semantics for exception handling. This is a deliberate design choice for efficiency and consistency with PyLog's overall architecture.

### Technical Details

**ISO Prolog Behavior:**
- When an exception is thrown, the ball (exception term) is copied before being passed to the catcher
- This copy preserves the variable bindings at the exact moment of the throw
- Subsequent modifications to the original variables don't affect the caught ball

**PyLog Behavior:**
- When an exception is thrown, the ball is reified (variables are replaced with their current values)
- This reification happens at throw time and creates a ground term
- The caught ball reflects the variable bindings at the moment of throw, but through reification rather than copying

### Practical Implications

In most practical cases, the behavior is identical. The difference only becomes apparent in edge cases where:

1. Variables in the thrown ball would be modified after throw but before catch
2. Complex term structures with shared variables need precise copying semantics

### Example

```prolog
% This pattern shows the difference:
test(X, Y) :-
    X = original,
    catch(
        (X = modified, throw(X)),  % This fails in both systems
        Y,
        true
    ).
```

The above example fails in both PyLog and ISO Prolog because `X` cannot be unified with both `original` and `modified`. However, in cases where the throw happens with unbound variables that are later bound, the systems might differ.

### Rationale

1. **Performance**: Reification is more efficient than deep copying
2. **Consistency**: Aligns with PyLog's explicit stack and trail-based architecture
3. **Simplicity**: Avoids complex copy semantics implementation
4. **Practical equivalence**: For typical use cases, the behavior is indistinguishable

### Compatibility Note

This design choice means PyLog may behave differently from ISO Prolog in very specific edge cases involving exception handling with complex variable sharing patterns. Such cases are rare in practical Prolog programming.

### Future Considerations

If strict ISO compliance is needed, an optional ISO mode could be implemented:

```python
if self.mode == "iso":
    # Use copy_term semantics
    ball = copy_term(ball, self.store)
else:
    # Use reification (current behavior)
    ball = self._reify_term(ball)
```

However, this is not currently planned as the practical benefits don't justify the implementation complexity.