# Attributed Variables in PyLog

## Overview

Attributed variables are a powerful mechanism in Prolog that allows attaching arbitrary metadata (attributes) to unbound variables. This metadata is organized by modules (namespaces) and can be used to implement constraint solvers, type systems, and other advanced features.

PyLog implements attributed variables as described in Stage 4 of the development plan, providing a foundation for CLP(FD) and other constraint systems.

## Core API

### Basic Operations

```prolog
% Attach an attribute to a variable
?- put_attr(X, module_name, value).

% Retrieve an attribute from a variable
?- get_attr(X, module_name, Value).

% Remove an attribute from a variable
?- del_attr(X, module_name).
```

### Predicates

- **`put_attr(+Var, +Module, +Value)`**: Attaches or updates an attribute for the given module
- **`get_attr(+Var, +Module, -Value)`**: Retrieves an attribute value, fails if not present
- **`del_attr(+Var, +Module)`**: Removes an attribute from a variable

## Unification Hooks

When an attributed variable is unified with another term, PyLog calls registered hooks to allow custom unification behavior. This enables constraint propagation and validation.

### Hook Registration

Hooks are registered at the engine level:

```python
def my_hook(engine, varid, other):
    """
    Called when an attributed variable is unified.

    Args:
        engine: The Prolog engine instance
        varid: ID of the attributed variable being unified
        other: The term being unified with

    Returns:
        bool: True to allow unification, False to fail
    """
    return True

engine.register_attr_hook("module_name", my_hook)
```

### Hook Dispatch Order

When two attributed variables are unified (var-var aliasing), hooks are called from both perspectives to ensure symmetry:

1. First: `hook(engine, var1, Var(var2))`
2. Second: `hook(engine, var2, Var(var1))`

Both hooks must succeed for unification to proceed.

## Attribute Merging

When two attributed variables are unified:

1. The winner of the union-find becomes the root
2. Attributes from both variables are merged at the root
3. For overlapping modules, the root's value takes precedence
4. Non-overlapping modules are combined

Example:
```prolog
?- put_attr(X, color, red),
   put_attr(X, size, large),
   put_attr(Y, color, blue),
   put_attr(Y, weight, 10),
   X = Y.
% Result: Root has {color: red, size: large, weight: 10}
% (root's 'red' wins over 'blue' for color module)
```

## Backtracking

All attribute operations are properly trailed and undone on backtracking:

```prolog
?- (put_attr(X, test, 1), fail) ; get_attr(X, test, V).
% Fails - attribute was undone by backtracking
```

## Example: Positive Integer Constraint

```python
def positive_hook(engine, varid, other):
    """Only allow positive integers."""
    if isinstance(other, Int):
        return other.value > 0
    return True  # Allow variables/structures

engine.register_attr_hook("positive", positive_hook)
```

```prolog
?- put_attr(X, positive, true), X = 5.
% Succeeds

?- put_attr(X, positive, true), X = -3.
% Fails - hook rejects negative value
```

## Example: Type Checking

```python
def type_hook(engine, varid, other):
    """Enforce type constraints."""
    attrs = engine.store.attrs.get(varid, {})
    expected = attrs.get("type")

    if expected == Atom("integer"):
        return isinstance(other, Int)
    elif expected == Atom("atom"):
        return isinstance(other, Atom)
    elif expected == Atom("list"):
        return isinstance(other, PrologList)
    return True

engine.register_attr_hook("type", type_hook)
```

```prolog
?- put_attr(X, type, integer), X = 42.
% Succeeds

?- put_attr(X, type, integer), X = hello.
% Fails - type mismatch
```

## Performance Characteristics

The attributed variables system has been designed for efficiency:

- **< 1% overhead** when attributes are not used
- **< 5% overhead** with sparse attributes (10% of variables)
- **< 20% overhead** with dense attributes (90% of variables)

Performance is achieved through:
- Lazy initialization of attribute storage
- Efficient dictionary-based module storage
- Minimal overhead in deref() operations
- Proper trail segmentation for fast backtracking

## Integration with Existing Stages

Attributed variables integrate seamlessly with PyLog's existing functionality:

### Stage 1: Basic Operations
- Unification, cut (!), and basic builtins work correctly with attributed variables
- `var/1` returns true for attributed but unbound variables
- `nonvar/1` returns true only after binding

### Stage 2: Operators
- Arithmetic and comparison operators work with attributed variables after binding
- Attributes are preserved through operations

### Stage 3: Indexing
- First-argument indexing functions correctly with attributed variables
- Choicepoints properly handle attribute state

## Implementation Details

### Storage
- Attributes stored in `Store.attrs` dictionary: `{varid: {module: value}}`
- Only root variables in union-find structure have attributes
- Attributes follow variables through aliasing

### Trail Entries
- `('attr', varid, module, old_value)` - Attribute changes
- Properly restored on backtracking via `undo_to()`

### Hook Registry
- Engine maintains `hook_registry: Dict[str, Callable]`
- Hooks called during unification in `unify_helpers.py`
- False return from hook causes unification failure

## Testing

Comprehensive test coverage ensures correctness:

### Property Tests (`test_attr_properties.py`)
- Confluence: Order of operations doesn't affect result
- Associativity: Aliasing chains behave correctly
- Backtracking: Complete state restoration

### Integration Tests (`test_attr_integration.py`)
- Compatibility with all PyLog stages
- Example constraint implementations
- Edge cases and stress testing

### Performance Tests (`test_attr_performance.py`)
- Overhead measurements
- Scalability with many modules/variables
- Memory usage tracking

## Future Extensions

The attributed variables system provides the foundation for:

1. **CLP(FD)** - Constraint Logic Programming over Finite Domains
2. **Type Systems** - Static or dynamic type checking
3. **Debugging Tools** - Variable history tracking
4. **Custom Constraints** - User-defined constraint systems
5. **Tabling** - Memoization attributes

## References

- [SWI-Prolog Attributed Variables](https://www.swi-prolog.org/pldoc/man?section=attvar)
- [SICStus Prolog Attributed Variables](https://sicstus.sics.se/sicstus/docs/latest/html/sicstus/lib_002datts.html)
- ISO Prolog Part 2: Modules (attributed variables specification)