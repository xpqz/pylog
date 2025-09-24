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

## Troubleshooting

### Common Issues and Solutions

#### 1. Attributes Not Preserved After Unification
**Problem**: Attributes disappear after unifying variables.

**Solution**: Ensure your hook returns `True` to allow unification. Check that attributes are attached to the root variable after union-find operations:
```python
root_id = engine.store.find(varid)
attrs = engine.store.attrs.get(root_id, {})
```

#### 2. Hook Not Being Called
**Problem**: Registered hook isn't triggered during unification.

**Solution**:
- Verify the module name matches exactly between `put_attr` and hook registration
- Check that the variable is actually attributed before unification
- Ensure hook is registered before the unification occurs

#### 3. Unexpected Backtracking Behavior
**Problem**: Attributes not properly restored after failure.

**Solution**: All attribute operations are automatically trailed. If custom operations modify attributes directly, use the Store's methods rather than direct manipulation:
```python
# Wrong - bypasses trailing
engine.store.attrs[varid][module] = value

# Correct - properly trailed
engine.store.put_attr(varid, module, value)
```

#### 4. Performance Degradation
**Problem**: Significant slowdown with attributed variables.

**Solution**:
- Check if many variables have unused attributes
- Consider lazy attribute initialization
- Profile hook functions for expensive operations
- Use sparse attribute patterns when possible

## Performance Tuning Guidelines

### Optimization Strategies

#### 1. Minimize Attribute Access
- Cache frequently accessed attributes in local variables
- Use batch operations when updating multiple attributes
- Avoid redundant `get_attr` calls in tight loops

#### 2. Efficient Hook Implementation
```python
def optimized_hook(engine, varid, other):
    # Fast path for common cases
    if isinstance(other, Var):
        return True  # Defer checking to later

    # Only fetch attributes when needed
    if not isinstance(other, Int):
        return True

    # Now do expensive attribute checking
    attrs = engine.store.attrs.get(varid, {})
    # ...
```

#### 3. Strategic Attribute Placement
- Attach attributes only to variables that need them
- Use module-specific attributes rather than generic ones
- Clean up attributes when no longer needed with `del_attr`

#### 4. Memory Management
- Periodically compact the union-find structure during GC
- Consider attribute pooling for frequently created/destroyed attributes
- Use weak references for auxiliary attribute data when appropriate

### Profiling Attributed Variables

To identify performance bottlenecks:

```python
import cProfile
import pstats

# Profile your Prolog code
profiler = cProfile.Profile()
profiler.enable()

# Run your query
engine.query("your_goal(X)")

profiler.disable()
stats = pstats.Stats(profiler)
stats.sort_stats('cumulative')
stats.print_stats(20)  # Top 20 functions
```

Look for:
- High call counts to `get_attr`/`put_attr`
- Slow hook functions
- Excessive trail operations

## Migration Guide from Other Prolog Systems

### From SWI-Prolog

PyLog's attributed variables closely follow SWI-Prolog's model:

**Similarities**:
- Same core predicates: `put_attr/3`, `get_attr/3`, `del_attr/2`
- Module-based attribute organization
- Hook-based unification customization

**Differences**:
- Hook signature: PyLog uses `(engine, varid, other)` vs SWI's module-based hooks
- No automatic attribute portray hooks (implement manually if needed)
- No `attr_unify_hook/2` declaration - register hooks programmatically

**Migration example**:
```prolog
% SWI-Prolog
:- module(myconstraint, []).
:- use_module(library(atts)).

attr_unify_hook(Attr, Other) :-
    % Handle unification
    ...

% PyLog equivalent (in Python)
def myconstraint_hook(engine, varid, other):
    # Handle unification
    ...

engine.register_attr_hook("myconstraint", myconstraint_hook)
```

### From SICStus Prolog

**Key differences**:
- SICStus uses `Module:get_atts/2` vs PyLog's `get_attr/3`
- No `put_atts/2` with modification lists - use individual `put_attr` calls
- Simpler hook mechanism in PyLog

**Migration pattern**:
```prolog
% SICStus
get_atts(Var, module(Attr))

% PyLog
get_attr(Var, module, Attr)
```

### From ECLiPSe Prolog

**Main changes**:
- ECLiPSe's `meta_attribute/2` declaration not needed
- Suspension mechanisms must be implemented manually
- Demon-style constraints need explicit propagation queues

### General Migration Tips

1. **Start with core functionality**: Port basic attribute operations first
2. **Adapt hooks incrementally**: Convert unification hooks one module at a time
3. **Test backtracking thoroughly**: Ensure proper trail restoration
4. **Profile early**: Compare performance characteristics between systems
5. **Leverage PyLog specifics**: Use Python's features for complex data structures

### Compatibility Layer Example

For easier migration, create a compatibility layer:

```python
class SWICompatibility:
    def __init__(self, engine):
        self.engine = engine

    def put_atts(self, var, attrs):
        """SICStus-style attribute setting."""
        for module, value in attrs.items():
            self.engine.store.put_attr(var.id, module, value)

    def get_atts(self, var, module):
        """SICStus-style attribute getting."""
        return self.engine.store.get_attr(var.id, module)
```

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