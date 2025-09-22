# Stage 4: Attributed Variables Implementation Plan

*Extensibility mechanism for constraint solvers and domain-specific extensions*

---

## Overview

Stage 4 introduces attributed variables, providing the fundamental extensibility mechanism that enables constraint solvers (like CLP(FD) in Stage 5) and other domain-specific extensions to integrate cleanly with the core Prolog engine. This mechanism allows modules to attach arbitrary data to variables and intercede during unification through hooks.

## Goals

1. **Clean extensibility** - Module-based attribute system without engine modifications
2. **Hook-based integration** - Intercept unification to enforce constraints
3. **Complete backtracking** - All attribute changes properly trailed and restored
4. **Zero overhead** - No performance impact when attributes not used
5. **Module isolation** - Attributes from different modules coexist without interference
6. **Foundation for CLP(FD)** - Enable Stage 5 constraint solver implementation

## Invariants

The attributed variable system maintains these invariants:

1. **Attribute consistency** - After dereferencing, only root variables carry attributes
2. **Trail completeness** - Every attribute mutation has a corresponding trail entry
3. **Hook atomicity** - All hooks for a unification succeed or all fail (transaction-like)
4. **Module isolation** - One module's attributes never affect another's unless explicitly coordinated
5. **Backtrack restoration** - After undo_to(), attributes match exact prior state
6. **Fast path preservation** - Variables without attributes follow existing code paths unchanged

## Architecture

### Core Components

#### 1. Attribute Storage
```python
class Store:
    """Extended with attribute support."""
    def __init__(self):
        self.cells: List[Cell] = []
        # Sparse dict: only vars with attributes have entries
        self.attrs: Dict[int, Dict[str, Any]] = {}

    def get_attrs(self, varid: int) -> Optional[Dict[str, Any]]:
        """Get all attributes for a variable (after deref)."""
        root = self.deref(varid)[1]
        return self.attrs.get(root)

    def get_attr(self, varid: int, module: str) -> Optional[Any]:
        """Get specific attribute for a variable."""
        attrs = self.get_attrs(varid)
        return attrs.get(module) if attrs else None

    def put_attr(self, varid: int, module: str, value: Any, trail: Trail) -> None:
        """Set attribute with trailing."""
        root = self.deref(varid)[1]

        # Trail the old value (None if new)
        if root in self.attrs and module in self.attrs[root]:
            old_value = self.attrs[root][module]
        else:
            old_value = None
        trail.push(('attr', root, module, old_value))

        # Set new value
        if root not in self.attrs:
            self.attrs[root] = {}
        self.attrs[root][module] = value

    def del_attr(self, varid: int, module: str, trail: Trail) -> None:
        """Delete attribute with trailing."""
        root = self.deref(varid)[1]
        if root in self.attrs and module in self.attrs[root]:
            old_value = self.attrs[root][module]
            trail.push(('attr', root, module, old_value))
            del self.attrs[root][module]
            if not self.attrs[root]:
                del self.attrs[root]
```

#### 2. Trail Extension
```python
def undo_to(mark: int, trail: Any, store: Any) -> None:
    """Extended to handle attribute trail entries."""
    entries = trail.entries if hasattr(trail, "entries") else trail

    while len(entries) > mark:
        entry = entries.pop()
        tag = entry[0]

        if tag == "parent":
            _, vid, old_parent = entry
            store.cells[vid].ref = old_parent

        elif tag == "bind":
            _, vid, old_cell = entry
            store.cells[vid] = old_cell

        elif tag == "rank":
            _, vid, old_rank = entry
            store.cells[vid].rank = old_rank

        elif tag == "attr":
            # Restore attribute value
            _, vid, module, old_value = entry
            if old_value is None:
                # Was not present before
                if vid in store.attrs and module in store.attrs[vid]:
                    del store.attrs[vid][module]
                    if not store.attrs[vid]:
                        del store.attrs[vid]
            else:
                # Restore old value
                if vid not in store.attrs:
                    store.attrs[vid] = {}
                store.attrs[vid][module] = old_value
```

#### 3. Hook Registry
```python
class Engine:
    """Extended with attribute hook support."""
    def __init__(self, ...):
        # Existing initialization
        self._attr_hooks: Dict[str, Callable] = {}

    def register_attr_hook(self, module: str, hook: Callable) -> None:
        """Register a unification hook for a module.

        Hook signature: hook(engine, varid, other_term) -> bool
        - engine: This engine instance (for context)
        - varid: The attributed variable being unified
        - other_term: What it's being unified with
        - Returns: True to continue, False to fail unification
        """
        self._attr_hooks[module] = hook

    def dispatch_attr_hooks(self, varid: int, other_term: Any) -> bool:
        """Call all relevant hooks for a variable's attributes."""
        attrs = self.store.get_attrs(varid)
        if not attrs:
            return True  # No attributes, continue

        # Call each module's hook
        for module, value in attrs.items():
            if module in self._attr_hooks:
                hook = self._attr_hooks[module]
                if not hook(self, varid, other_term):
                    return False  # Hook vetoed unification

        return True  # All hooks approved
```

#### 4. Unification Integration
```python
class TrailAdapter:
    """Adapter to pass engine context to unify without changing signatures."""
    def __init__(self, trail: Trail, store: Store, engine: Optional[Engine] = None):
        self.trail = trail
        self.store = store
        self.engine = engine  # For hook dispatch

    def push(self, entry):
        self.trail.push(entry)

    def mark(self):
        return self.trail.mark()

def unify(term1, term2, store, trail_adapter, occurs_check=False):
    """Extended unify to handle attributed variables."""
    # ... existing unification logic ...

    # When binding var to non-var
    if t1_tag == "UNBOUND" and t2_tag != "UNBOUND":
        # Check attribute hooks
        if trail_adapter.engine:
            if not trail_adapter.engine.dispatch_attr_hooks(t1_root, term2):
                return False  # Hook rejected

        # Proceed with normal binding
        bind_root_to_term(t1_root, term2, store, trail_adapter)
        return True

    # When unifying two variables (aliasing)
    if t1_tag == "UNBOUND" and t2_tag == "UNBOUND":
        # Check hooks on both sides
        if trail_adapter.engine:
            attrs1 = store.get_attrs(t1_root)
            attrs2 = store.get_attrs(t2_root)

            # Call hooks for overlapping modules
            if attrs1 and attrs2:
                all_modules = set(attrs1.keys()) | set(attrs2.keys())
                for module in all_modules:
                    if module in trail_adapter.engine._attr_hooks:
                        hook = trail_adapter.engine._attr_hooks[module]
                        # Hook sees both variables
                        if not hook(trail_adapter.engine, t1_root, Var(t2_root)):
                            return False
                        if not hook(trail_adapter.engine, t2_root, Var(t1_root)):
                            return False

        # Merge attributes to the union-find root
        if store.attrs:
            merge_attributes(t1_root, t2_root, store, trail_adapter)

        # Normal union operation
        union_vars(t1_root, t2_root, store, trail_adapter)
        return True

def merge_attributes(v1: int, v2: int, store: Store, trail: TrailAdapter):
    """Merge attributes when unifying variables."""
    # Determine which will be the root after union
    # (using union-by-rank logic)
    c1, c2 = store.cells[v1], store.cells[v2]
    if c1.rank < c2.rank:
        child, root = v1, v2
    elif c1.rank > c2.rank:
        child, root = v2, v1
    else:
        child, root = v2, v1  # arbitrary but consistent

    # Move child's attributes to root
    if child in store.attrs:
        child_attrs = store.attrs[child]
        for module, value in child_attrs.items():
            # Trail the change
            trail.push(('attr', child, module, value))

            # Move to root (may overwrite - hooks should handle)
            if root not in store.attrs:
                store.attrs[root] = {}
            if module in store.attrs[root]:
                # Module exists on both - hook should have resolved
                trail.push(('attr', root, module, store.attrs[root][module]))
            store.attrs[root][module] = value

        # Clear child's attributes
        del store.attrs[child]
```

### Builtin Predicates (engine-conformant signatures)

In this codebase builtins have the uniform signature `fn(engine, args_tuple) -> bool` and are registered in `Engine._register_builtins` as methods.

#### put_attr/3
```python
def _builtin_put_attr(self, args: tuple) -> bool:
    # put_attr(+Var, +Module, +Value)
    if len(args) != 3:
        return False
    var_term, module_term, value_term = args

    # Var must be (dereferenced) Var
    while isinstance(var_term, Var):
        res = self.store.deref(var_term.id)
        if res[0] == "BOUND":
            # Binding an attributed var to nonvar is allowed, but put_attr expects a var
            return False
        _, root_vid = res
        break
    else:
        return False

    # Module must be Atom
    if not isinstance(module_term, Atom):
        return False

    # Trail and set attribute (engine runtime Trail supports 'attr')
    if not hasattr(self.store, 'attrs'):
        self.store.attrs = {}
    old = None
    if root_vid in self.store.attrs and module_term.name in self.store.attrs[root_vid]:
        old = self.store.attrs[root_vid][module_term.name]
    self.trail.push_attr(root_vid, module_term.name, old)
    self.store.attrs.setdefault(root_vid, {})[module_term.name] = value_term
    return True
```

#### get_attr/3
```python
def _builtin_get_attr(self, args: tuple) -> bool:
    # get_attr(+Var, +Module, ?Value)
    if len(args) != 3:
        return False
    var_term, module_term, value_pat = args

    # Var must be a Var
    if not isinstance(var_term, Var):
        return False
    res = self.store.deref(var_term.id)
    if res[0] != "UNBOUND":
        return False
    _, root_vid = res

    # Module must be Atom
    if not isinstance(module_term, Atom):
        return False

    value = None
    if hasattr(self.store, 'attrs'):
        value = self.store.attrs.get(root_vid, {}).get(module_term.name)
    if value is None:
        return False

    trail_adapter = TrailAdapter(self.trail, store=self.store, engine=self)
    return unify(value, value_pat, self.store, trail_adapter, occurs_check=self.occurs_check)
```

#### del_attr/2
```python
def _builtin_del_attr(self, args: tuple) -> bool:
    # del_attr(+Var, +Module)
    if len(args) != 2:
        return False
    var_term, module_term = args

    if not isinstance(var_term, Var):
        return False
    res = self.store.deref(var_term.id)
    if res[0] != "UNBOUND":
        return False
    _, root_vid = res
    if not isinstance(module_term, Atom):
        return False

    if hasattr(self.store, 'attrs') and root_vid in self.store.attrs and module_term.name in self.store.attrs[root_vid]:
        old = self.store.attrs[root_vid][module_term.name]
        self.trail.push_attr(root_vid, module_term.name, old)
        del self.store.attrs[root_vid][module_term.name]
        if not self.store.attrs[root_vid]:
            del self.store.attrs[root_vid]
    else:
        # Nothing to delete; succeed deterministically
        pass
    return True
```

### Integration Points

#### Engine Integration
```python
# Register builtins inside Engine._register_builtins():
self._builtins[("put_attr", 3)] = lambda eng, args: eng._builtin_put_attr(args)
self._builtins[("get_attr", 3)] = lambda eng, args: eng._builtin_get_attr(args)
self._builtins[("del_attr", 2)] = lambda eng, args: eng._builtin_del_attr(args)

# Ensure TrailAdapter(engine=self, store=self.store) is used in unify calls
# (already true via Engine._unify).
```

#### Tracer Integration (Optional)
```python
class PortsTracer:
    def trace_attr_event(self, kind: str, varid: int, module: str, value: Any):
        """Emit internal event for attribute changes (debug only)."""
        if not self.enable_internal_events:
            return

        self.emit_internal_event(
            kind=f"attr_{kind}",  # attr_put, attr_get, attr_del
            varid=varid,
            module=module,
            value=self._format_term(value) if value else None
        )
```

## Implementation Phases

### Phase 4.0: Core Attribute Storage (2 days)
**Goal**: Basic attribute storage and trail support

**Deliverables**:
- Extend Store with attrs dict
- Add attr trail entry type
- Extend undo_to for attribute restoration
- Unit tests for storage operations

**Tests**:
- Set/get/delete attributes
- Trail and restore attributes
- Multiple modules on same variable
- Backtracking restores exact state

### Phase 4.1: Builtin Predicates (2 days)
**Goal**: API surface for Prolog code

**Deliverables**:
- Implement put_attr/3
- Implement get_attr/3
- Implement del_attr/2
- Integration with engine dispatch

**Tests**:
- Each builtin with valid inputs
- Failure cases (non-var, missing attr)
- Unification in get_attr/3
- Determinism properties

### Phase 4.2: Hook Registry & Var-Nonvar Integration (3 days)
**Goal**: Module hooks can intercept unification

**Deliverables**:
- Hook registry in engine
- register_attr_hook API
- Integration in var-nonvar unification
- TrailAdapter with engine reference

**Tests**:
- Hook called on unification
- Hook can reject unification
- Multiple modules' hooks called
- Backtracking after hook failure

### Phase 4.3: Var-Var Aliasing (3 days)
**Goal**: Handle attribute merge during aliasing

**Deliverables**:
- merge_attributes function
- Integration in var-var unification
- Hook dispatch for overlapping modules
- Attribute migration to union-find root

**Tests**:
- Attributes visible through alias
- Overlapping modules handled
- Backtrack restores separation
- Union-find root carries merged attrs

### Phase 4.4: Testing & Documentation (2 days)
**Goal**: Comprehensive validation

**Deliverables**:
- Property tests for confluence
- Integration tests with multiple modules
- Performance benchmarks (zero overhead)
- Documentation and examples

**Tests**:
- "must_be_even" example module
- Multiple modules interaction
- Stress tests (many attrs/modules)
- Performance: < 1% overhead when unused

## Test Plan

### Unit Tests
```python
def test_basic_attribute_operations():
    """Test put/get/del on single variable."""
    store = Store()
    trail = Trail()

    v = store.new_var()

    # Put attribute
    store.put_attr(v, "test", Int(42), trail)
    assert store.get_attr(v, "test") == Int(42)

    # Overwrite
    store.put_attr(v, "test", Int(99), trail)
    assert store.get_attr(v, "test") == Int(99)

    # Delete
    store.del_attr(v, "test", trail)
    assert store.get_attr(v, "test") is None

    # Backtrack
    undo_to(0, trail, store)
    assert store.get_attr(v, "test") is None

def test_aliasing_preserves_attributes():
    """Test attributes visible through aliasing."""
    engine = Engine(...)

    # Create two variables
    X = Var(engine.store.new_var())
    Y = Var(engine.store.new_var())

    # Put attribute on X
    engine.store.put_attr(X.id, "test", Int(1), engine.trail)

    # Unify X = Y
    assert engine.unify_terms(X, Y)

    # Attribute visible through Y
    assert engine.store.get_attr(Y.id, "test") == Int(1)

    # Backtrack
    mark = engine.trail.mark()
    undo_to(mark, engine.trail, engine.store)

    # Attributes separated again
    assert engine.store.get_attr(X.id, "test") == Int(1)
    assert engine.store.get_attr(Y.id, "test") is None
```

### Integration Tests
```python
def test_must_be_even_constraint():
    """Example constraint module using hooks."""

    def must_be_even_hook(engine, varid, other):
        """Hook that enforces even numbers."""
        if isinstance(other, Int):
            return other.value % 2 == 0
        return True  # Don't constrain non-integers

    engine = Engine(...)
    engine.register_attr_hook("must_be_even", must_be_even_hook)

    # Run Prolog code
    result = engine.run_query("""
        ?- put_attr(X, must_be_even, true),
           X = 4.
    """)
    assert len(result) == 1  # Succeeds

    result = engine.run_query("""
        ?- put_attr(X, must_be_even, true),
           X = 3.
    """)
    assert len(result) == 0  # Fails
```

### Property Tests
```python
def test_hook_confluence():
    """Order of attribute operations doesn't affect outcome."""
    # Property: For any sequence of put_attr/unify operations,
    # different orderings that respect dependencies yield same result

    @hypothesis.given(
        ops=strategies.lists(
            strategies.one_of(
                strategies.tuples(strategies.just("put"), var_id(), module(), value()),
                strategies.tuples(strategies.just("unify"), var_id(), var_id())
            )
        )
    )
    def check_confluence(ops):
        # Apply ops in different valid orders
        # Assert final states are identical
        pass
```

### Performance Tests
```python
def test_zero_overhead_without_attributes():
    """No performance impact when attributes not used."""

    # Baseline: unification without attributes
    engine_plain = Engine(...)
    time_plain = benchmark_unification(engine_plain, n=10000)

    # With attribute support but unused
    engine_attr = Engine(...)  # Has attr support
    time_attr = benchmark_unification(engine_attr, n=10000)

    # Should be within 1%
    overhead = (time_attr - time_plain) / time_plain
    assert overhead < 0.01
```

## Acceptance Criteria

### Functionality
- [ ] put_attr/3, get_attr/3, del_attr/2 work correctly
- [ ] Hooks called during unification
- [ ] Hooks can reject unification
- [ ] Attributes preserved through aliasing
- [ ] Complete backtracking restoration
- [ ] Multiple modules coexist peacefully

### Performance
- [ ] < 1% overhead when no attributes used
- [ ] < 5% overhead with sparse attributes (10% vars)
- [ ] < 10% overhead with dense attributes (90% vars)
- [ ] Hook dispatch O(modules) not O(variables)

### Quality
- [ ] 100% trail coverage for attribute operations
- [ ] Property tests pass (confluence, etc.)
- [ ] Integration with existing test suite
- [ ] Documentation with examples
- [ ] Error messages helpful

### Integration
- [ ] Clean integration with unify
- [ ] Minimal changes to existing code
- [ ] Tracer support for debugging
- [ ] REPL commands for inspection

## Example Usage

### Basic Attribute Operations
```prolog
?- put_attr(X, color, red).
true.

?- put_attr(X, color, red), get_attr(X, color, C).
C = red.

?- put_attr(X, color, red), put_attr(X, size, large),
   get_attr(X, color, C), get_attr(X, size, S).
C = red, S = large.

?- put_attr(X, data, [1,2,3]), X = Y, get_attr(Y, data, D).
D = [1,2,3].
```

### Constraint Example
```prolog
% Define a "positive" constraint
:- module(positive, []).

attr_unify_hook(positive, Var, Other) :-
    ( integer(Other) ->
        Other > 0  % Reject non-positive integers
    ; true        % Accept non-integers (for now)
    ).

% Use the constraint
?- put_attr(X, positive, true), X = 5.
true.

?- put_attr(X, positive, true), X = -3.
false.

?- put_attr(X, positive, true), X = Y, Y = 10.
true.
```

### Multiple Modules
```prolog
?- put_attr(X, mod1, data1),
   put_attr(X, mod2, data2),
   X = Y,
   get_attr(Y, mod1, D1),
   get_attr(Y, mod2, D2).
D1 = data1, D2 = data2.
```

## Implementation Notes

### Design Decisions
1. **Sparse storage**: Only variables with attributes have entries in store.attrs
2. **Module strings**: Module names are strings, not atoms (simpler, no interning)
3. **Trail completeness**: Every attribute change is trailed, even overwrites
4. **Hook simplicity**: Hooks are simple predicates, not full goals
5. **No persistence**: Attributes not preserved through copy_term (Stage 4 scope)

### Performance Considerations
1. **Fast path**: Check for attrs dict presence before any lookup
2. **Lazy creation**: attrs dict and per-var dicts created on demand
3. **Direct dispatch**: Hook registry for O(1) lookup by module
4. **Batch operations**: Consider batching trail entries in hot paths
5. **Memory locality**: Keep attrs near cells if possible

### Future Extensions (Beyond Stage 4)
1. **Attribute portray**: Custom printing for attributed variables
2. **copy_term/2 hooks**: Preserve attributes through copying
3. **Module coordination**: Protocol for modules to negotiate
4. **Attribute inspection**: Debugging predicates to examine attrs
5. **Performance**: Specialized paths for common patterns

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Performance regression | High | Careful fast-path preservation, benchmarking |
| Complex integration | Medium | TrailAdapter pattern, minimal changes |
| Hook infinite loops | Medium | Document hook responsibilities clearly |
| Memory overhead | Low | Sparse storage, cleanup on backtrack |
| Module conflicts | Medium | Clear module isolation, documentation |

## Dependencies

### Required from Previous Stages
- Stage 0: Core engine with explicit stacks
- Stage 1: Basic unification and trail
- Stage 2: Indexing (compatible but independent)
- Stage 3: Tracer (optional integration)

### Enables Future Stages
- Stage 5: CLP(FD) uses attributes for domains
- Stage 5.5: Reification uses attributes for watches
- Stage 6: Global constraints use shared attributes

## References

- SWI-Prolog: [Attributed Variables](https://www.swi-prolog.org/pldoc/man?section=attvar)
- ECLiPSe: [Attribute Handling](http://eclipseclp.org/doc/userman/umsroot087.html)
- Theory: [Attributed Variables: A Versatile Unification Mechanism](https://citeseerx.ist.psu.edu/document?doi=10.1.1.47.5457)
- Implementation: [hProlog attributed variables](https://github.com/vscosta/yap-6.3/blob/master/docs/attributes.md)

## Summary

Stage 4 provides the crucial extensibility mechanism that transforms PyLog from a pure Prolog engine into a platform for constraint programming and domain-specific extensions. The implementation is carefully designed to have zero overhead when not used, while providing clean integration points for modules to participate in unification through hooks. This sets the foundation for Stage 5's CLP(FD) implementation and beyond.
