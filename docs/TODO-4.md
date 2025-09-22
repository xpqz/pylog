# TODO: Stage 4 (Attributed Variables)

**Key Policy**: Attributes live only on union-find root variables after dereferencing. All attribute mutations are trailed with before-images for exact restoration on backtrack.

## Phase 1: Core Attribute Storage (4.0) ✅ COMPLETED (PR #115)

### 1. Extend Store with Attribute Support
- [x] Write test: Store initializes with empty attrs dict
- [x] Write test: get_attrs returns None for variable without attributes
- [x] Write test: get_attrs follows deref to root
- [x] Write test: get_attr returns specific module value
- [x] Write test: get_attr returns None for missing module
- [x] Add store.attrs: Dict[int, Dict[str, Any]] field to Store.__init__
- [x] Implement Store.get_attrs(varid) method
- [x] Implement Store.get_attr(varid, module) method
- [x] Verify sparse storage (no entry for vars without attrs)

### 2. Implement Attribute Mutation with Trailing
- [x] Write test: put_attr sets attribute value
- [x] Write test: put_attr overwrites existing value
- [x] Write test: put_attr trails old value (or None if new)
- [x] Write test: del_attr removes attribute
- [x] Write test: del_attr trails old value before deletion
- [x] Write test: del_attr succeeds even if attribute absent
- [x] Implement Store.put_attr(varid, module, value, trail)
- [x] Implement Store.del_attr(varid, module, trail)
- [x] Verify trail entries have format ('attr', varid, module, old_value)
- [x] Verify attributes stored on deref'd root

### 3. Extend Trail for Attribute Entries
- [x] Write test: Trail handles ('attr', ...) entries
- [x] Write test: undo_to restores attribute to old value
- [x] Write test: undo_to removes attribute if was None
- [x] Write test: undo_to handles multiple attr changes to same var
- [x] Extend prolog/unify/trail.py:undo_to with 'attr' case
- [x] Implement restoration logic for attr entries
- [x] Verify exact state restoration after backtrack

### 4. Verify Runtime Trail (Already Implemented)
- [x] Confirm: Runtime Trail.push_attr exists in prolog/engine/runtime.py:260
- [x] Write test: Runtime Trail.push_attr creates correct entry format
- [x] Write test: Runtime Trail.unwind_to handles attr entries correctly
- [x] Write test: Multiple modules on same var restored in correct order
- [x] Write test: Attr restoration preserves exact state after backtrack
- [x] Verify runtime and unify trail implementations align

## Phase 2: Builtin Predicates (4.1)

### 1. Implement put_attr/3
- [ ] Write test: put_attr/3 with unbound variable succeeds
- [ ] Write test: put_attr/3 with variable that derefs to bound term fails
- [ ] Write test: put_attr/3 with non-atom module fails
- [ ] Write test: put_attr/3 trails attribute change
- [ ] Write test: put_attr/3 wrong arity fails
- [ ] Implement Engine._builtin_put_attr(self, args: tuple) -> bool
- [ ] Verify deref to ensure variable is unbound (not bound term)
- [ ] Verify module is Atom
- [ ] Trail and set attribute value
- [ ] Register in Engine._register_builtins with (put_attr, 3) key

### 2. Implement get_attr/3
- [ ] Write test: get_attr/3 retrieves existing attribute
- [ ] Write test: get_attr/3 fails for missing attribute
- [ ] Write test: get_attr/3 unifies third arg with value
- [ ] Write test: get_attr/3 with variable that derefs to bound term fails
- [ ] Write test: get_attr/3 with non-atom module fails
- [ ] Write test: get_attr/3 wrong arity fails
- [ ] Implement Engine._builtin_get_attr(self, args: tuple) -> bool
- [ ] Verify variable is unbound after deref (not bound term)
- [ ] Verify module is Atom
- [ ] Retrieve attribute and unify with third arg
- [ ] Use TrailAdapter for unification
- [ ] Register in Engine._register_builtins with (get_attr, 3) key

### 3. Implement del_attr/2
- [ ] Write test: del_attr/2 removes existing attribute
- [ ] Write test: del_attr/2 succeeds for missing attribute
- [ ] Write test: del_attr/2 trails deletion
- [ ] Write test: del_attr/2 with variable that derefs to bound term fails
- [ ] Write test: del_attr/2 with non-atom module fails
- [ ] Write test: del_attr/2 wrong arity fails
- [ ] Implement Engine._builtin_del_attr(self, args: tuple) -> bool
- [ ] Verify variable is unbound after deref (not bound term)
- [ ] Verify module is Atom
- [ ] Trail and delete attribute if present
- [ ] Clean up empty attr dicts
- [ ] Register in Engine._register_builtins with (del_attr, 2) key

### 4. Integration Tests for Builtins
- [ ] Write test: put_attr then get_attr retrieves value
- [ ] Write test: put_attr then del_attr then get_attr fails
- [ ] Write test: Multiple modules on same variable
- [ ] Write test: Backtrack restores attributes
- [ ] Write test: Attributes visible through aliases
- [ ] Write test: All three builtins recognized via Prolog queries
- [ ] Create test program using all three builtins
- [ ] Verify Prolog-level usage works correctly
- [ ] Verify builtins registered in Engine._register_builtins

## Phase 3: Hook Registry & Var-Nonvar Integration (4.2)

### 1. Implement Hook Registry
- [ ] Write test: Engine initializes with empty _attr_hooks
- [ ] Write test: register_attr_hook stores hook function
- [ ] Write test: register_attr_hook overwrites existing hook
- [ ] Write test: Hook signature (engine, varid, other) -> bool
- [ ] Add engine._attr_hooks: Dict[str, Callable] to Engine.__init__
- [ ] Implement Engine.register_attr_hook(module, hook)
- [ ] Verify hook storage and retrieval

### 2. Implement Hook Dispatch
- [ ] Write test: dispatch_attr_hooks returns True when no attrs
- [ ] Write test: dispatch_attr_hooks calls relevant hooks
- [ ] Write test: dispatch_attr_hooks returns False if hook fails
- [ ] Write test: All hooks must succeed for True result
- [ ] Write test: Hooks called in sorted module name order (deterministic)
- [ ] Implement Engine.dispatch_attr_hooks(varid, other_term)
- [ ] Dereference varid to get root variable
- [ ] Get attributes for root variable (fast path: skip if not in store.attrs)
- [ ] Sort module names for deterministic iteration
- [ ] Call each module's hook if registered
- [ ] Pass dereferenced root varid to hooks
- [ ] Short-circuit on first False

### 3. Integrate with TrailAdapter
- [ ] Write test: TrailAdapter carries engine reference
- [ ] Write test: TrailAdapter.engine accessible in unify
- [ ] Write test: TrailAdapter with None engine doesn't crash
- [ ] Modify TrailAdapter.__init__ to accept engine parameter
- [ ] Update Engine to pass self to TrailAdapter
- [ ] Verify engine available during unification

### 4. Hook Integration in Var-Nonvar Unification
- [ ] Write test: Hook called when unifying attributed var with nonvar
- [ ] Write test: Hook not called for vars without attributes (fast path)
- [ ] Write test: Hook rejection causes unification failure
- [ ] Write test: Hook success allows unification to proceed
- [ ] Write test: Multiple hooks all called
- [ ] Write test: Backtrack after hook failure restores state
- [ ] Locate var-nonvar binding in unify code (bind_root_to_term path)
- [ ] Check if varid in store.attrs before dispatching (fast path)
- [ ] Add hook dispatch before bind_root_to_term
- [ ] Verify hooks can veto unification
- [ ] Ensure proper cleanup on failure

### 5. Example Hook Implementation
- [ ] Write test: "must_be_even" hook rejects odd integers
- [ ] Write test: "must_be_even" hook accepts even integers
- [ ] Write test: "must_be_even" hook accepts non-integers
- [ ] Implement must_be_even_hook function
- [ ] Test with put_attr(X, must_be_even, true), X = N scenarios
- [ ] Verify constraint enforcement

## Phase 4: Var-Var Aliasing (4.3)

### 1. Attribute Visibility Through Aliases
- [ ] Write test: Attributes visible through alias after X = Y
- [ ] Write test: put_attr on X, unify X=Y, get_attr on Y works
- [ ] Write test: Attributes follow union-find root
- [ ] Write test: Only root carries attributes after aliasing
- [ ] Verify get_attrs follows deref correctly
- [ ] Test attribute access through alias chains

### 2. Implement Attribute Merging
- [ ] Write test: merge_attributes moves attrs to UF root
- [ ] Write test: Child's attributes cleared after merge
- [ ] Write test: Overlapping modules properly trailed (both deletion and overwrite)
- [ ] Write test: All attribute moves are trailed
- [ ] Implement merge_attributes(v1, v2, store, trail) function
- [ ] Determine which var becomes root BEFORE merge (union-by-rank logic)
- [ ] For each module in child's attrs:
  - [ ] Trail deletion from child: ('attr', child, module, old_value)
  - [ ] If module exists on root, trail overwrite: ('attr', root, module, old_root_value)
  - [ ] Move attribute from child to root
- [ ] Clear child's attr dict completely
- [ ] Ensure trail ordering allows correct restoration

### 3. Hook Dispatch for Var-Var Aliasing
- [ ] Write test: Hooks called for overlapping modules
- [ ] Write test: Hook sees both variables in aliasing
- [ ] Write test: Either hook can veto aliasing
- [ ] Write test: Non-overlapping modules don't interfere
- [ ] Add hook dispatch in var-var unification path
- [ ] Call hooks for all modules present on either var
- [ ] Pass both variables to overlapping module hooks
- [ ] Abort aliasing if any hook returns False

### 4. Integration in Var-Var Unification
- [ ] Write test: Full var-var unification with attributes
- [ ] Write test: Backtrack after failed aliasing restores separation
- [ ] Write test: Successful aliasing merges attributes correctly
- [ ] Locate var-var unification in unify code (union_vars path)
- [ ] Compute winner/child via union-by-rank BEFORE calling hooks
- [ ] Add hook dispatch before union (only if attrs present)
- [ ] Call merge_attributes BEFORE union_vars (preserve child identity for trailing)
- [ ] Perform normal union operation after successful merge
- [ ] Verify complete integration

### 5. Complex Aliasing Scenarios
- [ ] Write test: Chain of aliases X=Y, Y=Z with attrs
- [ ] Write test: Multiple modules across multiple variables
- [ ] Write test: Circular aliasing with attributes
- [ ] Write test: Backtrack through complex aliasing
- [ ] Create comprehensive aliasing test cases
- [ ] Verify attributes always on current root

## Phase 5: Testing & Documentation (4.4)

### 1. Property Tests
- [ ] Write property test: Attribute operations are confluent
- [ ] Write property test: Order of put_attr doesn't affect final state
- [ ] Write property test: Aliasing is associative with attributes
- [ ] Write property test: Backtrack always restores exact state
- [ ] Implement hypothesis strategies for attr operations
- [ ] Run property tests with many random scenarios
- [ ] Verify no surprising behaviors

### 2. Performance Benchmarks
- [ ] Write benchmark: Unification without attributes (baseline)
- [ ] Write benchmark: Unification with attrs but no hooks
- [ ] Write benchmark: Unification with active hooks
- [ ] Write benchmark: Dense attributes (90% vars have attrs)
- [ ] Write benchmark: Sparse attributes (10% vars have attrs)
- [ ] Measure overhead percentages
- [ ] Verify < 1% overhead when attrs not used
- [ ] Verify < 5% overhead with sparse attrs
- [ ] Verify < 10% overhead with dense attrs

### 3. Integration with Existing Tests
- [ ] Run full Stage 1 test suite with attr support
- [ ] Run full Stage 2 test suite with attr support
- [ ] Run full Stage 3 test suite with attr support
- [ ] Verify no regressions
- [ ] Check memory usage doesn't increase
- [ ] Ensure determinism maintained

### 4. Tracer Integration (Optional)
- [ ] Write test: Tracer emits attr_put events
- [ ] Write test: Tracer emits attr_get events
- [ ] Write test: Tracer emits attr_del events
- [ ] Write test: Tracer emits attr_hook events
- [ ] Write test: Internal events only when enabled
- [ ] Add trace_attr_event to PortsTracer
- [ ] Emit events in put_attr/get_attr/del_attr
- [ ] Emit events in hook dispatch
- [ ] Verify events in trace output

### 5. Documentation
- [ ] Write user guide for attributed variables
- [ ] Create example constraint modules
- [ ] Document hook protocol and expectations
- [ ] Add API reference for builtins
- [ ] Create troubleshooting guide
- [ ] Document performance characteristics
- [ ] Add migration guide from SWI-Prolog attrs

### 6. Example Applications
- [ ] Implement "must_be_positive" constraint
- [ ] Implement "must_be_different" constraint
- [ ] Implement simple type checking module
- [ ] Create demo showing multiple modules
- [ ] Write tutorial for creating new constraints
- [ ] Show integration with backtracking

## Phase 6: Validation & Polish

### 1. Error Handling
- [ ] Write test: Helpful error for put_attr on bound var
- [ ] Write test: Helpful error for wrong arity
- [ ] Write test: Helpful error for non-atom module
- [ ] Write test: Hook exceptions handled gracefully
- [ ] Improve error messages
- [ ] Add debug helpers for attributes
- [ ] Document common pitfalls

### 2. Edge Cases
- [ ] Write test: Massive number of attributes on one var
- [ ] Write test: Massive number of vars with attrs
- [ ] Write test: Very deep aliasing chains
- [ ] Write test: Circular references in attribute values
- [ ] Write test: Attributes containing variables
- [ ] Handle all edge cases gracefully
- [ ] Document any limitations

### 3. Final Integration
- [ ] Code review for clean integration
- [ ] Verify minimal changes to existing code
- [ ] Check no performance regressions
- [ ] Ensure clean module boundaries
- [ ] Update ARCH.md with Stage 4 details
- [ ] Update PLAN.md if needed

## Acceptance Checklist

### Functionality
- [ ] put_attr/3, get_attr/3, del_attr/2 work correctly
- [ ] Hooks intercept unification successfully
- [ ] Attributes preserved through aliasing
- [ ] Complete backtracking restoration
- [ ] Multiple modules coexist peacefully
- [ ] All tests pass

### Performance
- [ ] < 1% overhead when no attributes used
- [ ] < 5% overhead with sparse attributes (10% vars)
- [ ] < 10% overhead with dense attributes (90% vars)
- [ ] Hook dispatch O(modules) not O(variables)
- [ ] No memory leaks

### Quality
- [ ] 100% trail coverage for attribute operations
- [ ] Property tests pass
- [ ] Integration with existing stages clean
- [ ] Documentation complete
- [ ] Examples working

### Integration
- [ ] Clean integration with unify
- [ ] Minimal changes to existing code
- [ ] Tracer support (optional)
- [ ] No breaking changes

## Key File References

### Existing Infrastructure
- Runtime Trail with attr support: `prolog/engine/runtime.py:260`
- TrailAdapter with engine/store: `prolog/engine/trail_adapter.py:11`
- Unify core helpers: `prolog/unify/unify.py:21`
- Unify trail undo (needs attr case): `prolog/unify/trail.py:58`
- Engine builtins registration: `prolog/engine/engine.py:280`
- Store implementation: `prolog/unify/store.py`

### Files to Modify
- Add attr case to: `prolog/unify/trail.py:undo_to`
- Add attr storage to: `prolog/unify/store.py:Store`
- Add builtins to: `prolog/engine/engine.py`
- Add hook dispatch to: `prolog/unify/unify.py`

### Files to Create
- Attribute tests: `prolog/tests/unit/test_attributed_vars.py`
- Hook tests: `prolog/tests/unit/test_attr_hooks.py`
- Integration tests: `prolog/tests/scenarios/test_attr_scenarios.py`