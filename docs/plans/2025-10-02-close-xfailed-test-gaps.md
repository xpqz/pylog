# Close XFailed Test Gaps Implementation Plan

## Overview

PyLog has achieved remarkable implementation completeness with 6,861 passing tests, but 21 xfailed tests and 12 skipped tests represent important functionality gaps that prevent claiming 100% feature completeness. This plan addresses these gaps systematically to achieve true implementation completeness.

## Current State Analysis

### Test Status Summary
- **6,861 tests passing** ✅ (99.5% success rate)
- **21 xfailed tests** ❌ (expected failures - functionality gaps)
- **12 skipped tests** ⚠️ (conditional/unimplemented features)
- **1 performance test failure** (indexing regression)

### Key Discoveries

#### 1. CLP(FD) Reification Extensions
**Gap**: Element/3 and all_different/1 constraints lack reification support
**Impact**: Complex constraint modeling scenarios fail
**Files**: `prolog/clpfd/props/element.py`, `prolog/clpfd/props/alldiff.py`
**Missing**: Entailment detection functions and reification integration

#### 2. Exception Handling Edge Cases
**Gap**: Cut barrier semantics, state restoration, and choice point interactions incomplete
**Impact**: Advanced Prolog catch/throw scenarios don't work correctly
**Files**: `prolog/engine/engine.py` exception handling code
**Missing**: Proper trail granularity and scope management

#### 3. Debug/Trace Integration
**Gap**: Filter system exists but not connected to tracer infrastructure
**Impact**: Advanced debugging features unavailable
**Files**: `prolog/debug/tracer.py`, `prolog/debug/filters.py`, `prolog/repl.py`
**Missing**: Integration bridge between components

#### 4. Advanced Operators & ISO Mode
**Gap**: Structural comparison operators and ISO error handling
**Impact**: Standards compliance limitations
**Files**: `prolog/engine/engine.py` builtin registration
**Missing**: Runtime implementations and error taxonomy

## Desired End State

**Complete PyLog implementation with:**
- ✅ All 33 test gaps resolved (21 xfailed + 12 skipped)
- ✅ Full CLP(FD) reification support for all constraints
- ✅ Robust exception handling covering all edge cases
- ✅ Complete debug/trace system with filtering
- ✅ ISO-compliant operator support and error handling
- ✅ 100% test pass rate (6,894+ tests passing)

### Key Verification Points:
- No more xfailed or skipped tests (except truly optional features)
- Complex constraint reification scenarios work correctly
- Exception handling passes all ISO compliance tests
- Full REPL debugging features functional
- Performance benchmarks maintained

## What We're NOT Doing

- Advanced CLP(FD) algorithms (edge-finding, flow-based GCC)
- Complete ISO Prolog compliance (focus on current test scope)
- Performance optimization beyond maintaining current benchmarks
- New constraint types beyond fixing existing reification gaps
- REPL UI/UX improvements beyond functional completeness

## Implementation Approach

**Systematic gap closure approach:**
1. **Build on existing infrastructure** - PyLog has excellent foundations
2. **Incremental integration** - Connect existing components rather than rewrite
3. **Test-driven fixes** - Use existing xfailed tests as success criteria
4. **Preserve architecture** - Maintain stable interfaces and patterns
5. **Comprehensive validation** - Ensure no regressions in 6,861 passing tests

## Phase 1: Debug/Trace Integration (Highest Impact)

### Overview
Connect the existing TraceFilters system to PortsTracer and REPL, enabling full debugging capabilities. This provides immediate value and helps debug other issues.

### Changes Required:

#### 1. PortsTracer Filter Integration
**File**: `prolog/debug/tracer.py`
**Changes**: Add filter storage and application methods

```python
# Add imports at top of file
from typing import Optional, Callable
from prolog.debug.filters import TraceFilters

# Add to PortsTracer class around line 92
def __init__(self, ...):
    # existing code...
    self._filters: Optional[TraceFilters] = None

def set_filters(self, filters: TraceFilters) -> None:
    """Set TraceFilters instance for event filtering."""
    self._filters = filters

def set_filter(self, filter_func: Callable) -> None:
    """Set simple filter function (REPL compatibility)."""
    # Create simple TraceFilters wrapper for REPL
    simple_filters = TraceFilters()
    simple_filters._custom_filter = filter_func
    self._filters = simple_filters

def _should_emit(self, event) -> bool:
    """Apply filters to determine if event should be emitted."""
    if self._filters is None:
        return True
    return self._filters.should_emit_event(event)
```

#### 2. Engine Filter Configuration
**File**: `prolog/engine/engine.py`
**Changes**: Add method to configure tracer filters without changing init signature

```python
# Add imports at top of file
from typing import Optional
from prolog.debug.filters import TraceFilters

# Add method to Engine class (keep __init__ signature unchanged)
def set_trace_filters(self, filters: Optional[TraceFilters]) -> None:
    """Configure trace filters for the engine's tracer."""
    if self.tracer and filters:
        self.tracer.set_filters(filters)
```

#### 3. REPL Integration Bridge
**File**: `prolog/repl.py`
**Changes**: Create TraceFilters from REPL state and connect to engine

```python
# Add imports at top of file
from prolog.debug.filters import TraceFilters

# Add method around line 690
def _create_trace_filters(self):
    """Create TraceFilters instance from current REPL settings."""
    filters = TraceFilters()

    # Configure spypoints
    if self.spypoints:
        for pred_id in self.spypoints:
            filters.add_spypoint(pred_id)

    # Configure sampling if enabled
    if hasattr(self, 'trace_sampling'):
        filters.sampling = self.trace_sampling

    return filters

# Modify _recreate_engine around line 692
def _recreate_engine(self):
    # Create engine with existing signature
    self.engine = Engine(
        self.program,
        trace=self.trace_enabled,
        debug=self.debug_enabled
    )

    # Configure trace filters after engine creation
    if self.trace_enabled:
        trace_filters = self._create_trace_filters()
        self.engine.set_trace_filters(trace_filters)
```

#### 4. Bindings Collection Enhancement
**File**: `prolog/engine/engine.py`
**Changes**: Collect variable bindings in trace events

```python
# Modify _trace_port around line 282 (keep existing signature)
def _trace_port(self, port, goal, depth_override=None):
    if self.tracer:
        # Collect bindings if trace filters require them
        bindings = None
        if (self.tracer._filters and
            self.tracer._filters.bindings_policy != 'none'):
            bindings = self._collect_current_bindings(goal)

        # Use existing emit_event API with bindings in data parameter
        data = {}
        if bindings:
            data['bindings'] = bindings
        if depth_override is not None:
            data['depth_override'] = depth_override

        event = self.tracer.emit_event(port, goal, data if data else None)
```

### Success Criteria:

#### Automated Verification:
- [ ] All tests pass: `uv run pytest prolog/tests/unit/test_repl_debug_integration.py`
- [ ] Filter tests pass: `uv run pytest prolog/tests/unit/test_filters.py -k "not xfail"`
- [ ] No regressions: `uv run pytest prolog/tests/ --tb=short`

#### Manual Verification:
- [ ] REPL `trace on/off` commands work correctly
- [ ] REPL `spy/unspy` commands affect trace output
- [ ] JSON trace output includes proper filtering
- [ ] Trace filters work with complex recursive predicates
- [ ] Bindings collection respects filter policies

---

## Phase 2: CLP(FD) Reification Extensions

### Overview
Implement reification support for element/3 and all_different/1 constraints by adding entailment detection and integration with existing reification framework.

### Changes Required:

#### 1. Element Constraint Entailment
**File**: `prolog/clpfd/entailment.py`
**Changes**: Add element entailment detection function

```python
def check_element_entailment(store, index_var, list_vars, value_var, list_values, ground_value=None):
    """Check entailment of element(Index, List, Value) constraint.

    Args:
        store: Variable store
        index_var: Variable ID for index (None if ground)
        list_vars: List of variable IDs for list elements (None for ground elements)
        value_var: Variable ID for value (None if ground)
        list_values: List of ground values (None for variables)
        ground_value: Ground value if value_var is None

    Returns:
        Entailment.TRUE if constraint is definitely satisfied
        Entailment.FALSE if constraint is definitely violated
        Entailment.UNKNOWN if uncertain
    """
    # Get domains
    index_domain = get_domain(store, index_var) if index_var else None
    value_domain = get_domain(store, value_var) if value_var else None

    # Case 1: Index is singleton - check if List[index] matches Value
    if index_domain and index_domain.is_singleton():
        idx = index_domain.min()
        if 1 <= idx <= len(list_vars):
            list_idx = idx - 1
            if list_vars[list_idx] is None:
                # Ground list element
                list_val = list_values[list_idx]
                if value_var is None:
                    # Both ground - direct comparison
                    return Entailment.TRUE if list_val == ground_value else Entailment.FALSE
                else:
                    # Value is variable - check domain
                    return (Entailment.TRUE if value_domain.contains(list_val)
                           else Entailment.FALSE if value_domain.is_singleton()
                           else Entailment.UNKNOWN)
        else:
            return Entailment.FALSE  # Index out of bounds

    # Case 2: Value is singleton or ground - check feasible positions
    target_value = None
    if value_var is None:
        target_value = ground_value
    elif value_domain and value_domain.is_singleton():
        target_value = value_domain.min()

    if target_value is not None:
        feasible_positions = []

        for i, var_id in enumerate(list_vars):
            if index_domain and not index_domain.contains(i + 1):
                continue  # Index position not possible

            if var_id is None:
                # Ground element
                if list_values[i] == target_value:
                    feasible_positions.append(i + 1)
            else:
                # Variable element - check domain
                elem_domain = get_domain(store, var_id)
                if elem_domain and elem_domain.contains(target_value):
                    feasible_positions.append(i + 1)

        if not feasible_positions:
            return Entailment.FALSE
        elif (index_domain and
              len(feasible_positions) == 1 and
              index_domain.is_singleton() and
              index_domain.contains(feasible_positions[0])):
            return Entailment.TRUE

    return Entailment.UNKNOWN
```

#### 2. All-Different Entailment (Simplified)
**File**: `prolog/clpfd/entailment.py`
**Changes**: Add basic all_different entailment detection

```python
def check_all_different_entailment(store, var_ids):
    """Check entailment of all_different(Vars) constraint.

    Note: This is a simplified version focusing on obvious cases.
    Complex Hall interval analysis left for future enhancement.
    """
    # Collect domains for all variables
    domains = []
    for var_id in var_ids:
        if var_id is None:
            return Entailment.UNKNOWN  # Ground values not supported in Phase 2
        domain = get_domain(store, var_id)
        if domain is None or domain.is_empty():
            return Entailment.FALSE
        domains.append(domain)

    # Check for obvious violations (duplicate singletons)
    singletons = [d.min() for d in domains if d.is_singleton()]
    if len(singletons) != len(set(singletons)):
        return Entailment.FALSE

    # Check for obvious entailment (all singleton with distinct values)
    if all(d.is_singleton() for d in domains):
        return Entailment.TRUE

    # Check impossible cases (too few values for all variables)
    all_values = set()
    for domain in domains:
        all_values.update(domain.iter_values())

    if len(all_values) < len(domains):
        return Entailment.FALSE

    # Conservative: return UNKNOWN for complex cases
    # Future enhancement: implement full Hall interval analysis
    return Entailment.UNKNOWN
```

#### 3. Reification Integration (Positive Constraints Only)
**File**: `prolog/clpfd/props/element.py` and `prolog/clpfd/props/alldiff.py`
**Changes**: Add reification support for positive constraints

```python
# Add imports at top of element.py
from prolog.clpfd.props.reif import create_reification_propagator
from prolog.clpfd.entailment import check_element_entailment

def create_element_reification_propagator(bool_var, index_var, list_vars, value_var, list_values, ground_value=None):
    """Create reification propagator for element constraint (positive only)."""

    def element_entailment_check(store):
        return check_element_entailment(store, index_var, list_vars, value_var, list_values, ground_value)

    def post_element(store, trail, engine):
        # Post positive element constraint
        prop = create_element_propagator(index_var, list_vars, value_var, list_values)
        # Register and run propagator via existing infrastructure
        return engine.post_constraint(prop, [index_var] + [v for v in list_vars if v] + ([value_var] if value_var else []))

    def post_element_negation(store, trail, engine):
        # Phase 2 limitation: negative element constraints not implemented
        # This could be added in a future phase via disjunctive decomposition
        raise NotImplementedError("Negative element reification not yet supported")

    return create_reification_propagator(
        bool_var, element_entailment_check, post_element, post_element_negation
    )
```

#### 4. Builtin Integration
**File**: `prolog/engine/builtins_clpfd.py`
**Changes**: Add reification variants of element and all_different

```python
def _builtin_element_reif(engine, bool_term, index_term, list_term, value_term):
    """Reified element constraint: Bool #<==> element(Index, List, Value)."""
    # Validate and extract parameters (similar to existing element builtin)
    # Create reification propagator instead of direct propagator
    prop = create_element_reification_propagator(bool_var, index_var, list_vars, value_var, list_values)
    # Register and run propagator...

def _builtin_all_different_reif(engine, bool_term, vars_term):
    """Reified all_different constraint: Bool #<==> all_different(Vars)."""
    # Similar pattern for all_different reification
    pass
```

### Success Criteria:

#### Automated Verification:
- [ ] Element reification tests pass: `uv run pytest prolog/tests/unit/test_element.py -k "reification"`
- [ ] All-different reification tests pass: `uv run pytest prolog/tests/unit/test_clpfd_all_different.py -k "reification"`
- [ ] Reification scenarios pass: `uv run pytest prolog/tests/scenarios/test_clpfd_reification_scenarios.py`
- [ ] No regressions in CLP(FD) tests: `uv run pytest prolog/tests/unit/test_clpfd*.py`

#### Manual Verification:
- [ ] Basic element/all_different reification works (positive cases only)
- [ ] Reification propagation is efficient (no performance degradation)
- [ ] Boolean variables properly constrained in all cases
- [ ] Backtracking correctly restores reification state

**Phase 2 Limitations:**
- Negative constraint reification (¬element, ¬all_different) deferred to future phases
- Complex Hall interval analysis for all_different simplified to basic cases
- Ground value support in constraints may be limited

---

## Phase 3: Exception Handling Refinement

### Overview
Fix edge cases in catch/throw implementation using targeted fixes within the existing engine framework. Focus on cut barrier semantics and state restoration without modifying core trail structures.

### Changes Required:

#### 1. Cut Barrier Enforcement (Targeted Fix)
**File**: `prolog/engine/engine.py`
**Changes**: Improve cut barrier detection and enforcement in existing exception handling

```python
# Enhance _handle_throw around line 711 to respect catch barriers
def _handle_throw(self, exception):
    """Enhanced throw handling with proper cut barrier respect."""
    # Find matching catch choicepoint, respecting cut barriers
    catch_cp = None
    for i in range(len(self.choicepoints) - 1, -1, -1):
        cp = self.choicepoints[i]
        if cp.kind == ChoicepointKind.CATCH:
            # Try to match the exception using existing trial unification
            if self._match_exception_safely(cp, exception):
                catch_cp = cp
                catch_height = i
                break

    if catch_cp:
        # Use existing state restoration with enhanced validation
        self._restore_to_catch_safely(catch_cp, catch_height, exception)
        return True

    return False

def _match_exception_safely(self, catch_cp, exception):
    """Safely match exception using existing trial unification."""
    # Use existing _match_only infrastructure
    # but with additional validation for complex structures
    try:
        return self._match_only(catch_cp.catcher, exception.ball)
    except Exception:
        return False

def _restore_to_catch_safely(self, catch_cp, catch_height, exception):
    """Restore to catch baseline with additional safety checks."""
    # Use existing restoration logic but with validation
    original_trail_len = len(self.trail.entries)

    # Existing restoration code
    self.trail.unwind_to(catch_cp.trail_top)
    self.choicepoints = self.choicepoints[:catch_height + 1]
    self.goal_stack = self.goal_stack[:catch_cp.goal_height]
    self.frames = self.frames[:catch_cp.frame_height]

    # Additional validation for complex unification chains
    self._validate_restoration_consistency()

    # Re-unify with enhanced error handling
    try:
        self._reunify_exception_safely(catch_cp, exception)
    except Exception:
        # If re-unification fails, ensure we're still in valid state
        self._ensure_minimal_valid_state()
```

#### 2. Streaming Cursor State Management (Conservative)
**File**: `prolog/engine/engine.py`
**Changes**: Improve streaming cursor cleanup during exceptions

```python
# Enhance exception handling to check for streaming cursor attributes safely
def _cleanup_streaming_state_safely(self, cp_height):
    """Clean up streaming cursors when unwinding to catch (conservative approach)."""
    for j in range(len(self.choicepoints) - 1, cp_height, -1):
        cp = self.choicepoints[j]
        # Only clean up if the CP actually has streaming cursor data
        if hasattr(cp, 'payload') and cp.payload and 'streaming_cursor' in cp.payload:
            try:
                cp.payload['streaming_cursor'].close()
            except (AttributeError, KeyError):
                pass  # Ignore cleanup failures

def _validate_restoration_consistency(self):
    """Validate engine state consistency after catch restoration."""
    # Basic consistency checks without deep structural changes
    # Ensure choicepoint stack is valid
    if self.choicepoints:
        for i, cp in enumerate(self.choicepoints):
            if cp.goal_height > len(self.goal_stack):
                # Repair inconsistency conservatively
                cp.goal_height = len(self.goal_stack)

    # Ensure frame stack is valid
    if self.frames:
        for frame in self.frames:
            if hasattr(frame, 'goal_height'):
                frame.goal_height = min(frame.goal_height, len(self.goal_stack))
```

#### 3. Streaming Cursor State Management
**File**: `prolog/engine/engine.py`
**Changes**: Improve streaming cursor handling during exceptions

```python
# Enhance exception handling around line 745
def _handle_throw(self, exception):
    """Enhanced throw handling with proper state cleanup."""
    # Find matching catch choicepoint
    for i in range(len(self.choicepoints) - 1, -1, -1):
        cp = self.choicepoints[i]
        if cp.kind == ChoicepointKind.CATCH:
            # Try to match the exception
            if self._match_exception(cp, exception):
                # Clean up streaming cursors properly
                self._cleanup_streaming_state(i)

                # Restore state to catch baseline
                self._restore_to_catch_baseline(cp, exception)
                return True

    return False

def _cleanup_streaming_state(self, cp_height):
    """Clean up streaming cursors when unwinding to catch."""
    # Properly close any streaming cursors created after this catch point
    for j in range(len(self.choicepoints) - 1, cp_height, -1):
        cp = self.choicepoints[j]
        if hasattr(cp, 'streaming_cursor'):
            cp.streaming_cursor.close()
```

#### 4. State Restoration Improvements
**File**: `prolog/engine/engine.py`
**Changes**: Enhance state restoration after catch

```python
# Improve state restoration around line 750
def _restore_to_catch_baseline(self, catch_cp, exception):
    """Restore engine state to catch baseline with enhanced trail handling."""
    # Restore trail with detailed unwinding
    self.trail.unwind_to_with_validation(catch_cp.trail_top)

    # Restore other state components
    self.choicepoints = self.choicepoints[:catch_cp.cp_height + 1]
    self.goal_stack = self.goal_stack[:catch_cp.goal_height]
    self.frames = self.frames[:catch_cp.frame_height]

    # Validate state consistency after restoration
    self._validate_post_catch_state()

    # Re-unify exception with catcher for visible bindings
    self._reunify_exception(catch_cp, exception)

def _validate_post_catch_state(self):
    """Validate engine state consistency after catch restoration."""
    # Check that all variable references are still valid
    # Ensure no dangling references in complex structures
    # Validate choicepoint stack consistency
    pass
```

### Success Criteria:

#### Automated Verification:
- [ ] Exception tests pass: `uv run pytest prolog/tests/unit/test_catch_throw_comprehensive.py`
- [ ] Exception handling tests pass: `uv run pytest prolog/tests/unit/test_exception_handling.py`
- [ ] No regressions in core tests: `uv run pytest prolog/tests/unit/test_catch.py`

#### Manual Verification:
- [ ] Complex list unification properly restored after catch
- [ ] Cut barriers prevent cuts from escaping catch scope
- [ ] Multiple catch alternatives at choice points work correctly
- [ ] Recursive predicates with nested catch/throw handle correctly
- [ ] Streaming cursors cleaned up properly during exception unwinding

---

## Phase 4: Operators & ISO Mode

### Overview
Implement missing runtime support for structural comparison operators and basic ISO mode error handling to achieve standards compliance.

### Changes Required:

#### 1. Structural Comparison Operators
**File**: `prolog/engine/builtins_iso.py`
**Changes**: Add runtime implementations for ==, \==, @<, @>, @=<, @>=

```python
def _builtin_structural_equal(engine, term1, term2):
    """Structural equality: Term1 == Term2."""
    # Structural comparison without unification
    result = _structural_compare(term1, term2) == 0
    return result

def _builtin_structural_not_equal(engine, term1, term2):
    """Structural inequality: Term1 \\== Term2."""
    result = _structural_compare(term1, term2) != 0
    return result

def _builtin_term_less(engine, term1, term2):
    """Term ordering: Term1 @< Term2."""
    result = _structural_compare(term1, term2) < 0
    return result

def _structural_compare(term1, term2):
    """Standard term ordering comparison.

    Returns: -1 if term1 < term2, 0 if equal, 1 if term1 > term2
    Standard order: variables < numbers < atoms < compound terms
    """
    # Implement ISO standard term ordering
    type1 = _term_order_type(term1)
    type2 = _term_order_type(term2)

    if type1 != type2:
        return type1 - type2

    # Same type - compare within type
    if isinstance(term1, Var) and isinstance(term2, Var):
        return term1.id - term2.id
    elif isinstance(term1, Int) and isinstance(term2, Int):
        return term1.value - term2.value
    elif isinstance(term1, Atom) and isinstance(term2, Atom):
        return -1 if term1.name < term2.name else (1 if term1.name > term2.name else 0)
    elif isinstance(term1, Struct) and isinstance(term2, Struct):
        # Compare arity first, then functor, then args
        if term1.arity != term2.arity:
            return term1.arity - term2.arity
        if term1.functor != term2.functor:
            return -1 if term1.functor < term2.functor else 1
        # Compare arguments recursively
        for a1, a2 in zip(term1.args, term2.args):
            cmp = _structural_compare(a1, a2)
            if cmp != 0:
                return cmp
        return 0

    return 0

def _term_order_type(term):
    """Get term ordering type value."""
    if isinstance(term, Var):
        return 0
    elif isinstance(term, (Int, float)):
        return 1
    elif isinstance(term, Atom):
        return 2
    elif isinstance(term, (Struct, List)):
        return 3
    else:
        return 4
```

#### 2. Power Operator Implementation
**File**: `prolog/engine/builtins_iso.py`
**Changes**: Add exponentiation operator support

```python
def _builtin_power(engine, result_term, expr_term):
    """Power operator: Result is Base ** Exponent."""
    try:
        base_val, exp_val = _evaluate_arithmetic_binary(expr_term, "**")
        result = base_val ** exp_val
        return _unify_arithmetic_result(engine, result_term, result)
    except (TypeError, ValueError, OverflowError) as e:
        # In ISO mode, should throw evaluation_error
        return False

def _evaluate_power(base, exponent):
    """Evaluate exponentiation with proper type handling."""
    if isinstance(base, int) and isinstance(exponent, int):
        if exponent >= 0:
            return base ** exponent
        else:
            # Negative exponent - return float result
            return float(base) ** exponent
    else:
        return float(base) ** float(exponent)
```

#### 3. ISO Mode Framework
**File**: `prolog/engine/engine.py`
**Changes**: Add ISO mode parameter and error handling

```python
# Add to Engine.__init__
def __init__(self, program, trace=False, debug=False, iso_mode=False):
    # existing code...
    self.iso_mode = iso_mode
    self._setup_error_handling()

def _setup_error_handling(self):
    """Configure error handling based on mode."""
    if self.iso_mode:
        self.error_handler = ISOErrorHandler()
    else:
        self.error_handler = DevModeErrorHandler()

class ISOErrorHandler:
    """ISO-compliant error handling."""

    def handle_undefined_predicate(self, functor, arity):
        """Throw existence_error for undefined predicates."""
        error_term = Struct("existence_error",
                           (Atom("procedure"),
                            Struct("/", (Atom(functor), Int(arity)))))
        raise PrologThrow(error_term)

    def handle_type_error(self, expected_type, actual_term):
        """Throw type_error for type mismatches."""
        error_term = Struct("type_error", (Atom(expected_type), actual_term))
        raise PrologThrow(error_term)

    def handle_instantiation_error(self, term):
        """Throw instantiation_error for unbound variables."""
        error_term = Struct("instantiation_error", ())
        raise PrologThrow(error_term)

class DevModeErrorHandler:
    """Development-friendly error handling."""

    def handle_undefined_predicate(self, functor, arity):
        """Log warning and fail gracefully."""
        print(f"Warning: undefined predicate {functor}/{arity}")
        return False

    def handle_type_error(self, expected_type, actual_term):
        """Log helpful error and fail."""
        print(f"Type error: expected {expected_type}, got {actual_term}")
        return False
```

#### 4. Builtin Registration
**File**: `prolog/engine/engine.py`
**Changes**: Register new operators in builtin table

```python
# Add to _register_builtins around line 400
self._builtins[("==", 2)] = lambda eng, args: _builtin_structural_equal(eng, *args)
self._builtins[("\\==", 2)] = lambda eng, args: _builtin_structural_not_equal(eng, *args)
self._builtins[("@<", 2)] = lambda eng, args: _builtin_term_less(eng, *args)
self._builtins[("@>", 2)] = lambda eng, args: _builtin_term_greater(eng, *args)
self._builtins[("@=<", 2)] = lambda eng, args: _builtin_term_less_equal(eng, *args)
self._builtins[("@>=", 2)] = lambda eng, args: _builtin_term_greater_equal(eng, *args)
self._builtins[("**", 2)] = lambda eng, args: _builtin_power(eng, *args)
```

### Success Criteria:

#### Automated Verification:
- [ ] Operator tests pass: `uv run pytest prolog/tests/unit/test_operator_integration.py`
- [ ] ISO mode tests pass: `uv run pytest prolog/tests/unit/test_engine_loop.py -k "iso_mode"`
- [ ] No regressions in arithmetic: `uv run pytest prolog/tests/unit/test_arithmetic*.py`

#### Manual Verification:
- [ ] Structural comparison works correctly (@<, @>, ==, \==)
- [ ] Power operator handles all numeric types properly
- [ ] ISO mode throws proper error terms for violations
- [ ] Dev mode maintains user-friendly error messages
- [ ] Term ordering follows ISO standard specifications

---

## Testing Strategy

### Unit Tests Approach
- **Convert xfailed to passing**: Remove `@pytest.mark.xfail` decorators as functionality is implemented
- **Add regression protection**: Ensure new functionality doesn't break existing tests
- **Edge case coverage**: Focus on the specific scenarios that currently fail

### Integration Tests
- **End-to-end debugging workflows**: REPL trace/spy commands → engine → output
- **Complex constraint scenarios**: Reified constraints in realistic problems
- **Exception handling chains**: Nested catch/throw with complex state
- **Mixed operator expressions**: Complex expressions using new operators

### Manual Testing Steps
1. **REPL Debugging Session**: Enable trace, set spypoints, verify filtered output
2. **Complex CLP(FD) Model**: Build constraint model using reified element/all_different
3. **Exception Handling Stress Test**: Deeply nested catch/throw with complex unifications
4. **Operator Expression Evaluation**: Test structural comparison and power operators
5. **ISO Compliance Check**: Verify error handling matches ISO standards

### Performance Considerations

**No Performance Degradation**: All changes must maintain current benchmark performance
- Trace filtering should have minimal overhead when disabled
- Reification should only add cost when actually used
- Exception handling improvements should not slow normal execution
- New operators should match existing arithmetic performance

**Benchmark Validation**:
- Run full benchmark suite after each phase
- Monitor memory usage in constraint solving
- Verify trace overhead stays under 5% when disabled

## Migration Notes

**Backward Compatibility**: All changes maintain existing API compatibility
- Existing code continues to work unchanged
- New features are opt-in (ISO mode, advanced filtering)
- Default behavior remains development-friendly

**Configuration Changes**:
- Engine accepts new optional parameters (`iso_mode`, `trace_filters`)
- REPL gains new capabilities but existing commands unchanged
- Test suite grows but existing test behavior preserved

## References

- **Original Analysis**: This plan based on comprehensive analysis of 33 failing tests
- **Existing Architecture**: Builds on PyLog's excellent existing infrastructure
- **ISO Prolog Standard**: ISO/IEC 13211-1:1995 for compliance requirements
- **SWI-Prolog Reference**: For behavioral compatibility verification
- **PyLog Documentation**: Stage implementation plans and architecture docs

---

## Success Metrics

**Primary Goal**: Achieve 100% test pass rate (0 xfailed, 0 skipped tests)
**Secondary Goals**:
- Maintain all 6,861 existing passing tests
- No performance regression in benchmarks
- Complete CLP(FD) reification support
- Full-featured debugging system
- ISO-compliant operator support

**Completion Criteria**: All phases implemented, all tests passing, comprehensive manual validation completed.

---

## Appendix: Test Inventory

### XFailed Tests by Phase

**Phase 1: Debug/Trace Integration (6 tests)**
- `prolog/tests/unit/test_repl_debug_integration.py:24` - "Requires full trace implementation"
- `prolog/tests/unit/test_repl_debug_integration.py:46` - "Requires full trace implementation"
- `prolog/tests/unit/test_repl_debug_integration.py:68` - "Requires full trace implementation"
- `prolog/tests/unit/test_repl_debug_integration.py:101` - "Requires full trace implementation"
- `prolog/tests/unit/test_repl_debug_integration.py:164` - "Requires full spy implementation"
- `prolog/tests/unit/test_filters.py:505` - "PortsTracer not yet filter-aware; enable when integrated"

**Phase 2: CLP(FD) Reification (4 tests)**
- `prolog/tests/unit/test_element.py:551` - "Reification support not yet implemented"
- `prolog/tests/unit/test_element.py:568` - "Reification support not yet implemented"
- `prolog/tests/unit/test_clpfd_all_different.py:299` - "Requires hook integration for all_different constraint"
- `prolog/tests/unit/test_clpfd_all_different.py:406` - "Requires hook integration for all_different constraint"

**Phase 3: Exception Handling (7 tests)**
- `prolog/tests/unit/test_catch_throw_comprehensive.py:160` - "List unification restoration still has issues"
- `prolog/tests/unit/test_catch_throw_comprehensive.py:416` - "Cut barrier semantics not fully implemented"
- `prolog/tests/unit/test_catch_throw_comprehensive.py:511` - "Catch at choice points not fully working"
- `prolog/tests/unit/test_catch_throw_comprehensive.py:632` - "Recursive predicates with catch have edge cases"
- `prolog/tests/unit/test_exception_handling.py:50` - "Catch with streaming cursors not fully implemented"
- `prolog/tests/unit/test_exception_handling.py:87` - "Catch unification failure case not fully handled"
- `prolog/tests/unit/test_exception_handling.py:134` - "Catch with cut interaction has edge cases"

**Phase 4: Operators & ISO Mode (4 tests)**
- `prolog/tests/unit/test_operator_integration.py:179` - "Structural comparison operators (@<, @>, ==, \\==, \\=) not in Stage 1.5 scope"
- `prolog/tests/unit/test_operator_integration.py:364` - "** (power) operator not implemented in Stage 1.5 engine"
- `prolog/tests/unit/test_engine_loop.py:953` - "ISO mode not yet implemented"
- `prolog/tests/unit/test_metrics.py:521` - "throw/catch integration not yet wired in engine"

### Conditional Tests (Skipped)

**Context-Dependent (12 tests)**
- `prolog/tests/unit/test_operator_integration.py:325` - "Canonical ';' form not in Stage 1.5 engine scope"
- `prolog/tests/unit/test_operator_integration.py:382` - "Test uses term ordering operator (@=<) not in Stage 1.5 engine scope"
- `prolog/tests/unit/test_clpfd_disjunction.py:40,100,124` - "Disjunction operators temporarily disabled"
- `prolog/tests/scenarios/test_clpfd_reification_scenarios.py:15` - "Requires #\\/ (disjunction) operator not yet implemented"
- Various performance/conditional skips based on environment

### Total Impact
- **21 xfailed tests** → **0 xfailed tests** (100% reduction)
- **12 skipped tests** → **~8 skipped tests** (conditional/environment-dependent remain)
- **Target**: 6,861 + 25 = **6,886+ passing tests**

### Test Coverage by Category
1. **Reification**: 4 tests covering element/all_different constraint reification
2. **Exception Handling**: 7 tests covering complex catch/throw edge cases
3. **Debug Integration**: 6 tests covering REPL spy/trace functionality
4. **Operators**: 4 tests covering structural comparison and power operators
5. **Scenarios**: 2 tests covering complex constraint combinations

This systematic approach ensures no test gap is missed and provides clear success metrics for each phase.