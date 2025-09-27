# Stage 5.5: CLP(FD) Reification Implementation Plan

## Overview

Implement reification support for CLP(FD) constraints, enabling Boolean variables to represent the truth values of constraints. This allows programs to explicitly reason about constraint satisfaction through B #<==> C (equivalence), B #==> C (implication), and B #<== C (converse implication) operators.

**Initial Scope**: This phase supports reification of binary relational constraints where each side is a variable or integer constant. Reification of general linear expressions and global constraints is deferred to future phases.

## Current State Analysis

PyLog has completed Stage 5 (CLP(FD) Core) with:
- Domain representation with intervals and revision tracking (`prolog/clpfd/domain.py`)
- PropagationQueue with priority levels (`prolog/clpfd/queue.py`)
- Basic propagators: equality, comparisons, linear sums (`prolog/clpfd/props/`)
- Attributed variable integration via Stage 4 hooks
- Labeling strategies for search

**What's missing**:
- Boolean domain specialization (0/1 domains)
- Entailment detection for constraints
- Reification propagators and operators
- Self-notification loop prevention for reified constraints

### Key Discoveries:
- Current propagators use factory pattern: `create_*_propagator()` functions
- Propagation uses 3-level priority system (HIGH/MED/LOW)
- Domain changes tracked via revision counters for efficiency
- Trail-based backtracking already handles domain restoration

## Desired End State

A complete reification system that:
- Represents constraint truth values as Boolean CLP(FD) variables (0/1 domains)
- Implements B #<==> C, B #==> C, B #<== C operators
- Provides entailment detection (constraint definitely true/false/unknown)
- Supports directional posting: B=1 posts C, B=0 posts ¬C
- Prevents self-notification loops in reified constraint networks
- Integrates seamlessly with existing constraints

### Verification:
- `B #<==> (X #= 3), X in 1..5, B=0` correctly prunes X to {1,2,4,5}
- `X in 5..9, Y in 1..4, B #<==> (X #> Y)` detects entailment: B=1
- Complex reified constraint networks propagate without loops
- All existing CLP(FD) tests continue to pass

## What We're NOT Doing

- No modification to existing propagator internals (they remain as-is)
- No general Boolean satisfiability solver (just constraint reification)
- No reification of global constraints (all_different, etc.) in this phase
- No reification of general linear expressions (defer to later phase)
- No optimization of reified constraint networks
- No symbolic Boolean expressions (B1 and B2, B1 or B2, etc.)

## Implementation Approach

Build reification as a layer on top of existing CLP(FD) infrastructure:
- Use standard 0/1 domains for Boolean variables
- Implement entailment detection as a separate check per constraint type
- Create reification propagator that manages bidirectional constraint posting
- Add guards to prevent infinite propagation cycles
- Leverage existing trail and backtracking mechanisms

## Phase 1: Operator Parsing Support ✅

### Overview
Add reification operators to the parser's operator table to enable proper parsing of #<=>, #==>, #<== expressions.

### Changes Required:

#### 1. Add Operators to Table
**File**: `prolog/parser/reader.py`
**Changes**: Add reification operators to OPERATOR_TABLE

```python
# Add to OPERATOR_TABLE (around line 50, with other CLP(FD) operators)
# Use same precedence as #= (700) for consistency
("#<=>", 700, "xfx"),  # Equivalence
("#==>", 700, "xfx"),  # Forward implication
("#<==", 700, "xfx"),  # Backward implication
```

#### 2. Operator Parsing Tests
**File**: `prolog/tests/unit/test_operator_reification.py` (new file)
**Changes**: Test operator parsing

```python
"""Tests for reification operator parsing."""

import pytest
from prolog.parser.reader import Reader
from prolog.ast.terms import Struct, Var

class TestReificationOperatorParsing:
    """Test that reification operators parse correctly."""

    def test_equivalence_operator_parses(self):
        """Test B #<=> C parses as Struct."""
        reader = Reader()
        term = reader.read_term("B #<=> (X #= Y)")

        assert isinstance(term, Struct)
        assert term.functor == "#<=>"
        assert len(term.args) == 2

    def test_implication_operators_parse(self):
        """Test #==> and #<== operators."""
        reader = Reader()

        forward = reader.read_term("B #==> (X #< 5)")
        assert isinstance(forward, Struct)
        assert forward.functor == "#==>"

        backward = reader.read_term("B #<== (Y #> 3)")
        assert isinstance(backward, Struct)
        assert backward.functor == "#<=="

    def test_precedence_with_other_operators(self):
        """Test precedence interactions."""
        reader = Reader()

        # Should parse as (B #<=> (X #= Y)), not ((B #<=> X) #= Y)
        term = reader.read_term("B #<=> X #= Y")
        assert term.functor == "#<=>"
        inner = term.args[1]
        assert isinstance(inner, Struct)
        assert inner.functor == "#="
```

### Success Criteria:

#### Automated Verification:
- [x] Unit tests pass: `pytest prolog/tests/unit/test_operator_reification.py`
- [x] Operators parse with correct precedence and associativity
- [x] Integration with existing operator table works

#### Manual Verification:
- [x] REPL accepts reification operator syntax
- [x] No conflicts with existing operators

---

## Phase 2: Boolean Domain Support

### Overview
Add utilities for working with Boolean (0/1) domains and detecting Boolean variables.

### Changes Required:

#### 1. Boolean Domain Utilities
**File**: `prolog/clpfd/boolean.py` (new file)
**Changes**: Create utilities for Boolean domain handling

```python
"""Boolean domain utilities for CLP(FD) reification."""

from prolog.clpfd.domain import Domain
from prolog.clpfd.api import get_domain, set_domain
from prolog.ast.terms import Int

# Standard Boolean domain
BOOL_DOMAIN = Domain(((0, 1),))

def is_boolean_domain(domain):
    """Check if domain represents Boolean values (subset of {0,1})."""
    if domain is None or domain.is_empty():
        return False

    # Check if all values are in {0, 1}
    for low, high in domain.intervals:
        if low < 0 or high > 1:
            return False

    return True

def ensure_boolean_var(store, var_id, trail):
    """Ensure variable has Boolean domain, narrowing if needed."""
    current = get_domain(store, var_id)
    if current is None:
        # No domain yet - set to Boolean
        set_domain(store, var_id, BOOL_DOMAIN, trail)
        return True

    # Intersect with Boolean domain
    new_domain = current.intersect(BOOL_DOMAIN)
    if new_domain.is_empty():
        return False  # Not compatible with Boolean

    if new_domain != current:
        set_domain(store, var_id, new_domain, trail)

    return True

def get_boolean_value(store, var_id):
    """Get Boolean value if determined, None if unknown.

    Returns:
        1 if variable is bound to 1
        0 if variable is bound to 0
        None if variable is not yet determined
    """
    deref = store.deref(var_id)
    if deref[0] == "BOUND":
        # Variable is ground
        if isinstance(deref[2], Int):
            val = deref[2].value
            if val in (0, 1):
                return val
    else:
        # Check domain
        domain = get_domain(store, deref[1])
        if domain and domain.is_singleton():
            val = domain.min()
            if val in (0, 1):
                return val

    return None  # Unknown
```

### Success Criteria:

#### Automated Verification:
- [ ] Unit tests pass: `uv run pytest prolog/tests/unit/test_clpfd_boolean.py`
- [ ] Boolean domain detection works correctly
- [ ] ensure_boolean_var properly constrains domains
- [ ] get_boolean_value returns correct values

#### Manual Verification:
- [ ] Boolean utilities integrate cleanly with existing domain system
- [ ] No performance impact on non-Boolean domains

---

## Phase 3: Entailment Detection Framework ✅

### Overview
Implement entailment detection to determine when constraints are definitely satisfied or violated.

### Changes Required:

#### 1. Entailment Detection Interface
**File**: `prolog/clpfd/entailment.py` (new file)
**Changes**: Create entailment detection for each constraint type

```python
"""Entailment detection for CLP(FD) constraints."""

from enum import Enum
from prolog.clpfd.api import get_domain
from prolog.ast.terms import Int

class Entailment(Enum):
    TRUE = 1      # Constraint is definitely satisfied
    FALSE = 0     # Constraint is definitely violated
    UNKNOWN = -1  # Constraint satisfaction is undetermined

def check_equality_entailment(store, x_arg, y_arg):
    """Check if X #= Y is entailed, dis-entailed, or unknown.

    Args:
        store: Variable store
        x_arg: Either var_id or (None, int_value) for constant
        y_arg: Either var_id or (None, int_value) for constant
    """
    # Handle var-int and int-int cases
    if isinstance(x_arg, tuple) and x_arg[0] is None:
        # X is a constant
        x_val = x_arg[1]
        if isinstance(y_arg, tuple) and y_arg[0] is None:
            # Both constants
            return Entailment.TRUE if x_val == y_arg[1] else Entailment.FALSE
        else:
            # X is constant, Y is variable
            y_dom = get_domain(store, y_arg)
            if y_dom:
                if not y_dom.contains(x_val):
                    return Entailment.FALSE
                if y_dom.is_singleton() and y_dom.min() == x_val:
                    return Entailment.TRUE
            return Entailment.UNKNOWN

    if isinstance(y_arg, tuple) and y_arg[0] is None:
        # Y is constant, X is variable
        y_val = y_arg[1]
        x_dom = get_domain(store, x_arg)
        if x_dom:
            if not x_dom.contains(y_val):
                return Entailment.FALSE
            if x_dom.is_singleton() and x_dom.min() == y_val:
                return Entailment.TRUE
        return Entailment.UNKNOWN

    # Both are variables
    x_dom = get_domain(store, x_arg)
    y_dom = get_domain(store, y_arg)

    if x_dom and y_dom:
        # Check domain intersection
        intersection = x_dom.intersect(y_dom)

        if intersection.is_empty():
            return Entailment.FALSE  # Domains disjoint

        # Check for singleton domains
        if x_dom.is_singleton() and y_dom.is_singleton():
            if x_dom.min() == y_dom.min():
                return Entailment.TRUE
            else:
                return Entailment.FALSE

        # Special case: one singleton, check if value is in other domain
        if x_dom.is_singleton():
            val = x_dom.min()
            if not y_dom.contains(val):
                return Entailment.FALSE
            if y_dom.is_singleton():
                return Entailment.TRUE
        elif y_dom.is_singleton():
            val = y_dom.min()
            if not x_dom.contains(val):
                return Entailment.FALSE

    return Entailment.UNKNOWN

def check_less_than_entailment(store, x_arg, y_arg):
    """Check if X #< Y is entailed, dis-entailed, or unknown."""
    # Handle constants
    if isinstance(x_arg, tuple) and x_arg[0] is None:
        x_val = x_arg[1]
        if isinstance(y_arg, tuple) and y_arg[0] is None:
            return Entailment.TRUE if x_val < y_arg[1] else Entailment.FALSE
        else:
            y_dom = get_domain(store, y_arg)
            if y_dom:
                if x_val < y_dom.min():
                    return Entailment.TRUE
                if x_val >= y_dom.max():
                    return Entailment.FALSE
            return Entailment.UNKNOWN

    if isinstance(y_arg, tuple) and y_arg[0] is None:
        y_val = y_arg[1]
        x_dom = get_domain(store, x_arg)
        if x_dom:
            if x_dom.max() < y_val:
                return Entailment.TRUE
            if x_dom.min() >= y_val:
                return Entailment.FALSE
        return Entailment.UNKNOWN

    # Both variables
    x_dom = get_domain(store, x_arg)
    y_dom = get_domain(store, y_arg)

    if x_dom and y_dom:
        if x_dom.max() < y_dom.min():
            return Entailment.TRUE
        if x_dom.min() >= y_dom.max():
            return Entailment.FALSE

    return Entailment.UNKNOWN

def check_less_equal_entailment(store, x_id, y_id):
    """Check if X #=< Y is entailed, dis-entailed, or unknown."""
    from prolog.clpfd.api import get_domain

    x_dom = get_domain(store, x_id)
    y_dom = get_domain(store, y_id)

    if x_dom and y_dom:
        if x_dom.max() <= y_dom.min():
            return Entailment.TRUE

        if x_dom.min() > y_dom.max():
            return Entailment.FALSE

    return Entailment.UNKNOWN

def check_greater_than_entailment(store, x_id, y_id):
    """Check if X #> Y is entailed, dis-entailed, or unknown."""
    # X #> Y is equivalent to Y #< X
    return check_less_than_entailment(store, y_id, x_id)

def check_greater_equal_entailment(store, x_id, y_id):
    """Check if X #>= Y is entailed, dis-entailed, or unknown."""
    # X #>= Y is equivalent to Y #=< X
    return check_less_equal_entailment(store, y_id, x_id)

def check_not_equal_entailment(store, x_arg, y_arg):
    """Check if X #\= Y is entailed, dis-entailed, or unknown."""
    # Inverse of equality
    eq_result = check_equality_entailment(store, x_arg, y_arg)

    if eq_result == Entailment.TRUE:
        return Entailment.FALSE
    elif eq_result == Entailment.FALSE:
        return Entailment.TRUE
    else:
        return Entailment.UNKNOWN

# Negation mappings for constraints
CONSTRAINT_NEGATIONS = {
    "#=": "#\\=",     # ¬(X #= Y) = X #\= Y
    "#\\=": "#=",     # ¬(X #\= Y) = X #= Y
    "#<": "#>=",      # ¬(X #< Y) = X #>= Y
    "#=<": "#>",      # ¬(X #=< Y) = X #> Y
    "#>": "#=<",      # ¬(X #> Y) = X #=< Y
    "#>=": "#<",      # ¬(X #>= Y) = X #< Y
}
```

### Success Criteria:

#### Automated Verification:
- [x] Unit tests pass: `uv run pytest prolog/tests/unit/test_clpfd_entailment.py`
- [x] Entailment detection correct for all constraint types
- [x] Handles ground variables properly
- [x] Returns UNKNOWN when appropriate

#### Manual Verification:
- [x] Entailment checks are efficient
- [x] No false positives or negatives in entailment detection

---

## Phase 3: Core Reification Propagator

### Overview
Implement the main reification propagator that manages B #<==> C relationships.

### Changes Required:

#### 1. Reification Propagator
**File**: `prolog/clpfd/props/reif.py` (new file)
**Changes**: Create the core reification propagator

```python
"""Reification propagator for CLP(FD) constraints."""

from typing import Tuple, Optional, List, Dict, Callable
from prolog.clpfd.boolean import get_boolean_value, ensure_boolean_var
from prolog.clpfd.entailment import Entailment

def create_reification_propagator(
    b_id: int,
    constraint_type: str,
    constraint_args: tuple,
    check_entailment: Callable,
    post_constraint: Callable,
    post_negation: Callable
):
    """Create a reification propagator for B #<==> Constraint.

    Args:
        b_id: Boolean variable ID
        constraint_type: Type of constraint being reified
        constraint_args: Arguments to the constraint
        check_entailment: Function to check constraint entailment
        post_constraint: Function to post the constraint
        post_negation: Function to post constraint negation

    Returns:
        Propagator function
    """
    # Track if we've already posted to prevent loops
    posted_true = [False]
    posted_false = [False]

    def reification_propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        """Propagate reification constraint B #<==> C.

        Three rules:
        1. If C is entailed, set B = 1
        2. If C is dis-entailed, set B = 0
        3. If B is determined:
           - B = 1: post C
           - B = 0: post ¬C
        """
        from prolog.clpfd.api import get_domain, set_domain
        from prolog.clpfd.domain import Domain

        changed = []

        # Ensure B has Boolean domain
        if not ensure_boolean_var(store, b_id, trail):
            return ("fail", None)

        # Check current value of B
        b_value = get_boolean_value(store, b_id)

        # Check entailment of the constraint
        entailment = check_entailment(store, *constraint_args)

        # Rule 1 & 2: Update B based on constraint entailment
        if entailment == Entailment.TRUE:
            # Constraint is satisfied - B must be 1
            b_dom = get_domain(store, b_id)
            if b_dom and not b_dom.contains(1):
                return ("fail", None)

            if b_value != 1:
                new_dom = Domain(((1, 1),))
                set_domain(store, b_id, new_dom, trail)
                changed.append(b_id)
                b_value = 1

        elif entailment == Entailment.FALSE:
            # Constraint is violated - B must be 0
            b_dom = get_domain(store, b_id)
            if b_dom and not b_dom.contains(0):
                return ("fail", None)

            if b_value != 0:
                new_dom = Domain(((0, 0),))
                set_domain(store, b_id, new_dom, trail)
                changed.append(b_id)
                b_value = 0

        # Rule 3: Post constraint based on B's value
        if b_value == 1 and not posted_true[0]:
            # B = 1: Post the constraint
            posted_true[0] = True
            success = post_constraint(engine, store, trail, *constraint_args)
            if not success:
                return ("fail", None)

        elif b_value == 0 and not posted_false[0]:
            # B = 0: Post the constraint's negation
            posted_false[0] = True
            success = post_negation(engine, store, trail, *constraint_args)
            if not success:
                return ("fail", None)

        return ("ok", changed if changed else None)

    return reification_propagator

def create_implication_propagator(
    b_id: int,
    constraint_type: str,
    constraint_args: tuple,
    check_entailment: Callable,
    post_constraint: Callable,
    forward: bool = True
):
    """Create propagator for B #==> C (forward) or B #<== C (backward).

    Forward (B #==> C): If B=1 then C must hold
    Backward (B #<== C): If C holds then B=1
    """
    posted = [False]

    def implication_propagator(store, trail, engine, cause) -> Tuple[str, Optional[List[int]]]:
        from prolog.clpfd.api import get_domain, set_domain
        from prolog.clpfd.domain import Domain

        changed = []

        # Ensure B has Boolean domain
        if not ensure_boolean_var(store, b_id, trail):
            return ("fail", None)

        b_value = get_boolean_value(store, b_id)
        entailment = check_entailment(store, *constraint_args)

        if forward:
            # B #==> C: B=1 implies C
            if b_value == 1 and not posted[0]:
                posted[0] = True
                success = post_constraint(engine, store, trail, *constraint_args)
                if not success:
                    return ("fail", None)

            # If C is false, B must be 0
            if entailment == Entailment.FALSE:
                if b_value != 0:
                    b_dom = get_domain(store, b_id)
                    if b_dom and not b_dom.contains(0):
                        return ("fail", None)
                    new_dom = Domain(((0, 0),))
                    set_domain(store, b_id, new_dom, trail)
                    changed.append(b_id)
        else:
            # B #<== C: C implies B=1
            if entailment == Entailment.TRUE:
                if b_value != 1:
                    b_dom = get_domain(store, b_id)
                    if b_dom and not b_dom.contains(1):
                        return ("fail", None)
                    new_dom = Domain(((1, 1),))
                    set_domain(store, b_id, new_dom, trail)
                    changed.append(b_id)

            # If B=0, C must be false (contrapositive)
            if b_value == 0 and not posted[0]:
                posted[0] = True
                # For backward implication with B=0, we can't directly
                # post ¬C in general. This is weaker than equivalence.
                # We just fail if C is already entailed
                if entailment == Entailment.TRUE:
                    return ("fail", None)

        return ("ok", changed if changed else None)

    return implication_propagator
```

### Success Criteria:

#### Automated Verification:
- [ ] Unit tests pass: `pytest prolog/tests/unit/test_clpfd_reif.py`
- [ ] B #<==> C propagates correctly in both directions
- [ ] Implication propagators work for #==> and #<==
- [ ] Self-notification prevention works
- [ ] Priority levels (HIGH/MED) used consistently

#### Manual Verification:
- [ ] No infinite propagation loops
- [ ] Constraint posting is idempotent
- [ ] Watchers added for both B and constraint variables

---

## Phase 4: Reification Builtins

### Overview
Implement the user-facing builtins for reification: #<=>, #==>, #<==.

### Changes Required:

#### 1. Reification Builtins
**File**: `prolog/engine/builtins_clpfd.py`
**Changes**: Add reification operator builtins

```python
# Add after existing CLP(FD) builtins (around line 900)

def _builtin_fd_reif_equiv(engine, b_term, constraint_term):
    """B #<=> C - Boolean B is equivalent to constraint C holding."""
    store = engine.store
    trail = engine.trail

    # Parse B (can be variable or Int(0|1))
    if isinstance(b_term, Var):
        b_deref = store.deref(b_term.id)
        if b_deref[0] == "BOUND":
            # B is bound - get value
            if isinstance(b_deref[2], Int) and b_deref[2].value in (0, 1):
                b_value = b_deref[2].value
                b_id = None  # Will handle as constant
            else:
                return False  # Not a valid Boolean
        else:
            b_id = b_deref[1]
            b_value = None
    elif isinstance(b_term, Int) and b_term.value in (0, 1):
        b_id = None
        b_value = b_term.value
    else:
        return False  # Invalid B term

    # If B is already determined, just post constraint or its negation
    if b_value is not None:
        if b_value == 1:
            # Post the constraint
            return _post_constraint_directly(engine, constraint_term)
        else:
            # Post the negation
            return _post_constraint_negation(engine, constraint_term)

    # Ensure B has Boolean domain
    if not ensure_boolean_var(store, b_id, trail):
        return False

    # Parse the constraint
    if not isinstance(constraint_term, Struct):
        return False

    # Determine constraint type and create appropriate propagator
    constraint_type = constraint_term.functor

    if constraint_type == "#=" and len(constraint_term.args) == 2:
        # Equality constraint
        x_term, y_term = constraint_term.args
        x_id, y_id = _extract_var_ids(engine, x_term, y_term)
        if x_id is None or y_id is None:
            return False

        from prolog.clpfd.entailment import check_equality_entailment
        from prolog.clpfd.props.equality import create_equality_propagator

        def post_equality(engine, store, trail, x, y):
            prop = create_equality_propagator(x, y)
            return _run_propagator_once(engine, prop, [x, y])

        def post_not_equality(engine, store, trail, x, y):
            from prolog.clpfd.props.neq import create_not_equal_propagator
            prop = create_not_equal_propagator(x, y)
            return _run_propagator_once(engine, prop, [x, y])

        prop = create_reification_propagator(
            b_id, "#=", (x_id, y_id),
            check_equality_entailment,
            post_equality,
            post_not_equality
        )

        # Register and schedule
        queue = _ensure_queue(engine)
        pid = queue.register(prop)

        # Add watchers on all variables
        add_watcher(store, b_id, pid, Priority.HIGH, trail)
        add_watcher(store, x_id, pid, Priority.HIGH, trail)
        add_watcher(store, y_id, pid, Priority.HIGH, trail)

        # Initial propagation
        queue.schedule(pid, Priority.HIGH)
        return queue.run_to_fixpoint(store, trail, engine)

    elif constraint_type == "#<" and len(constraint_term.args) == 2:
        # Less-than constraint
        x_term, y_term = constraint_term.args
        x_id, y_id = _extract_var_ids(engine, x_term, y_term)
        if x_id is None or y_id is None:
            return False

        from prolog.clpfd.entailment import check_less_than_entailment
        from prolog.clpfd.props.comparison import create_less_than_propagator

        def post_less_than(engine, store, trail, x, y):
            prop = create_less_than_propagator(x, y)
            return _run_propagator_once(engine, prop, [x, y])

        def post_not_less_than(engine, store, trail, x, y):
            # ¬(X < Y) is equivalent to X >= Y
            from prolog.clpfd.props.comparison import create_greater_equal_propagator
            prop = create_greater_equal_propagator(x, y)
            return _run_propagator_once(engine, prop, [x, y])

        prop = create_reification_propagator(
            b_id, "#<", (x_id, y_id),
            check_less_than_entailment,
            post_less_than,
            post_not_less_than
        )

        # Register and schedule
        queue = _ensure_queue(engine)
        pid = queue.register(prop)

        add_watcher(store, b_id, pid, Priority.MED, trail)
        add_watcher(store, x_id, pid, Priority.MED, trail)
        add_watcher(store, y_id, pid, Priority.MED, trail)

        queue.schedule(pid, Priority.MED)
        return queue.run_to_fixpoint(store, trail, engine)

    # Add similar cases for #=<, #>, #>=, #\=

    return False  # Unknown constraint type

def _builtin_fd_reif_implies(engine, b_term, constraint_term):
    """B #==> C - If Boolean B is 1, then constraint C must hold."""
    # Similar structure to equiv, but using create_implication_propagator
    # with forward=True
    pass

def _builtin_fd_reif_implied(engine, b_term, constraint_term):
    """B #<== C - If constraint C holds, then Boolean B must be 1."""
    # Similar structure to equiv, but using create_implication_propagator
    # with forward=False
    pass

def _extract_var_ids(engine, x_term, y_term):
    """Extract variable IDs from terms, handling dereferencing."""
    from prolog.ast.terms import Var

    store = engine.store

    if isinstance(x_term, Var):
        x_deref = store.deref(x_term.id)
        if x_deref[0] != "UNBOUND":
            return None, None
        x_id = x_deref[1]
    else:
        return None, None

    if isinstance(y_term, Var):
        y_deref = store.deref(y_term.id)
        if y_deref[0] != "UNBOUND":
            return None, None
        y_id = y_deref[1]
    else:
        return None, None

    return x_id, y_id

def _run_propagator_once(engine, prop, var_ids):
    """Run a propagator once without scheduling."""
    store = engine.store
    trail = engine.trail

    result = prop(store, trail, engine, None)
    return result[0] != "fail"
```

#### 2. Helper Functions for Constraint Posting
**File**: `prolog/engine/builtins_clpfd.py`
**Changes**: Add helper functions for posting constraints

```python
def _post_constraint_directly(engine, constraint_term):
    """Post a constraint using existing builtins."""
    if not isinstance(constraint_term, Struct):
        return False

    functor = constraint_term.functor
    if len(constraint_term.args) != 2:
        return False

    x_term, y_term = constraint_term.args

    # Route to appropriate builtin
    if functor == "#=":
        return _builtin_fd_eq(engine, x_term, y_term)
    elif functor == "#\\=":
        return _builtin_fd_neq(engine, x_term, y_term)
    elif functor == "#<":
        return _builtin_fd_lt(engine, x_term, y_term)
    elif functor == "#=<":
        return _builtin_fd_le(engine, x_term, y_term)
    elif functor == "#>":
        return _builtin_fd_gt(engine, x_term, y_term)
    elif functor == "#>=":
        return _builtin_fd_ge(engine, x_term, y_term)
    else:
        return False

def _post_constraint_negation(engine, constraint_term):
    """Post the negation of a constraint."""
    if not isinstance(constraint_term, Struct):
        return False

    functor = constraint_term.functor
    if len(constraint_term.args) != 2:
        return False

    # Get negated functor from mapping
    from prolog.clpfd.entailment import CONSTRAINT_NEGATIONS
    negated = CONSTRAINT_NEGATIONS.get(functor)
    if not negated:
        return False

    # Create negated constraint and post it
    negated_constraint = Struct(negated, constraint_term.args)
    return _post_constraint_directly(engine, negated_constraint)
```

#### 3. Register Builtins
**File**: `prolog/engine/engine.py`
**Changes**: Register reification operators in `_register_builtins`

```python
# Add in _register_builtins method (around line 370)
# Use the existing builtin map pattern
from prolog.engine.builtins_clpfd import (
    _builtin_fd_reif_equiv,
    _builtin_fd_reif_implies,
    _builtin_fd_reif_implied
)

# In _register_builtins method:
self._builtins[("#<=>", 2)] = lambda eng, args: _builtin_fd_reif_equiv(eng, *args)
self._builtins[("#==>", 2)] = lambda eng, args: _builtin_fd_reif_implies(eng, *args)
self._builtins[("#<==", 2)] = lambda eng, args: _builtin_fd_reif_implied(eng, *args)
```

### Success Criteria:

#### Automated Verification:
- [ ] Unit tests pass: `uv run pytest prolog/tests/unit/test_clpfd_reif_builtins.py`
- [ ] All reification operators parse and execute correctly
- [ ] Integration with existing constraints works
- [ ] Proper error handling for invalid arguments

#### Manual Verification:
- [ ] REPL queries with reification work as expected
- [ ] Performance acceptable for reified constraint networks

---

## Phase 5: Integration and Testing

### Overview
Comprehensive testing and integration with existing CLP(FD) system.

### Changes Required:

#### 1. Integration Tests
**File**: `prolog/tests/unit/test_clpfd_reification.py` (new file)
**Changes**: Comprehensive test suite

```python
"""Tests for CLP(FD) reification support."""

import pytest
from prolog.engine.engine import Engine, Program
from prolog.ast.terms import Var, Int, Struct
from prolog.clpfd.api import get_domain
from prolog.clpfd.boolean import get_boolean_value

class TestBasicReification:
    """Test basic reification functionality."""

    def test_equivalence_with_equality(self):
        """Test B #<=> (X #= 3)."""
        engine = Engine(Program(()))
        store = engine.store

        # Create variables
        b = Var(store.new_var(), "B")
        x = Var(store.new_var(), "X")

        # X in 1..5
        from prolog.engine.builtins_clpfd import _builtin_in
        _builtin_in(engine, x, Struct("..", (Int(1), Int(5))))

        # B #<=> (X #= 3)
        from prolog.engine.builtins_clpfd import _builtin_fd_reif_equiv
        constraint = Struct("#=", (x, Int(3)))
        assert _builtin_fd_reif_equiv(engine, b, constraint)

        # B should still be unknown (0..1)
        b_val = get_boolean_value(store, b.id)
        assert b_val is None

        # Set B = 0
        engine.unify(b, Int(0))

        # X should be pruned to exclude 3
        x_dom = get_domain(store, x.id)
        assert not x_dom.contains(3)
        assert x_dom.contains(1)
        assert x_dom.contains(5)

    def test_entailment_detection(self):
        """Test that entailment is detected correctly."""
        engine = Engine(Program(()))
        store = engine.store

        # Create variables
        b = Var(store.new_var(), "B")
        x = Var(store.new_var(), "X")
        y = Var(store.new_var(), "Y")

        # X in 5..9, Y in 1..4
        from prolog.engine.builtins_clpfd import _builtin_in
        _builtin_in(engine, x, Struct("..", (Int(5), Int(9))))
        _builtin_in(engine, y, Struct("..", (Int(1), Int(4))))

        # B #<=> (X #> Y)
        from prolog.engine.builtins_clpfd import _builtin_fd_reif_equiv
        constraint = Struct("#>", (x, y))
        assert _builtin_fd_reif_equiv(engine, b, constraint)

        # B should be determined to 1 (entailed)
        b_val = get_boolean_value(store, b.id)
        assert b_val == 1

    def test_implication_forward(self):
        """Test B #==> C (forward implication)."""
        engine = Engine(Program(()))
        store = engine.store

        # Create variables
        b = Var(store.new_var(), "B")
        x = Var(store.new_var(), "X")

        # X in 1..10
        from prolog.engine.builtins_clpfd import _builtin_in
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        # B #==> (X #< 5)
        from prolog.engine.builtins_clpfd import _builtin_fd_reif_implies
        constraint = Struct("#<", (x, Int(5)))
        assert _builtin_fd_reif_implies(engine, b, constraint)

        # Set B = 1
        engine.unify(b, Int(1))

        # X should be constrained to < 5
        x_dom = get_domain(store, x.id)
        assert x_dom.max() == 4

    def test_no_infinite_loop(self):
        """Test that self-notification doesn't cause infinite loops."""
        engine = Engine(Program(()))
        store = engine.store

        # Create variables
        b1 = Var(store.new_var(), "B1")
        b2 = Var(store.new_var(), "B2")
        x = Var(store.new_var(), "X")

        # X in 1..10
        from prolog.engine.builtins_clpfd import _builtin_in
        _builtin_in(engine, x, Struct("..", (Int(1), Int(10))))

        # B1 #<=> (X #= 5)
        from prolog.engine.builtins_clpfd import _builtin_fd_reif_equiv
        constraint1 = Struct("#=", (x, Int(5)))
        assert _builtin_fd_reif_equiv(engine, b1, constraint1)

        # B2 #<=> (X #\= 5)
        constraint2 = Struct("#\\=", (x, Int(5)))
        assert _builtin_fd_reif_equiv(engine, b2, constraint2)

        # B1 and B2 should be opposites
        # This should not cause infinite propagation
        engine.unify(b1, Int(1))

        b2_val = get_boolean_value(store, b2.id)
        assert b2_val == 0

class TestComplexReification:
    """Test complex reification scenarios."""

    def test_chained_reification(self):
        """Test chains of reified constraints."""
        # Test scenarios with multiple reified constraints
        pass

    def test_reification_with_labeling(self):
        """Test reification interacting with labeling."""
        # Test that labeling works correctly with reified constraints
        pass
```

### Success Criteria:

#### Automated Verification:
- [ ] All unit tests pass: `pytest prolog/tests/unit/test_clpfd_reification.py`
- [ ] Integration tests pass: `pytest prolog/tests/unit/test_clpfd*.py`
- [ ] No regression in existing CLP(FD) tests
- [ ] Property tests for confluence still pass
- [ ] Operator parsing tests pass: `pytest prolog/tests/unit/test_operator_reification.py`

#### Manual Verification:
- [ ] Complex reified constraint problems solve correctly
- [ ] No performance degradation for non-reified constraints
- [ ] REPL interaction feels natural
- [ ] Backtracking properly restores state

---

## Testing Strategy

### Unit Tests:
- Operator parsing for #<=>, #==>, #<==
- Boolean domain utilities (is_boolean_domain, ensure_boolean_var, get_boolean_value)
- Entailment detection for each constraint type:
  - Var-var cases
  - Var-int cases
  - Int-int cases
  - Singleton domain cases
  - Disjoint domain cases
- Reification propagator behavior with B as variable or Int(0|1)
- Implication propagator behavior (forward and backward)
- Self-notification prevention

### Integration Tests:
- End-to-end reification via parsed terms (not just direct calls)
- Reification with existing constraints
- Complex reified constraint networks
- Interaction with labeling
- Backtracking behavior:
  - Binding B then backtracking restores domains
  - Constraint posting then backtracking removes watchers
- Downstream propagation when B=1 forces constraint

### Property Tests:
- Confluence: Different posting orders yield same results
- Monotonicity: Constraints only narrow, never expand
- Completeness: All valid solutions found

### Manual Testing Steps:
1. Test B #<=> (X #= Y) with various domain configurations
2. Verify entailment detection with bounds constraints
3. Test implication operators with complex expressions
4. Verify no infinite loops with circular reifications
5. Test performance with large reified constraint networks
6. Verify REPL accepts syntax: `?- B #<=> (X #= 3), X in 1..5, B = 0.`

## Performance Considerations

- Boolean domain checks should be O(1) for common case
- Entailment detection should avoid recomputation
- Propagator posting tracking prevents duplicate work
- Self-notification guards must be efficient
- Consider caching entailment results within propagation cycle

## Migration Notes

- Existing CLP(FD) code continues to work unchanged
- Reification is opt-in via new operators
- No changes to existing propagator implementations
- Document that reification may weaken propagation in some cases

## References

- Original plan: `docs/PLAN.md` Stage 5.5 (lines 342-357)
- CLP(FD) implementation: `prolog/clpfd/`
- Stage 4 attributed variables: `prolog/engine/engine.py:2679-2875`
- SWI-Prolog reification: https://www.swi-prolog.org/pldoc/man?section=clpfd-reification