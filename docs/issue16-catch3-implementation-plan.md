# Issue #16 — `catch/3` Re-landing Plan (Implementation Spec)

This document describes a disciplined plan to re-introduce `catch/3` with ISO-style semantics into the current single-loop VM. It is written to be pasted directly into the GitHub issue.

---

## Goals (ISO semantics)

- **`throw/1`** performs a non-local exit.
- **`catch/3`**:
  - Runs `Goal`. If `Goal` throws `Ball` that **unifies** with `Catcher`, unwind to the **catch call site**, re‑unify `Ball` with `Catcher`, and run `Recovery`.
  - Does **not** resume `Goal` after a catch.
  - Backtracking after `Recovery` finishes **or fails** proceeds to **pre‑catch** choicepoints (transparent backtracking).
- **Cut scope** unaffected: a cut in `Recovery` commits in the surrounding caller scope (ISO).

---

## Preconditions (engine invariants to rely on)

1. **Per‑CP trail stamps**  
   - Assign `cp.stamp = trail.next_stamp()` **on CP creation**.  
   - On **enter/redo** that CP: `trail.set_current_stamp(cp.stamp)`.

2. **Cell‑local `write_stamp` + restore on untrail**  
   - Each variable cell stores its `write_stamp`.  
   - Trail entries save **old value + old write_stamp**; untrail restores both.  
   - No global “var→stamp” map that survives untrail.

3. **Conditional `POP_FRAME`**  
   - Pop a frame **only** if no CP still references it.

4. **Heights‑only restoration order**  
   - **trail → goals → frames → CPs**.

If any of these aren’t true after the revert, restore them first—they’re foundational.

---

## Data structures

### `ChoicepointKind.CATCH`

```python
@dataclass(slots=True)
class CatchCP:
    kind: ChoicepointKind = CATCH
    # Baselines captured BEFORE pushing any wrappers/goals
    trail_top: int
    goal_height: int
    frame_height: int
    cp_height: int
    # Runtime
    stamp: int                 # window stamp
    phase: Literal["GOAL","RECOVERY"]
    catcher: Term
    recovery: Term
    owner_frame_id: int        # optional, for asserts
```

> No separate “catch record” stack is needed; the **CATCH CP** itself is the redo anchor and carries baselines and payload.

---

## Control flow (pseudocode)

### `throw/1` builtin (Stage‑1)

```python
def _builtin_throw(ball):
    # Deref ball; Stage‑1: must be instantiated (else instantiation_error in later stage).
    raise PrologThrow(ball)
```

### `catch/3` builtin (creation path)

```python
def _builtin_catch(goal, catcher, recovery):
    # 0) Deref/check callability where needed (Stage‑1: dev‑mode can just fail on non‑callable).
    # 1) Capture baselines BEFORE pushing anything:
    base = Baselines(
        trail_top = trail.position(),
        goal_h    = goal_stack.height(),
        frame_h   = len(frame_stack),
        cp_h      = len(cp_stack)
    )
    # 2) Create CATCH CP (phase=GOAL), assign stamp at creation
    cp = CatchCP(
        trail_top=base.trail_top,
        goal_height=base.goal_h,
        frame_height=base.frame_h,
        cp_height=base.cp_h,
        stamp=trail.next_stamp(),
        phase="GOAL",
        catcher=catcher,
        recovery=recovery,
        owner_frame_id=frame_stack[-1].frame_id if frame_stack else -1
    )
    cp_stack.append(cp)
    # 3) Push the user's Goal to execute
    goal_stack.push(Goal.from_term(goal))
    return OK
```

### Main loop: handling a **caught throw**

```python
try:
    # normal dispatch loop...
except PrologThrow as exc:
    ball = exc.ball
    # Find nearest CATCH CP in GOAL phase and in scope
    i = len(cp_stack) - 1
    while i >= 0:
        cp = cp_stack[i]
        if cp.kind is CATCH and cp.phase == "GOAL" and cp.cp_height <= len(cp_stack)-1:
            # In scope; try to match
            # 1) Restore to baselines
            trail.unwind_to(cp.trail_top)
            goal_stack.shrink_to(cp.goal_height)
            while len(frame_stack) > cp.frame_height: frame_stack.pop()
            while len(cp_stack)   > cp.cp_height:    cp_stack.pop()
            # 2) Switch stamp/window for the catch window
            trail.set_current_stamp(cp.stamp)
            # 3) Re-unify BALL with CATCHER (bindings visible to Recovery)
            if not unify(ball, cp.catcher, store, trail_adapter, occurs_check=occurs):
                # Not this catcher; continue searching outer CATCH
                i -= 1
                continue
            # 4) Phase switch: mark cp as RECOVERY and push Recovery
            cp.phase = "RECOVERY"
            cp_stack.append(cp)  # (cp already there after shrink; ok to leave as top)
            goal_stack.push(Goal.from_term(cp.recovery))
            break
        i -= 1
    else:
        # No catcher matched: rethrow outward
        raise
```

### Backtracking into a **CATCH CP**

When the VM chooses a CP to redo and it’s a CATCH:

```python
def redo_catch(cp: CatchCP):
    trail.set_current_stamp(cp.stamp)
    if cp.phase == "GOAL":
        # Goal exhausted without a throw -> transparent failure of catch/3
        cp_stack.pop()
        return FAIL
    else:  # "RECOVERY"
        # Recovery exhausted -> transparent failure of catch/3
        cp_stack.pop()
        return FAIL
```
> Recovery’s own choicepoints drive multiple recovery answers; the CATCH CP does not enumerate those—it only stands as the **call‑site redo anchor**.

---

## Integration notes & pitfalls to avoid

- **Do not** pop pre‑catch CPs when catching: use `while len(cp_stack) > cp.cp_height` (strict `>`), not `>=`.
- **Do not** advance the trail stamp on redo; rely on “stamp on CP” + “restore var.write_stamp on untrail”.
- **Var–var links must trail** via the adapter (no bypass path).
- **No CONTROL sentinels** are needed for catch; the CP is sufficient.

---

## Implementation plan (small, safe slices)

1. **Re‑land `PrologThrow` (uncaught)**  
   - Builtin `throw/1` raises `PrologThrow(ball)`.  
   - Add a minimal `pytest.raises(PrologThrow)` test to ensure the runner propagates uncaught throws.

2. **Introduce `ChoicepointKind.CATCH` & redo handler**  
   - Extend CP enum + CP dataclass; add stamp on creation.  
   - Add `redo_catch` to the dispatcher; make it fail in both phases (as above).

3. **Wire `catch/3` to create a CATCH CP (phase=GOAL)**  
   - Capture baselines **before** pushing anything.  
   - Push CATCH CP then push user `Goal`.

4. **Catch path in main loop**  
   - In the `except PrologThrow` handler: implement the search + restore + re‑unify + phase switch + push Recovery (as pseudocode above).  
   - Keep assertions:
     - `assert len(cp_stack) == cp.cp_height` after shrink  
     - `assert len(frame_stack) >= cp.frame_height`  
     - `assert goal_stack.height() == cp.goal_height`

5. **Asserts & tracing** (cheap):  
   - On entering/redoing any CP: `assert trail.current_stamp == cp.stamp`.  
   - Emit NDJSON events for `catch_switch` and `cp_push/cp_pop`.

6. **Micro‑tests before full suite** (fast feedback):  
   - `catch((X=1 ; (X=2, throw(t)) ; X=3), t, X=caught)` ⇒ `[1, caught]`.  
   - `p(1). p(2). catch(throw(t), t, r(Y)). r(10). r(20).` ⇒ `2*2=4` solutions (`p×r`).  
   - Recovery fails: `p(1). p(2). catch(throw(t), t, fail)` ⇒ `X=1; X=2`.  
   - Cut in recovery (ISO): `p(1). p(2). r(10). r(20). catch(throw(t), t, (r(Y),!))` ⇒ **1** solution.

7. **Run full advanced tests**  
   - Fix only test expectations that encode non‑ISO recovery/cut behavior.  
   - Keep projection policy consistent (either bound‑only or include unbound vars; document it).

---

## Optional (but helpful) ergonomics

- **Helper**: `capture_baselines()` utility used by catch and (later) ITE, nondet builtins.  
- **Debug**: add `engine.debug_snapshot()` that dumps (frames, CPs, goals, trail pos, stamp) for use in trace on failure.

---

## Done criteria

- All core catch/throw tests green (including advanced: transparent backtracking, nested catch, disjunction interactions).  
- No regressions in basic backtracking/ports/trail‑stamp tests.  
- Assert/trace shows: pre‑catch CPs preserved on catch; CATCH CP phases behave as specified; stamps restored on redo.
