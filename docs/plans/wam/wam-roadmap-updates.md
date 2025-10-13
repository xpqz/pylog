# WAM Roadmap Updates

This document summarises improvements made to the WAM roadmap and phase documentation based on comprehensive reviews.

## Update 2: Independence Strategy (2025-10-13)

### Overview
Major architectural revision to minimize tree-walker dependencies and clarify that WAM is a replacement, not a complement. The tree-walker will serve solely as a differential testing baseline during development and will be removed in Phase 9.

### Key Changes

#### Architecture Philosophy
- **Before**: Dual-engine strategy with potential long-term coexistence
- **After**: WAM is independent replacement; tree-walker for testing only

#### Shared Components
- **Before**: Parser/AST, Unify/Store, CLP(FD), Tracer (shared implementations)
- **After**: Parser/AST only; all runtime components independent

#### Builtin Implementation (Phase 4)
- **Before**: Bridge to tree-walker builtins with marshalling layer
- **After**: Native WAM builtins operating directly on heap cells
- **Rationale**: Marshalling overhead defeats WAM performance gains; cleaner to port once than maintain bridge forever

#### CLP(FD) Integration (Phase 7)
- **Before**: Bridge to tree-walker CLP(FD) initially, port later
- **After**: Independent WAM CLP(FD) implementation from start
- **Rationale**: Heap structure incompatibility makes bridging complex; native implementation cleaner

### Files Modified

#### docs/plans/wam/WAM-ARCH.md
- **Section 1.2**: Rewrote ecosystem relationship to emphasize independence
- **Section 12.1**: Completely replaced builtin bridge with native implementation strategy
- **Section 12.3**: Updated CLP(FD) integration to be independent
- Updated architecture diagram to show minimal sharing (parser/AST only)

#### docs/plans/wam/phase-4-control-builtins.md (NEW)
- Replaced `phase-4-control-builtins-bridge.md` entirely
- Added comprehensive native builtin implementations:
  - Arithmetic evaluation engine with full operator support
  - Type check builtins (var/1, atom/1, integer/1, etc.)
  - Meta-call implementation (call/1)
- Removed all bridge-related content
- Clear scope: disjunction, if-then-else, core builtins only

#### docs/plans/wam/wam-roadmap.md
- Updated guiding principles: "dual-engine strategy" → "independence strategy"
- Phase 4 title: "builtins bridge" → "core builtins"
- Phase 7: removed bridge references, emphasized native CLP(FD)
- Integration section: clarified tree-walker role as testing baseline only
- Git strategy: added explicit migration path (Phase 6-8 WAM default, Phase 9 remove tree-walker)
- Module resolution: removed bridge references

#### docs/plans/wam/wam-instruction-set.md
- Removed entire "Builtins Bridge" section
- Removed `call_builtin` instruction from arity table
- Updated Phase 4 description in Evolution section

#### docs/plans/wam/README.md
- Updated phase-4 link to new filename

### Files Removed
- `phase-4-control-builtins-bridge.md` (obsolete)

### Git Strategy
Complete revision to ensure `main` remains deployable at all times:

- **Before**: Small frequent merges to `main` behind feature flag; dual-engine on `main`
- **After**: Long-lived `wam-dev` branch; `main` stays tree-walker only until Phase 9

**Key decisions**:
- `main` contains only tree-walker code (production-ready)
- `wam-dev` contains both engines during development
- Weekly `main` → `wam-dev` syncs to prevent divergence
- No `wam-dev` → `main` merge until Phase 8 complete
- Documentation can merge to `main` independently
- Parser/AST improvements land on `main` first, pulled into `wam-dev`
- "Big Merge" happens between Phase 8 and Phase 9

**Benefits**:
1. `main` always deployable (critical requirement)
2. Parallel development without interference
3. Clear integration point (end of Phase 8)
4. Hotfixes isolated to `main` only
5. Production releases unaffected by experimental code

### Migration Path
- **Phase 0-8**: Development on `wam-dev` branch (tree-walker + WAM coexist)
- **Phase 8 → 9**: "Big Merge" from `wam-dev` to `main`
- **Phase 9**: Feature flag initially (`PYLOG_ENGINE=wam` default), then remove tree-walker

### Benefits
1. **Performance**: No marshalling overhead on every builtin call
2. **Simplicity**: No format conversion, no lifecycle coupling
3. **Correctness**: Single source of truth for each feature
4. **Maintainability**: No bridge code to maintain long-term

### Risks Addressed
- **Reimplementation effort**: Core builtins are small and well-specified (10-15 in Phase 4)
- **Testing overhead**: Differential tests validate parity with tree-walker
- **Feature coverage**: Incremental rollout per phase ensures nothing missed

---

## Update 1: Initial Documentation (2025-10-13)

## Overview

The WAM roadmap and phase documentation were reviewed for correctness and completeness. The documentation was found to be technically sound with excellent phase isolation and testing emphasis. The updates below address organisational improvements and fill documentation gaps.

## New Documents Created

### 1. wam-instruction-set.md
**Location**: `docs/plans/wam-instruction-set.md`

Complete instruction set reference including:
- All instructions grouped by category (control, unification, indexing, etc.)
- Detailed semantics for each instruction
- Register effects and notes
- Instruction arity reference table for validation
- Opcode encoding and dispatch strategy notes
- Evolution tracking across phases

**Purpose**: Provides single source of truth for instruction semantics, validation rules, and implementation status.

### 2. wam-benchmarks.md
**Location**: `docs/plans/wam-benchmarks.md`

Comprehensive benchmarking methodology including:
- Benchmark categories (micro, scenario, stress)
- Specific benchmark definitions (append, member, n-queens, etc.)
- Measurement protocol (warmup, runs, statistics)
- Metrics collection (time, instructions, memory, GC)
- Reporting format (JSON schema)
- Performance targets per phase
- Regression detection strategy
- CI integration approach
- Pyodide-specific considerations

**Purpose**: Ensures consistent, reproducible performance measurement across development phases.

## Phase Documentation Updates

### Phase 0 - Foundations
**File**: `phase-0-foundations.md`

**Added**: Decision point for dispatch strategy selection
- Document methodology and benchmark results
- Define criteria for revisiting decision in Phase 5.5 (if dispatch >20% of execution time)

### Phase 1 - Unification
**File**: `phase-1-unification.md`

**Added**: Decision point for LIST representation
- Choose between dedicated LIST cell vs STR with `./2` functor
- Document criteria: simplicity, performance, debuggability
- Require documentation of choice and rationale in Phase 1 completion

### Phase 2 - Control & Backtracking
**File**: `phase-2-control-backtracking.md`

**Added**: Note on environment trimming deferral
- Explicitly states environment trimming deferred to Phase 5.5
- Notes requirements: safe point identification, liveness analysis

### Phase 3 - Compiler
**File**: `phase-3-compiler.md`

**Added**: Module resolution details
- Current module context and default module (`user`)
- Qualified call semantics (`M:Goal`)
- Import/export table handling
- Built-in predicates under `system` module
- Symbol table keying (`module:name/arity`)
- Error message format for undefined predicates

**Added**: Decision point for persisted bytecode
- Phase 3 supports in-memory loading
- Defers disk persistence pending:
  - Use case validation (deployment, startup time)
  - Versioning strategy
  - Security model (signatures, verification)
- Evaluate by Phase 9; document decision

### Phase 5.5 - Optimisation
**File**: `phase-5-5-optimization.md`

**Added**: Environment trimming assignment
- Implement optional `trim N` instruction
- Requires liveness analysis at call boundaries
- Document trade-offs (complexity vs memory savings)
- Enable selectively based on profiling

### Phase 7 - Attributed Variables
**File**: `phase-7-attributed-vars-clpfd.md`

**Clarified**: GC dependency path
- GC strongly recommended but not strictly required
- Two paths defined:
  - **Preferred**: Full Phase 6 GC operational
  - **Lightweight**: Conservative attribute retention with cleanup hooks
- Decision point: If Phase 7 proceeds without Phase 6, document memory growth limits and plan Phase 6 integration before production use

### Phase 9 - Web/Pyodide
**File**: `phase-9-web-packaging.md`

**Added**: Bytecode security section
- Apply Phase 3 validation to network-sourced bytecode
- Threat model: malformed bytecode crash prevention
- Validation checks: opcode range, arity, register bounds, label resolution
- Sandboxing notes: worker isolation
- Error handling: structured errors, fallback to tree-walker

## Roadmap Updates

**File**: `wam-roadmap.md`

**Added**: References to new documentation
- Instruction set reference link in Phase 0 deliverables
- Benchmark methodology link in Benchmarks & KPIs section
- New "Supporting Documentation" section with:
  - Implementation references (instruction set, benchmarks, phase plans)
  - Academic references (preserved from original)

## Issues Addressed

### High Priority (Completed)
1. ✓ Clarified GC dependency path for Phase 7
2. ✓ Created complete instruction set reference document
3. ✓ Added comprehensive benchmark methodology document
4. ✓ Expanded module resolution in Phase 3

### Medium Priority (Completed)
5. ✓ Added network security note to Phase 9
6. ✓ Documented deferred decisions with resolution points
7. ✓ Assigned environment trimming to Phase 5.5

### Structural Improvements
8. ✓ Cross-referenced supporting documents from roadmap
9. ✓ Established decision points for deferred choices
10. ✓ Improved traceability of features across phases

## Issues Identified But Not Addressed

### Lower Priority Items
These items were noted but not updated as they require broader discussion:

1. **Phase numbering rationale**: Phase 3.5 for exceptions could be explained (exceptions touch multiple concerns: control flow, unification, error handling)

2. **Performance bounds precision**: Targets like "2–5× faster" and "within 2×" could be tightened with specific benchmark/aggregate clarification

3. **Rollback planning**: No explicit contingency if a phase proves infeasible

4. **Cut semantics consolidation**: Cut interactions with exceptions, disjunction, and tabling spread across phases; could benefit from central reference

5. **Migration guide**: Backwards compatibility and user migration from tree-walker to WAM not yet planned in detail

## Recommendations for Future Work

### Documentation
- Consider creating `wam-cut-semantics.md` to centralise cut behaviour across all features
- Add `wam-migration-guide.md` when approaching production readiness
- Create visual diagrams for memory layout (heap/stack/trail structure)

### Process
- Establish regular documentation review cadence (per-phase or quarterly)
- Create checklist for phase completion including documentation updates
- Define criteria for promoting experimental features to required

### Testing
- Expand differential testing guidance with concrete test case selection criteria
- Document order-sensitivity handling in solution comparison
- Add trace comparison methodology details

## Summary

The updates strengthen the WAM roadmap by:
- Filling critical documentation gaps (instruction set, benchmarks)
- Clarifying dependencies and decision points
- Improving cross-referencing and traceability
- Adding security considerations for web deployment
- Documenting deferred decisions with explicit resolution criteria

The technical content remains sound. These updates improve organisation, completeness, and maintainability of the implementation plan.

## Files Modified
- `docs/plans/wam-roadmap.md`
- `docs/plans/phase-0-foundations.md`
- `docs/plans/phase-1-unification.md`
- `docs/plans/phase-2-control-backtracking.md`
- `docs/plans/phase-3-compiler.md`
- `docs/plans/phase-5-5-optimization.md`
- `docs/plans/phase-7-attributed-vars-clpfd.md`
- `docs/plans/phase-9-web-packaging.md`

## Files Created
- `docs/plans/wam-instruction-set.md`
- `docs/plans/wam-benchmarks.md`
- `docs/plans/wam-roadmap-updates.md` (this file)
