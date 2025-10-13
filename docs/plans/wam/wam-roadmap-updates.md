# WAM Roadmap Updates - 2025-10-13

This document summarises improvements made to the WAM roadmap and phase documentation based on a comprehensive review.

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
