# WAM Implementation Plans

This directory contains the complete documentation for the Warren Abstract Machine (WAM) implementation in PyLog.

## Primary Documents

### [WAM-ARCH.md](WAM-ARCH.md) ⭐
**Definitive architectural reference** providing complete technical specification of the WAM design. Covers:
- Memory architecture (heap, stacks, trail, registers) in comprehensive detail
- Instruction set architecture and dispatch mechanisms
- Unification model and binding protocol
- Control flow, compilation, and module system
- Integration points (builtins, CLP(FD), tracing, DAP)
- Execution model and invariants
- Design rationale and performance characteristics
- Comparison with classic WAM
- 20 major sections, 79KB, 2500+ lines

### [wam-roadmap.md](wam-roadmap.md)
Main roadmap document outlining the incremental path from tree-walking interpreter to WAM-based runtime. Covers:
- Goals and guiding principles
- Phase overview (0–9 plus future work)
- Testing strategy
- Benchmarking and performance targets
- Git strategy and integration approach
- Risks and mitigations

### [wam-instruction-set.md](wam-instruction-set.md)
Complete instruction set reference including:
- All instructions grouped by category
- Detailed semantics and register effects
- Instruction arity table for validation
- Opcode encoding notes
- Evolution tracking across phases

### [wam-benchmarks.md](wam-benchmarks.md)
Comprehensive benchmark methodology covering:
- Benchmark categories (micro, scenario, stress)
- Measurement protocol and metrics
- Performance targets per phase
- Regression detection strategy
- CI integration approach

### [wam-roadmap-updates.md](wam-roadmap-updates.md)
Summary of documentation improvements made on 2025-10-13, including rationale for changes.

## Phase Implementation Plans

Detailed plans for each implementation phase:

- [phase-0-foundations.md](phase-0-foundations.md) — Scaffolding, data structures, dispatcher
- [phase-1-unification.md](phase-1-unification.md) — Core unification and term access
- [phase-2-control-backtracking.md](phase-2-control-backtracking.md) — Call/return, environments, choicepoints, cut
- [phase-3-compiler.md](phase-3-compiler.md) — Prolog to WAM compiler
- [phase-3-5-exceptions-occurs-check.md](phase-3-5-exceptions-occurs-check.md) — Exception handling and occurs-check policy
- [phase-4-control-builtins.md](phase-4-control-builtins.md) — Disjunction, if-then-else, core builtins
- [phase-5-indexing.md](phase-5-indexing.md) — First-argument indexing
- [phase-5-5-optimization.md](phase-5-5-optimization.md) — Peephole optimisations, LCO, trimming
- [phase-6-gc.md](phase-6-gc.md) — Garbage collection
- [phase-7-attributed-vars-clpfd.md](phase-7-attributed-vars-clpfd.md) — Attributed variables and CLP(FD) integration
- [phase-8-tracing-iso.md](phase-8-tracing-iso.md) — Tracing, debugging, DAP, ISO polish
- [phase-9-web-packaging.md](phase-9-web-packaging.md) — Pyodide readiness and packaging

## Phase Dependencies

```
Phase 0 (Foundations)
  ↓
Phase 1 (Unification)
  ↓
Phase 2 (Control)
  ↓
Phase 3 (Compiler) ← needs 1+2
  ↓
Phase 3.5 (Exceptions) ← needs 1+2
  ↓
Phase 4 (Full control + bridge) ← needs 3
  ↓
Phase 5 (Indexing) ← needs 3
  ↓
Phase 5.5 (Optimisation) ← needs 5
  ↓
Phase 6 (GC) ← needs 1+2
  ↓
Phase 7 (Attributed vars) ← needs 1+2, recommends 6
  ↓
Phase 8 (Tracing/ISO) ← needs all control+compiler
  ↓
Phase 9 (Web/Pyodide) ← needs stable runtime
```

## Reading Order

### For Overview
1. Start with [wam-roadmap.md](wam-roadmap.md) for high-level plan
2. Read [WAM-ARCH.md](WAM-ARCH.md) for complete architectural understanding
3. Review [wam-instruction-set.md](wam-instruction-set.md) for instruction reference
4. Check [wam-benchmarks.md](wam-benchmarks.md) for measurement approach

### For Implementation
1. Read [WAM-ARCH.md](WAM-ARCH.md) to understand the complete design
2. Read phase plans in order (0 through 9) for implementation steps
3. Reference instruction set as needed
4. Follow benchmark methodology for measurements
5. Update [wam-roadmap-updates.md](wam-roadmap-updates.md) if making significant changes

### For Deep Understanding
1. [WAM-ARCH.md](WAM-ARCH.md) - Complete architectural specification (start here for technical depth)
2. Phase plans - Detailed implementation guidance per phase
3. Instruction set - Opcode-level semantics
4. Benchmarks - Performance measurement and targets

## Key Principles

- **Incremental**: Each phase builds on stable interfaces
- **Testable**: Strong unit and differential testing throughout
- **Observable**: Debug hooks and tracing at every stage
- **Correct first**: Performance comes after correctness
- **Independence strategy**: WAM implementation proceeds independently; tree-walker coexists during development and will be replaced when WAM is complete

## Status

**Current Status**: Phase 0 (Foundations) in progress.

- Phase 0 scaffold complete: Machine state, dispatch loop, instruction handlers, debug snapshots
- Comprehensive unit test suite covering machine initialization, execution, and invariants
- All core data structures and execution framework operational
- Documentation updated as of 2025-10-14
