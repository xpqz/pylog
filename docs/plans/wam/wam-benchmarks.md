# WAM Benchmark Methodology

This document defines the benchmarking strategy for measuring PyLog WAM performance across implementation phases.

## Objectives

1. Track performance improvements across phases
2. Detect regressions early in development
3. Establish baseline comparisons with tree-walker
4. Measure Pyodide/WASM overhead
5. Guide optimisation decisions with data

## Benchmark Categories

### Microbenchmarks (Core Operations)

Small, focused tests measuring specific operations:

#### `append/3` - List concatenation
```prolog
% Deterministic forward: append([1,2],[3,4],X)
% Backward generation: append(X,Y,[1,2,3,4])
```
- **Metrics**: Solutions/sec, instruction count, heap allocations
- **Variants**: Fixed lists (10, 100, 1000 elements), nondeterministic generation

#### `member/2` - List membership
```prolog
% member(X, [1,2,3,...,N])
```
- **Metrics**: Solutions/sec, choicepoint creation overhead
- **Variants**: First/middle/last element, full enumeration

#### `naive_reverse/2` - Quadratic reversal
```prolog
% reverse([1,2,3,...,N], R)
```
- **Metrics**: Time to completion, instruction count
- **Purpose**: Stress test unification and list building

#### `permutation/2` - Backtracking stress
```prolog
% permutation([1,2,3,4,5], P)
```
- **Metrics**: Solutions/sec, choicepoint/trail overhead
- **Purpose**: Measure backtracking efficiency

#### Simple recursion - Tail call behaviour
```prolog
count_down(0).
count_down(N) :- N > 0, N1 is N - 1, count_down(N1).
```
- **Metrics**: Frame allocation, stack depth, LCO effectiveness (Phase 5.5+)
- **Variants**: 1K, 10K, 100K iterations

### Scenario Benchmarks (Real Programs)

Representative programs combining multiple features:

#### N-Queens (size 8)
```prolog
% Place N queens on NxN board with no conflicts
```
- **Metrics**: Time to first/all solutions, backtrack count
- **Features**: Indexing, cut, arithmetic

#### Symbolic differentiation
```prolog
% diff(X^2 + 3*X, X, D)
```
- **Metrics**: Time to solution, unification count
- **Features**: Deep structure unification, determinism

#### Path finding (graph traversal)
```prolog
% path(a, z, Graph, Path)
```
- **Metrics**: Solutions/sec, choicepoint pruning via cut
- **Features**: Backtracking, cut, indexing

#### CLP(FD) sudoku (4×4)
```prolog
% Minimal constraint satisfaction problem
```
- **Metrics**: Propagation time, labeling time, total time
- **Features**: Attributed variables, propagation queue, backtracking
- **Phases**: Only valid from Phase 7 onwards

### Stress Tests (Scalability)

Long-running tests for memory and stability:

#### Million choicepoints
```prolog
% Generate and backtrack through 1M choicepoints
```
- **Metrics**: Peak memory, GC frequency/time, stability
- **Purpose**: Validate GC (Phase 6) and memory management

#### Deep recursion (50K frames)
```prolog
% Non-tail recursion to stress environment stack
```
- **Metrics**: Stack memory, frame allocation overhead
- **Purpose**: Stress environment management

#### Large heap allocation
```prolog
% Build large structures (lists of 100K+ elements)
```
- **Metrics**: Heap growth, GC behaviour, allocation time
- **Purpose**: Memory management validation

## Measurement Methodology

### Environment Setup

**Hardware baseline**:
- Record CPU model, core count, RAM
- Run on consistent hardware when possible
- Note thermal throttling or background load

**Software baseline**:
- Python version (3.10+ required)
- uv version
- OS and kernel version
- For Pyodide: browser and version

### Execution Protocol

1. **Warmup**: Run benchmark 3 times, discard results (JIT warmup, cache effects)
2. **Measurement**: Run benchmark 10 times, record all results
3. **Statistics**: Report median, mean, std dev, min, max
4. **Outliers**: Flag runs >2 std dev from mean; investigate anomalies

### Metrics Collection

For each benchmark run, record:

- **Time**: Wall-clock time (µs precision)
- **Instructions**: Total instruction dispatch count (WAM only)
- **Memory**: Peak heap size, peak stack size
- **GC**: GC invocations, total GC time (Phase 6+)
- **Solutions**: Number of solutions generated
- **Backtrack count**: Choicepoint retry operations

### Reporting Format

```json
{
  "benchmark": "append_deterministic_10",
  "phase": "P5",
  "engine": "wam",
  "date": "2025-10-13",
  "environment": {
    "python": "3.11.5",
    "cpu": "Apple M1",
    "os": "macOS 14.5"
  },
  "runs": 10,
  "results": {
    "time_us": {"median": 245, "mean": 248, "stddev": 12, "min": 238, "max": 271},
    "instructions": {"median": 1234, "mean": 1235, "stddev": 2},
    "heap_peak_bytes": {"median": 8192, "mean": 8201, "stddev": 45},
    "solutions": 1
  }
}
```

Store results in `docs/benchmarks/results/phaseN/` as dated JSONL files.

## Comparison Baselines

### Tree-Walker Baseline (Phase 1+)
- Run identical benchmark suite under tree-walker
- Record results in same format with `"engine": "tree"`
- Calculate speedup ratio: `tree_time / wam_time`

### Inter-Phase Comparison
- Compare same benchmark across phases
- Track instruction count reduction (optimisations)
- Flag regressions (>10% slowdown without explanation)

### Pyodide Comparison (Phase 9)
- Run native Python WAM vs Pyodide WAM
- Calculate overhead ratio: `pyodide_time / native_time`
- Target: <2× overhead for microbenchmarks

## Performance Targets

### Phase-Specific Targets

| Phase | Target vs Tree-Walker | Notes |
|-------|----------------------|-------|
| P0    | N/A                  | Baseline recording only |
| P1    | No target            | Correctness focus |
| P2    | No target            | Correctness focus |
| P3    | No target            | Correctness focus |
| P5    | Competitive          | Indexing should show benefits on member/append |
| P5.5  | 2–5× faster          | Optimisations target common patterns |
| P9    | <2× Pyodide overhead | WASM-specific target |

### Bounds (Non-Blocking)
- Individual microbenchmark regression >20%: investigate before merge
- Scenario benchmark regression >15%: investigate before merge
- Memory growth unbounded: blocking (fix required)

## Regression Detection

### CI Integration (Non-Blocking)
- Run microbenchmark suite on every PR (tree + WAM)
- Store results in artefacts
- Comment on PR with summary table (no failure gates initially)

### Trending
- Maintain historical results in git
- Plot trends over time (instructions/solution, time/solution)
- Flag sustained regressions (>3 consecutive PRs slower)

## Profiling Deep Dives

When benchmarks indicate issues, use:

### Python `cProfile`
```bash
python -m cProfile -o profile.stats benchmark_script.py
python -m pstats profile.stats
```
- Identify hot functions (dispatch, unify, deref)
- Guide optimisation focus

### Instruction Histograms
- Count instruction dispatch frequency
- Identify high-overhead opcodes
- Guide fusion/peephole decisions (Phase 5.5)

### Memory Profiling
```bash
python -m memory_profiler benchmark_script.py
```
- Track allocation hotspots
- Validate GC effectiveness (Phase 6)

## Documentation Requirements

### Per-Phase Deliverables
- Baseline results JSON for all microbenchmarks
- Comparison table vs previous phase (if applicable)
- Regression notes and explanations
- Profile data for key optimisations

### Optimisation Justification
When adding optimisations (Phase 5.5), document:
1. Benchmark showing problem (before data)
2. Optimisation description
3. Benchmark showing improvement (after data)
4. Trade-offs (code complexity, maintainability)

## Benchmark Evolution

### Adding New Benchmarks
- Propose in phase plan or separate doc
- Justify coverage gap
- Provide reference implementation (tree-walker)
- Add to CI if stable

### Deprecating Benchmarks
- If feature removed or superseded
- Archive results, document reason
- Keep code in `benchmarks/deprecated/` for history

## Tools & Scripts

### Benchmark Runner
Location: `prolog/tools/bench.py` (to be created)

```bash
# Run all microbenchmarks
uv run python -m prolog.tools.bench --category micro --engine wam

# Run specific benchmark with custom iterations
uv run python -m prolog.tools.bench --benchmark append_10 --runs 100

# Compare engines
uv run python -m prolog.tools.bench --compare tree,wam

# Output format
uv run python -m prolog.tools.bench --format json > results.jsonl
```

### Result Analyser
Location: `prolog/tools/bench_analyse.py` (to be created)

```bash
# Compare two result files
uv run python -m prolog.tools.bench_analyse results_p5.jsonl results_p5.5.jsonl

# Generate markdown report
uv run python -m prolog.tools.bench_analyse --report markdown results.jsonl > RESULTS.md

# Detect regressions
uv run python -m prolog.tools.bench_analyse --regression-threshold 0.15 old.jsonl new.jsonl
```

## Pyodide-Specific Considerations

### Browser Variability
- Test in Chrome, Firefox, Safari (different JS engines)
- Record browser version in environment
- Note JIT warmup differences (V8 vs SpiderMonkey vs JavaScriptCore)

### Message Passing Overhead
- Measure worker boundary crossing cost separately
- Batch solutions to amortise overhead
- Compare single-solution vs 100-solution queries

### Memory Constraints
- Browser memory limits lower than native
- Measure peak memory explicitly
- Validate GC triggers under constraint

## Review & Iteration

- Review benchmark suite quarterly
- Update targets based on implementation progress
- Retire obsolete benchmarks, add new coverage
- Maintain this document as methodology evolves

## References

- SWI-Prolog benchmarking: `swipl-bench` tool methodology
- WAM literature: typical operation counts for append/member
- Python profiling: official `cProfile` and `memory_profiler` docs
