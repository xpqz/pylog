# Performance Benchmarks

This directory contains performance benchmarks for PyLog's core systems:
- **Tracing Infrastructure**: Call/exit/redo/fail port tracing overhead
- **Indexing Performance**: First-argument and type indexing optimization
- **Reification Performance**: CLP(FD) reification constraint overhead

## Running Benchmarks

### Run all benchmarks without enforcement
```bash
# Just run and report times (default behavior)
uv run pytest prolog/tests/benchmarks/ -v

# Run only perf-marked tests
uv run pytest prolog/tests/benchmarks/ -v -m perf

# Exclude slow tests
uv run pytest prolog/tests/benchmarks/ -v -m "perf and not slow"
```

### Run benchmarks with performance enforcement
```bash
# Enable performance assertions (accepts 1, true, yes, on)
CI_ENFORCE_PERF=true uv run pytest prolog/tests/benchmarks/ -v

# With reduced hashset noise in CI
PYTHONHASHSEED=0 CI_ENFORCE_PERF=1 uv run pytest prolog/tests/benchmarks/ -v

# This will fail if performance targets are not met
```

### Environment Variables

#### Enforcement Control
- `CI_ENFORCE_PERF`: Enable performance assertions (values: `1`, `true`, `yes`, `on`)

#### Configurable Thresholds

**Tracing Thresholds:**
- `PERF_INFRASTRUCTURE_MAX`: Max overhead for trace infrastructure (default: 5.0%)
- `PERF_PRETTY_MAX`: Max overhead for pretty tracing (default: 25.0%)
- `PERF_JSONL_MAX`: Max overhead for JSONL tracing (default: 35.0%)
- `PERF_COLLECTOR_MAX`: Max overhead for CollectorSink (default: 15.0%)
- `PERF_BACKTRACKING_MAX`: Max overhead with heavy backtracking (default: 45.0%)
- `PERF_SCALING_MAX`: Max scaling factor large/small (default: 3.0x)
- `PERF_FIRST_EVENT_MAX`: Max overhead for first event (default: 5.0x)
- `PERF_EVENT_RATE_MIN`: Min events per second (default: 1000)

**Reification Thresholds:**
- `PERF_REIF_MODULE_MAX`: Max module loading overhead (default: 5.0%)
- `PERF_REIF_SIMPLE_MAX`: Max simple reification overhead (default: 20.0%)
- `PERF_REIF_CONSTRAINT_TYPES_MAX`: Max overhead per constraint type (default: 30.0%)
- `PERF_REIF_POSTING_MAX`: Max constraint posting overhead (default: 15.0%)
- `PERF_REIF_SCALING_MAX`: Max scaling factor for chain constraints (default: calculated)
- `PERF_REIF_NETWORK_SCALING_MAX`: Max scaling factor for network constraints (default: calculated)
- `PERF_REIF_MEMORY_MAX`: Max memory overhead (default: 50.0%)

Example with custom thresholds:
```bash
CI_ENFORCE_PERF=true PERF_PRETTY_MAX=30 PERF_JSONL_MAX=40 uv run pytest prolog/tests/benchmarks/ -v
```

## Performance Targets

When `CI_ENFORCE_PERF=1` is set, the following targets are enforced:

- **Trace infrastructure overhead**: ≤5% (trace=True with no-op filter)
- **Pretty tracing overhead**: ≤25% (human-readable output to file)
- **JSONL tracing overhead**: ≤35% (machine-readable output to file)
- **CollectorSink overhead**: ≤15% (in-memory collection, no I/O)
- **Heavy backtracking overhead**: ≤45% (with complex backtracking patterns)
- **Scaling factor**: ≤3x (large programs vs small programs)
- **Time to first event**: ≤5x overhead on trivial queries
- **Event creation rate**: ≥1000 events/sec

## Benchmark Categories

### Tracing Performance (`test_trace_performance_overhead.py`)
- `TestTracingOverhead`: Basic overhead measurements for different sink types
- `TestScalabilityOverhead`: How overhead scales with program size
- `TestTracingWithBacktracking`: Overhead with heavy backtracking
- `TestMicroBenchmarks`: Time to first event and event creation rate
- `test_sink_performance_comparison`: Side-by-side comparison of sink types

### Indexing Performance (`test_indexing_performance.py`)
- First-argument indexing optimization benchmarks
- Type-based indexing performance tests
- Clause selection scaling tests

### Reification Performance (`test_reification_performance.py`)
- `TestReificationPerformance`: Comprehensive reification overhead analysis
  - **Baseline Tests**: Ensure reification doesn't slow down non-reified constraints
  - **Overhead Tests**: Measure reification vs direct constraint posting
  - **Scaling Tests**: Chain and network constraint scaling behavior
  - **Pathological Cases**: Infinite loop prevention and deep nesting
  - **Memory Tests**: Memory overhead measurement
  - **Scenario Tests**: Boolean circuits and conditional scheduling

## Implementation Notes

### Methodology
- Uses **median** instead of mean for robustness
- Includes **warmup iterations** to avoid JIT effects
- **Disables GC** during measurement for consistency
- Reports **IQR** (interquartile range) for variability
- Runs `gc.collect()` before each measurement

### Test Structure
All performance tests:
1. Create a baseline measurement (trace=False)
2. Create a traced measurement (trace=True with sink)
3. Calculate overhead percentage
4. Report results to stdout
5. Conditionally assert based on `CI_ENFORCE_PERF`

### Adding New Benchmarks
When adding new benchmarks:
1. Mark with `@pytest.mark.perf`
2. Add `@pytest.mark.slow` if test takes >1s
3. Use `should_enforce()` method for conditional assertions
4. Report results even when not enforcing
5. Use the `measure_execution_time()` helper for consistency

## CI Integration

In CI pipelines, you can:

1. **Development branches**: Run without enforcement to track trends
   ```yaml
   - run: uv run pytest prolog/tests/benchmarks/ -v
   ```

2. **Release branches**: Enforce performance targets
   ```yaml
   - run: CI_ENFORCE_PERF=1 uv run pytest prolog/tests/benchmarks/ -v
   ```

3. **Nightly builds**: Run full slow suite with enforcement
   ```yaml
   - run: CI_ENFORCE_PERF=1 uv run pytest prolog/tests/benchmarks/ -v -m "perf"
   ```

## Interpreting Results

Output includes:
- **Median times**: More stable than mean
- **Overhead percentages**: Relative cost of tracing
- **Scaling factors**: How overhead changes with program size
- **Event rates**: Throughput metrics

Example output:
```
Trace infrastructure overhead: 3.2% (median: 0.052s vs 0.050s)
Pretty tracing overhead: 18.5% (median: 0.059s vs 0.050s)
JSONL tracing overhead: 22.1% (median: 0.061s vs 0.050s)
CollectorSink overhead: 8.3% (median: 0.054s vs 0.050s)
```

## Troubleshooting

### High variance between runs
- Increase iteration count in `measure_execution_time()`
- Check for background processes
- Ensure consistent CPU governor settings

### Failing performance targets
- Profile with `cProfile` to find hotspots
- Check for unnecessary allocations in trace path
- Verify filter predicates are efficient
- Consider batching trace events

### Platform differences
- macOS/Linux/Windows may have different I/O characteristics
- Adjust targets based on platform if needed
- Use platform-specific markers if necessary