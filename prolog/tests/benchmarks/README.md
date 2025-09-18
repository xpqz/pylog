# Performance Benchmarks

This directory contains performance benchmarks for PyLog's tracing infrastructure.

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
# Enable performance assertions in CI
CI_ENFORCE_PERF=1 uv run pytest prolog/tests/benchmarks/ -v

# This will fail if performance targets are not met
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

### Overhead Tests
- `TestTracingOverhead`: Basic overhead measurements for different sink types
- `TestScalabilityOverhead`: How overhead scales with program size
- `TestTracingWithBacktracking`: Overhead with heavy backtracking

### Micro Benchmarks
- `TestMicroBenchmarks.test_time_to_first_event`: Overhead for the first trace event
- `TestMicroBenchmarks.test_event_creation_rate`: Raw event creation throughput

### Comparison Tests
- `test_sink_performance_comparison`: Side-by-side comparison of sink types

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