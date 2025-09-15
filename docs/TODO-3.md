# TODO: Stage 3 (Debug & Observability)

**Key Policy**: step_id increments ONLY when an event is emitted after filters (post-filter). Filtered events never increment step_id.

## Phase 1: Core Infrastructure (3.0) - Minimal Ports Tracer

### 1. Create Debug Module Structure
- [ ] Create prolog/debug/ directory
- [ ] Create prolog/debug/__init__.py
- [ ] Create prolog/debug/tracer.py
- [ ] Create prolog/debug/events.py
- [ ] Add module docstrings explaining debug infrastructure
- [ ] Import necessary dependencies (Terms, Engine types)

### 2. Implement TraceEvent Dataclass
- [ ] Write test: TraceEvent contains all required fields
- [ ] Write test: TraceEvent schema version is 1
- [ ] Write test: TraceEvent run_id is valid UUID string
- [ ] Write test: Port must be one of 'call'|'exit'|'redo'|'fail'
- [ ] Write test: step_id is positive integer
- [ ] Write test: Depths and heights are non-negative integers
- [ ] Implement TraceEvent dataclass with frozen=True, slots=True
- [ ] Add validation for port values
- [ ] Add pretty and canonical goal representations
- [ ] Verify dataclass immutability

### 3. Implement PortsTracer Class
- [ ] Write test: PortsTracer initializes with engine reference
- [ ] Write test: Step counter starts at 0
- [ ] Write test: Run ID is generated as UUID
- [ ] Write test: Default bindings_policy is 'none'
- [ ] Write test: Default max_term_depth is 4
- [ ] Write test: Default max_items_per_list is 10
- [ ] Implement PortsTracer.__init__
- [ ] Implement step counter management (post-filter only)
- [ ] Implement run_id generation
- [ ] Add configuration attributes
- [ ] Add pred_id interning cache (reset per run_id)
- [ ] Verify initialization

### 4. Engine Integration Hooks
- [ ] Write test: Engine accepts trace parameter
- [ ] Write test: Engine creates PortsTracer when trace=True
- [ ] Write test: Engine.tracer is None when trace=False
- [ ] Write test: Engine._global_step_id not used (step_id in tracer)
- [ ] Modify Engine.__init__ to accept trace parameter
- [ ] Add tracer creation logic
- [ ] Add _trace_port stub method
- [ ] Verify integration points

### 5. Port Detection Logic
- [ ] Write test: CALL port emitted at goal entry
- [ ] Write test: EXIT port emitted on success
- [ ] Write test: FAIL port emitted on failure
- [ ] Write test: REDO port emitted on backtrack retry
- [ ] Write test: Port sequence follows valid transitions
- [ ] Implement _trace_port method in Engine
- [ ] Hook into _dispatch_predicate
- [ ] Hook into backtracking logic
- [ ] Verify port emission

### 6. Stack Depth Tracking & Invariants
- [ ] Write test: frame_depth equals len(frame_stack)
- [ ] Write test: cp_depth equals len(choicepoints)
- [ ] Write test: goal_height equals len(goal_stack)
- [ ] Write test: write_stamp is monotonic
- [ ] Write test: Depths update correctly during execution
- [ ] Write test: Depths restored correctly on backtrack
- [ ] Implement depth extraction in emit_event
- [ ] Add write_stamp tracking
- [ ] Create invariant_checker helper (reused in later phases)
- [ ] Verify invariants hold

### 7. Predicate ID Interning
- [ ] Write test: Predicate IDs format as "name/arity"
- [ ] Write test: Same predicate gets same ID
- [ ] Write test: Different predicates get different IDs
- [ ] Write test: Built-ins have correct IDs
- [ ] Write test: Interning cache resets per run_id
- [ ] Implement pred_id extraction
- [ ] Add interning cache for efficiency
- [ ] Verify ID consistency

### 8. Pretty Output Sink (Minimal)
- [ ] Write test: Pretty format is human-readable single line
- [ ] Write test: Format includes step_id in brackets
- [ ] Write test: Format includes port name
- [ ] Write test: Format includes goal pretty form
- [ ] Write test: Format includes frame and cp depths
- [ ] Write test: Long goals are truncated with ellipsis
- [ ] Write test: Caps apply (max_term_depth=4, max_items_per_list=10)
- [ ] Implement minimal PrettyTraceSink
- [ ] Format: "[123] call(5): append([1,2], [3], _G123) @ frame=2 cp=1"
- [ ] Add term truncation logic
- [ ] Direct stdout output (no buffering yet)
- [ ] Verify output format

## Phase 2: Metrics & Counters (3.2)

### 1. Create Metrics Module
- [ ] Create prolog/debug/metrics.py
- [ ] Add module docstring
- [ ] Import necessary types
- [ ] Set up module structure

### 2. PredMetrics Dataclass
- [ ] Write test: PredMetrics tracks calls/exits/fails/redos
- [ ] Write test: PredMetrics tracks unifications/backtracks
- [ ] Write test: All counters start at 0
- [ ] Write test: Counters increment correctly
- [ ] Implement PredMetrics dataclass with frozen=True, slots=True
- [ ] Add increment methods
- [ ] Verify counter accuracy

### 3. EngineMetrics Class
- [ ] Write test: Global counters start at 0
- [ ] Write test: unifications_attempted increments
- [ ] Write test: unifications_succeeded increments
- [ ] Write test: backtracks_taken increments
- [ ] Write test: cuts_executed increments
- [ ] Write test: alternatives_pruned increments
- [ ] Write test: exceptions_thrown/caught increment
- [ ] Write test: candidates_considered/yielded from indexing
- [ ] Implement EngineMetrics class
- [ ] Add per-predicate metrics dict
- [ ] Add reset() method
- [ ] Add to_dict() export method
- [ ] Verify metrics collection

### 4. Engine Integration for Metrics
- [ ] Write test: Engine accepts debug parameter
- [ ] Write test: Engine creates EngineMetrics when debug=True
- [ ] Write test: Metrics updated during unification
- [ ] Write test: Metrics updated during backtracking
- [ ] Write test: Metrics updated during cut
- [ ] Write test: Metrics reset between queries
- [ ] Write test: Zero overhead when debug=False
- [ ] Modify Engine.__init__ for debug parameter
- [ ] Hook metrics into unification
- [ ] Hook metrics into backtracking
- [ ] Hook metrics into cut execution
- [ ] Hook metrics into indexing (if available)
- [ ] Verify metric updates

## Phase 3: Snapshots & Diffs (3.1)

### 1. Create Snapshot Module
- [ ] Create prolog/debug/snapshot.py
- [ ] Add module docstring
- [ ] Import necessary types
- [ ] Set up module structure

### 2. Snapshot Dataclasses
- [ ] Write test: EngineSnapshot contains all heights and tops
- [ ] Write test: CPSnapshot captures choicepoint state
- [ ] Write test: FrameSnapshot captures frame state
- [ ] Write test: Snapshots are immutable
- [ ] Implement EngineSnapshot dataclass with frozen=True, slots=True
- [ ] Implement CPSnapshot dataclass with frozen=True, slots=True
- [ ] Implement FrameSnapshot dataclass with frozen=True, slots=True
- [ ] Add JSON serialization
- [ ] Verify dataclass structure

### 3. SnapshotManager Implementation
- [ ] Write test: snapshot() captures current engine state
- [ ] Write test: Store size via store.size() not store.cells
- [ ] Write test: Trail length matches actual trail
- [ ] Write test: Heights match stack sizes
- [ ] Write test: Choicepoints captured correctly
- [ ] Write test: Frames captured correctly
- [ ] Implement SnapshotManager.snapshot
- [ ] Extract all engine state (via public APIs only)
- [ ] Build structured snapshot
- [ ] Verify state capture

### 4. Diff Computation
- [ ] Write test: diff() computes height deltas
- [ ] Write test: diff() identifies CP changes
- [ ] Write test: diff() identifies frame changes
- [ ] Write test: diff() produces typed SnapshotDiff
- [ ] Write test: diff() handles identical snapshots
- [ ] Implement SnapshotManager.diff
- [ ] Compute structured differences
- [ ] Format human-readable output
- [ ] Format JSON output
- [ ] Verify diff accuracy

### 5. No-Growth Verification
- [ ] Write test: Pure backtracking shows no monotonic growth
- [ ] Write test: Heights stable across backtrack cycles
- [ ] Write test: member/2 iteration snapshots show no drift
- [ ] Write test: Trail compaction detected in diffs
- [ ] Write test: Memory hints included when available
- [ ] Add memory measurement utilities
- [ ] Verify no-growth property

## Phase 4: Output Sinks (3.4) - Pretty & JSONL

### 1. Create Sinks Module
- [ ] Create prolog/debug/sinks.py
- [ ] Add module docstring
- [ ] Import JSON utilities
- [ ] Set up module structure

### 2. TraceSink Abstract Base
- [ ] Write test: TraceSink is abstract
- [ ] Write test: TraceSink has write_event method
- [ ] Write test: TraceSink has ring buffer with maxlen
- [ ] Write test: TraceSink tracks events_dropped_total
- [ ] Implement TraceSink ABC
- [ ] Add ring buffer implementation
- [ ] Add drop counter logic
- [ ] Add batch buffer for efficiency
- [ ] Verify abstract interface

### 3. Enhanced PrettyTraceSink
- [ ] Write test: Buffered writes reduce I/O calls
- [ ] Write test: Output can be redirected to file
- [ ] Write test: Caps always applied (max_term_depth, max_items_per_list)
- [ ] Enhance PrettyTraceSink with buffering
- [ ] Add file output option
- [ ] Add buffer flushing logic
- [ ] Verify enhanced output

### 4. JSONLTraceSink Implementation
- [ ] Write test: JSONL outputs one JSON object per line
- [ ] Write test: Schema version is 1
- [ ] Write test: Compact keys used (sid, p, fd, etc.)
- [ ] Write test: Port encoded as integer (0-3)
- [ ] Write test: Optional fields omitted when None
- [ ] Write test: Bindings included when enabled
- [ ] Write test: Timestamps included when enabled
- [ ] Write test: Caps apply to bindings output
- [ ] Write test: run_id and query_id included
- [ ] Implement JSONLTraceSink.write_event
- [ ] Use compact schema format
- [ ] Encode ports as integers
- [ ] Handle optional fields
- [ ] Verify JSON validity

### 5. FileTraceSink with Rotation
- [ ] Write test: FileTraceSink writes to specified path
- [ ] Write test: File rotates at max_size_mb
- [ ] Write test: Keeps max_files rotated files
- [ ] Write test: Rotation preserves old files
- [ ] Write test: Batch writing reduces I/O
- [ ] Implement FileTraceSink class
- [ ] Add rotation logic
- [ ] Add batch buffer (flush every N events or M ms)
- [ ] Add flush on close
- [ ] Verify file operations

### 6. Backpressure Handling
- [ ] Write test: Buffer drops events when full
- [ ] Write test: Drop counters track lost events
- [ ] Write test: Drop reason recorded
- [ ] Write test: Engine continues despite drops
- [ ] Write test: step_id stays contiguous (only emitted events count)
- [ ] Write test: Engine results unaffected by drops
- [ ] Implement backpressure logic
- [ ] Add drop policies
- [ ] Add warning on drops
- [ ] Expose events_dropped_total in metrics
- [ ] Verify resilience

## Phase 5: Filters & Spypoints (3.0 continued)

### 1. Create Filters Module
- [ ] Create prolog/debug/filters.py
- [ ] Add module docstring
- [ ] Import filter types
- [ ] Set up module structure

### 2. TraceFilters Implementation
- [ ] Write test: Port filter allows only specified ports
- [ ] Write test: Predicate filter allows only specified predicates
- [ ] Write test: Depth range filters by frame depth
- [ ] Write test: Sampling reduces event count by factor
- [ ] Write test: Filter precedence is port→pred→depth→sample
- [ ] Write test: All filters applied correctly
- [ ] Write test: Filtered events don't increment step_id
- [ ] Implement TraceFilters class
- [ ] Implement should_emit logic
- [ ] Add filter combination
- [ ] Verify filter behavior

### 3. Spypoint Management
- [ ] Write test: Spypoints stored as (name, arity) tuples
- [ ] Write test: Only spypoint predicates traced when set
- [ ] Write test: Multiple spypoints can be active
- [ ] Write test: Spypoints cleared with unspy
- [ ] Implement spypoint set management
- [ ] Integrate with predicate filter
- [ ] Add spy/unspy logic
- [ ] Verify spypoint filtering

### 4. Filter Integration
- [ ] Write test: Tracer applies filters before emit
- [ ] Write test: step_id increments only for emitted events
- [ ] Write test: Sampling is deterministic with seed
- [ ] Hook filters into emit_event (before step_id increment)
- [ ] Optimize filter checking
- [ ] Verify performance impact

### 5. Variable Bindings Projection
- [ ] Write test: 'none' policy includes no bindings
- [ ] Write test: 'names' policy includes variable names only
- [ ] Write test: 'names_values' includes names and values
- [ ] Write test: Values respect max_term_depth
- [ ] Write test: Lists respect max_items_per_list
- [ ] Write test: Deterministic variable naming
- [ ] Write test: Same var gets same name in run
- [ ] Write test: Fresh vars get _G prefix
- [ ] Implement binding extraction logic
- [ ] Add policy configuration
- [ ] Add depth/length capping
- [ ] Use existing fresh-var map from engine
- [ ] Verify binding output

## Phase 6: Replay Tool & CI Integration (3.5)

### 1. Create Trace Replay Tool
- [ ] Create tools/replay_trace.py
- [ ] Write test: Tool reads JSONL traces
- [ ] Write test: Reconstructs last N steps (default 200)
- [ ] Write test: Validates trace consistency
- [ ] Write test: Detects invariant violations
- [ ] Write test: Uses invariant_checker from Phase 1
- [ ] Implement JSONL parser
- [ ] Implement step reconstruction
- [ ] Add validation logic
- [ ] Verify replay accuracy

### 2. CI Integration
- [ ] Update GitHub workflow
- [ ] Write test: CI captures traces on failure
- [ ] Write test: Artifacts include last 10MB traces
- [ ] Write test: Snapshot included in artifacts
- [ ] Add trace collection to CI
- [ ] Configure artifact upload
- [ ] Test failure scenarios
- [ ] Verify CI integration

### 3. Debug Scripts
- [ ] Create debug/analyze_trace.py
- [ ] Create debug/trace_stats.py
- [ ] Write test: Scripts parse JSONL correctly
- [ ] Write test: Statistics computed accurately
- [ ] Implement analysis tools
- [ ] Add statistical summaries
- [ ] Document tool usage

## Phase 7: Exporters (3.3)

### 1. Graph Export Module
- [ ] Create prolog/debug/exporters.py
- [ ] Add module docstring
- [ ] Import DOT utilities
- [ ] Set up module structure

### 2. Call Graph Exporter
- [ ] Write test: Call graph includes all predicates
- [ ] Write test: Edges represent calls
- [ ] Write test: Static analysis of clauses
- [ ] Write test: DOT format is valid
- [ ] Write test: Graphviz can render output
- [ ] Implement export_call_graph
- [ ] Parse clause bodies
- [ ] Build graph structure
- [ ] Generate DOT output
- [ ] Verify graph correctness

### 3. Constraint Graph Placeholder
- [ ] Write test: Constraint graph has placeholder nodes
- [ ] Write test: Respects variable identity
- [ ] Write test: DOT format is valid
- [ ] Write test: Ready for Stage 5 integration
- [ ] Implement export_constraint_graph
- [ ] Create placeholder structure
- [ ] Generate DOT output
- [ ] Document future integration

## Phase 8: Internal Debug Events (Optional Extension)

### 1. InternalEvent Class
- [ ] Write test: InternalEvent extends TraceEvent
- [ ] Write test: Kind field identifies event type
- [ ] Write test: Details dict contains event data
- [ ] Write test: Internal events off by default
- [ ] Write test: Enabling doesn't change 4-port stream
- [ ] Implement InternalEvent dataclass with frozen=True, slots=True
- [ ] Add kind validation
- [ ] Add details structure
- [ ] Verify event structure

### 2. Choicepoint Events
- [ ] Write test: cp_push event on choicepoint creation
- [ ] Write test: cp_pop event on choicepoint removal
- [ ] Write test: Events include CP kind and pred_id
- [ ] Write test: Trail_top included in CP events
- [ ] Hook into push_choicepoint
- [ ] Hook into pop_choicepoint
- [ ] Extract CP metadata
- [ ] Verify CP tracking

### 3. Frame Events
- [ ] Write test: frame_push on frame creation
- [ ] Write test: frame_pop on frame removal
- [ ] Write test: Events include frame_id and pred_id
- [ ] Hook into frame operations
- [ ] Extract frame metadata
- [ ] Verify frame tracking

### 4. Cut and Catch Events
- [ ] Write test: cut_commit event on cut execution
- [ ] Write test: Event includes alternatives_pruned count
- [ ] Write test: catch_switch event on exception catching
- [ ] Hook into cut execution
- [ ] Hook into catch mechanism
- [ ] Extract pruning counts
- [ ] Verify special events

## Phase 9: REPL Integration

### 1. Trace Commands
- [ ] Write test: 'trace on' enables tracing
- [ ] Write test: 'trace off' disables tracing
- [ ] Write test: 'trace json FILE' enables JSONL output
- [ ] Write test: 'trace pretty FILE' enables pretty output
- [ ] Write test: 'trace sample N' sets sampling rate
- [ ] Update REPL command parser
- [ ] Implement cmd_trace method
- [ ] Handle trace configuration
- [ ] Verify command behavior

### 2. Spy Commands
- [ ] Write test: 'spy name/arity' adds spypoint
- [ ] Write test: 'unspy name/arity' removes spypoint
- [ ] Write test: 'spys' lists all spypoints
- [ ] Write test: 'untrace' clears all spypoints
- [ ] Implement cmd_spy method
- [ ] Implement cmd_unspy method
- [ ] Implement cmd_spys method
- [ ] Verify spy management

### 3. Snapshot Commands
- [ ] Write test: 'snapshot' captures current state
- [ ] Write test: Snapshot output is readable
- [ ] Write test: Snapshot includes all key metrics
- [ ] Implement cmd_snapshot method
- [ ] Format snapshot output
- [ ] Verify snapshot display

### 4. Metrics Commands
- [ ] Write test: 'metrics' displays current metrics
- [ ] Write test: 'metrics reset' clears counters
- [ ] Write test: Metrics output is formatted nicely
- [ ] Implement cmd_metrics method
- [ ] Format metrics display
- [ ] Verify metrics output

## Phase 10: Determinism & Performance

### 1. Trace Determinism Tests
- [ ] Write test: Identical runs produce identical traces
- [ ] Write test: JSONL byte-for-byte equal with timestamps disabled
- [ ] Write test: Variable names deterministic
- [ ] Write test: Step IDs always sequential (post-filter)
- [ ] Write test: 4-port stream identical with use_indexing=False/True
- [ ] Disable timestamps for testing
- [ ] Verify deterministic output
- [ ] Document determinism guarantees

### 2. Port Sequence Validation
- [ ] Write test: CALL must precede EXIT/FAIL
- [ ] Write test: REDO only after EXIT
- [ ] Write test: No invalid transitions
- [ ] Write test: Sequence checker detects violations
- [ ] Implement sequence validator
- [ ] Add to test suite
- [ ] Verify sequences

### 3. Performance Testing
- [ ] Write test: ≤5% overhead with tracing disabled
- [ ] Write test: ≤20% overhead with pretty tracing
- [ ] Write test: ≤30% overhead with JSONL tracing
- [ ] Write test: ≤10% overhead with metrics only
- [ ] Create benchmark suite
- [ ] Measure baseline performance
- [ ] Measure with each feature
- [ ] Verify overhead targets

### 4. Memory Impact
- [ ] Write test: Memory bounded with large traces
- [ ] Write test: Ring buffer prevents unbounded growth
- [ ] Write test: File rotation prevents disk fill
- [ ] Write test: Interning cache bounded per run_id
- [ ] Measure memory usage
- [ ] Test with long-running queries
- [ ] Verify memory bounds

## Phase 11: Documentation

### 1. Create TRACE_FORMAT.md Stub
- [ ] Create docs/TRACE_FORMAT.md
- [ ] Document schema version 1
- [ ] Document compact keys (sid, p, fd, cd, gh, ws, pid, g, rid)
- [ ] Document port encoding (0=call, 1=exit, 2=redo, 3=fail)
- [ ] Document optional fields (b=bindings, t=timestamp)
- [ ] Note: "timestamps optional, determinism relies on step ordering"
- [ ] Add example traces
- [ ] Document parsing approach

### 2. User Documentation
- [ ] Update README with Stage 3 features
- [ ] Document trace commands
- [ ] Document spy commands
- [ ] Document snapshot usage
- [ ] Document metrics collection
- [ ] Add debugging guide
- [ ] Create troubleshooting section

### 3. API Documentation
- [ ] Document PortsTracer API
- [ ] Document TraceSink interface
- [ ] Document SnapshotManager API
- [ ] Document filter system
- [ ] Document exporter functions
- [ ] Add docstrings to all public methods
- [ ] Generate API docs

### 4. Performance Documentation
- [ ] Document overhead measurements
- [ ] Document optimization strategies
- [ ] Document memory usage
- [ ] Document best practices
- [ ] Add performance tips

## Phase 12: Integration & Final Testing

### 1. End-to-End Tests
- [ ] Write test: Complete trace of append/3
- [ ] Write test: Complete trace of member/2
- [ ] Write test: Trace with backtracking
- [ ] Write test: Trace with cut
- [ ] Write test: Trace with exceptions
- [ ] Run full scenario tests
- [ ] Verify trace completeness
- [ ] Check invariants using invariant_checker

### 2. Compatibility Tests
- [ ] Run all Stage 1 tests with tracing
- [ ] Run all Stage 1.5 tests with tracing
- [ ] Run all Stage 2 tests with tracing
- [ ] Verify no semantic changes
- [ ] Verify no test failures
- [ ] Check performance impact

### 3. Stress Tests
- [ ] Write test: 1M events without crash
- [ ] Write test: 100MB trace file handling
- [ ] Write test: Deep recursion tracing
- [ ] Write test: Many spypoints active
- [ ] Run stress scenarios
- [ ] Verify stability

## Phase 13: Final Validation

### 1. Acceptance Criteria Verification
- [ ] ✓ ≤5% overhead with tracing disabled
- [ ] ✓ ≤20% overhead with pretty tracing
- [ ] ✓ ≤30% overhead with JSONL tracing
- [ ] ✓ Traces deterministic (timestamps disabled)
- [ ] ✓ All invariants maintained
- [ ] ✓ Valid port sequences only
- [ ] ✓ Snapshot/diff working correctly
- [ ] ✓ Metrics accurate
- [ ] ✓ Filters working
- [ ] ✓ Spypoints working
- [ ] ✓ REPL integration complete
- [ ] ✓ File rotation working
- [ ] ✓ Exporters generating valid DOT
- [ ] ✓ Replay tool functional
- [ ] ✓ CI integration complete

### 2. Code Quality
- [ ] Run black formatter
- [ ] Remove debug prints
- [ ] Review TODOs
- [ ] Check test coverage
- [ ] Run linter
- [ ] Fix warnings

### 3. Final Testing
- [ ] Full test suite passes
- [ ] Performance tests pass
- [ ] Integration tests pass
- [ ] Property tests pass
- [ ] Manual testing complete

### 4. Stage 3 Completion
- [ ] Create PR for Stage 3
- [ ] Ensure CI passes
- [ ] Code review completed
- [ ] Merge to main
- [ ] Update project status
- [ ] Stage 3 complete

## Implementation Notes

### Key Design Decisions
- **step_id policy**: Increments ONLY after filters, in emit_event
- **Dataclass design**: All use frozen=True, slots=True for immutability and memory efficiency
- **Encapsulation**: Tracer uses only public engine APIs (store.size(), not store.cells)
- **Interning**: pred_id cache resets per run_id to prevent unbounded growth
- **Backpressure**: Ring buffer with batch flush, events_dropped_total exposed
- **Determinism**: Timestamps optional, byte-for-byte reproducibility when disabled
- **Caps by default**: max_term_depth=4, max_items_per_list=10, bindings_policy='none'

### Performance Targets
- Tracing disabled: ≤5% overhead (null checks only)
- Pretty tracing: ≤20% overhead
- JSONL tracing: ≤30% overhead
- Metrics only: ≤10% overhead

### Testing Strategy
- **Invariant checker**: Created once in Phase 1, reused throughout
- **Indexing parity**: 4-port stream identical with/without indexing
- **No-growth property**: Snapshots over pure backtracking show no drift
- **Determinism**: JSONL traces byte-for-byte identical (timestamps off)

### Phase Dependencies
- Phase 1 creates invariant_checker used by Phases 6, 10, 12
- Phase 2 metrics exposed in Phase 4 backpressure
- Phase 3 snapshots used in CI (Phase 6)
- Filters (Phase 5) must come after sinks (Phase 4)
- TRACE_FORMAT.md stub (Phase 11) can be created early

### Integration Points
- Engine hooks: _trace_port, metrics updates
- REPL commands: trace, spy, snapshot, metrics
- CI: Artifact collection on failure
- Tools: replay_trace.py for debugging