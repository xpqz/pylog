# Client-Only Web REPL for PyLog — Implementation Plan

This plan describes how to ship a fully client-side "Try PyLog" REPL that runs entirely in the browser (no backend) using WebAssembly via Pyodide. Scope is an embeddable widget you can place on the homepage and documentation pages.

## Goals

- **100% client-only**: no server process required once assets are served
- **Quick startup UX**: lazy-load Pyodide only when the user clicks "Try"
- **Responsive UI**: run the engine inside a Web Worker; support cancel/abort
- **Safety**: step/time budgets; disable/avoid unsafe built-ins; guard memory
- **Feature set**: load examples, run queries, stream solutions, pretty-print terms, basic tracing toggle (optional)

## Non-goals (initial)

- File I/O, consulting files from disk
- Network access from Prolog
- Full REPL richness (history across sessions, modules/files, consulting URLs)
  - These can come later

## Architecture Overview

### Main Thread (UI)

- Minimal terminal-like view (xterm.js optional) or simple textarea + Run button
- "Load examples" menu with canned snippets:
  - Hello: `append([1,2],[3,4],X)`
  - Lists: `member(X,[a,b,c])`
  - CLP(FD): `X in 1..10, X #> 5, label([X])`
- Lazy load: only load Pyodide/worker on first interaction

### Worker (Pyodide Runtime)

- Boot Pyodide, load PyLog package (wheel) via micropip
- Expose `init`/`run`/`abort`/`reset` methods over postMessage
- Construct a PyLog Engine per session; parse with Reader; run queries; yield solutions
- Enforce limits: `max_steps`, `max_solutions`, execution timeout; track memory usage if possible

## PyLog Architecture Analysis

### Core Compatibility

**Excellent Pyodide compatibility:**
- Pure Python codebase (~106K LOC, no native extensions)
- All dependencies are pure Python:
  - `lark>=1.1.0` (parser generator) ✅
  - `prompt-toolkit>=3.0.52` (not needed for web)
  - `pygments>=2.19.2` (not needed for web)

### Core API

Clean separation between engine and REPL:
```python
from prolog.parser.parser import parse_query, parse_program
from prolog.engine.engine import Engine
from prolog.ast.clauses import Program
from prolog.ast.pretty import pretty

# Parse query
goals = parse_query("?- member(X,[1,2,3]).")

# Create engine with empty program
engine = Engine(Program(tuple()))

# Run query
solutions = engine.run(goals)
# Returns: [{'X': Int(1)}, {'X': Int(2)}, {'X': Int(3)}]

# Pretty-print terms
for sol in solutions:
    for var, val in sol.items():
        print(f"{var} = {pretty(val)}")
```

### Engine Options

```python
Engine(
    program,                    # Program(clauses)
    occurs_check=False,        # Occurs check in unification
    max_solutions=None,        # Limit solutions
    max_steps=None,            # Step budget (safety)
    trace=False,               # Enable tracing
    use_indexing=False,        # First-argument indexing
    mode="dev"                 # "dev" or "iso"
)
```

**For web demo**: Use `max_steps=100000`, `max_solutions=100` as defaults.

### Key Risks Identified

1. **Lark in Pyodide**: Need to verify lark wheel works in Pyodide (likely yes - pure Python)
2. **Wheel size**: PyLog + lark may be 1-2 MB compressed
3. **Startup time**: Pyodide itself is ~6-8 MB, takes 3-5 seconds to initialize
4. **Memory**: Browser WASM limits (~2GB typical, but varies)
5. **File I/O**: `consult()` builtin needs special handling

## Packaging Options

### Option A: Wheel via micropip (RECOMMENDED)

**Approach:**
- Build pure-Python wheel from existing `pyproject.toml`
- Host wheel in `mkdocs/docs/try/assets/pylog-VERSION.whl`
- Load in worker: `await micropip.install('./assets/pylog-VERSION.whl')`

**Pros:**
- Clean upgrade path; reuses CI artifacts
- Small footprint; standard distribution
- Easy to version and cache

**Cons:**
- Wheel fetch adds one network request (~1-2MB)
- Requires build step in CI

### Option B: Embedded package tree

**Approach:**
- Copy `prolog/` directory to `mkdocs/docs/try/pkg/`
- Mount in Pyodide FS: `pyodide.FS.mount(...)`
- Add to path: `sys.path.insert(0, '/pkg')`

**Pros:**
- No micropip; works fully offline with site assets
- No build step

**Cons:**
- Larger repo footprint (~500+ files)
- Must keep in sync with source manually
- Less standard

**Decision:** Use Option A for cleaner separation and standard packaging.

## Worker API (postMessage RPC)

### Message Types: Main → Worker

#### `init`
```javascript
{ type: 'init' }
```
Load Pyodide, install PyLog, import modules; returns version info.

#### `run`
```javascript
{
  type: 'run',
  code: "X = 1, Y is X + 1",
  options: {
    maxSteps: 100000,      // Step budget (optional)
    maxSolutions: 100,     // Solution limit (optional)
    trace: false           // Enable tracing (optional)
  }
}
```
Parse the text into a query and execute. Stream results as `solution` events.

#### `abort`
```javascript
{ type: 'abort' }
```
Cancel the current run (terminates worker as last resort).

#### `reset`
```javascript
{ type: 'reset' }
```
Tear down the Engine and create a fresh one (clear state between runs).

### Message Types: Worker → Main

#### `ready`
```javascript
{
  type: 'ready',
  pylogVersion: '0.1.0',
  pyodideVersion: '0.24.1'
}
```

#### `solution`
```javascript
{
  type: 'solution',
  index: 0,                                    // Solution number (0-based)
  bindings: {
    X: { kind: 'Int', value: 1 },
    Y: { kind: 'Int', value: 2 }
  },
  pretty: "X = 1, Y = 2"                      // Pre-formatted string
}
```

#### `done`
```javascript
{
  type: 'done',
  solutions: 1,
  elapsedMs: 3
}
```

#### `error`
```javascript
{
  type: 'error',
  message: 'ReaderError at position 5: unexpected token',
  position: 5,              // Optional: character position
  token: '@@'               // Optional: problematic token
}
```

#### `progress`
```javascript
{
  type: 'progress',
  message: 'Loading Pyodide...'
}
```

## Runtime Constraints and Adaptations

### File I/O Built-ins

**Issue:** `consult(filename)` tries to read files from disk.

**Solution:**
- Pyodide provides in-memory FS by default (MEMFS)
- Option 1: Disable file operations entirely (stub with error message)
- Option 2: Allow MEMFS operations only
- Option 3: Provide `consult(user)` alternative (paste program in textarea)

**Recommendation:** Stub file operations with helpful message:
```
?- consult('foo.pl').
ERROR: File operations not available in web demo.
       Use the program editor or consult(user) instead.
```

### Streams and JSON

**Good news:** JSON built-ins already work with Python file-like objects (`StringIO`). These work fine in Pyodide.

### Environment Variables

Code that reads `os.environ.get(...)` generally works in Pyodide but shouldn't be relied on. Use explicit options in the demo.

### Threading

Not used by PyLog. Keep everything single-threaded within the Worker.

## Limits and Safety

### Step Budget
Pass `max_steps` to Engine. Also maintain a wall-clock timeout in Worker (e.g., 10 seconds).

### Max Solutions
Clip to a reasonable number for demos (e.g., 100) to prevent runaway enumeration.

### Output Limits
- Truncate very large term prints with a "… +N more …" marker
- Allow toggling full expansion (up to reasonable limit)
- Limit output area to 1000 lines; auto-scroll to bottom

### Memory Guard
Optional soft limit (e.g., stop if list grows beyond threshold) to avoid OOM. May not be feasible initially.

### Abort Mechanism
- First attempt: Set flag in worker, check periodically in Engine loop
- Last resort: Terminate worker and restart (loses state)

## Performance Considerations

### Lazy Load
Defer Pyodide (~6-8 MB) until user clicks "Try It" button or first interaction.

### Cache
- Rely on HTTP caching for Pyodide assets
- Consider service worker to pre-cache Pyodide + wheel for repeat visits
- Show download progress on first load

### Worker
Keep UI responsive. "Stop" button posts abort message and can terminate worker if needed.

### Initial Load Sequence
1. User clicks "Try It" → Show loading spinner
2. Load Pyodide from CDN (3-5 seconds)
3. Install PyLog wheel via micropip (1-2 seconds)
4. Import modules and initialize Engine
5. Send `ready` message → Enable input

**Total first-load time:** 5-10 seconds typical

## Build &amp; Hosting

### File Structure
```
mkdocs/docs/try/
├── index.html          # Container markup + example list + links to docs
├── pyrepl.js           # UI glue (main thread), renders output, manages state
├── worker.js           # Boots Pyodide; loads PyLog; handles RPC
├── examples.js         # Canned query library
├── assets/
│   └── pylog-0.1.0.whl # PyLog wheel (built from CI)
└── README.md           # Implementation notes
```

### MkDocs Integration
Add to `mkdocs.yml`:
```yaml
nav:
  - Try PyLog: try/index.md
```

Create simple landing page `mkdocs/docs/try/index.md` that embeds the REPL widget.

### Homepage Callout
Add small "Try PyLog in your browser" card on homepage (`mkdocs/docs/index.md`) linking to `/try/`.

## Staged Roadmap

### Stage 1: MVP (Core Functionality)

**Goal:** Working REPL that runs simple queries

**Tasks:**
1. Build pure-Python wheel suitable for Pyodide
2. Create `mkdocs/docs/try/index.html`, `pyrepl.js`, `worker.js` (plain JS for simplicity)
3. Worker loads Pyodide, installs wheel, imports:
   - `prolog.engine.engine.Engine`
   - `prolog.parser.parser.parse_query, parse_program`
   - `prolog.ast.clauses.Program`
   - `prolog.ast.pretty.pretty`
4. Run path:
   - `parse_query(user_text)` → goals
   - `Engine(Program(())).run(goals)` → solutions
   - Stream JSON bindings via postMessage
5. Implement limits: `max_steps`, `max_solutions`, wall-clock timeout, abort
6. Wire homepage callout ("Try PyLog in your browser — no install")

**UI:**
- `<textarea id="input"></textarea>`
- `<button id="run">Run</button>` `<button id="stop">Stop</button>`
- `<pre id="output"></pre>`
- Script wires buttons to postMessage; appends solution lines to output; disables Run during execution

**Deliverable:** Self-contained demo page running basic queries

### Stage 2: Examples &amp; Polish

**Tasks:**
1. Example library:
   - Dropdown menu with canned queries
   - Load from embedded `examples.js` or fetch from `prolog/examples/*.pl`
   - Categories: Lists, Arithmetic, CLP(FD), Hanoi, Sudoku
2. Error handling:
   - Pretty-print `ReaderError` with position highlighting
   - Show parse errors with column markers
   - Link unknown operators to docs
3. Output formatting:
   - Toggle compact/verbose mode
   - Copy solution button
   - Syntax highlighting for output (optional)
4. Loading UX:
   - Progress bar during Pyodide initialization
   - Message: "Downloading Python runtime (6 MB, first visit only)..."

**Deliverable:** Polished demo with examples and better error messages

### Stage 3: Advanced Features

**Tasks:**
1. Terminal experience:
   - Integrate xterm.js for shell feel
   - Multi-line input with Shift+Enter
   - Command history navigation (up/down arrows)
   - Paging for many solutions (press Space for next)
2. State management:
   - `consult(user)` via textarea (multi-line program input mode)
   - `listing()` to view loaded clauses
   - localStorage persistence (opt-in checkbox)
3. Tracing preview:
   - Toggle trace mode checkbox
   - Show port counts (CALL/EXIT/REDO/FAIL) summary
   - Link to full tracing guide in docs

**Deliverable:** Feature-rich REPL approaching native experience

### Stage 4: Production Polish

**Tasks:**
1. Performance:
   - Service worker for Pyodide caching
   - Preload on homepage scroll (optional)
   - Measure and optimize load time
2. Integration:
   - Homepage "Try PyLog" callout card with screenshot
   - Embeddable iframe snippet for docs pages
   - Deep links to pre-filled queries (`?q=append([1],[2],X)`)
3. Documentation:
   - "How it works" section
   - Limitations page (no file I/O, no networking, memory limits)
   - Embed guide for other sites
4. Testing:
   - Browser compatibility matrix (Chrome, Firefox, Safari, Edge)
   - Mobile testing (iOS Safari, Chrome Android)
   - Load testing (100+ solutions)
   - Error recovery testing (infinite loops, OOM)

**Deliverable:** Production-ready web REPL

## Implementation Checklist

### Pre-implementation
- [ ] Verify lark wheel works in Pyodide (test in browser console)
- [ ] Measure PyLog wheel size (compressed and uncompressed)
- [ ] Test basic Engine usage in Pyodide REPL

### Stage 1: MVP
- [ ] Build PyLog wheel suitable for Pyodide (pure Python; no native deps)
- [ ] Create `mkdocs/docs/try/` directory structure
- [ ] Implement `worker.js`:
  - [ ] Load Pyodide from CDN
  - [ ] Install PyLog wheel via micropip
  - [ ] Import Engine, parser, Program, pretty
  - [ ] Implement `init` handler
  - [ ] Implement `run` handler with solution streaming
  - [ ] Implement `abort` handler
  - [ ] Implement `reset` handler
- [ ] Implement `pyrepl.js`:
  - [ ] Worker lifecycle management
  - [ ] Message routing (init, run, abort, reset)
  - [ ] Solution rendering
  - [ ] Error display
  - [ ] Loading states
- [ ] Implement `index.html`:
  - [ ] Input textarea
  - [ ] Run/Stop buttons
  - [ ] Output area
  - [ ] Loading spinner
- [ ] Wire homepage callout in `mkdocs/docs/index.md`
- [ ] Test basic queries:
  - [ ] `X = 1`
  - [ ] `append([1,2],[3,4],X)`
  - [ ] `member(X,[a,b,c])`
  - [ ] Error: `X @@@ Y` (unknown operator)
  - [ ] Infinite: `loop :- loop. ?- loop.` (hits step limit)

### Stage 2: Examples &amp; Polish
- [ ] Create `examples.js` with canned queries
- [ ] Add example dropdown menu
- [ ] Implement error formatting with position highlighting
- [ ] Add copy solution button
- [ ] Implement loading progress indicator
- [ ] Test with examples from `prolog/examples/`

### Stage 3: Advanced Features
- [ ] Integrate xterm.js (optional)
- [ ] Add multi-line input mode
- [ ] Implement command history
- [ ] Add `consult(user)` support
- [ ] Implement `listing()` display
- [ ] Add localStorage persistence (opt-in)
- [ ] Add trace toggle and summary display

### Stage 4: Production Polish
- [ ] Implement service worker for caching
- [ ] Add deep linking support
- [ ] Write "How it works" documentation
- [ ] Create embed guide
- [ ] Browser compatibility testing
- [ ] Mobile testing
- [ ] Performance benchmarking
- [ ] Load testing with large solutions

## Open Questions

### Technical

1. **Lark in Pyodide**: Does lark wheel work in Pyodide without modification?
   - **Test:** Try `micropip.install('lark')` in Pyodide REPL
   - **Likely:** Yes, lark is pure Python

2. **Wheel size**: What's the actual download size impact?
   - **Action:** Build wheel and measure (gzip compressed)
   - **Estimate:** 500 KB - 2 MB compressed

3. **CLP(FD) performance**: How well does constraint solving perform in WASM?
   - **Test:** Run Sudoku and N-queens in browser
   - **Likely:** Slower than native (~2-5x), but acceptable for demos

4. **Memory limits**: Can we detect and handle browser OOM gracefully?
   - **Research:** Pyodide memory management APIs
   - **Fallback:** Rely on step limits as primary safety mechanism

### UX &amp; Product

5. **localStorage policy**: Require opt-in or auto-persist?
   - **Recommendation:** Opt-in checkbox (privacy-friendly)
   - **Clear button:** Always provide way to wipe state

6. **Example complexity**: How much CLP(FD) to expose by default?
   - **Recommendation:** Start simple (linear constraints only)
   - **Gate heavy demos** behind "Advanced" category with compute warning

7. **Error verbosity**: Full Prolog error terms or simplified messages?
   - **Recommendation:** Simplified by default, toggle for "full error"

8. **History persistence**: Across sessions or single-session only?
   - **Recommendation:** Single-session by default, opt-in for persistence

## Success Metrics

### Performance
- Page loads in &lt;3s (with cached assets)
- Pyodide initializes in &lt;5s (first load)
- Simple queries run in &lt;100ms
- CLP(FD) queries run in &lt;2s (Sudoku row)

### Reliability
- No crashes on runaway recursion (step limits work)
- Graceful degradation on OOM
- Works in 95%+ of browser versions (last 2 major versions)

### Usability
- Users can run first query within 10 seconds of page load
- Error messages are helpful and actionable
- Examples cover 80% of common use cases

## Security Considerations

### Sandboxing
- Pyodide runs in Web Worker (isolated from main thread)
- No access to user's filesystem
- No network access from Prolog code
- WASM provides memory safety

### Resource Limits
- Step budget prevents infinite loops
- Solution limit prevents memory exhaustion
- Wall-clock timeout prevents UI freeze
- Worker termination as last resort

### User Data
- No data sent to server (100% client-side)
- localStorage is optional and user-controlled
- No analytics or tracking (unless MkDocs adds it)

## Alternatives Considered

### PyScript
**Rejected:** PyScript adds extra abstraction layer and is heavier than raw Pyodide.

### Server-Side REPL
**Rejected:** Requires infrastructure, introduces security concerns, and adds latency.

### WASM Direct Compilation
**Rejected:** Would require rewriting PyLog in Rust/C++/Go. Massive effort, loses Python advantages.

### JupyterLite
**Rejected:** Too heavy for simple REPL embed. Overkill for our use case.

## Future Enhancements (Post-MVP)

### Near-term
- [ ] PWA: Offline support with service worker
- [ ] Share query via URL (encode query in hash fragment)
- [ ] Export solutions as JSON/CSV
- [ ] Dark mode toggle

### Long-term
- [ ] Multi-file projects (tabs or file tree)
- [ ] Consult from URL (with CORS support)
- [ ] Debugger UI (step through execution)
- [ ] Collaborative REPL (share session via WebRTC)

## References

### Pyodide
- Main site: https://pyodide.org
- Loading packages: https://pyodide.org/en/stable/usage/loading-packages.html
- micropip: https://pyodide.org/en/stable/usage/api/micropip-api.html

### PyLog
- Engine API: `prolog/engine/engine.py`
- Parser API: `prolog/parser/parser.py`
- Pretty printer: `prolog/ast/pretty.py`
- Examples: `prolog/examples/`

### Web Workers
- MDN: https://developer.mozilla.org/en/docs/Web/API/Web_Workers_API/Using_web_workers

### xterm.js (optional)
- Main site: https://xtermjs.org/
- Basic example: https://xtermjs.org/docs/guides/using-addons/

## Rollout Strategy

1. **Create feature branch:** `web-repl`
2. **Stage 1 PR:** MVP REPL (self-contained, docs pages only)
   - No changes to main PyLog code
   - Only add files under `mkdocs/docs/try/`
   - Add small callout to homepage
3. **Stage 2 PR:** Examples + polish
4. **Stage 3 PR:** Advanced features (optional, can defer)
5. **Announce:** Blog post, social media, Show HN

## Maintenance Plan

### Wheel Updates
- Build wheel in CI on each release
- Upload to `mkdocs/docs/try/assets/pylog-VERSION.whl`
- Update version reference in `worker.js`
- Test that new wheel loads in browser

### Pyodide Updates
- Monitor Pyodide releases
- Test new versions for compatibility
- Update CDN URL in `worker.js`

### Documentation
- Keep examples in sync with PyLog features
- Update limitations page when constraints are lifted
- Maintain browser compatibility matrix

## Conclusion

This plan provides a clear, staged roadmap to implement a fully client-side web REPL for PyLog. The approach minimizes risk by:

1. **Starting with MVP:** Prove core functionality works before investing in polish
2. **Leveraging existing code:** No changes to PyLog engine required
3. **Using standard tools:** Pyodide, micropip, Web Workers
4. **Implementing safety limits:** Step budgets, solution limits, timeouts
5. **Providing clear fallbacks:** Error messages, abort mechanisms

The staged approach allows for incremental delivery and validation at each step. Stage 1 alone provides significant value, while later stages add polish and advanced features.
