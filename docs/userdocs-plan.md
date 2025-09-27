PyLog User Documentation Plan

Purpose

- Provide clear, incremental user-facing docs for PyLog.
- Guide users from zero-to-productive in the REPL, through ISO‑style Prolog basics, into CLP(FD) modeling and solving.
- Serve as the content blueprint for a mkdocs‑material site (navigation, pages, and examples).

Audience and Assumptions

- Users with basic programming knowledge; no prior Prolog required.
- Focus on practical usage (REPL-centric), with links to deeper internals where helpful.
- Show runnable examples; prefer small, self‑contained snippets.

Style and Conventions

- Prefer REPL transcripts for examples; annotate key points succinctly.
- Use admonitions for notes, tips, and warnings.
- Use consistent query/result formatting:

  ```text
  ?- goal(args).
  true.  % or results shown as bindings
  ```

- Use “Prolog” code fences for source snippets and “text” for REPL transcripts.
- Cross‑link related topics (e.g., arithmetic → operators; CLP(FD) → labeling).

mkdocs‑material Structure (nav skeleton)

- Home: index.md
- Getting Started:
  - getting-started/install.md
  - getting-started/repl.md
  - getting-started/hello-prolog.md
- Prolog Basics:
  - basics/terms.md
  - basics/facts-and-rules.md
  - basics/queries-and-unification.md
  - basics/lists-and-structures.md
  - basics/control.md
  - basics/arithmetic.md
  - basics/operators.md
  - basics/builtins.md
- CLP(FD):
  - clpfd/intro.md
  - clpfd/domains-and-in.md
  - clpfd/linear-constraints.md
  - clpfd/sum-constraints.md
  - clpfd/all-different.md
  - clpfd/labeling.md
  - clpfd/examples.md
- Cookbook:
  - cookbook/family-relations.md
  - cookbook/graph-reachability.md
  - cookbook/send-more-money.md
  - cookbook/sudoku-row.md
  - cookbook/n-queens.md
  - cookbook/scheduling.md
- Reference:
  - reference/builtins-reference.md
  - reference/operators-table.md
  - reference/grammar-summary.md
  - reference/errors.md
- Guides:
  - guides/tracing-and-debugging.md
  - guides/performance-and-limits.md
  - guides/compatibility-iso.md
- FAQ: faq.md

mkdocs.yml Notes (to be authored later)

- Enable search, light/dark toggle, code copy buttons, and navigation tabs.
- Use “features: navigation.tabs, content.code.copy, content.code.annotate, toc.integrate”.
- Configure repo URL for “Edit on GitHub” links (optional).

Page‑by‑Page Outline

1) Home (index.md)
- What is PyLog? Short description and goals.
- Key features: tree‑walking interpreter, ISO‑style subset, REPL, CLP(FD).
- Quickstart (2–3 commands) and links to “Getting Started”.
- Pointers to examples (Cookbook) and CLP(FD) tour.

2) Getting Started

2.1 install.md
- Supported Python versions; install via pip (if published) or from source.
- Launching the REPL: `pylog` command or `python -m prolog.repl`.
- Verifying installation: run a simple fact query in the REPL.

2.2 repl.md (REPL Essentials)
- Prompt, entering goals, ending with `.`.
- Multiple solutions, backtracking behavior (pressing `;` or how PyLog shows alternatives).
- Loading code: `consult/1` equivalent via `consult_string` or CLI options.
- Saving and reusing sessions (if applicable); resetting state.

2.3 hello-prolog.md
- Minimal database (parent facts), a few rules (ancestor), and sample queries.
- Show how unification/bindings appear in results.

3) Prolog Basics

3.1 terms.md (Terms)
- Atoms, integers, variables, compound terms, lists.
- Construction syntax; how lists map to `'.'/2` internally.
- REPL examples: printing and matching terms.

3.2 facts-and-rules.md
- Facts (head only) vs rules (head :- body).
- Source order, determinism and backtracking.
- Example: family relations; recursive rules.

3.3 queries-and-unification.md
- Pose a goal; how unification binds variables; multiple answers.
- Equality `=/2`, disequality `\=/2`, arithmetic evaluation `is/2`.
- REPL traces of standard patterns (member, append) if included.

3.4 lists-and-structures.md
- Lists: pattern `[H|T]`, empty `[]`, list operations (member‑like examples).
- Structures: functor/arity; deconstruction with `=..` (univ) and `functor/3`, `arg/3`.

3.5 control.md
- Cut `!/0`, `fail/0`, `true/0`, `once/1`, `call/1`.
- When and why to use `cut`; common pitfalls.

3.6 arithmetic.md
- `is/2` evaluation; supported operators (`+`, `-`, `*`, `//`, `mod`, unary `-`, etc.).
- Integer arithmetic semantics; precedence and associativity.
- Examples mixing arithmetic with logic.

3.7 operators.md
- Operator table and precedence overview (Stage 1.5 operators).
- How PyLog parses operators; examples showing readability improvements.
- Links to Reference/operators-table.md for the full listing.

3.8 builtins.md (Core Builtins Overview)
- Overview of implemented ISO builtins used in examples.
- Link to full Reference for exhaustive argument modes and corner cases.

4) CLP(FD) — Constraint Logic Programming over Finite Domains

4.1 intro.md
- What CLP(FD) is; how constraints enrich Prolog.
- Supported primitives and consistency levels (bounds consistency for linear; Hall intervals for `all_different/1`).
- Concept of domains, propagation, and labeling.

4.2 domains-and-in.md (Posting Domains)
- `in/2` — set a domain for a variable (intervals, unions, sets).
- Domain operations (internally) and what users see in the REPL.
- Example: `X in 1..9, Y in {1,3,5}.`

4.3 linear-constraints.md (Linear Relations)
- Linear expression syntax: `1000*S + 100*E + 10*N + D`.
- Posting constraints: `#=`, `#\=`, `#<`, `#=<`, `#>`, `#>=`.
- How propagation tightens bounds; examples:
  - `X in 1..10, Y in 1..10, X + Y #= 10.` → both 1..9
  - `2*X + 3*Y #=< 20.` → tightened maxima.
- Limitations: non‑linear terms rejected; integer arithmetic only.

4.4 sum-constraints.md (Sum Specialization)
- `sum(List) #= Value` modeling via linear or specialized sum propagator (user‑level framing).
- When sums prune faster; examples with multiple variables.

4.5 all-different.md
- Semantics of `all_different/1`; value elimination and Hall‑interval pruning.
- Examples: `{1..2},{1..2},{1..3}` → the third becomes `{3}`.
- Pigeonhole failures detected early; common modeling patterns (Sudoku row).

4.6 labeling.md (Search)
- `label/1` and `labeling/2` strategies (what’s implemented today).
- Interplay with propagation; effect on performance.
- Examples: finding first solution vs all solutions; domain‑splitting choices.

4.7 examples.md (Putting It Together)
- SEND+MORE=MONEY (cryptarithm): full model and run; discuss pruning.
- Sudoku single row: nine vars, `all_different/1`, domain 1..9.
- 8‑Queens with simple CLP(FD) constraints on rows/columns/diagonals.

5) Cookbook (Task‑oriented How‑Tos)

5.1 family-relations.md
- Facts/rules, queries, and variations (e.g., symmetric relations).

5.2 graph-reachability.md
- Edges, path/2 recursion, avoiding cycles.

5.3 send-more-money.md
- Alternative modelings; heuristics for quicker solutions; discussion of constraint strength.

5.4 sudoku-row.md
- Basic row; ideas to extend to full Sudoku (row/col/subgrid all_different).

5.5 n-queens.md
- Modeling diagonals; performance tips; labeling choices.

5.6 scheduling.md
- Simple precedence constraints; showing domain narrowing and search.

6) Reference

6.1 builtins-reference.md
- Alphabetical listing of builtins provided by PyLog with brief descriptions.
- Link back into Basics pages for common usage; note departures from ISO.

6.2 operators-table.md
- Complete operator table with precedence, associativity, and fixity.

6.3 grammar-summary.md
- Summary of term, clause, and list syntax; how the Reader tokenizes and parses.

6.4 errors.md
- Common errors and messages; troubleshooting tips (e.g., missing `.` terminator).

7) Guides

7.1 tracing-and-debugging.md
- Tracing goals and ports (if applicable); interpreting traces; minimal examples.

7.2 performance-and-limits.md
- Practical tips: keep constraints linear; use domains early; prefer `all_different/1` over pairwise `#\=/2`.
- Known limitations: integer‑only arithmetic, bounds consistency scope, operator support scope.

7.3 compatibility-iso.md
- What subset of ISO Prolog semantics is supported; differences to SWI.

8) FAQ (faq.md)
- Short answers to recurring questions (why no floats? why label? how to get all solutions?).

Cross‑Cutting Example Conventions

- Prefer small REPL transcripts that start cleanly (no prior bindings).
- Where relevant, show both constraint posting and post‑labeling solutions.
- Use comments sparingly inside code blocks to minimize noise.

Planned Cross‑Links

- Basics/arithmetic → Basics/operators → Reference/operators-table.
- CLP(FD)/linear-constraints → CLP(FD)/labeling → Cookbook examples.
- Guides/performance-and-limits → all CLP(FD) pages.

Appendix (Optional, later if needed)

- Internals overview: engine loop, attributed variables, propagation queue (high‑level, non‑API).
- Development notes linking to developer docs already in the repo.

Implementation Notes

- Each page should include at least one runnable REPL example.
- Keep pages short; prefer more pages with tight focus over monoliths.
- Add “Next steps” and “Related topics” sections at page bottoms to encourage exploration.

