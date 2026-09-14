# TODO

Current work is tracked in GitHub issues, grouped under epics. This file is the
index; it does not restate what the issues already say.

- Issues: https://github.com/xpqz/pylog/issues
- Epics carry the `epic` label and hold the per-stage task lists.

## Open epics

| Epic | Scope | State |
|------|-------|-------|
| [#381](https://github.com/xpqz/pylog/issues/381) | WAM Phase 4: full control features and core builtins | Disjunction, arithmetic and type-check builtins merged to `wam-dev`. If-then-else ([#383](https://github.com/xpqz/pylog/issues/383)) open in PR [#410](https://github.com/xpqz/pylog/pull/410). |
| [#414](https://github.com/xpqz/pylog/issues/414) | Run the original `harness.pl` inside PyLog (ISO Phase B) | B1-B3 merged. Remaining: [#423](https://github.com/xpqz/pylog/issues/423) Python runner as fallback, [#419](https://github.com/xpqz/pylog/issues/419) reporting and CI integration. |
| [#301](https://github.com/xpqz/pylog/issues/301) | Web REPL via Pyodide | Stages 0-2 delivered. Remaining: [#296](https://github.com/xpqz/pylog/issues/296) streaming, [#297](https://github.com/xpqz/pylog/issues/297) terminal features, [#298](https://github.com/xpqz/pylog/issues/298) persistence, [#299](https://github.com/xpqz/pylog/issues/299) trace UI, [#300](https://github.com/xpqz/pylog/issues/300) production polish. |

## Other open items

- [#432](https://github.com/xpqz/pylog/issues/432) Directive support for `:- op(...)` and friends
- [#397](https://github.com/xpqz/pylog/issues/397) Duplication in `asm.py`

## ISO conformance

`iso_test_js/iso.tst` is the conformance suite; see
[ISO_TESTING_USAGE.md](ISO_TESTING_USAGE.md) for how to run it and
`iso_test_js/pylog.skip` for skips and deliberate divergences.

Conformance is measured, not assumed. Run `make iso-full` for the current
numbers rather than quoting a figure from here. Two standing caveats:

- A large block of clauses in the suite still fails to parse and so is not
  counted at all.
- PyLog follows SWI in preference to this test file, so some recorded failures
  are deliberate divergences rather than gaps. The runner does not yet
  distinguish the two; that is part of [#419](https://github.com/xpqz/pylog/issues/419).

## Completed stages

Per-stage checklists from the staged build-out, kept for reference:

- [Stage -1](TODO--1.md) Unifier workbench
- [Stage 0](TODO-0.md) Core shapes and explicit stacks
- [Stage 1](TODO-1.md) Minimal ISO builtins, operator-free
- [Stage 1.5](TODO-1.5.md) Operators via reader
- [Stage 2](TODO-2.md) Indexing for performance
- [Stage 3](TODO-3.md) Debug and observability
- [Stage 4](TODO-4.md) Attributed variables
