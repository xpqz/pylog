# PyLog User Guide

PyLog is a Prolog interpreter written in Python. It supports a practical subset of ISO Prolog and adds finite‑domain constraint solving (CLP(FD)).

This guide helps you:

- run goals in the REPL
- write facts and rules
- model and solve problems with CLP(FD)

See Getting Started for installation and the REPL, then work through Prolog Basics before visiting CLP(FD).

What's new
----------

- SWI‑style JSON support with dual representations:
  - Classic: `json([key=value,...])` with `@(null)`, `@(true)`, `@(false)`
  - Dict: `PrologDict` terms
- Option aliases: `json_object(term|dict)` alongside `mode(classic|dict)`

Start here: Getting Started → JSON Quickstart. See Reference → JSON for full details, and Basics → Dicts for dict operations.
