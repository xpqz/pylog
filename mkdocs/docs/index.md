# PyLog User Guide

PyLog is a Prolog interpreter written in Python. It supports a practical subset of ISO Prolog and adds finite‑domain constraint solving (CLP(FD)).

<div style="background: #e3f2fd; border: 1px solid #1976d2; border-radius: 8px; padding: 16px; margin: 16px 0;">
  <h3 style="margin-top: 0; color: #1976d2;">🚀 Try PyLog in Your Browser</h3>
  <p style="margin-bottom: 8px;">Experience PyLog directly in your browser without any installation! Our interactive REPL runs entirely client-side using WebAssembly.</p>
  <p style="margin-bottom: 0;">
    <a href="try/" style="background: #1976d2; color: white; padding: 8px 16px; border-radius: 4px; text-decoration: none; font-weight: bold;">Try PyLog Now →</a>
  </p>
</div>

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
