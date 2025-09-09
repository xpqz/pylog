"""SWI-Prolog baseline testing harness.

This module provides utilities to run Prolog programs in SWI-Prolog
and extract results for comparison with PyLog behavior.
"""

import json
import os
import subprocess
import tempfile
import textwrap
from pathlib import Path
from typing import Optional


# Allow override via environment variables
SWIPL = os.environ.get("SWIPL", "swipl")
TIMEOUT = float(os.environ.get("SWIPL_TIMEOUT_SEC", "5"))
DEBUG_PROG = os.environ.get("SWIPL_DEBUG_PROG", "").lower() in ("1", "true", "yes")


def _run_swipl(pl_source: str, goal: str, mode: str, var: Optional[str] = None) -> str:
    """Run a Prolog program in SWI-Prolog and extract results.
    
    Args:
        pl_source: The Prolog program source code
        goal: The goal to execute
        mode: Either "count" (count solutions) or "onevar" (extract single variable)
        var: Variable name for onevar mode
        
    Returns:
        Raw output from SWI-Prolog
        
    Raises:
        RuntimeError: If SWI-Prolog execution fails
    """
    # Write temp .pl file combining program and driver
    with tempfile.TemporaryDirectory() as td:
        p = Path(td) / "prog.pl"
        
        # Prepare the program
        prog = textwrap.dedent(pl_source).strip() + "\n" if pl_source else ""
        
        if mode == "count":
            # Count solutions using aggregate_all
            main = f"""
            :- use_module(library(aggregate)).
            
            run :-
                Goal = ({goal}),
                (   aggregate_all(count, Goal, N)
                ->  format("~d", [N])
                ;   format("0", [])
                ),
                halt.
            """
        elif mode == "onevar":
            if not var:
                raise ValueError("var is required for mode=onevar")
            # Collect all bindings for a single variable
            # Use numbervars to stabilize anonymous variables
            # Use JSON library for proper escaping
            main = f"""
            :- use_module(library(http/json)).
            
            run :-
                Goal = ({goal}),
                findall({var}, Goal, L0),
                maplist(numbervars_stable, L0, L1),
                maplist(term_string, L1, SL),
                json_write(current_output, SL),
                halt.
            
            % Helper to apply numbervars
            numbervars_stable(Term, Term) :-
                numbervars(Term, 0, _).
            """
        else:
            raise ValueError(f"Unknown mode: {mode}")
        
        # Write the combined program
        full_program = prog + "\n" + main
        p.write_text(full_program, encoding="utf-8")
        
        # Call SWI-Prolog quietly (using -g run -t halt for explicit control)
        try:
            res = subprocess.run(
                [SWIPL, "-q", "-g", "run", "-t", "halt", "-s", str(p)],
                capture_output=True,
                text=True,
                timeout=TIMEOUT
            )
        except subprocess.TimeoutExpired:
            raise RuntimeError(f"SWI-Prolog execution timed out after {TIMEOUT}s")
        except FileNotFoundError:
            raise RuntimeError(f"SWI-Prolog not found at: {SWIPL}")
        
        if res.returncode != 0:
            error_msg = res.stderr.strip() or res.stdout.strip()
            # Include program text in debug mode for easier debugging
            if DEBUG_PROG:
                error_msg += f"\n\nProgram:\n{full_program}"
            raise RuntimeError(f"SWI-Prolog failed: {error_msg}")
        
        return res.stdout.strip()


def swi_count(program_pl: str, goal_pl: str) -> int:
    """Count the number of solutions for a goal in SWI-Prolog.
    
    Args:
        program_pl: Prolog program source (can be empty string)
        goal_pl: Goal to execute
        
    Returns:
        Number of solutions
    """
    out = _run_swipl(program_pl, goal_pl, "count")
    return int(out or "0")


def swi_onevar(program_pl: str, goal_pl: str, var: str) -> list[str]:
    """Get all bindings for a single variable from SWI-Prolog.
    
    Args:
        program_pl: Prolog program source (can be empty string)
        goal_pl: Goal to execute
        var: Variable name to extract (e.g., "X")
        
    Returns:
        List of string representations of the variable bindings
    """
    out = _run_swipl(program_pl, goal_pl, "onevar", var=var)
    
    # Parse the JSON-like output
    try:
        # Handle empty list
        if out == "[]":
            return []
        # Parse as JSON
        return json.loads(out)
    except json.JSONDecodeError:
        # Fallback: return raw output for debugging
        return [out] if out else []


def is_swipl_available() -> bool:
    """Check if SWI-Prolog is available on the system.
    
    Returns:
        True if swipl can be executed, False otherwise
    """
    try:
        result = subprocess.run(
            [SWIPL, "--version"],
            capture_output=True,
            timeout=2
        )
        return result.returncode == 0
    except (subprocess.TimeoutExpired, FileNotFoundError):
        return False