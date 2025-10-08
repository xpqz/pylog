"""Dynamic database builtins extracted from engine.py.

This module implements dynamic predicate management including:
- dynamic/1: Declare predicates as dynamic
- assertz/1, asserta/1: Add clauses to the database
- retract/1: Remove first matching clause
- retractall/1: Remove all matching clauses
- abolish/1: Remove all clauses for predicates
"""

from typing import Optional, Dict, Any
from prolog.ast.terms import Term, Atom, Int, Struct, List as PrologList
from prolog.ast.clauses import Clause, Program as ProgramClass
from prolog.engine.indexed_program import IndexedProgram
from prolog.engine.utils.terms import prolog_list_to_python_list
from prolog.unify.store import Store
from prolog.engine.runtime import Trail
from prolog.engine.trail_adapter import TrailAdapter
from prolog.unify.unify import unify


def register(registry: Dict[tuple[str, int], Any]) -> None:
    """Register dynamic database builtins with the engine.

    Args:
        registry: The builtin registry to add entries to
    """
    registry[("dynamic", 1)] = lambda eng, args: builtin_dynamic(eng, args)
    registry[("assertz", 1)] = lambda eng, args: builtin_assert(eng, args, append=True)
    registry[("asserta", 1)] = lambda eng, args: builtin_assert(eng, args, append=False)
    registry[("retract", 1)] = lambda eng, args: builtin_retract(eng, args)
    registry[("retractall", 1)] = lambda eng, args: builtin_retractall(eng, args)
    registry[("abolish", 1)] = lambda eng, args: builtin_abolish(eng, args)


# --- Helper functions ---


def parse_pred_indicator(term: Term) -> Optional[tuple[str, int]]:
    """Parse a predicate indicator Name/Arity into (name, arity)."""
    if isinstance(term, Struct) and term.functor == "/" and len(term.args) == 2:
        name_t, arity_t = term.args
        if isinstance(name_t, Atom) and isinstance(arity_t, Int):
            return (name_t.name, int(arity_t.value))
    return None


def flatten_conjunction(term: Term) -> tuple[Term, ...]:
    """Flatten a conjunction term into a tuple of goals."""
    if isinstance(term, Struct) and term.functor == "," and len(term.args) == 2:
        left, right = term.args
        return flatten_conjunction(left) + flatten_conjunction(right)
    return (term,)


def term_to_clause(term: Term) -> Optional[Clause]:
    """Convert a term to a Clause (fact or rule)."""
    if isinstance(term, (Atom, Struct)) and not (
        isinstance(term, Struct) and term.functor == ":-"
    ):
        return Clause(term, tuple())
    if isinstance(term, Struct) and term.functor == ":-" and len(term.args) == 2:
        head_t, body_t = term.args
        if isinstance(head_t, (Atom, Struct)):
            body_goals = flatten_conjunction(body_t)
            return Clause(head_t, tuple(body_goals))
    return None


def clause_key_from_head(head: Term) -> tuple[str, int]:
    """Compute (name, arity) from a clause head (Atom or Struct)."""
    if isinstance(head, Atom):
        return (head.name, 0)
    assert isinstance(head, Struct)
    return (head.functor, len(head.args))


def extract_predicate_key(pattern: Term) -> Optional[tuple[str, int]]:
    """Extract predicate (name, arity) from a head or Head :- Body pattern."""
    if isinstance(pattern, Atom):
        return (pattern.name, 0)
    if isinstance(pattern, Struct) and pattern.functor != ":-":
        return (pattern.functor, len(pattern.args))
    if (
        isinstance(pattern, Struct)
        and pattern.functor == ":-"
        and len(pattern.args) == 2
        and isinstance(pattern.args[0], (Atom, Struct))
    ):
        head_t = pattern.args[0]
        if isinstance(head_t, Atom):
            return (head_t.name, 0)
        return (head_t.functor, len(head_t.args))
    return None


def set_program_clauses(engine, clauses: list[Clause]) -> None:
    """Replace the engine program with given clauses, preserving indexing mode."""
    if engine.use_indexing:
        engine.program = IndexedProgram.from_clauses(clauses)
    else:
        engine.program = ProgramClass(tuple(clauses))


def build_clause_term_copy(
    engine, existing: Clause, target_store: Optional[Store] = None
) -> Term:
    """Copy a clause (fact or rule) into a term; optionally into target_store."""
    var_map: Dict[int, int] = {}
    head_copy = engine._copy_term_recursive(
        existing.head, var_map, target_store=target_store
    )
    if existing.body:
        # Rebuild conjunction from body goals: g1, (g2, (...))
        body_term: Optional[Term] = None
        for g in reversed(existing.body):
            g_copy = engine._copy_term_recursive(g, var_map, target_store=target_store)
            body_term = (
                g_copy if body_term is None else Struct(",", (g_copy, body_term))
            )
        return Struct(":-", (head_copy, body_term))
    else:
        return head_copy


# --- Builtin implementations ---


def builtin_dynamic(engine, args: tuple) -> bool:
    """dynamic/1 - Declare predicates as dynamic.

    Accepts:
    - Single predicate indicator: dynamic(p/1)
    - List of indicators: dynamic([p/1, q/2])
    - Conjunction: dynamic((p/1, q/2))
    """
    if len(args) != 1:
        return False
    term = args[0]

    # Allow single Name/Arity or list of Name/Arity
    single = parse_pred_indicator(term)
    if single:
        engine._dynamic_preds.add(single)
        return True

    # List form: dynamic([p/1, q/2, ...])
    if isinstance(term, PrologList):
        items = prolog_list_to_python_list(term)
        if items is None:
            return False
        preds: list[tuple[str, int]] = []
        for it in items:
            pi = parse_pred_indicator(it)
            if not pi:
                return False
            preds.append(pi)
        # All valid — add them
        for pi in preds:
            engine._dynamic_preds.add(pi)
        return True

    # Comma-conjunction form: dynamic((p/1, q/2)) or nested ,
    if isinstance(term, Struct) and term.functor == ",":
        items = flatten_conjunction(term)
        preds: list[tuple[str, int]] = []
        for it in items:
            pi = parse_pred_indicator(it)
            if not pi:
                return False
            preds.append(pi)
        for pi in preds:
            engine._dynamic_preds.add(pi)
        return True

    return False


def builtin_assert(engine, args: tuple, append: bool = True) -> bool:
    """assertz/1 or asserta/1 - Add a clause to the database.

    Args:
        engine: The Prolog engine
        args: Tuple containing the clause to assert
        append: If True, add at end (assertz); if False, add at beginning (asserta)
    """
    if len(args) != 1:
        return False
    cl = term_to_clause(args[0])
    if cl is None:
        return False
    head = cl.head
    if isinstance(head, Atom):
        key = (head.name, 0)
    else:
        key = (head.functor, len(head.args))  # type: ignore

    if not engine._require_dynamic(key):
        return False

    clauses = list(engine.program.clauses)
    if append:
        # insert after last same-predicate clause
        insert_pos = len(clauses)
        for i in range(len(clauses) - 1, -1, -1):
            h = clauses[i].head
            if isinstance(h, Atom):
                k = (h.name, 0)
            else:
                k = (h.functor, len(h.args))  # type: ignore
            if k == key:
                insert_pos = i + 1
                break
        clauses.insert(insert_pos, cl)
    else:
        # insert before first same-predicate clause
        insert_pos = 0
        for i, existing in enumerate(clauses):
            h = existing.head
            if isinstance(h, Atom):
                k = (h.name, 0)
            else:
                k = (h.functor, len(h.args))  # type: ignore
            if k == key:
                insert_pos = i
                break
            insert_pos = i + 1
        clauses.insert(insert_pos, cl)

    set_program_clauses(engine, clauses)
    return True


def builtin_retract(engine, args: tuple) -> bool:
    """retract/1 - Remove first matching clause and bind variables."""
    if len(args) != 1:
        return False
    pattern = args[0]
    key = extract_predicate_key(pattern)
    if key is None or not engine._require_dynamic(key):
        return False

    # Try unification against each clause; bind variables in pattern on success
    clauses = list(engine.program.clauses)
    for i, existing in enumerate(clauses):
        # Quick skip if different predicate
        eh = existing.head
        ek = clause_key_from_head(eh)
        if ek != key:
            continue

        clause_term = build_clause_term_copy(engine, existing)

        # Try unify pattern with clause_term; keep bindings on success
        trail_pos = engine.trail.position()
        trail_adapter = TrailAdapter(engine.trail)
        if unify(
            pattern, clause_term, engine.store, trail_adapter, engine.occurs_check
        ):
            # Remove the clause and succeed
            del clauses[i]
            set_program_clauses(engine, clauses)
            return True
        else:
            # Undo bindings and try next
            engine.trail.unwind_to(trail_pos, engine.store)
    return False


def builtin_retractall(engine, args: tuple) -> bool:
    """retractall(+HeadOrClause) — remove all matching clauses for a dynamic predicate.

    Matches by unification against the full clause term if provided (Head :- Body),
    or just the Head if a head-only term is given. Does not bind variables in the
    query (bindings are unwound after matching).
    """
    if len(args) != 1:
        return False
    pattern = args[0]
    # Extract key for candidate filtering; also detect if pattern includes a body
    key = extract_predicate_key(pattern)
    if key is None or not engine._require_dynamic(key):
        return False

    # Use a temporary store+trail per clause match to avoid touching the engine store/trail
    remaining: list[Clause] = []
    for cl in engine.program.clauses:
        # Skip non-key predicates quickly
        eh = cl.head
        ek = clause_key_from_head(eh)
        if ek != key:
            remaining.append(cl)
            continue

        # Build terms for matching within a temporary store
        temp_store = Store()
        # Copy clause into temp store
        clause_term = build_clause_term_copy(engine, cl, target_store=temp_store)
        # Copy pattern into temp store
        pat_copy = engine._copy_term_recursive(pattern, {}, target_store=temp_store)
        # For head-only patterns and rules, match on head only
        candidate: Term = clause_term
        if (
            isinstance(pat_copy, (Atom, Struct))
            and not (isinstance(pat_copy, Struct) and pat_copy.functor == ":-")
            and isinstance(clause_term, Struct)
            and clause_term.functor == ":-"
        ):
            candidate = clause_term.args[0]

        temp_trail = Trail()
        temp_adapter = TrailAdapter(temp_trail)
        if unify(pat_copy, candidate, temp_store, temp_adapter, engine.occurs_check):
            # Match: drop clause (do not keep)
            continue
        else:
            remaining.append(cl)

    # Update program if any clause was removed
    if len(remaining) != len(engine.program.clauses):
        set_program_clauses(engine, remaining)

    return True


def builtin_abolish(engine, args: tuple) -> bool:
    """abolish/1 - Remove all clauses for specified predicates.

    Accepts:
    - Single predicate indicator: abolish(p/1)
    - List of indicators: abolish([p/1, q/2])
    - Conjunction: abolish((p/1, q/2))
    """
    if len(args) != 1:
        return False
    term = args[0]
    pis: list[tuple[str, int]] = []
    single = parse_pred_indicator(term)
    if single:
        pis = [single]
    elif isinstance(term, PrologList):
        items = prolog_list_to_python_list(term)
        if items is None:
            return False
        for it in items:
            pi = parse_pred_indicator(it)
            if not pi:
                return False
            pis.append(pi)
    elif isinstance(term, Struct) and term.functor == ",":
        for it in flatten_conjunction(term):
            pi = parse_pred_indicator(it)
            if not pi:
                return False
            pis.append(pi)
    else:
        return False

    # Check dynamic for all
    for pred in pis:
        if not engine._require_dynamic(pred):
            return False

    # Remove all clauses for each pred
    remaining = list(engine.program.clauses)
    to_remove = set(pis)
    new_clauses = []
    for cl in remaining:
        if isinstance(cl.head, Atom):
            k = (cl.head.name, 0)
        else:
            k = (cl.head.functor, len(cl.head.args))  # type: ignore
        if k in to_remove:
            continue
        new_clauses.append(cl)
    set_program_clauses(engine, new_clauses)
    return True
