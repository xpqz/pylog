"""ISO-compliant catch/3 micro-tests to lock down correct behavior.

ISO Prolog Semantics for catch/3:
==================================

1. **No new predicate scope**: catch/3 does not create a new predicate frame.
   Recovery executes at the call site, not in an isolated scope.

2. **Cut behavior**: A cut (!) inside Recovery commits within the surrounding 
   scope. It can prune choicepoints that existed before the catch/3 call.
   This is ISO behavior - Recovery runs "as if" typed at the catch call site.

3. **Recovery failure**: If Recovery fails, the entire catch/3 call fails.
   This causes normal backtracking to pre-catch choicepoints.

4. **No resume after catch**: Once an exception is caught and Recovery begins,
   the original Goal never resumes - even if Recovery succeeds.

5. **Backtracking transparency**: After normal success (no throw) or after
   successful Recovery, backtracking proceeds transparently through the
   catch boundary to explore pre-catch alternatives.

These tests validate our engine follows ISO Prolog semantics exactly.
Tests expecting different behavior should be marked with @pytest.mark.xfail.
"""

from prolog.engine.engine import Engine
from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.ast.clauses import Program, Clause


def mk_fact(functor, *args):
    """Create a fact clause."""
    head = Atom(functor) if not args else Struct(functor, args)
    return Clause(head, ())


def mk_rule(functor, args, body):
    """Create a rule clause."""
    head = Atom(functor) if not args else Struct(functor, args)
    return Clause(head, (body,) if not isinstance(body, tuple) else body)


def program(*clauses):
    """Create a program from clauses."""
    return Program(clauses)


def test_cut_in_recovery_iso():
    """Test ISO semantics: cut in Recovery affects outer predicates."""
    # p/1: 1,2 ; r/1: 10,20 ; cut in Recovery
    p = program(
        mk_fact("p", Int(1)),
        mk_fact("p", Int(2)),
        mk_fact("r", Int(10)),
        mk_fact("r", Int(20)),
    )
    engine = Engine(p)
    
    # Query: p(X), catch(throw(t), t, (r(R), !))
    # ISO: cut in recovery prunes p/1 alternatives
    results = engine.run([
        Struct(",", (
            Struct("p", (Var(0, "X"),)),
            Struct("catch", (
                Struct("throw", (Atom("t"),)),
                Atom("t"),
                Struct(",", (Struct("r", (Var(1, "R"),)), Atom("!")))
            ))
        ))
    ])
    
    # ISO expects exactly 1 solution: X=1, R=10
    # The cut eliminates both r(20) AND p(2)
    assert len(results) == 1
    assert results[0]["X"] == Int(1)
    assert results[0]["R"] == Int(10)


def test_recovery_fails_outer_backtrack():
    """Test that recovery failure allows backtracking to outer choicepoints."""
    # p/1: 1,2 ; Recovery=fail
    p = program(
        mk_fact("p", Int(1)),
        mk_fact("p", Int(2)),
    )
    engine = Engine(p)
    
    # Query: p(X), catch(throw(t), t, fail)
    # Recovery fails → catch fails → backtrack to p/1
    results = engine.run([
        Struct(",", (
            Struct("p", (Var(0, "X"),)),
            Struct("catch", (
                Struct("throw", (Atom("t"),)),
                Atom("t"),
                Atom("fail")
            ))
        ))
    ])
    
    # ISO expects 0 solutions (recovery failure causes conjunction failure)
    assert len(results) == 0


def test_no_resume_after_recovery():
    """Test that goal doesn't resume after recovery."""
    p = program(
        mk_fact("p", Int(1)),
        mk_fact("p", Int(2)),
    )
    engine = Engine(p)
    
    # Query: catch((p(X), throw(t), p(Y)), t, true)
    # After throw and recovery, should NOT execute p(Y)
    results = engine.run([
        Struct("catch", (
            Struct(",", (
                Struct(",", (
                    Struct("p", (Var(0, "X"),)),
                    Struct("throw", (Atom("t"),))
                )),
                Struct("p", (Var(1, "Y"),))  # Never executed
            )),
            Atom("t"),
            Atom("true")
        ))
    ])
    
    # Should get 1 solution from recovery
    # With bound-only projection, Y should not appear (unbound)
    assert len(results) == 1
    assert "Y" not in results[0]  # Unbound variables omitted


def test_backtrack_order_with_throw():
    """Test backtracking order with throw in disjunction."""
    p = program()
    engine = Engine(p)
    
    # Query: catch((X=1 ; (X=2, throw(t)) ; X=3), t, X=caught)
    # Should get X=1 (first alternative), then X=caught (recovery)
    # Should NOT get X=3 (after throw point)
    results = engine.run([
        Struct("catch", (
            Struct(";", (
                Struct("=", (Var(0, "X"), Int(1))),
                Struct(";", (
                    Struct(",", (
                        Struct("=", (Var(0, "X"), Int(2))),
                        Struct("throw", (Atom("t"),))
                    )),
                    Struct("=", (Var(0, "X"), Int(3)))
                ))
            )),
            Atom("t"),
            Struct("=", (Var(0, "X"), Atom("caught")))
        ))
    ])
    
    # Two solutions: X=1 (normal), X=caught (recovery)
    assert len(results) == 2
    assert results[0]["X"] == Int(1)
    assert results[1]["X"] == Atom("caught")


def test_catch_success_allows_backtrack():
    """Test that successful catch allows backtracking to outer predicates."""
    p = program(
        mk_fact("p", Int(1)),
        mk_fact("p", Int(2)),
    )
    engine = Engine(p)
    
    # Query: p(X), catch(true, _, fail)
    # Goal succeeds without throwing, catch succeeds
    results = engine.run([
        Struct(",", (
            Struct("p", (Var(0, "X"),)),
            Struct("catch", (
                Atom("true"),  # Succeeds without throwing
                Var(1, "_"),   # Any catcher
                Atom("fail")   # Recovery (never executed)
            ))
        ))
    ])
    
    # Should get both p/1 solutions since no exception was thrown
    assert len(results) == 2
    assert {res["X"] for res in results} == {Int(1), Int(2)}


if __name__ == "__main__":
    # Run tests
    test_cut_in_recovery_iso()
    print("✓ Cut in recovery (ISO)")
    
    test_recovery_fails_outer_backtrack()
    print("✓ Recovery fails")
    
    test_no_resume_after_recovery()
    print("✓ No resume after recovery")
    
    test_backtrack_order_with_throw()
    print("✓ Backtrack order sanity")
    
    test_catch_success_allows_backtrack()
    print("✓ Catch success allows backtrack")
    
    print("\nAll ISO catch/3 micro-tests passed!")