"""Tests for dynamic database extraction to builtins/dynamic_db.py module.

This test suite verifies that:
1. Dynamic DB builtins are properly registered via the new module
2. Helper functions work correctly in their new locations
3. All functionality maintains backward compatibility
4. Indexing rebuild behavior is preserved
"""

from prolog.ast.terms import Atom, Int, Struct, List as PrologList, Var
from prolog.ast.clauses import Program, Clause
from prolog.engine.engine import Engine


def pi(name: str, arity: int) -> Struct:
    """Helper to build Name/Arity predicate indicator term."""
    return Struct("/", (Atom(name), Int(arity)))


def plist(items):
    """Helper to build a proper Prolog list from Python items."""
    result = Atom("[]")
    for it in reversed(items):
        result = PrologList((it,), result)
    return result


class TestDynamicDBRegistration:
    """Test that dynamic DB builtins are properly registered through the new module."""

    def test_all_dynamic_builtins_registered(self):
        """Verify all dynamic DB builtins are registered in engine."""
        engine = Engine(Program(()))

        # Check that all dynamic DB builtins are registered
        expected_builtins = [
            ("dynamic", 1),
            ("asserta", 1),
            ("assertz", 1),
            ("retract", 1),
            ("retractall", 1),
            ("abolish", 1),
        ]

        for name, arity in expected_builtins:
            assert (name, arity) in engine._builtins, f"{name}/{arity} not registered"
            assert engine._is_builtin(
                Struct(name, tuple(Var(i, f"V{i}") for i in range(arity)))
            )

    def test_dynamic_registration_via_new_module(self):
        """Test that builtins work through new registration pattern."""
        engine = Engine(Program(()))

        # Test dynamic/1 registration
        assert engine._execute_builtin(Struct("dynamic", (pi("test", 1),)))

        # Test assertz/1
        assert engine._execute_builtin(Struct("assertz", (Struct("test", (Int(1),)),)))

        # Query should succeed
        sols = engine.run([Struct("test", (Int(1),))])
        assert len(sols) == 1


class TestHelperFunctionExtraction:
    """Test that helper functions work correctly in their new locations."""

    def test_parse_pred_indicator(self):
        """Test parsing of Name/Arity predicate indicators."""
        engine = Engine(Program(()))

        # After extraction, this should work via imported helper
        # Note: We'll access via the engine method wrapper initially
        result = engine._parse_pred_indicator(pi("foo", 2))
        assert result == ("foo", 2)

        result = engine._parse_pred_indicator(Atom("not_indicator"))
        assert result is None

    def test_flatten_conjunction(self):
        """Test flattening of conjunction terms."""
        engine = Engine(Program(()))

        # Simple conjunction a,b
        conj = Struct(",", (Atom("a"), Atom("b")))
        result = engine._flatten_conjunction(conj)
        assert result == (Atom("a"), Atom("b"))

        # Nested conjunction a,(b,c)
        nested = Struct(",", (Atom("a"), Struct(",", (Atom("b"), Atom("c")))))
        result = engine._flatten_conjunction(nested)
        assert result == (Atom("a"), Atom("b"), Atom("c"))

        # Single term
        result = engine._flatten_conjunction(Atom("single"))
        assert result == (Atom("single"),)

    def test_term_to_clause(self):
        """Test conversion of terms to clauses."""
        engine = Engine(Program(()))

        # Fact: p(1)
        fact = Struct("p", (Int(1),))
        clause = engine._term_to_clause(fact)
        assert clause is not None
        assert clause.head == fact
        assert clause.body == tuple()

        # Rule: p(X) :- q(X)
        rule = Struct(":-", (Struct("p", (Var(0, "X"),)), Struct("q", (Var(0, "X"),))))
        clause = engine._term_to_clause(rule)
        assert clause is not None
        assert clause.head == Struct("p", (Var(0, "X"),))
        assert clause.body == (Struct("q", (Var(0, "X"),)),)

        # Invalid term
        invalid = Var(0, "X")
        clause = engine._term_to_clause(invalid)
        assert clause is None


class TestDynamicPredicateManagement:
    """Test dynamic predicate declaration and management."""

    def test_dynamic_single_declaration(self):
        """Test single predicate dynamic declaration."""
        engine = Engine(Program(()))

        # Declare p/1 as dynamic
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))

        # Should be able to assert to it
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))

        # Should not be able to assert to undeclared q/1
        assert not engine._execute_builtin(Struct("assertz", (Struct("q", (Int(1),)),)))

    def test_dynamic_list_declaration(self):
        """Test list form of dynamic declaration."""
        engine = Engine(Program(()))

        # dynamic([p/1, q/2])
        lst = plist([pi("p", 1), pi("q", 2)])
        assert engine._execute_builtin(Struct("dynamic", (lst,)))

        # Both should be assertable
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(
            Struct("assertz", (Struct("q", (Int(1), Int(2))),))
        )

    def test_dynamic_conjunction_declaration(self):
        """Test conjunction form of dynamic declaration."""
        engine = Engine(Program(()))

        # dynamic((p/1, q/2))
        conj = Struct(",", (pi("p", 1), pi("q", 2)))
        assert engine._execute_builtin(Struct("dynamic", (conj,)))

        # Both should be assertable
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(
            Struct("assertz", (Struct("q", (Int(1), Int(2))),))
        )


class TestAssertBuiltins:
    """Test assert/asserta/assertz functionality."""

    def test_assertz_appends(self):
        """Test that assertz adds clauses at the end."""
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))

        # Add p(1) then p(2)
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(2),)),)))

        # Query with variable should return in order
        x = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x.id, "X")]
        sols = engine.run([Struct("p", (x,))])
        assert len(sols) == 2
        assert sols[0]["X"] == Int(1)
        assert sols[1]["X"] == Int(2)

    def test_asserta_prepends(self):
        """Test that asserta adds clauses at the beginning."""
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))

        # Add p(1) then asserta p(2)
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("asserta", (Struct("p", (Int(2),)),)))

        # Query with variable should return p(2) first
        x = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x.id, "X")]
        sols = engine.run([Struct("p", (x,))])
        assert len(sols) == 2
        assert sols[0]["X"] == Int(2)  # asserta'd clause comes first
        assert sols[1]["X"] == Int(1)

    def test_assert_rule(self):
        """Test asserting rules (not just facts)."""
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))

        # Assert rule: p(X) :- p(X)
        rule = Struct(":-", (Struct("p", (Var(0, "X"),)), Struct("p", (Var(0, "X"),))))
        assert engine._execute_builtin(Struct("assertz", (rule,)))

        # The rule should be added to the program
        # (Note: this would create infinite recursion if called)


class TestRetractBuiltins:
    """Test retract/retractall functionality."""

    def test_retract_removes_first_match(self):
        """Test that retract removes only the first matching clause."""
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))

        # Add multiple facts
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(2),)),)))
        assert engine._execute_builtin(
            Struct("assertz", (Struct("p", (Int(1),)),))
        )  # duplicate

        # Retract p(1) - should remove first occurrence
        assert engine._execute_builtin(Struct("retract", (Struct("p", (Int(1),)),)))

        # Should still have p(2) and second p(1)
        x = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x.id, "X")]
        sols = engine.run([Struct("p", (x,))])
        assert len(sols) == 2
        assert sols[0]["X"] == Int(2)
        assert sols[1]["X"] == Int(1)

    def test_retract_binds_variables(self):
        """Test that retract binds variables when matching."""
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 2),)))

        # Add facts
        assert engine._execute_builtin(
            Struct("assertz", (Struct("p", (Int(1), Atom("a"))),))
        )
        assert engine._execute_builtin(
            Struct("assertz", (Struct("p", (Int(2), Atom("b"))),))
        )

        # retract(p(X, a)) should bind X=1
        x = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x.id, "X")]
        sols = engine.run([Struct("retract", (Struct("p", (x, Atom("a"))),))])
        assert len(sols) == 1
        assert sols[0]["X"] == Int(1)

        # p(1,a) should be gone
        assert len(engine.run([Struct("p", (Int(1), Atom("a")))])) == 0
        # p(2,b) should remain
        assert len(engine.run([Struct("p", (Int(2), Atom("b")))])) == 1

    def test_retractall_removes_all_matches(self):
        """Test that retractall removes all matching clauses."""
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))

        # Add multiple matching and non-matching facts
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(2),)),)))
        assert engine._execute_builtin(
            Struct("assertz", (Struct("p", (Int(1),)),))
        )  # duplicate

        # retractall(p(1)) should remove all p(1) facts
        assert engine._execute_builtin(Struct("retractall", (Struct("p", (Int(1),)),)))

        # Only p(2) should remain
        x = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x.id, "X")]
        sols = engine.run([Struct("p", (x,))])
        assert len(sols) == 1
        assert sols[0]["X"] == Int(2)

    def test_retractall_doesnt_bind(self):
        """Test that retractall doesn't bind variables."""
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))

        # Add facts
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(2),)),)))

        # retractall(p(X)) should not bind X
        x = Var(engine.store.new_var("X"), "X")
        engine._query_vars = [(x.id, "X")]
        sols = engine.run([Struct("retractall", (Struct("p", (x,)),))])
        assert len(sols) == 1
        assert isinstance(sols[0]["X"], Var)  # X remains unbound

        # All p/1 facts should be gone
        assert len(engine.run([Struct("p", (Int(1),))])) == 0
        assert len(engine.run([Struct("p", (Int(2),))])) == 0


class TestAbolishBuiltin:
    """Test abolish functionality."""

    def test_abolish_single_predicate(self):
        """Test abolishing a single predicate."""
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))
        assert engine._execute_builtin(Struct("dynamic", (pi("q", 1),)))

        # Add facts to both
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("q", (Int(1),)),)))

        # Abolish p/1
        assert engine._execute_builtin(Struct("abolish", (pi("p", 1),)))

        # p/1 should be gone
        assert len(engine.run([Struct("p", (Int(1),))])) == 0
        # q/1 should remain
        assert len(engine.run([Struct("q", (Int(1),))])) == 1

    def test_abolish_list_form(self):
        """Test abolishing multiple predicates via list."""
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))
        assert engine._execute_builtin(Struct("dynamic", (pi("q", 1),)))
        assert engine._execute_builtin(Struct("dynamic", (pi("r", 1),)))

        # Add facts
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("q", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("r", (Int(1),)),)))

        # Abolish p/1 and q/1
        lst = plist([pi("p", 1), pi("q", 1)])
        assert engine._execute_builtin(Struct("abolish", (lst,)))

        # p/1 and q/1 should be gone
        assert len(engine.run([Struct("p", (Int(1),))])) == 0
        assert len(engine.run([Struct("q", (Int(1),))])) == 0
        # r/1 should remain
        assert len(engine.run([Struct("r", (Int(1),))])) == 1

    def test_abolish_conjunction_form(self):
        """Test abolishing multiple predicates via conjunction."""
        engine = Engine(Program(()))
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))
        assert engine._execute_builtin(Struct("dynamic", (pi("q", 1),)))

        # Add facts
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("q", (Int(1),)),)))

        # Abolish via conjunction
        conj = Struct(",", (pi("p", 1), pi("q", 1)))
        assert engine._execute_builtin(Struct("abolish", (conj,)))

        # Both should be gone
        assert len(engine.run([Struct("p", (Int(1),))])) == 0
        assert len(engine.run([Struct("q", (Int(1),))])) == 0


class TestIndexingPreservation:
    """Test that indexing is properly rebuilt when modifying dynamic predicates."""

    def test_indexing_rebuilt_after_assert(self):
        """Test that indexing is rebuilt after asserting clauses."""
        # Create engine with indexing enabled
        engine = Engine(Program(()), use_indexing=True)
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))

        # Assert facts
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Atom("a"),)),)))
        assert engine._execute_builtin(
            Struct("assertz", (Struct("p", (PrologList((Int(2),), Atom("[]")),)),))
        )

        # Queries should work efficiently with indexing
        assert len(engine.run([Struct("p", (Int(1),))])) == 1
        assert len(engine.run([Struct("p", (Atom("a"),))])) == 1
        assert len(engine.run([Struct("p", (PrologList((Int(2),), Atom("[]")),))])) == 1

    def test_indexing_rebuilt_after_retract(self):
        """Test that indexing is rebuilt after retracting clauses."""
        engine = Engine(Program(()), use_indexing=True)
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))

        # Add facts
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(2),)),)))

        # Retract one
        assert engine._execute_builtin(Struct("retract", (Struct("p", (Int(1),)),)))

        # Query should still work with indexing
        assert len(engine.run([Struct("p", (Int(1),))])) == 0
        assert len(engine.run([Struct("p", (Int(2),))])) == 1

    def test_indexing_rebuilt_after_abolish(self):
        """Test that indexing is rebuilt after abolishing predicates."""
        engine = Engine(Program(()), use_indexing=True)
        assert engine._execute_builtin(Struct("dynamic", (pi("p", 1),)))
        assert engine._execute_builtin(Struct("dynamic", (pi("q", 1),)))

        # Add facts
        assert engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("q", (Int(1),)),)))

        # Abolish p/1
        assert engine._execute_builtin(Struct("abolish", (pi("p", 1),)))

        # Queries should work with rebuilt index
        assert len(engine.run([Struct("p", (Int(1),))])) == 0
        assert len(engine.run([Struct("q", (Int(1),))])) == 1


class TestBackwardCompatibility:
    """Test that all existing functionality remains intact."""

    def test_mixed_static_and_dynamic(self):
        """Test mixing static and dynamic predicates."""
        # Start with static predicates
        program = Program(
            (
                Clause(Struct("static", (Int(1),)), ()),
                Clause(Struct("static", (Int(2),)), ()),
            )
        )
        engine = Engine(program)

        # Add dynamic predicates
        assert engine._execute_builtin(Struct("dynamic", (pi("dyn", 1),)))
        assert engine._execute_builtin(Struct("assertz", (Struct("dyn", (Int(3),)),)))

        # Both should be queryable
        assert len(engine.run([Struct("static", (Int(1),))])) == 1
        assert len(engine.run([Struct("dyn", (Int(3),))])) == 1

    def test_error_conditions_preserved(self):
        """Test that error conditions are preserved."""
        engine = Engine(Program(()))

        # Assert without dynamic declaration should fail
        assert not engine._execute_builtin(Struct("assertz", (Struct("p", (Int(1),)),)))

        # Retract from non-dynamic should fail
        assert not engine._execute_builtin(Struct("retract", (Struct("p", (Int(1),)),)))

        # Abolish non-dynamic should fail
        assert not engine._execute_builtin(Struct("abolish", (pi("p", 1),)))

        # Invalid predicate indicators should fail
        assert not engine._execute_builtin(Struct("dynamic", (Atom("not_indicator"),)))
        assert not engine._execute_builtin(Struct("dynamic", (plist([Atom("bad")]),)))
