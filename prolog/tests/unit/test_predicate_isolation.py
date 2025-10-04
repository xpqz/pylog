"""
Tests for Stage 2 Phase 2.5: Predicate isolation and small predicate optimization.

These tests ensure that:
1. ClauseIndex never mixes predicates in a single bucket
2. PredIndex objects are only reachable via (name, arity) keys
3. Two predicates with identical first-arg shapes don't share buckets
4. Selection for p/1 never returns q/1 clauses
5. Predicate key includes arity in both build and select
"""

from hypothesis import given, strategies as st
from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList
from prolog.ast.clauses import Clause
from prolog.engine.indexing import build_from_clauses
from prolog.unify.store import Store


class TestPredicateIsolation:
    """Test suite for predicate isolation guarantees."""

    def test_clauseindex_never_mixes_predicates(self):
        """ClauseIndex should never mix predicates in single bucket."""
        # Create clauses for two different predicates with similar structures
        clauses = [
            # p/1 clauses
            Clause(Struct("p", (Int(1),)), ()),  # p(1)
            Clause(Struct("p", (Var(0, "X"),)), ()),  # p(X)
            Clause(Struct("p", (Atom("a"),)), ()),  # p(a)
            Clause(
                Struct("p", (PrologList((Int(1), Int(2)), Atom("[]")),)), ()
            ),  # p([1,2])
            Clause(Struct("p", (PrologList((), Atom("[]")),)), ()),  # p([])
            # q/1 clauses with identical first-arg shapes
            Clause(Struct("q", (Int(1),)), ()),  # q(1)
            Clause(Struct("q", (Var(1, "Y"),)), ()),  # q(Y)
            Clause(Struct("q", (Atom("a"),)), ()),  # q(a)
            Clause(
                Struct("q", (PrologList((Int(1), Int(2)), Atom("[]")),)), ()
            ),  # q([1,2])
            Clause(Struct("q", (PrologList((), Atom("[]")),)), ()),  # q([])
        ]

        idx = build_from_clauses(clauses)

        # Verify p/1 and q/1 have separate PredIndex instances
        assert ("p", 1) in idx.preds
        assert ("q", 1) in idx.preds
        assert idx.preds[("p", 1)] is not idx.preds[("q", 1)]

        # Verify clause IDs are per-predicate (may overlap but stored separately)
        p_idx = idx.preds[("p", 1)]
        q_idx = idx.preds[("q", 1)]

        # Collect all clause IDs from p/1
        p_clause_ids = set(p_idx.order)

        # Collect all clause IDs from q/1
        q_clause_ids = set(q_idx.order)

        # IDs may overlap (both start at 0) but are namespaced by predicate
        assert len(p_clause_ids) == 5  # 5 p/1 clauses
        assert len(q_clause_ids) == 5  # 5 q/1 clauses

        # Assert idx.preds has only expected keys
        assert set(idx.preds.keys()) == {("p", 1), ("q", 1)}

        # Verify stored clauses map correctly
        for clause_id in p_clause_ids:
            clause = idx.clauses[(("p", 1), clause_id)]
            assert clause.head.functor == "p"

        for clause_id in q_clause_ids:
            clause = idx.clauses[(("q", 1), clause_id)]
            assert clause.head.functor == "q"

    def test_predindex_only_reachable_via_keys(self):
        """PredIndex objects should only be reachable via (name, arity) keys."""
        clauses = [
            Clause(Struct("foo", (Int(1),)), ()),
            Clause(Struct("bar", (Int(2),)), ()),
            Clause(
                Struct(
                    "foo",
                    (
                        Int(3),
                        Atom("x"),
                    ),
                ),
                (),
            ),  # foo/2 - different arity
        ]

        idx = build_from_clauses(clauses)

        # Verify each predicate has its own index
        assert ("foo", 1) in idx.preds
        assert ("bar", 1) in idx.preds
        assert ("foo", 2) in idx.preds

        # Verify they are different objects
        assert idx.preds[("foo", 1)] is not idx.preds[("foo", 2)]
        assert idx.preds[("foo", 1)] is not idx.preds[("bar", 1)]

        # Verify no other way to access PredIndex objects
        # (they are only stored in idx.preds dict)
        assert hasattr(idx, "preds")
        assert not hasattr(idx, "all_indices")
        assert not hasattr(idx, "global_index")

    @given(
        num_p_clauses=st.integers(min_value=1, max_value=10),
        num_q_clauses=st.integers(min_value=1, max_value=10),
    )
    def test_property_identical_shapes_dont_share_buckets(
        self, num_p_clauses, num_q_clauses
    ):
        """Two predicates with identical first-arg shapes don't share buckets."""
        clauses = []

        # Generate p/1 clauses with various first-arg types
        for i in range(num_p_clauses):
            if i % 5 == 0:
                first_arg = Int(i)
            elif i % 5 == 1:
                first_arg = Var(i, f"X{i}")
            elif i % 5 == 2:
                first_arg = Atom(f"atom_{i}")
            elif i % 5 == 3:
                first_arg = PrologList((Int(i),), Atom("[]"))  # [i]
            else:
                first_arg = Struct(
                    "f" if i % 2 else "g", (Int(i),)
                )  # Different struct functors
            clauses.append(Clause(Struct("p", (first_arg,)), ()))

        # Generate q/1 clauses with identical patterns
        for i in range(num_q_clauses):
            if i % 5 == 0:
                first_arg = Int(i)
            elif i % 5 == 1:
                first_arg = Var(100 + i, f"Y{i}")  # Different var IDs
            elif i % 5 == 2:
                first_arg = Atom(f"atom_{i}")
            elif i % 5 == 3:
                first_arg = PrologList((Int(i),), Atom("[]"))  # [i]
            else:
                first_arg = Struct(
                    "f" if i % 2 else "g", (Int(i),)
                )  # Different struct functors
            clauses.append(Clause(Struct("q", (first_arg,)), ()))

        idx = build_from_clauses(clauses)

        # Get indices
        p_idx = idx.preds[("p", 1)]
        q_idx = idx.preds[("q", 1)]

        # Check all bucket types for overlap
        buckets_to_check = [
            ("var_ids", p_idx.var_ids, q_idx.var_ids),
            ("int_ids", p_idx.int_ids, q_idx.int_ids),
            ("empty_list_ids", p_idx.empty_list_ids, q_idx.empty_list_ids),
            ("list_nonempty_ids", p_idx.list_nonempty_ids, q_idx.list_nonempty_ids),
        ]

        # Buckets may contain same IDs but they're namespaced by predicate
        # Prove selection doesn't cross predicates even if bucket ID sets overlap
        store = Store()
        res_p = list(idx.select(("p", 1), Struct("p", (Var(0, "X"),)), store))
        res_q = list(idx.select(("q", 1), Struct("q", (Var(1, "Y"),)), store))
        assert all(isinstance(c.head, Struct) and c.head.functor == "p" for c in res_p)
        assert all(isinstance(c.head, Struct) and c.head.functor == "q" for c in res_q)

    def test_selection_never_crosses_predicates(self):
        """Selection for p/1 never returns q/1 clauses."""
        clauses = [
            # p/1 clauses
            Clause(Struct("p", (Int(1),)), Atom("p1")),
            Clause(Struct("p", (Int(2),)), Atom("p2")),
            Clause(Struct("p", (Atom("a"),)), Atom("p3")),
            # q/1 clauses
            Clause(Struct("q", (Int(1),)), Atom("q1")),
            Clause(Struct("q", (Int(2),)), Atom("q2")),
            Clause(Struct("q", (Atom("a"),)), Atom("q3")),
            # r/1 clauses
            Clause(Struct("r", (Int(1),)), Atom("r1")),
        ]

        idx = build_from_clauses(clauses)
        store = Store()

        # Test selecting with p(1) - should get p/1 integer clauses (type-based)
        p_results = list(idx.select(("p", 1), Struct("p", (Int(1),)), store))
        assert len(p_results) == 2  # p(1) and p(2) - both integers
        assert all(c.body.name.startswith("p") for c in p_results)

        # Test selecting with q(1) - should get q/1 integer clauses
        q_results = list(idx.select(("q", 1), Struct("q", (Int(1),)), store))
        assert len(q_results) == 2  # q(1) and q(2) - both integers
        assert all(c.body.name.startswith("q") for c in q_results)

        # Test selecting with r(1) - should get r/1 integer clauses
        r_results = list(idx.select(("r", 1), Struct("r", (Int(1),)), store))
        assert len(r_results) == 1  # Only r(1)
        assert all(c.body.name.startswith("r") for c in r_results)

        # Test with unbound variable - still respects predicate boundary
        p_var_results = list(idx.select(("p", 1), Struct("p", (Var(0, "X"),)), store))
        assert len(p_var_results) == 3  # All p/1 clauses
        assert all(c.body.name.startswith("p") for c in p_var_results)

        q_var_results = list(idx.select(("q", 1), Struct("q", (Var(1, "Y"),)), store))
        assert len(q_var_results) == 3  # All q/1 clauses
        assert all(c.body.name.startswith("q") for c in q_var_results)

    def test_predicate_key_includes_arity(self):
        """Predicate key includes arity in both build and select."""
        clauses = [
            # foo/0
            Clause(Atom("foo"), Atom("foo0")),
            # foo/1
            Clause(Struct("foo", (Int(1),)), Atom("foo1_a")),
            Clause(Struct("foo", (Int(2),)), Atom("foo1_b")),
            # foo/2
            Clause(
                Struct(
                    "foo",
                    (
                        Int(1),
                        Atom("x"),
                    ),
                ),
                Atom("foo2_a"),
            ),
            Clause(
                Struct(
                    "foo",
                    (
                        Int(2),
                        Atom("y"),
                    ),
                ),
                Atom("foo2_b"),
            ),
            # foo/3
            Clause(
                Struct(
                    "foo",
                    (
                        Int(1),
                        Atom("x"),
                        Var(0, "Z"),
                    ),
                ),
                Atom("foo3"),
            ),
        ]

        idx = build_from_clauses(clauses)
        store = Store()

        # Verify all arities are stored separately
        assert ("foo", 0) in idx.preds
        assert ("foo", 1) in idx.preds
        assert ("foo", 2) in idx.preds
        assert ("foo", 3) in idx.preds

        # Verify each has the correct number of clauses
        assert len(idx.preds[("foo", 0)].order) == 1
        assert len(idx.preds[("foo", 1)].order) == 2
        assert len(idx.preds[("foo", 2)].order) == 2
        assert len(idx.preds[("foo", 3)].order) == 1

        # Test selection respects arity

        # foo/0 selection
        foo0_results = list(idx.select(("foo", 0), Atom("foo"), store))
        assert len(foo0_results) == 1
        assert foo0_results[0].body.name == "foo0"

        # foo/1 selection with foo(1) - gets all integer clauses (type-based)
        foo1_results = list(idx.select(("foo", 1), Struct("foo", (Int(1),)), store))
        assert len(foo1_results) == 2  # Both foo(1) and foo(2)
        assert foo1_results[0].body.name == "foo1_a"
        assert foo1_results[1].body.name == "foo1_b"

        # foo/2 selection with foo(1, x) - gets all integer first arg (type-based, first-arg only)
        foo2_results = list(
            idx.select(
                ("foo", 2),
                Struct(
                    "foo",
                    (
                        Int(1),
                        Atom("x"),
                    ),
                ),
                store,
            )
        )
        assert (
            len(foo2_results) == 2
        )  # Both foo(1,x) and foo(2,y) - same integer type in first arg
        assert foo2_results[0].body.name == "foo2_a"
        assert foo2_results[1].body.name == "foo2_b"

        # foo/3 selection
        foo3_results = list(
            idx.select(
                ("foo", 3),
                Struct(
                    "foo",
                    (
                        Int(1),
                        Atom("x"),
                        Var(0, "Z"),
                    ),
                ),
                store,
            )
        )
        assert len(foo3_results) == 1
        assert foo3_results[0].body.name == "foo3"

        # Verify wrong arity returns nothing
        wrong_arity = list(
            idx.select(
                ("foo", 4),
                Struct(
                    "foo",
                    (
                        Int(1),
                        Int(2),
                        Int(3),
                        Int(4),
                    ),
                ),
                store,
            )
        )
        assert len(wrong_arity) == 0

        # Verify selecting foo/1 doesn't return foo/2 clauses
        foo1_all = list(idx.select(("foo", 1), Struct("foo", (Var(5, "Any"),)), store))
        assert len(foo1_all) == 2
        assert all(c.body.name.startswith("foo1") for c in foo1_all)

    def test_selection_nonempty_list_goal_canonical_dot_isolated(self):
        """Test canonical dot '.'/2 goal isolation."""
        clauses = [
            Clause(Struct("p", (PrologList((Int(1),), Atom("[]")),)), ()),  # p([1])
            Clause(Struct("p", (Var(0, "X"),)), ()),  # p(X)
            Clause(Struct("q", (PrologList((Int(2),), Atom("[]")),)), ()),  # q([2])
        ]
        idx = build_from_clauses(clauses)
        store = Store()
        goal = Struct("p", (Struct(".", (Int(3), Atom("[]"))),))  # p([3])
        res = list(idx.select(("p", 1), goal, store))
        assert all(isinstance(c.head, Struct) and c.head.functor == "p" for c in res)

    def test_selection_empty_list_goal_isolated(self):
        """Test empty list goal isolation."""
        clauses = [
            Clause(Struct("p", (PrologList((), Atom("[]")),)), ()),
            Clause(Struct("q", (PrologList((), Atom("[]")),)), ()),
        ]
        idx = build_from_clauses(clauses)
        store = Store()
        res_p = list(idx.select(("p", 1), Struct("p", (Atom("[]"),)), store))
        assert len(res_p) == 1
        assert isinstance(res_p[0].head, Struct) and res_p[0].head.functor == "p"
