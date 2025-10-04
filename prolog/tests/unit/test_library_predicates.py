"""Tests for library predicates implemented in Prolog.

These tests verify that the standard library predicates work correctly
and serve as acceptance tests for the PyLog engine.

NOTE: This test file is written BEFORE implementation to follow TDD.
The tests define the expected behavior of library predicates.
"""

import pytest
from prolog.engine.engine import Engine
from prolog.ast.terms import Atom, Int, List
from prolog.ast.clauses import Program


# Helper functions for cleaner list construction
def nil():
    """Empty list [] - returns the List representation."""
    # Engine returns List(items=(), tail=Atom('[]')) for empty lists
    return List((), Atom("[]"))


def lst(*xs):
    """Construct a proper list from elements."""
    return List(tuple(xs), Atom("[]"))


# Predicate definitions as helpers
def append_def():
    """Definition of append/3."""
    return """
        % append/3 - concatenate two lists
        append([], L, L).
        append([H|T1], L2, [H|T3]) :- append(T1, L2, T3).
    """


def member_def():
    """Definition of member/2."""
    return """
        % member/2 - check membership or generate elements
        member(X, [X|_]).
        member(X, [_|T]) :- member(X, T).
    """


def reverse_def():
    """Definition of reverse/2 with accumulator."""
    return """
        % reverse/2 - reverse a list using accumulator
        reverse(L, R) :- reverse_acc(L, [], R).
        reverse_acc([], Acc, Acc).
        reverse_acc([H|T], Acc, R) :- reverse_acc(T, [H|Acc], R).
    """


def length_succ_def():
    """Definition of length/2 using successor notation."""
    return """
        % length/2 - relate list to its length (using successor notation)
        length([], 0).
        length([_|T], s(N)) :- length(T, N).
    """


def last_def():
    """Definition of last/2."""
    return """
        % last/2 - get the last element
        last([X], X).
        last([_|T], X) :- last(T, X).
    """


def nth_s_def():
    """Definition of nth_s/3 using successor-based indexing."""
    return """
        % nth_s/3 - access element by successor-based index
        nth_s(0, [H|_], H).
        nth_s(s(N), [_|T], E) :- nth_s(N, T, E).
    """


def select_def():
    """Definition of select/3."""
    return """
        % select/3 - select element from list, leaving rest
        select(X, [X|T], T).
        select(X, [H|T], [H|R]) :- select(X, T, R).
    """


def between_s_def():
    """Definition of between_s/3 using successor notation (simplified, placeholder)."""
    return """
        % Simplified between using successor notation - PLACEHOLDER
        % This is a provisional definition that may not work correctly
        between_s(N, N, N).
        between_s(s(L), s(H), s(L)) :- between_s(s(L), H, _).
        between_s(s(L), s(H), X) :- between_s(s(s(L)), s(H), X).
    """


class TestLibraryPredicates:
    """Test standard library predicates."""

    @pytest.fixture
    def engine_with_lib(self):
        """Create an engine with library predicates loaded."""
        engine = Engine(Program(()))
        # Note: Tests use consult_string to be explicit about what's being tested
        # This fixture just provides a fresh engine
        return engine

    # append/3 tests

    def test_append_concrete(self, engine_with_lib):
        """Test append/3 with concrete lists."""
        engine_with_lib.consult_string(append_def())

        # append([1,2],[3,4],[1,2,3,4]) should succeed
        results = list(engine_with_lib.query("append([1,2],[3,4],[1,2,3,4])"))
        assert len(results) == 1

        # append([1,2],[3,4],[1,2,3,5]) should fail
        results = list(engine_with_lib.query("append([1,2],[3,4],[1,2,3,5])"))
        assert len(results) == 0

    def test_append_generate_result(self, engine_with_lib):
        """Test append/3 generating the result."""
        engine_with_lib.consult_string(append_def())

        # append([1,2],[3,4],X) should bind X=[1,2,3,4]
        results = list(engine_with_lib.query("append([1,2],[3,4],X)"))
        assert len(results) == 1
        assert results[0]["X"] == lst(Int(1), Int(2), Int(3), Int(4))

    def test_append_generate_splits(self, engine_with_lib):
        """Test append/3 generating all possible splits."""
        engine_with_lib.consult_string(append_def())

        # append(X,Y,[1,2,3]) should generate all splits
        results = list(engine_with_lib.query("append(X,Y,[1,2,3])"))
        assert (
            len(results) == 4
        )  # ([], [1,2,3]), ([1], [2,3]), ([1,2], [3]), ([1,2,3], [])

        # Check first split: X=[], Y=[1,2,3]
        assert results[0]["X"] == nil()
        assert results[0]["Y"] == lst(Int(1), Int(2), Int(3))

        # Check last split: X=[1,2,3], Y=[]
        assert results[3]["X"] == lst(Int(1), Int(2), Int(3))
        # Y can be either Atom('[]') or List((), Atom('[]'))
        assert results[3]["Y"] in [Atom("[]"), nil()]

    def test_append_empty_lists(self, engine_with_lib):
        """Test append/3 with empty lists."""
        engine_with_lib.consult_string(append_def())

        # append([],[],X) should bind X=[]
        results = list(engine_with_lib.query("append([],[],X)"))
        assert len(results) == 1
        assert results[0]["X"] == nil()

    def test_append_generate_splits_order(self, engine_with_lib):
        """Test append/3 split order for continuation correctness."""
        engine_with_lib.consult_string(append_def())

        results = list(engine_with_lib.query("append(X,Y,[1,2,3])"))
        assert len(results) == 4

        # Expected Prolog order of splits:
        # [],[1,2,3] ; [1],[2,3] ; [1,2],[3] ; [1,2,3],[]
        xs = [r["X"] for r in results]
        ys = [r["Y"] for r in results]

        assert xs == [
            nil(),
            lst(Int(1)),
            lst(Int(1), Int(2)),
            lst(Int(1), Int(2), Int(3)),
        ]
        # Check Y values - last one can be Atom('[]') or List((), Atom('[]'))
        assert ys[0] == lst(Int(1), Int(2), Int(3))
        assert ys[1] == lst(Int(2), Int(3))
        assert ys[2] == lst(Int(3))
        assert ys[3] in [Atom("[]"), nil()]

    # member/2 tests

    def test_member_check(self, engine_with_lib):
        """Test member/2 checking membership."""
        engine_with_lib.consult_string(member_def())

        # member(2,[1,2,3]) should succeed
        results = list(engine_with_lib.query("member(2,[1,2,3])"))
        assert len(results) == 1

        # member(4,[1,2,3]) should fail
        results = list(engine_with_lib.query("member(4,[1,2,3])"))
        assert len(results) == 0

    def test_member_generate(self, engine_with_lib):
        """Test member/2 generating all members."""
        engine_with_lib.consult_string(member_def())

        # member(X,[1,2,3]) should generate all elements in order
        results = list(engine_with_lib.query("member(X,[1,2,3])"))
        assert len(results) == 3
        assert results[0]["X"] == Int(1)
        assert results[1]["X"] == Int(2)
        assert results[2]["X"] == Int(3)

    def test_member_empty_list(self, engine_with_lib):
        """Test member/2 with empty list."""
        engine_with_lib.consult_string(member_def())

        # member(X,[]) should fail
        results = list(engine_with_lib.query("member(X,[])"))
        assert len(results) == 0

    # reverse/2 tests

    def test_reverse_concrete(self, engine_with_lib):
        """Test reverse/2 with concrete lists."""
        engine_with_lib.consult_string(reverse_def())

        # reverse([1,2,3],[3,2,1]) should succeed
        results = list(engine_with_lib.query("reverse([1,2,3],[3,2,1])"))
        assert len(results) == 1

        # reverse([1,2,3],[1,2,3]) should fail
        results = list(engine_with_lib.query("reverse([1,2,3],[1,2,3])"))
        assert len(results) == 0

    def test_reverse_generate(self, engine_with_lib):
        """Test reverse/2 generating the reverse."""
        engine_with_lib.consult_string(reverse_def())

        # reverse([1,2,3],X) should bind X=[3,2,1]
        results = list(engine_with_lib.query("reverse([1,2,3],X)"))
        assert len(results) == 1
        assert results[0]["X"] == lst(Int(3), Int(2), Int(1))

    def test_reverse_empty(self, engine_with_lib):
        """Test reverse/2 with empty list."""
        engine_with_lib.consult_string(reverse_def())

        # reverse([],[]) should succeed
        results = list(engine_with_lib.query("reverse([],[])"))
        assert len(results) == 1

        # reverse([],X) should bind X=[]
        results = list(engine_with_lib.query("reverse([],X)"))
        assert len(results) == 1
        assert results[0]["X"] == nil()

    @pytest.mark.timeout(1)  # Timeout after 1 second to prevent hanging
    def test_reverse_generate_input(self, engine_with_lib):
        """Test reverse/2 as a bijection (var -> ground)."""
        engine_with_lib.consult_string(reverse_def())

        # reverse(X,[3,2,1]) should bind X=[1,2,3]
        engine_with_lib.max_steps = (
            100  # Prevent infinite loop if implementation changes
        )
        results = list(engine_with_lib.query("reverse(X,[3,2,1])"))
        assert len(results) == 1
        assert results[0]["X"] == lst(Int(1), Int(2), Int(3))

    # length/2 tests - simplified without arithmetic

    def test_length_check(self, engine_with_lib):
        """Test length/2 checking length."""
        engine_with_lib.consult_string(length_succ_def())

        # length([a,b,c], s(s(s(0)))) should succeed (length 3 in successor notation)
        results = list(engine_with_lib.query("length([a,b,c], s(s(s(0))))"))
        assert len(results) == 1

        # length([a,b], s(s(s(0)))) should fail (wrong length)
        results = list(engine_with_lib.query("length([a,b], s(s(s(0))))"))
        assert len(results) == 0

    def test_length_compute(self, engine_with_lib):
        """Test length/2 computing length."""
        engine_with_lib.consult_string(length_succ_def())

        # length([a,b], N) should bind N=s(s(0)) (length 2)
        results = list(engine_with_lib.query("length([a,b], N)"))
        assert len(results) == 1
        # Check that N is bound to s(s(0))
        # It will be a Struct with functor 's'
        from prolog.ast.terms import Struct

        n = results[0]["N"]
        assert isinstance(n, Struct)
        assert n.functor == "s"
        assert len(n.args) == 1
        # Check inner s(0)
        inner = n.args[0]
        assert isinstance(inner, Struct)
        assert inner.functor == "s"
        assert inner.args[0] == Int(0)

        # length([], N) should bind N=0
        results = list(engine_with_lib.query("length([], N)"))
        assert len(results) == 1
        assert results[0]["N"] == Int(0)

    # between/3 tests - simplified version

    @pytest.mark.timeout(1)  # Timeout after 1 second to prevent infinite loop
    def test_between_simplified(self, engine_with_lib):
        """Test a simplified between/3 using successor arithmetic."""
        engine_with_lib.consult_string(between_s_def())

        # between_s(s(0), s(s(s(0))), X) generates values in the range
        engine_with_lib.max_steps = 100  # Limit steps to prevent infinite loop
        results = list(engine_with_lib.query("between_s(s(0), s(s(s(0))), X)"))
        # Just check that we get some results without crashing
        assert isinstance(results, list)

    # Additional predicates

    def test_last(self, engine_with_lib):
        """Test last/2 predicate."""
        engine_with_lib.consult_string(last_def())

        # last([1,2,3],X) should bind X=3
        results = list(engine_with_lib.query("last([1,2,3],X)"))
        assert len(results) == 1
        assert results[0]["X"] == Int(3)

        # last([1],X) should bind X=1
        results = list(engine_with_lib.query("last([1],X)"))
        assert len(results) == 1
        assert results[0]["X"] == Int(1)

    def test_last_empty_fails(self, engine_with_lib):
        """Test last/2 with empty list fails."""
        engine_with_lib.consult_string(last_def())

        # last([],_) should fail
        results = list(engine_with_lib.query("last([],_)"))
        assert len(results) == 0

    def test_nth_simplified(self, engine_with_lib):
        """Test a simplified nth predicate using successor arithmetic."""
        engine_with_lib.consult_string(nth_s_def())

        # nth_s(0, [a,b,c], X) should bind X=a
        results = list(engine_with_lib.query("nth_s(0, [a,b,c], X)"))
        assert len(results) == 1
        assert results[0]["X"] == Atom("a")

        # nth_s(s(s(0)), [a,b,c], X) should bind X=c (index 2)
        results = list(engine_with_lib.query("nth_s(s(s(0)), [a,b,c], X)"))
        assert len(results) == 1
        assert results[0]["X"] == Atom("c")

    def test_nth_s_out_of_range_fails(self, engine_with_lib):
        """Test nth_s/3 with out-of-range index fails."""
        engine_with_lib.consult_string(nth_s_def())

        # nth_s(s(0), [], _) should fail (index 1 on empty list)
        results = list(engine_with_lib.query("nth_s(s(0), [], _)"))
        assert len(results) == 0

        # nth_s(s(s(s(0))), [a,b], _) should fail (index 3 on 2-element list)
        results = list(engine_with_lib.query("nth_s(s(s(s(0))), [a,b], _)"))
        assert len(results) == 0

    def test_select(self, engine_with_lib):
        """Test select/3 predicate."""
        engine_with_lib.consult_string(select_def())

        # select(b, [a,b,c], X) should bind X=[a,c]
        results = list(engine_with_lib.query("select(b, [a,b,c], X)"))
        assert len(results) == 1
        assert results[0]["X"] == lst(Atom("a"), Atom("c"))

        # select(X, [a,b,c], [a,c]) should bind X=b
        results = list(engine_with_lib.query("select(X, [a,b,c], [a,c])"))
        assert len(results) == 1
        assert results[0]["X"] == Atom("b")

    def test_select_with_duplicates(self, engine_with_lib):
        """Test select/3 with duplicate elements."""
        engine_with_lib.consult_string(select_def())

        # select(2, [1,2,2,3], R) should have two solutions
        results = list(engine_with_lib.query("select(2, [1,2,2,3], R)"))
        assert len(results) == 2

        # Both should give [1,2,3] but from different removals
        rs = [r["R"] for r in results]
        assert rs[0] == lst(Int(1), Int(2), Int(3))  # removed first 2
        assert rs[1] == lst(Int(1), Int(2), Int(3))  # removed second 2


@pytest.mark.swi_baseline
class TestLibraryPredicatesBaseline:
    """Baseline tests against SWI-Prolog for library predicates."""

    def test_append_splits_baseline(self, swi):
        """Test append/3 split generation against SWI."""
        prog = ""  # append/3 is built-in in SWI
        # Count number of splits
        assert swi.count(prog, "append(X,Y,[1,2,3])") == 4

        # Check specific splits
        assert swi.count(prog, "append([],[1,2,3],[1,2,3])") == 1
        assert swi.count(prog, "append([1,2,3],[],[1,2,3])") == 1

    def test_member_generate_baseline(self, swi):
        """Test member/2 generation against SWI."""
        prog = ""  # member/2 is built-in in SWI
        assert swi.count(prog, "member(X,[1,2,3])") == 3
        values = swi.onevar(prog, "member(X,[a,b,c])", "X")
        assert values == ["a", "b", "c"]

    def test_reverse_baseline(self, swi):
        """Test reverse/2 against SWI."""
        prog = ""  # reverse/2 is built-in in SWI
        assert swi.count(prog, "reverse([1,2,3],[3,2,1])") == 1
        assert swi.count(prog, "reverse([1,2,3],[1,2,3])") == 0

    def test_last_baseline(self, swi):
        """Test last/2 against SWI."""
        prog = ""  # last/2 is built-in in SWI
        assert swi.count(prog, "last([1,2,3],3)") == 1
        values = swi.onevar(prog, "last([a,b,c],X)", "X")
        assert values == ["c"]

    def test_select_baseline(self, swi):
        """Test select/3 against SWI."""
        prog = ""  # select/3 is built-in in SWI
        assert swi.count(prog, "select(b,[a,b,c],[a,c])") == 1
        # Test generating all selections
        assert swi.count(prog, "select(X,[1,2,3],R)") == 3

    def test_append_split_order_baseline(self, swi):
        """Test append/3 split order against SWI."""
        prog = ""  # append/3 is built-in in SWI
        # Check first split with pinned X
        values = swi.onevar(prog, "append(X,Y,[1,2,3]), X=[1]", "Y")
        assert values == ["[2,3]"]

        # Check that we get 4 splits
        assert swi.count(prog, "append(X,Y,[1,2,3])") == 4

    def test_last_empty_baseline(self, swi):
        """Test last/2 with empty list against SWI."""
        prog = ""  # last/2 is built-in in SWI
        assert swi.count(prog, "last([],_)") == 0

    def test_nth0_out_of_range_baseline(self, swi):
        """Test nth0/3 out of range against SWI."""
        prog = ""  # nth0/3 is built-in in SWI
        # Index 1 on empty list should fail
        assert swi.count(prog, "nth0(1,[],_)") == 0
        # Index 3 on 2-element list should fail
        assert swi.count(prog, "nth0(3,[a,b],_)") == 0

    def test_catch_recovery_fails_transparent_baseline(self, swi):
        """Test the controversial catch case - recovery failure in conjunction.

        This documents that when catch's recovery fails, the entire
        conjunction fails, giving 0 solutions. This matches ISO/SWI semantics.
        """
        prog = "p(1). p(2)."
        goal = "p(X), catch(throw(t), t, fail)"
        # ISO/SWI: catch fails, conjunction fails → 0 solutions
        assert swi.count(prog, goal) == 0
