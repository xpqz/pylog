"""Test that library files can be loaded and work correctly."""

from pathlib import Path
from prolog.engine.engine import Engine
from prolog.ast.terms import Atom, Int, List
from prolog.ast.clauses import Program


class TestLibraryLoading:
    """Test loading library files."""

    def test_load_lists_library(self):
        """Test that lists.pl can be loaded and its predicates work."""
        engine = Engine(Program(()))

        # Load the library file
        lib_path = Path("prolog/lib/lists.pl")
        assert lib_path.exists(), "Library file should exist"

        with open(lib_path, "r") as f:
            lib_code = f.read()

        # Load it into the engine
        engine.consult_string(lib_code)

        # Test a few key predicates to ensure they loaded correctly

        # Test append/3
        results = list(engine.query("append([1,2],[3,4],X)"))
        assert len(results) == 1
        expected = List((Int(1), Int(2), Int(3), Int(4)), Atom("[]"))
        assert results[0]["X"] == expected

        # Test member/2
        results = list(engine.query("member(2,[1,2,3])"))
        assert len(results) == 1

        # Test reverse/2
        results = list(engine.query("reverse([1,2,3],X)"))
        assert len(results) == 1
        expected = List((Int(3), Int(2), Int(1)), Atom("[]"))
        assert results[0]["X"] == expected

        # Test last/2
        results = list(engine.query("last([a,b,c],X)"))
        assert len(results) == 1
        assert results[0]["X"] == Atom("c")

        # Test select/3
        results = list(engine.query("select(b,[a,b,c],X)"))
        assert len(results) == 1
        expected = List((Atom("a"), Atom("c")), Atom("[]"))
        assert results[0]["X"] == expected

    def test_library_predicates_are_reusable(self):
        """Test that library predicates can be used in user-defined predicates."""
        engine = Engine(Program(()))

        # Load the library
        with open("prolog/lib/lists.pl", "r") as f:
            lib_code = f.read()
        engine.consult_string(lib_code)

        # Define a predicate that uses append/3
        engine.consult_string(
            """
            double_list(L, D) :- append(L, L, D).
        """
        )

        # Test the user-defined predicate
        results = list(engine.query("double_list([1,2], X)"))
        assert len(results) == 1
        expected = List((Int(1), Int(2), Int(1), Int(2)), Atom("[]"))
        assert results[0]["X"] == expected

    def test_library_predicates_interact_correctly(self):
        """Test that library predicates can be composed."""
        engine = Engine(Program(()))

        # Load the library
        with open("prolog/lib/lists.pl", "r") as f:
            lib_code = f.read()
        engine.consult_string(lib_code)

        # Test composing reverse and append
        engine.consult_string(
            """
            rev_append(L1, L2, R) :- 
                reverse(L1, R1),
                append(R1, L2, R).
        """
        )

        # Test: reverse [1,2] to get [2,1], then append [3,4] to get [2,1,3,4]
        results = list(engine.query("rev_append([1,2], [3,4], X)"))
        assert len(results) == 1
        expected = List((Int(2), Int(1), Int(3), Int(4)), Atom("[]"))
        assert results[0]["X"] == expected
