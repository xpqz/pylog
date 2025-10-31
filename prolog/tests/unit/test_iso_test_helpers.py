"""Tests for ISO test helper predicates.

These predicates are used by the ISO test suite to support test execution.
They provide functionality for file loading, term comparison, OS detection,
stream handling, and list comparison.
"""

import platform
from prolog.engine.engine import Engine
from prolog.ast.terms import Atom
from prolog.ast.clauses import Program
from prolog.engine.builtins.streams import get_stream_manager


class TestIsoTestEnsureLoaded:
    """Test iso_test_ensure_loaded/1 predicate."""

    def test_load_file_basic(self, tmp_path):
        """Test loading a simple Prolog file."""
        # Create a temporary Prolog file
        test_file = tmp_path / "test.pl"
        test_file.write_text("fact(a).\nfact(b).\nrule(X) :- fact(X).\n")

        engine = Engine(Program(()))

        # Load the file
        query = f"iso_test_ensure_loaded('{test_file}')"
        results = list(engine.query(query))
        assert len(results) == 1

        # Verify the predicates were loaded
        results = list(engine.query("fact(a)"))
        assert len(results) == 1

        results = list(engine.query("fact(X)"))
        assert len(results) == 2

        results = list(engine.query("rule(a)"))
        assert len(results) == 1

    def test_load_file_with_atom_path(self, tmp_path):
        """Test loading with atom argument."""
        test_file = tmp_path / "simple.pl"
        test_file.write_text("simple_fact.\n")

        engine = Engine(Program(()))
        results = list(engine.query(f"iso_test_ensure_loaded('{test_file}')"))
        assert len(results) == 1

        # Verify loaded
        results = list(engine.query("simple_fact"))
        assert len(results) == 1

    def test_load_nonexistent_file_fails(self):
        """Test that loading a non-existent file fails."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_ensure_loaded('/nonexistent/file.pl')"))
        assert len(results) == 0

    def test_load_file_wrong_arity(self):
        """Test that wrong arity fails."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_ensure_loaded"))
        assert len(results) == 0

        results = list(engine.query("iso_test_ensure_loaded(a, b)"))
        assert len(results) == 0


class TestIsoTestVariant:
    """Test iso_test_variant/2 predicate."""

    def test_simple_atoms_are_variants(self):
        """Test that identical atoms are variants."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_variant(a, a)"))
        assert len(results) == 1

    def test_different_atoms_not_variants(self):
        """Test that different atoms are not variants."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_variant(a, b)"))
        assert len(results) == 0

    def test_structures_same_variables_are_variants(self):
        """Test that f(X,Y) and f(A,B) are variants."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_variant(f(X,Y), f(A,B))"))
        assert len(results) == 1

    def test_structures_different_shape_not_variants(self):
        """Test that f(X,Y) and f(X,Y,Z) are not variants."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_variant(f(X,Y), f(X,Y,Z))"))
        assert len(results) == 0

    def test_structures_different_functor_not_variants(self):
        """Test that f(X) and g(X) are not variants."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_variant(f(X), g(X))"))
        assert len(results) == 0

    def test_variable_with_ground_not_variants(self):
        """Test that X and a are not variants."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_variant(X, a)"))
        assert len(results) == 0

    def test_complex_structures_are_variants(self):
        """Test complex nested structures."""
        engine = Engine(Program(()))
        query = "iso_test_variant(foo(bar(X,Y), baz(Z)), foo(bar(A,B), baz(C)))"
        results = list(engine.query(query))
        assert len(results) == 1

    def test_lists_are_variants(self):
        """Test that [X,Y,Z] and [A,B,C] are variants."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_variant([X,Y,Z], [A,B,C])"))
        assert len(results) == 1

    def test_lists_different_length_not_variants(self):
        """Test that [X,Y] and [A,B,C] are not variants."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_variant([X,Y], [A,B,C])"))
        assert len(results) == 0

    def test_ground_lists_identical_are_variants(self):
        """Test that [1,2,3] and [1,2,3] are variants."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_variant([1,2,3], [1,2,3])"))
        assert len(results) == 1

    def test_ground_lists_different_not_variants(self):
        """Test that [1,2,3] and [1,2,4] are not variants."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_variant([1,2,3], [1,2,4])"))
        assert len(results) == 0

    def test_variant_wrong_arity(self):
        """Test that wrong arity fails."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_variant(a)"))
        assert len(results) == 0

        results = list(engine.query("iso_test_variant(a, b, c)"))
        assert len(results) == 0


class TestIsoTestOs:
    """Test iso_test_os/1 predicate."""

    def test_os_returns_valid_value(self):
        """Test that iso_test_os/1 returns a valid OS identifier."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_os(OS)"))
        assert len(results) == 1

        os_term = results[0]["OS"]
        assert isinstance(os_term, Atom)
        # Should be one of the known OS values
        assert os_term.name in ["unix", "darwin", "windows", "linux"]

    def test_os_matches_platform(self):
        """Test that returned OS matches Python's platform."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_os(OS)"))
        os_term = results[0]["OS"]

        system = platform.system().lower()
        if system == "darwin":
            assert os_term.name == "darwin"
        elif system == "windows":
            assert os_term.name == "windows"
        elif system == "linux":
            assert os_term.name == "linux"
        else:
            # Other Unix-like systems
            assert os_term.name == "unix"

    def test_os_can_check_specific_value(self):
        """Test checking for specific OS value."""
        engine = Engine(Program(()))
        system = platform.system().lower()

        if system == "darwin":
            results = list(engine.query("iso_test_os(darwin)"))
            assert len(results) == 1

            results = list(engine.query("iso_test_os(windows)"))
            assert len(results) == 0

    def test_os_wrong_arity(self):
        """Test that wrong arity fails."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_os"))
        assert len(results) == 0

        results = list(engine.query("iso_test_os(a, b)"))
        assert len(results) == 0


class TestIsoTestNonRepositionableStream:
    """Test iso_test_non_repositionable_stream/1 predicate."""

    def test_creates_non_repositionable_stream(self):
        """Test that a non-repositionable stream is created."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_non_repositionable_stream(S)"))
        assert len(results) == 1

        # The stream should be bound to an atom representing the stream ID
        stream = results[0]["S"]
        assert isinstance(stream, Atom)

        # Verify the stream is registered in the StreamManager
        stream_manager = get_stream_manager()
        stream_obj = stream_manager.get_stream(stream.name)
        assert stream_obj is not None
        assert stream_obj.mode == "read"

    def test_stream_is_not_repositionable(self):
        """Test that the created stream is marked as non-repositionable."""
        engine = Engine(Program(()))

        # Create a non-repositionable stream
        results = list(engine.query("iso_test_non_repositionable_stream(S)"))
        assert len(results) == 1

        stream_id = results[0]["S"].name
        stream_manager = get_stream_manager()
        stream_obj = stream_manager.get_stream(stream_id)

        # Verify the stream is marked as non-repositionable
        assert stream_obj.repositionable is False

    def test_multiple_calls_create_different_streams(self):
        """Test that multiple calls create independent streams."""
        engine = Engine(Program(()))

        # Create two streams
        results1 = list(engine.query("iso_test_non_repositionable_stream(S1)"))
        results2 = list(engine.query("iso_test_non_repositionable_stream(S2)"))

        assert len(results1) == 1
        assert len(results2) == 1

        # Streams should be different
        stream1 = results1[0]["S1"]
        stream2 = results2[0]["S2"]
        assert stream1.name != stream2.name

    def test_wrong_arity(self):
        """Test that wrong arity fails."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_non_repositionable_stream"))
        assert len(results) == 0

        results = list(engine.query("iso_test_non_repositionable_stream(a, b)"))
        assert len(results) == 0


class TestIsoTestSameMembers:
    """Test iso_test_same_members/2 predicate."""

    def test_identical_lists_same_members(self):
        """Test that [1,2,3] and [1,2,3] have same members."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_same_members([1,2,3], [1,2,3])"))
        assert len(results) == 1

    def test_different_order_same_members(self):
        """Test that [1,2,3] and [3,2,1] have same members."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_same_members([1,2,3], [3,2,1])"))
        assert len(results) == 1

    def test_with_duplicates_same_members(self):
        """Test that [1,2,2,3] and [3,2,1,1,2] have same members after deduplication."""
        engine = Engine(Program(()))
        # sort/2 removes duplicates, so [1,2,2,3] -> [1,2,3] and [3,2,1,1,2] -> [1,2,3]
        results = list(engine.query("iso_test_same_members([1,2,2,3], [3,2,1,1,2])"))
        assert len(results) == 1

    def test_different_elements_not_same_members(self):
        """Test that [1,2,3] and [1,2,4] don't have same members."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_same_members([1,2,3], [1,2,4])"))
        assert len(results) == 0

    def test_different_length_not_same_members(self):
        """Test that [1,2] and [1,2,3] don't have same members."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_same_members([1,2], [1,2,3])"))
        assert len(results) == 0

    def test_empty_lists_same_members(self):
        """Test that [] and [] have same members."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_same_members([], [])"))
        assert len(results) == 1

    def test_empty_vs_nonempty_not_same_members(self):
        """Test that [] and [1] don't have same members."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_same_members([], [1])"))
        assert len(results) == 0

    def test_atoms_same_members(self):
        """Test with atom elements."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_same_members([a,b,c], [c,a,b])"))
        assert len(results) == 1

    def test_mixed_types_same_members(self):
        """Test with mixed types."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_same_members([1,a,2,b], [b,2,a,1])"))
        assert len(results) == 1

    def test_structures_same_members(self):
        """Test with structure elements."""
        engine = Engine(Program(()))
        query = "iso_test_same_members([f(1),g(2)], [g(2),f(1)])"
        results = list(engine.query(query))
        assert len(results) == 1

    def test_wrong_arity(self):
        """Test that wrong arity fails."""
        engine = Engine(Program(()))
        results = list(engine.query("iso_test_same_members([1,2,3])"))
        assert len(results) == 0

        results = list(engine.query("iso_test_same_members([1], [2], [3])"))
        assert len(results) == 0


class TestIsoTestHelpersIntegration:
    """Integration tests using multiple ISO test helper predicates together."""

    def test_load_and_test_variant(self, tmp_path):
        """Test loading a file and using variant/2."""
        test_file = tmp_path / "variant_test.pl"
        test_file.write_text("test_term(f(a,b)).\n")

        engine = Engine(Program(()))
        list(engine.query(f"iso_test_ensure_loaded('{test_file}')"))

        # Test that loaded term is variant with same structure
        results = list(engine.query("test_term(X), iso_test_variant(X, f(a,b))"))
        assert len(results) == 1

    def test_os_dependent_logic(self):
        """Test using os/1 for conditional logic."""
        engine = Engine(Program(()))

        # Define OS-specific predicate
        engine.consult_string(
            """
            path_separator(Sep) :-
                iso_test_os(unix),
                Sep = slash.
            path_separator(Sep) :-
                iso_test_os(darwin),
                Sep = slash.
            path_separator(Sep) :-
                iso_test_os(linux),
                Sep = slash.
            path_separator(Sep) :-
                iso_test_os(windows),
                Sep = backslash.
        """
        )

        # Should get appropriate separator for this platform
        results = list(engine.query("path_separator(S)"))
        assert len(results) >= 1  # At least one solution

        sep = results[0]["S"]
        system = platform.system().lower()
        if system == "windows":
            assert sep == Atom("backslash")
        else:
            assert sep == Atom("slash")

    def test_same_members_with_findall(self):
        """Test using same_members/2 with findall/3."""
        engine = Engine(Program(()))

        engine.consult_string(
            """
            item(1).
            item(2).
            item(3).
        """
        )

        # Collect all items and check they match expected set
        query = "findall(X, item(X), Items), iso_test_same_members(Items, [3,2,1])"
        results = list(engine.query(query))
        assert len(results) == 1
