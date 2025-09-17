"""Tests for graph exporters."""

import re
from prolog.ast.terms import Atom, Var, Int, Struct
from prolog.ast.clauses import Clause
from prolog.engine.engine import Program
from prolog.debug.exporters import (
    export_call_graph,
    export_constraint_graph,
    parse_dot_graph,
    extract_predicate_relationships
)


class TestCallGraphExporter:
    """Test call graph generation from Prolog programs."""

    def test_export_simple_facts(self):
        """Facts appear as nodes with no edges."""
        clauses = [
            Clause(Struct("parent", (Atom("tom"), Atom("bob"))), ()),
            Clause(Struct("parent", (Atom("bob"), Atom("pat"))), ()),
            Clause(Struct("male", (Atom("tom"),)), ()),
            Clause(Struct("male", (Atom("bob"),)), ())
        ]
        program = Program(tuple(clauses))

        dot = export_call_graph(program)

        # Should be valid DOT format
        assert dot.startswith("digraph call_graph {")
        assert dot.endswith("}")

        # Should have nodes for predicates
        assert '"parent/2"' in dot
        assert '"male/1"' in dot

        # Facts have no outgoing edges
        assert "->" not in dot

    def test_export_simple_rules(self):
        """Rules create edges from head to body predicates."""
        clauses = [
            Clause(Struct("parent", (Atom("tom"), Atom("bob"))), ()),
            Clause(Struct("parent", (Atom("bob"), Atom("pat"))), ()),
            Clause(
                Struct("grandparent", (Var(0, "X"), Var(1, "Z"))),
                (
                    Struct("parent", (Var(0, "X"), Var(2, "Y"))),
                    Struct("parent", (Var(2, "Y"), Var(1, "Z")))
                )
            )
        ]
        program = Program(tuple(clauses))

        dot = export_call_graph(program)

        # Should have nodes
        assert '"parent/2"' in dot
        assert '"grandparent/2"' in dot

        # Should have edge from grandparent to parent
        assert '"grandparent/2" -> "parent/2"' in dot

    def test_export_recursive_predicates(self):
        """Recursive predicates create self-edges."""
        clauses = [
            Clause(Struct("nat", (Int(0),)), ()),
            Clause(
                Struct("nat", (Struct("s", (Var(0, "N"),)),)),
                (Struct("nat", (Var(0, "N"),)),)
            )
        ]
        program = Program(tuple(clauses))

        dot = export_call_graph(program)

        # Should have node
        assert '"nat/1"' in dot

        # Should have self-edge
        assert '"nat/1" -> "nat/1"' in dot

    def test_export_mutual_recursion(self):
        """Mutually recursive predicates have bidirectional edges."""
        clauses = [
            Clause(Struct("even", (Int(0),)), ()),
            Clause(
                Struct("even", (Struct("s", (Struct("s", (Var(0, "N"),)),)),)),
                (Struct("even", (Var(0, "N"),)),)
            ),
            Clause(
                Struct("odd", (Struct("s", (Var(0, "N"),)),)),
                (Struct("even", (Var(0, "N"),)),)
            ),
            Clause(
                Struct("even", (Struct("s", (Var(0, "N"),)),)),
                (Struct("odd", (Var(0, "N"),)),)
            )
        ]
        program = Program(tuple(clauses))

        dot = export_call_graph(program)

        # Should have both nodes
        assert '"even/1"' in dot
        assert '"odd/1"' in dot

        # Should have mutual edges exactly once each
        assert dot.count('"even/1" -> "odd/1"') == 1
        assert dot.count('"odd/1" -> "even/1"') == 1
        # Self-edge for even's recursion
        assert '"even/1" -> "even/1"' in dot

    def test_export_with_builtins(self):
        """Built-in predicates appear as nodes with special styling."""
        clauses = [
            Clause(
                Struct("test", (Var(0, "X"), Var(1, "Y"))),
                (
                    Struct("is", (Var(0, "X"), Struct("+", (Int(1), Int(2))))),
                    Struct("=", (Var(1, "Y"), Var(0, "X"))),
                    Atom("!")
                )
            )
        ]
        program = Program(tuple(clauses))

        dot = export_call_graph(program)

        # Should have test predicate
        assert '"test/2"' in dot

        # Should have edges to builtins
        assert '"test/2" -> "is/2"' in dot
        assert '"test/2" -> "=/2"' in dot
        assert '"test/2" -> "!/0"' in dot

        # Built-ins should have special style (anchored to node)
        # At least one of these patterns should match for each builtin
        builtin_style_pattern = r'"\w+/\d+"\s*\[(?:[^\]]*(?:style=dashed|shape=box)[^\]]*)\]'
        assert re.search(builtin_style_pattern.replace(r'\w+/\d+', 'is/2'), dot)
        assert re.search(builtin_style_pattern.replace(r'\w+/\d+', '=/2'), dot)
        assert re.search(builtin_style_pattern.replace(r'\w+/\d+', '!/0'), dot)

    def test_export_complex_program(self):
        """Complex program with multiple predicates and relationships."""
        clauses = [
            # Facts
            Clause(Struct("parent", (Atom("tom"), Atom("bob"))), ()),
            Clause(Struct("parent", (Atom("bob"), Atom("ann"))), ()),
            Clause(Struct("male", (Atom("tom"),)), ()),
            Clause(Struct("male", (Atom("bob"),)), ()),
            Clause(Struct("female", (Atom("ann"),)), ()),

            # Rules
            Clause(
                Struct("father", (Var(0, "X"), Var(1, "Y"))),
                (
                    Struct("parent", (Var(0, "X"), Var(1, "Y"))),
                    Struct("male", (Var(0, "X"),))
                )
            ),
            Clause(
                Struct("mother", (Var(0, "X"), Var(1, "Y"))),
                (
                    Struct("parent", (Var(0, "X"), Var(1, "Y"))),
                    Struct("female", (Var(0, "X"),))
                )
            ),
            Clause(
                Struct("ancestor", (Var(0, "X"), Var(1, "Y"))),
                (Struct("parent", (Var(0, "X"), Var(1, "Y"))),)
            ),
            Clause(
                Struct("ancestor", (Var(0, "X"), Var(1, "Z"))),
                (
                    Struct("parent", (Var(0, "X"), Var(2, "Y"))),
                    Struct("ancestor", (Var(2, "Y"), Var(1, "Z")))
                )
            )
        ]
        program = Program(tuple(clauses))

        dot = export_call_graph(program)

        # All predicates should be nodes
        for pred in ["parent/2", "male/1", "female/1", "father/2", "mother/2", "ancestor/2"]:
            assert f'"{pred}"' in dot

        # Check edges
        assert '"father/2" -> "parent/2"' in dot
        assert '"father/2" -> "male/1"' in dot
        assert '"mother/2" -> "parent/2"' in dot
        assert '"mother/2" -> "female/1"' in dot
        assert '"ancestor/2" -> "parent/2"' in dot
        assert '"ancestor/2" -> "ancestor/2"' in dot  # Recursive

    def test_dot_format_validity(self):
        """Generated DOT should be syntactically valid."""
        clauses = [
            Clause(
                Struct("p", (Var(0, "X"),)),
                (Struct("q", (Var(0, "X"),)),)
            ),
            Clause(Struct("q", (Atom("a"),)), ()),
            Clause(Struct("q", (Atom("b"),)), ())  # Multiple clauses of q
        ]
        program = Program(tuple(clauses))

        dot = export_call_graph(program)

        # Basic structure
        assert dot.startswith("digraph call_graph {")
        assert dot.endswith("}")

        # Node declarations (should appear exactly once despite multiple clauses)
        node_p_count = len(re.findall(r'^\s*"p/1"\s*(?:\[.*?\])?;\s*$', dot, re.MULTILINE))
        node_q_count = len(re.findall(r'^\s*"q/1"\s*(?:\[.*?\])?;\s*$', dot, re.MULTILINE))
        assert node_p_count == 1, f"p/1 node declared {node_p_count} times"
        assert node_q_count == 1, f"q/1 node declared {node_q_count} times"

        # Edge declarations
        assert re.search(r'"p/1"\s*->\s*"q/1"\s*;', dot)

        # No duplicate edges (should deduplicate)
        edge_count = dot.count('"p/1" -> "q/1"')
        assert edge_count == 1

    def test_empty_program(self):
        """Empty program generates minimal graph."""
        program = Program(())
        dot = export_call_graph(program)

        assert dot.startswith("digraph call_graph {")
        assert dot.endswith("}")
        assert "->" not in dot  # No edges

    def test_extract_relationships_helper(self):
        """Test the helper function that extracts relationships."""
        clauses = [
            Clause(
                Struct("p", (Var(0, "X"),)),
                (
                    Struct("q", (Var(0, "X"),)),
                    Struct("r", (Var(0, "X"),))
                )
            ),
            Clause(Struct("q", (Atom("a"),)), ())  # q/1 fact
        ]
        program = Program(tuple(clauses))

        rels = extract_predicate_relationships(program)

        # Only p/1 has outgoing edges
        assert set(rels.keys()) == {("p", 1)}
        # p/1 calls q/1 and r/1
        assert set(rels[("p", 1)]) == {("q", 1), ("r", 1)}

    def test_graph_layout_hints(self):
        """Graph should include layout hints for better visualization."""
        clauses = [
            Clause(Struct("p", ()), ())
        ]
        program = Program(tuple(clauses))

        dot = export_call_graph(program)

        # Should have layout hints at graph scope
        has_rankdir = re.search(r'^\s*rankdir\s*=\s*(LR|TB|RL|BT)\s*;', dot, re.MULTILINE)
        has_node_defaults = re.search(r'^\s*node\s*\[', dot, re.MULTILINE)
        has_edge_defaults = re.search(r'^\s*edge\s*\[', dot, re.MULTILINE)
        assert has_rankdir or has_node_defaults or has_edge_defaults

    def test_operator_predicates_quoted_and_linked(self):
        """Operator predicates are properly quoted and linked."""
        clauses = [
            Clause(
                Struct("eq_test", (Var(0, "X"), Var(1, "Y"))),
                (Struct("=", (Var(0, "X"), Var(1, "Y"))),)
            ),
            Clause(Struct("cut_test", ()), (Atom("!"),))
        ]
        program = Program(tuple(clauses))

        dot = export_call_graph(program)

        # Operators are properly quoted as DOT identifiers
        assert '"=/2"' in dot
        assert '"!/0"' in dot
        assert '"eq_test/2"' in dot
        assert '"cut_test/0"' in dot

        # Edges to operators
        assert '"eq_test/2" -> "=/2"' in dot
        assert '"cut_test/0" -> "!/0"' in dot


class TestConstraintGraphExporter:
    """Test constraint graph generation (placeholder for CLP(FD))."""

    def test_placeholder_empty_graph(self):
        """Empty constraint graph generates minimal structure."""
        dot = export_constraint_graph([])

        assert dot.startswith("digraph constraint_graph {")
        assert dot.endswith("}")
        assert "label=" in dot  # Should have title

    def test_placeholder_with_variables(self):
        """Variables appear as nodes in constraint graph."""
        variables = [Var(0, "X"), Var(1, "Y"), Var(2, "Z")]
        dot = export_constraint_graph(variables)

        # Should have variable nodes
        assert "X" in dot or "var_0" in dot
        assert "Y" in dot or "var_1" in dot
        assert "Z" in dot or "var_2" in dot

    def test_placeholder_preserves_identity(self):
        """Variable identity is preserved in the graph."""
        var1 = Var(42, "X")
        var2 = Var(42, "X")  # Same ID
        var3 = Var(43, "X")  # Different ID

        dot = export_constraint_graph([var1, var2, var3])

        # Should recognize var1 and var2 as the same
        # Count unique variable nodes
        var_42_count = dot.count('"var_42"') + dot.count('"X_42"')
        var_43_count = dot.count('"var_43"') + dot.count('"X_43"')

        assert var_42_count > 0
        assert var_43_count > 0

    def test_placeholder_ready_for_clpfd(self):
        """Structure is ready for CLP(FD) integration."""
        variables = [Var(0, "X"), Var(1, "Y")]
        dot = export_constraint_graph(variables,
                                     constraints=[("neq", "X \\= Y")])

        # Should represent constraints somehow
        # Accept either a labelled edge or a graph label containing the constraint
        assert ('"var_0"' in dot and '"var_1"' in dot and 'neq' in dot) or ('X \\= Y' in dot)

    def test_constraint_graph_format(self):
        """Constraint graph has proper DOT format."""
        dot = export_constraint_graph([Var(0, "X")])

        # Basic structure
        assert dot.startswith("digraph constraint_graph {")
        assert dot.endswith("}")

        # Should have some styling
        assert "node [" in dot or "shape=" in dot


class TestDOTParser:
    """Test helper function for parsing DOT output."""

    def test_parse_simple_graph(self):
        """Parse basic DOT graph structure."""
        dot = """digraph test {
            "a" -> "b";
            "b" -> "c";
        }"""

        parsed = parse_dot_graph(dot)

        assert parsed["type"] == "digraph"
        assert parsed["name"] == "test"
        assert ("a", "b") in parsed["edges"]
        assert ("b", "c") in parsed["edges"]

    def test_parse_with_attributes(self):
        """Parse DOT with node and edge attributes."""
        dot = """digraph test {
            "a" [color=red];
            "b" [shape=box];
            "a" -> "b" [style=dashed];
        }"""

        parsed = parse_dot_graph(dot)

        assert "a" in parsed["nodes"]
        assert "b" in parsed["nodes"]
        assert ("a", "b") in parsed["edges"]

    def test_parse_layout_hints(self):
        """Parse graph-level attributes."""
        dot = """digraph test {
            rankdir=LR;
            node [shape=ellipse];
            "a" -> "b";
        }"""

        parsed = parse_dot_graph(dot)

        assert "rankdir" in parsed.get("attributes", {})


class TestIntegration:
    """Integration tests with real programs."""

    def test_call_graph_with_real_program(self):
        """Test with a realistic Prolog program."""
        from prolog.parser.reader import Reader

        program_text = """
        % List operations
        append([], L, L).
        append([H|T], L, [H|R]) :- append(T, L, R).

        member(X, [X|_]).
        member(X, [_|T]) :- member(X, T).

        length([], 0).
        length([_|T], N) :- length(T, N1), N is N1 + 1.

        % Using the predicates
        test(L) :- append([1,2], [3,4], L), member(2, L).
        """

        reader = Reader()
        clauses = reader.read_program(program_text)
        program = Program(tuple(clauses))

        dot = export_call_graph(program)

        # Check all predicates are present
        assert '"append/3"' in dot
        assert '"member/2"' in dot
        assert '"length/2"' in dot
        assert '"test/1"' in dot
        assert '"is/2"' in dot  # Built-in

        # Check relationships
        assert '"append/3" -> "append/3"' in dot  # Recursive
        assert '"member/2" -> "member/2"' in dot  # Recursive
        assert '"length/2" -> "length/2"' in dot  # Recursive
        assert '"length/2" -> "is/2"' in dot
        assert '"test/1" -> "append/3"' in dot
        assert '"test/1" -> "member/2"' in dot

    def test_both_exporters_consistent_format(self):
        """Both exporters should use consistent DOT format."""
        clauses = [Clause(Struct("p", ()), ())]
        program = Program(tuple(clauses))

        call_dot = export_call_graph(program)
        constraint_dot = export_constraint_graph([])

        # Both should start with digraph
        assert call_dot.startswith("digraph")
        assert constraint_dot.startswith("digraph")

        # Both should be properly closed
        assert call_dot.rstrip().endswith("}")
        assert constraint_dot.rstrip().endswith("}")

    def test_round_trip_property(self):
        """Export → parse → reconstruct preserves structure."""
        clauses = [
            Clause(Struct("parent", (Atom("a"), Atom("b"))), ()),
            Clause(
                Struct("grandparent", (Var(0, "X"), Var(1, "Z"))),
                (
                    Struct("parent", (Var(0, "X"), Var(2, "Y"))),
                    Struct("parent", (Var(2, "Y"), Var(1, "Z")))
                )
            )
        ]
        program = Program(tuple(clauses))

        # Export
        dot = export_call_graph(program)

        # Parse
        parsed = parse_dot_graph(dot)

        # Verify structure
        assert parsed["type"] == "digraph"
        assert parsed["name"] == "call_graph"

        # Check expected nodes exist
        expected_nodes = {"parent/2", "grandparent/2"}
        actual_nodes = set(parsed["nodes"].keys()) if "nodes" in parsed else set()
        assert expected_nodes.issubset(actual_nodes)

        # Check expected edges
        expected_edges = {("grandparent/2", "parent/2")}
        actual_edges = set(parsed["edges"]) if "edges" in parsed else set()
        assert expected_edges.issubset(actual_edges)