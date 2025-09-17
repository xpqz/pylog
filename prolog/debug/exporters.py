"""Graph exporters for visualization of Prolog programs."""

from typing import Dict, List, Tuple, Set, Any, Optional
from prolog.ast.terms import Term, Atom, Struct, Var
from prolog.engine.engine import Program
from prolog.ast.clauses import Clause


def export_call_graph(program: Program) -> str:
    """
    Export program's call graph in DOT format.

    Analyzes clause heads and bodies to extract predicate relationships.
    Each predicate becomes a node, calls become edges.
    """
    # Extract all relationships
    relationships = extract_predicate_relationships(program)

    # Collect all predicates (both callers and callees)
    all_predicates: Set[Tuple[str, int]] = set()
    for caller, callees in relationships.items():
        all_predicates.add(caller)
        all_predicates.update(callees)

    # Also include predicates that are only facts (no body)
    for clause in program.clauses:
        if isinstance(clause.head, Struct):
            pred_name = clause.head.functor
            pred_arity = len(clause.head.args)
            all_predicates.add((pred_name, pred_arity))
        elif isinstance(clause.head, Atom):
            all_predicates.add((clause.head.name, 0))

    # Build DOT output
    lines = []
    lines.append("digraph call_graph {")

    # Add layout hints
    lines.append("    rankdir=TB;")
    lines.append("    node [shape=ellipse];")

    # Identify built-in predicates
    builtins = {"is", "=", "\\=", "!", "true", "fail", "var", "nonvar",
                "atom", "call", "==", "\\==", "<", ">", "=<", ">=",
                "=..", "functor", "arg", "+", "-", "*", "/", "//", "mod"}

    # Add nodes
    for pred_name, pred_arity in sorted(all_predicates):
        node_id = f'"{pred_name}/{pred_arity}"'
        if pred_name in builtins:
            # Built-ins get special styling
            lines.append(f'    {node_id} [shape=box, style=dashed];')
        else:
            lines.append(f'    {node_id};')

    # Add edges (deduplicated)
    edges_added = set()
    for (caller_name, caller_arity), callees in relationships.items():
        caller_id = f'"{caller_name}/{caller_arity}"'
        for callee_name, callee_arity in callees:
            callee_id = f'"{callee_name}/{callee_arity}"'
            edge = (caller_id, callee_id)
            if edge not in edges_added:
                lines.append(f'    {caller_id} -> {callee_id};')
                edges_added.add(edge)

    lines.append("}")
    return "\n".join(lines)


def extract_predicate_relationships(program: Program) -> Dict[Tuple[str, int], Set[Tuple[str, int]]]:
    """
    Extract predicate call relationships from a program.

    Returns a dict mapping (predicate_name, arity) to set of called (predicate_name, arity).
    """
    relationships: Dict[Tuple[str, int], Set[Tuple[str, int]]] = {}

    for clause in program.clauses:
        # Get head predicate
        if isinstance(clause.head, Struct):
            head_name = clause.head.functor
            head_arity = len(clause.head.args)
        elif isinstance(clause.head, Atom):
            head_name = clause.head.name
            head_arity = 0
        else:
            continue  # Skip non-callable heads

        head_pred = (head_name, head_arity)

        # Extract body predicates
        if clause.body:
            called_preds = set()
            for goal in clause.body:
                if isinstance(goal, Struct):
                    called_preds.add((goal.functor, len(goal.args)))
                elif isinstance(goal, Atom):
                    called_preds.add((goal.name, 0))

            if called_preds:
                if head_pred not in relationships:
                    relationships[head_pred] = set()
                relationships[head_pred].update(called_preds)

    return relationships


def export_constraint_graph(variables: List[Var], constraints: Optional[List[Tuple[str, str]]] = None) -> str:
    """
    Export constraint graph in DOT format (placeholder for CLP(FD)).

    This is a placeholder implementation that will be extended
    when CLP(FD) is integrated in Stage 5.
    """
    lines = []
    lines.append("digraph constraint_graph {")
    lines.append('    label="Constraint Graph (CLP(FD) Placeholder)";')
    lines.append("    node [shape=circle];")

    # Track unique variables by ID
    unique_vars = {}
    for var in variables:
        if var.id not in unique_vars:
            unique_vars[var.id] = var

    # Add variable nodes
    for var_id, var in unique_vars.items():
        node_id = f'"var_{var_id}"'
        label = f'"{var.hint}_{var_id}"' if var.hint else node_id
        lines.append(f'    {node_id} [label={label}];')

    # Add constraint information if provided
    if constraints:
        lines.append('    // Constraints:')
        for constraint_type, constraint_desc in constraints:
            lines.append(f'    // {constraint_type}: {constraint_desc}')
            # In future, these would become edges or subgraphs

    lines.append("}")
    return "\n".join(lines)


def parse_dot_graph(dot: str) -> Dict[str, Any]:
    """
    Parse a DOT graph into a structured format.

    This is a simple parser for testing purposes.
    Returns dict with 'type', 'name', 'nodes', 'edges', and 'attributes'.
    """
    import re

    result = {
        "nodes": {},
        "edges": [],
        "attributes": {}
    }

    # Extract graph type and name
    match = re.search(r'(digraph|graph)\s+(\w+)\s*\{', dot)
    if match:
        result["type"] = match.group(1)
        result["name"] = match.group(2)

    # Extract nodes (simple pattern)
    node_pattern = r'"([^"]+)"\s*(?:\[([^\]]*)\])?;'
    for match in re.finditer(node_pattern, dot):
        node_id = match.group(1)
        attrs = match.group(2) or ""
        result["nodes"][node_id] = attrs

    # Extract edges
    edge_pattern = r'"([^"]+)"\s*->\s*"([^"]+)"'
    for match in re.finditer(edge_pattern, dot):
        result["edges"].append((match.group(1), match.group(2)))

    # Extract graph-level attributes
    if "rankdir" in dot:
        rankdir_match = re.search(r'rankdir\s*=\s*(\w+)', dot)
        if rankdir_match:
            result["attributes"]["rankdir"] = rankdir_match.group(1)

    return result