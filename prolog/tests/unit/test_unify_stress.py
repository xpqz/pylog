"""Stress tests for the unification engine.

These tests verify that the unification algorithm can handle extreme cases
without stack overflow, excessive memory usage, or performance degradation.
The implementation is fully iterative (no Python recursion) so these should
all complete successfully even with very large structures.

Test categories:
1. Deep nesting - Deeply nested structures (1000+ levels)
2. Wide structures - Structures with many arguments (10000+ args)
3. Long lists - Lists with many elements (10000+ items)
4. Many variables - Large numbers of variables and unions
5. Complex patterns - Combinations of the above
"""

import gc
import os
import random
import time
from typing import List

import pytest

from prolog.unify.store import Store
from prolog.unify.trail import undo_to
from prolog.unify.unify import unify
from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList


# Configuration for scaling stress tests based on environment
STRESS_SCALE = float(os.getenv("PROLOG_STRESS_SCALE", "1.0"))  # 0.5..3.0

def time_limit(base: float) -> float:
    """Scale time limits based on environment."""
    return base * max(1.0, STRESS_SCALE)  # Don't go below base time

# Scaled input sizes
NEST_DEPTH = max(1, int(1000 * STRESS_SCALE))
WIDE_ARGS = max(1, int(10000 * STRESS_SCALE))
WIDE_LIST = max(1, int(10000 * STRESS_SCALE))
MANY_VARS = max(1, int(100000 * STRESS_SCALE))
STAR_RAYS = max(1, int(1000 * STRESS_SCALE))
UNIFIES = max(1, int(10000 * STRESS_SCALE))
CHECKPOINTS = max(1, int(100 * STRESS_SCALE))
UNIFY_PER_CP = max(1, int(100 * STRESS_SCALE))


# Helper functions for building test structures

def build_deep_struct(depth: int, bottom_term) -> Struct:
    """Build deeply nested structure: f(f(f(...f(bottom)...)))."""
    result = bottom_term
    for i in range(depth):
        result = Struct(f"f{i % 10}", (result,))
    return result


def build_deep_list(depth: int, bottom_term) -> PrologList:
    """Build deeply nested list: [[[...[bottom]...]]]."""
    result = PrologList((bottom_term,))
    for _ in range(depth):
        result = PrologList((result,))
    return result


# Test 1: Deep nesting stress tests

@pytest.mark.stress
def test_deeply_nested_struct_unification():
    """Test unification of deeply nested structures (1000+ levels)."""
    gc.collect()  # Clean start
    store = Store()
    trail: List = []
    
    # Warmup
    unify(Atom("warmup"), Atom("warmup"), store, trail, occurs_check=False)
    
    x = store.new_var()
    y = store.new_var()
    
    # Build matching deep structures
    term1 = build_deep_struct(NEST_DEPTH, Var(x))
    term2 = build_deep_struct(NEST_DEPTH, Var(y))
    
    # They should unify, making X = Y
    gc.collect()
    start = time.time()
    result = unify(term1, term2, store, trail, occurs_check=False)
    elapsed = time.time() - start
    
    assert result is True, "Deep structures should unify"
    assert elapsed < time_limit(1.0), f"Deep unification took {elapsed:.3f}s (limit: {time_limit(1.0):.1f}s)"
    
    # Verify X and Y are unified
    from prolog.unify.unify_helpers import deref_term
    tag1, val1 = deref_term(Var(x), store)
    tag2, val2 = deref_term(Var(y), store)
    
    if tag1 == "VAR" and tag2 == "VAR":
        assert val1 == val2, "Variables should be unified"


@pytest.mark.stress
def test_deeply_nested_list_unification():
    """Test unification of deeply nested lists."""
    gc.collect()
    store = Store()
    trail: List = []
    
    x = store.new_var()
    y = store.new_var()
    
    list1 = build_deep_list(NEST_DEPTH, Var(x))
    list2 = build_deep_list(NEST_DEPTH, Var(y))
    
    gc.collect()
    start = time.time()
    result = unify(list1, list2, store, trail, occurs_check=False)
    elapsed = time.time() - start
    
    assert result is True, "Deep lists should unify"
    assert elapsed < time_limit(1.0), f"Deep list unification took {elapsed:.3f}s"


@pytest.mark.stress
def test_deeply_nested_mixed_structures():
    """Test unification of deeply nested mixed structures."""
    gc.collect()
    store = Store()
    trail: List = []
    
    # Build mixed deeply nested structure
    depth = max(1, int(500 * STRESS_SCALE))
    x = store.new_var()
    
    term1 = Var(x)
    for i in range(depth):
        if i % 2 == 0:
            term1 = Struct("f", (term1, Atom("a")))
        else:
            term1 = PrologList((term1, Int(i)))
    
    # Build matching structure with Y
    y = store.new_var()
    term2 = Var(y)
    for i in range(depth):
        if i % 2 == 0:
            term2 = Struct("f", (term2, Atom("a")))
        else:
            term2 = PrologList((term2, Int(i)))
    
    result = unify(term1, term2, store, trail, occurs_check=False)
    assert result is True, "Mixed nested structures should unify"


# Test 2: Wide structure stress tests

@pytest.mark.stress
def test_wide_struct_unification():
    """Test unification of structures with many arguments."""
    gc.collect()
    store = Store()
    trail: List = []
    
    # Create struct with many arguments
    num_args = WIDE_ARGS
    
    # f(X1, X2, ..., X10000)
    vars1 = [store.new_var() for _ in range(num_args)]
    args1 = tuple(Var(v) for v in vars1)
    struct1 = Struct("f", args1)
    
    # f(Y1, Y2, ..., Y10000)
    vars2 = [store.new_var() for _ in range(num_args)]
    args2 = tuple(Var(v) for v in vars2)
    struct2 = Struct("f", args2)
    
    gc.collect()
    start = time.time()
    result = unify(struct1, struct2, store, trail, occurs_check=False)
    elapsed = time.time() - start
    
    assert result is True, "Wide structures should unify"
    assert elapsed < time_limit(2.0), f"Wide struct unification took {elapsed:.3f}s"
    
    # Check random sample of variables are unified
    from prolog.unify.unify_helpers import deref_term
    sample_size = min(10, num_args)
    for i in random.sample(range(num_args), k=sample_size):
        tag1, val1 = deref_term(Var(vars1[i]), store)
        tag2, val2 = deref_term(Var(vars2[i]), store)
        if tag1 == "VAR" and tag2 == "VAR":
            assert val1 == val2, f"Variables at position {i} should be unified"


@pytest.mark.stress
def test_wide_list_unification():
    """Test unification of lists with many elements."""
    gc.collect()
    store = Store()
    trail: List = []
    
    # Create list with many elements
    num_items = WIDE_LIST
    
    # [X1, X2, ..., X10000]
    vars1 = [store.new_var() for _ in range(num_items)]
    items1 = tuple(Var(v) for v in vars1)
    list1 = PrologList(items1)
    
    # [Y1, Y2, ..., Y10000]
    vars2 = [store.new_var() for _ in range(num_items)]
    items2 = tuple(Var(v) for v in vars2)
    list2 = PrologList(items2)
    
    gc.collect()
    start = time.time()
    result = unify(list1, list2, store, trail, occurs_check=False)
    elapsed = time.time() - start
    
    assert result is True, "Wide lists should unify"
    assert elapsed < time_limit(2.0), f"Wide list unification took {elapsed:.3f}s"


@pytest.mark.stress
def test_wide_mixed_ground_unification():
    """Test unification of wide structures with mixed ground/variable terms."""
    gc.collect()
    store = Store()
    trail: List = []
    
    # Create struct with alternating atoms, ints, and vars
    num_args = max(1, int(5000 * STRESS_SCALE))
    args1 = []
    args2 = []
    
    for i in range(num_args):
        if i % 3 == 0:
            args1.append(Atom(f"a{i}"))
            args2.append(Atom(f"a{i}"))
        elif i % 3 == 1:
            args1.append(Int(i))
            args2.append(Int(i))
        else:
            args1.append(Var(store.new_var()))
            args2.append(Var(store.new_var()))
    
    struct1 = Struct("big", tuple(args1))
    struct2 = Struct("big", tuple(args2))
    
    result = unify(struct1, struct2, store, trail, occurs_check=False)
    assert result is True, "Mixed wide structures should unify"


# Test 3: Many variables stress tests

@pytest.mark.stress
def test_many_variable_creation():
    """Test creating and managing many variables."""
    gc.collect()
    store = Store()
    
    # Create many variables
    num_vars = MANY_VARS
    
    gc.collect()
    start = time.time()
    var_ids = [store.new_var() for _ in range(num_vars)]
    elapsed = time.time() - start
    
    assert len(store.cells) == num_vars, f"Should have {num_vars} cells"
    assert elapsed < time_limit(1.0), f"Creating {num_vars} variables took {elapsed:.3f}s"
    
    # All variables should be distinct
    assert len(set(var_ids)) == num_vars, "All variable IDs should be unique"


@pytest.mark.stress
def test_long_unification_chain():
    """Test long chains of variable unifications."""
    gc.collect()
    store = Store()
    trail: List = []
    
    # Create chain: X1 = X2, X2 = X3, ..., X(n-1) = Xn
    num_vars = STAR_RAYS  # Reuse constant
    var_ids = [store.new_var() for _ in range(num_vars)]
    
    gc.collect()
    start = time.time()
    for i in range(num_vars - 1):
        result = unify(Var(var_ids[i]), Var(var_ids[i + 1]), store, trail, occurs_check=False)
        assert result is True, f"Failed to unify var {i} with {i+1}"
    elapsed = time.time() - start
    
    assert elapsed < time_limit(1.0), f"Chain unification took {elapsed:.3f}s"
    
    # All variables should now be unified (same root)
    from prolog.unify.unify_helpers import deref_term
    tag0, val0 = deref_term(Var(var_ids[0]), store)
    for i in range(1, min(10, num_vars)):
        tag, val = deref_term(Var(var_ids[i]), store)
        if tag0 == "VAR" and tag == "VAR":
            assert val == val0, f"Variable {i} should have same root as variable 0"


@pytest.mark.stress
def test_star_pattern_unification():
    """Test star pattern: one variable unified with many others."""
    gc.collect()
    store = Store()
    trail: List = []
    
    # Create star: X = Y1, X = Y2, ..., X = Yn
    center = store.new_var()
    num_rays = STAR_RAYS
    ray_vars = [store.new_var() for _ in range(num_rays)]
    
    gc.collect()
    start = time.time()
    for ray in ray_vars:
        result = unify(Var(center), Var(ray), store, trail, occurs_check=False)
        assert result is True
    elapsed = time.time() - start
    
    assert elapsed < time_limit(1.0), f"Star unification took {elapsed:.3f}s"
    
    # All should be unified with center
    from prolog.unify.unify_helpers import deref_term
    tag_c, val_c = deref_term(Var(center), store)
    for ray in ray_vars[:10]:  # Check first 10
        tag_r, val_r = deref_term(Var(ray), store)
        if tag_c == "VAR" and tag_r == "VAR":
            assert val_r == val_c, "Ray should be unified with center"


# Test 4: Trail stress tests

@pytest.mark.stress
def test_massive_trail_and_undo():
    """Test trail with many entries and full undo."""
    gc.collect()
    store = Store()
    trail: List = []
    
    # Perform many unifications
    num_unifications = UNIFIES
    vars = [store.new_var() for _ in range(num_unifications * 2)]
    
    initial_snapshot = [(c.tag, c.ref, c.term, c.rank) for c in store.cells]
    mark = len(trail)
    
    gc.collect()
    start = time.time()
    for i in range(num_unifications):
        result = unify(Var(vars[i*2]), Var(vars[i*2 + 1]), store, trail, occurs_check=False)
        assert result is True
    unify_time = time.time() - start
    
    assert len(trail) > 0, "Trail should have entries"
    
    # Undo everything
    gc.collect()
    start = time.time()
    undo_to(mark, trail, store)
    undo_time = time.time() - start
    
    assert len(trail) == mark, "Trail should be restored"
    assert unify_time < time_limit(2.0), f"Unifications took {unify_time:.3f}s"
    assert undo_time < time_limit(1.0), f"Undo took {undo_time:.3f}s"
    
    # Store should be restored
    final_snapshot = [(c.tag, c.ref, c.term, c.rank) for c in store.cells]
    assert final_snapshot == initial_snapshot, "Store not fully restored"


@pytest.mark.stress
def test_incremental_trail_undo():
    """Test incremental undo of large trail."""
    store = Store()
    trail: List = []
    
    # Build up trail with checkpoints
    checkpoints = []
    num_checkpoints = CHECKPOINTS
    unifies_per_checkpoint = UNIFY_PER_CP
    
    for checkpoint in range(num_checkpoints):
        checkpoints.append(len(trail))
        for _ in range(unifies_per_checkpoint):
            x = store.new_var()
            y = store.new_var()
            unify(Var(x), Var(y), store, trail, occurs_check=False)
    
    # Undo to each checkpoint in reverse
    for checkpoint in reversed(checkpoints):
        undo_to(checkpoint, trail, store)
        assert len(trail) == checkpoint, f"Trail not at checkpoint {checkpoint}"


# Test 5: Complex pattern stress tests

@pytest.mark.stress
def test_large_graph_structure():
    """Test unification of large graph-like structures."""
    gc.collect()
    store = Store()
    trail: List = []
    
    # Create a graph-like structure with shared subterms
    # node(Id, [edge(Target1), edge(Target2), ...])
    num_nodes = 100
    nodes = []
    
    for i in range(num_nodes):
        # Each node has edges to next 3 nodes (with wraparound)
        edges = []
        for j in range(3):
            target = (i + j + 1) % num_nodes
            edges.append(Struct("edge", (Int(target),)))
        
        node = Struct("node", (Int(i), PrologList(tuple(edges))))
        nodes.append(node)
    
    # Create the full graph
    graph1 = PrologList(tuple(nodes))
    
    # Create second graph with variables in place of some node IDs
    nodes2 = []
    for i in range(num_nodes):
        edges = []
        for j in range(3):
            target = (i + j + 1) % num_nodes
            edges.append(Struct("edge", (Int(target),)))
        
        # Use variable for every 10th node ID
        node_id = Var(store.new_var()) if i % 10 == 0 else Int(i)
        node = Struct("node", (node_id, PrologList(tuple(edges))))
        nodes2.append(node)
    
    graph2 = PrologList(tuple(nodes2))
    
    gc.collect()
    start = time.time()
    result = unify(graph1, graph2, store, trail, occurs_check=False)
    elapsed = time.time() - start
    
    assert result is True, "Graph structures should unify"
    assert elapsed < time_limit(2.0), f"Graph unification took {elapsed:.3f}s"


@pytest.mark.stress
def test_deeply_shared_subterms():
    """Test structures with many shared subterms."""
    gc.collect()
    store = Store()
    trail: List = []
    
    # Create shared subterm
    x = store.new_var()
    shared = Struct("shared", (Var(x), Atom("data")))
    
    # Build structure that uses shared subterm many times
    # big(shared, f(shared, g(shared, h(shared))))...
    num_shares = STAR_RAYS  # Reuse constant
    args = []
    current = shared
    
    for i in range(num_shares):
        current = Struct(f"f{i % 26}", (shared, current))
        if i % 100 == 0:
            args.append(current)
    
    big1 = Struct("big", tuple(args))
    
    # Create second structure with different variable
    y = store.new_var()
    shared2 = Struct("shared", (Var(y), Atom("data")))
    
    args2 = []
    current2 = shared2
    
    for i in range(num_shares):
        current2 = Struct(f"f{i % 26}", (shared2, current2))
        if i % 100 == 0:
            args2.append(current2)
    
    big2 = Struct("big", tuple(args2))
    
    result = unify(big1, big2, store, trail, occurs_check=False)
    assert result is True, "Structures with shared subterms should unify"


# Test 6: Occurs check stress tests

@pytest.mark.stress
@pytest.mark.slow
def test_deep_occurs_check_performance():
    """Test occurs check performance on deep structures."""
    gc.collect()
    store = Store()
    trail: List = []
    
    # Build deep structure
    depth = max(1, int(500 * STRESS_SCALE))
    x = store.new_var()
    
    deep = Var(x)
    for i in range(depth):
        deep = Struct("f", (deep, Int(i)))
    
    # Try to unify X with the deep structure (should fail with occurs check)
    gc.collect()
    start = time.time()
    result = unify(Var(x), deep, store, trail, occurs_check=True)
    elapsed = time.time() - start
    
    assert result is False, "Should fail occurs check"
    assert elapsed < time_limit(1.0), f"Occurs check took {elapsed:.3f}s"
    assert len(trail) == 0, "Failed unification should leave no trail"


@pytest.mark.stress
@pytest.mark.slow
def test_wide_occurs_check_performance():
    """Test occurs check performance on wide structures."""
    gc.collect()
    store = Store()
    trail: List = []
    
    # Build wide structure with X in middle
    x = store.new_var()
    num_args = max(1, int(5000 * STRESS_SCALE))
    
    args = []
    for i in range(num_args):
        if i == num_args // 2:
            args.append(Var(x))
        else:
            args.append(Int(i))
    
    wide = Struct("wide", tuple(args))
    
    # Try to unify X with the wide structure
    gc.collect()
    start = time.time()
    result = unify(Var(x), wide, store, trail, occurs_check=True)
    elapsed = time.time() - start
    
    assert result is False, "Should fail occurs check"
    assert elapsed < time_limit(1.0), f"Wide occurs check took {elapsed:.3f}s"


# Test 7: Memory stress tests

@pytest.mark.stress
def test_memory_efficiency():
    """Test that memory usage stays reasonable with large structures."""
    store = Store()
    trail: List = []
    
    # Get baseline memory
    gc.collect()
    
    # Create large structure
    num_elements = WIDE_LIST
    items = [Int(i) for i in range(num_elements)]
    big_list = PrologList(tuple(items))
    
    # Unify with itself (should be quick and not duplicate)
    result = unify(big_list, big_list, store, trail, occurs_check=False)
    assert result is True
    assert len(trail) == 0, "Self-unification should produce no trail"
    
    # Create another list with variables
    vars = [Var(store.new_var()) if i % 10 == 0 else Int(i) 
            for i in range(num_elements)]
    var_list = PrologList(tuple(vars))
    
    result = unify(big_list, var_list, store, trail, occurs_check=False)
    assert result is True
    
    # Trail should only have entries for the variables that were bound
    # We expect roughly num_elements/10 variables to be bound
    expected_trail_size = num_elements // 10 + 100  # Add headroom
    assert len(trail) <= expected_trail_size, f"Trail too large: {len(trail)} > {expected_trail_size}"


# Test 8: Edge cases at scale

@pytest.mark.stress
def test_alternating_success_failure_pattern():
    """Test alternating successful and failing unifications at scale."""
    gc.collect()
    store = Store()
    trail: List = []
    
    num_attempts = 1000
    successes = 0
    failures = 0
    
    for i in range(num_attempts):
        x = store.new_var()
        y = store.new_var()
        
        if i % 2 == 0:
            # Should succeed
            term1 = Struct("f", (Var(x), Int(i)))
            term2 = Struct("f", (Var(y), Int(i)))
            mark = len(trail)
            result = unify(term1, term2, store, trail, occurs_check=False)
            assert result is True
            successes += 1
        else:
            # Should fail
            term1 = Struct("f", (Atom("a"), Int(i)))
            term2 = Struct("f", (Atom("b"), Int(i)))
            mark = len(trail)
            result = unify(term1, term2, store, trail, occurs_check=False)
            assert result is False
            assert len(trail) == mark, "Failed unification shouldn't modify trail"
            failures += 1
    
    assert successes == num_attempts // 2
    assert failures == num_attempts // 2


@pytest.mark.stress
def test_repeated_unification_idempotence_at_scale():
    """Test that repeated unification remains idempotent at scale."""
    gc.collect()
    store = Store()
    trail: List = []
    
    # Create complex structure
    num_vars = 100
    vars = [store.new_var() for _ in range(num_vars)]
    
    # Build interconnected structure
    structs = []
    for i in range(num_vars):
        args = [Var(vars[j]) for j in range(max(0, i-2), min(num_vars, i+3))]
        structs.append(Struct(f"s{i}", tuple(args)))
    
    big = Struct("container", tuple(structs))
    
    # First unification with copy
    big_copy = Struct("container", tuple(structs))
    
    mark1 = len(trail)
    result1 = unify(big, big_copy, store, trail, occurs_check=False)
    assert result1 is True
    
    # Second unification (should be idempotent)
    mark2 = len(trail)
    result2 = unify(big, big_copy, store, trail, occurs_check=False)
    assert result2 is True
    trail_entries_second = len(trail) - mark2
    
    assert trail_entries_second == 0, "Second unification should add no trail entries"
    
    # Third unification for good measure
    mark3 = len(trail)
    result3 = unify(big, big_copy, store, trail, occurs_check=False)
    assert result3 is True
    assert len(trail) == mark3, "Third unification should add no trail entries"


# Test 9: Path compression stress test

@pytest.mark.stress
def test_path_compression_under_stress():
    """Test that path compression works correctly under stress."""
    store = Store()
    trail: List = []
    
    # Build a long chain
    chain_length = max(100, int(5000 * STRESS_SCALE))
    vids = [store.new_var() for _ in range(chain_length)]
    
    # Manually create chain (not through unify)
    for i in range(len(vids) - 1):
        store.cells[vids[i]].ref = vids[i + 1]
    
    mark = len(trail)
    
    # Repeatedly unify ends to trigger compression
    for _ in range(50):
        result = unify(Var(vids[0]), Var(vids[-1]), store, trail, occurs_check=False)
        assert result is True
    
    # Undo and verify chain is restored
    undo_to(mark, trail, store)
    for i in range(len(vids) - 1):
        assert store.cells[vids[i]].ref == vids[i + 1], f"Chain broken at position {i}"


# Test 10: Performance benchmarks

@pytest.mark.stress
@pytest.mark.slow
def test_performance_baseline():
    """Establish performance baselines for various operations."""
    benchmarks = {}
    enforce_bench = os.getenv("PROLOG_ENFORCE_BENCH", "").lower() in ("1", "true")
    
    # Benchmark 1: Deep unification
    gc.collect()
    store = Store()
    trail = []
    depth = NEST_DEPTH
    x = store.new_var()
    deep1 = build_deep_struct(depth, Var(x))
    y = store.new_var()
    deep2 = build_deep_struct(depth, Var(y))
    
    start = time.time()
    unify(deep1, deep2, store, trail, occurs_check=False)
    benchmarks["deep_1000"] = time.time() - start
    
    # Benchmark 2: Wide unification
    gc.collect()
    store = Store()
    trail = []
    width = max(1, int(5000 * STRESS_SCALE))
    wide1 = Struct("f", tuple(Var(store.new_var()) for _ in range(width)))
    wide2 = Struct("f", tuple(Var(store.new_var()) for _ in range(width)))
    
    start = time.time()
    unify(wide1, wide2, store, trail, occurs_check=False)
    benchmarks["wide_5000"] = time.time() - start
    
    # Benchmark 3: Many variables
    gc.collect()
    store = Store()
    trail = []
    num_vars = max(1, int(10000 * STRESS_SCALE))
    
    start = time.time()
    var_ids = [store.new_var() for _ in range(num_vars)]
    benchmarks[f"create_{num_vars}_vars"] = time.time() - start
    
    # Conditionally enforce benchmark limits
    if enforce_bench:
        for name, elapsed in benchmarks.items():
            assert elapsed < time_limit(2.0), f"Benchmark {name} took {elapsed:.3f}s (> {time_limit(2.0):.1f}s)"
    
    # Always print results for reference (visible with pytest -s)
    print(f"\nPerformance benchmarks (scale={STRESS_SCALE}):")
    for name, elapsed in benchmarks.items():
        print(f"  {name}: {elapsed*1000:.1f}ms")