"""Property-based tests for unification.

These tests verify mathematical properties that should hold for any
well-behaved unification algorithm. We generate random terms and check
that the properties are satisfied across thousands of test cases.

Properties tested:
1. Symmetry: unify(A, B) == unify(B, A)
2. Idempotence: Successful unification can be repeated without effect
3. Trail Invertibility: undo_to(mark) exactly restores store state
"""

import random
from typing import Any, List, Tuple, Dict

import pytest

from prolog.unify.store import Store
from prolog.unify.trail import undo_to
from prolog.unify.unify import unify
from prolog.ast.terms import Atom, Int, Var, Struct, List as PrologList


# Random term generation

def random_atom(rng: random.Random) -> Atom:
    """Generate a random atom."""
    atoms = ["a", "b", "c", "foo", "bar", "baz", "nil", "true", "false"]
    return Atom(rng.choice(atoms))


def random_int(rng: random.Random) -> Int:
    """Generate a random integer."""
    return Int(rng.randint(-100, 100))


def random_var(store: Store, var_pool: List[int], rng: random.Random) -> Var:
    """Generate a random variable from the pool."""
    if rng.random() < 0.3 or not var_pool:
        # 30% chance to create new var, or always if pool empty
        vid = store.new_var()
        var_pool.append(vid)
    else:
        # 70% chance to reuse existing var
        vid = rng.choice(var_pool)
    return Var(vid)


def random_struct(store: Store, var_pool: List[int], depth: int, rng: random.Random) -> Struct:
    """Generate a random structure."""
    if depth > 3:  # Limit nesting
        # At max depth, use only atoms/ints
        args = tuple(
            random_atom(rng) if rng.random() < 0.5 else random_int(rng)
            for _ in range(rng.randint(0, 3))
        )
    else:
        # Generate random arguments
        num_args = rng.randint(0, 3)  # Reduced max arity for faster CI
        args = tuple(
            random_term(store, var_pool, depth + 1, rng)
            for _ in range(num_args)
        )
    
    functors = ["f", "g", "h", "p", "q", "cons", "node", "leaf"]
    return Struct(rng.choice(functors), args)


def random_list(store: Store, var_pool: List[int], depth: int, rng: random.Random) -> PrologList:
    """Generate a random list."""
    if depth > 3:  # Limit nesting
        # Simple list at max depth
        items = tuple(
            random_atom(rng) if rng.random() < 0.5 else random_int(rng)
            for _ in range(rng.randint(0, 3))
        )
        # 20% chance of variable tail
        if rng.random() < 0.2:
            tail = random_var(store, var_pool, rng)
        else:
            tail = Atom("[]")
    else:
        # Generate random items
        num_items = rng.randint(0, 4)  # Reduced for faster CI
        items = tuple(
            random_term(store, var_pool, depth + 1, rng)
            for _ in range(num_items)
        )
        
        # Random tail (30% var, 10% list, 60% empty)
        r = rng.random()
        if r < 0.3:
            tail = random_var(store, var_pool, rng)
        elif r < 0.4:
            tail = random_list(store, var_pool, depth + 1, rng)
        else:
            tail = Atom("[]")
    
    return PrologList(items, tail=tail)


def random_term(store: Store, var_pool: List[int], depth: int, rng: random.Random) -> Any:
    """Generate a random term of any type."""
    if depth > 4:
        # At max depth, prefer simple terms
        r = rng.random()
        if r < 0.4:
            return random_atom(rng)
        elif r < 0.8:
            return random_int(rng)
        else:
            return random_var(store, var_pool, rng)
    
    # Choose term type with weights
    r = rng.random()
    if r < 0.15:
        return random_atom(rng)
    elif r < 0.30:
        return random_int(rng)
    elif r < 0.50:
        return random_var(store, var_pool, rng)
    elif r < 0.75:
        return random_struct(store, var_pool, depth, rng)
    else:
        return random_list(store, var_pool, depth, rng)


def store_snapshot(store: Store) -> List[Tuple]:
    """Take a lossless snapshot of store state for Stage -1/0 comparison."""
    return [(c.tag, c.ref, c.term, c.rank) for c in store.cells]


def reify(term: Any, store: Store) -> Any:
    """Reify a term by following variable bindings.
    
    Read-only operation that produces a term with all bound variables
    replaced by their bindings. Unbound variables remain as variables.
    """
    if isinstance(term, Var):
        from prolog.unify.unify_helpers import deref_term
        tag, val = deref_term(term, store)
        if tag == "VAR":
            # Unbound variable, leave as is
            return Var(val)
        else:
            # Bound variable, reify the binding
            return reify(val, store)
    elif isinstance(term, Struct):
        return Struct(term.functor, tuple(reify(a, store) for a in term.args))
    elif isinstance(term, PrologList):
        return PrologList(
            tuple(reify(i, store) for i in term.items),
            tail=reify(term.tail, store)
        )
    else:
        # Atoms, Ints are already ground
        return term


def stores_equal(store1: Store, store2: Store) -> bool:
    """Check if two stores have identical state."""
    if len(store1.cells) != len(store2.cells):
        return False
    
    for i in range(len(store1.cells)):
        c1 = store1.cells[i]
        c2 = store2.cells[i]
        
        if c1.tag != c2.tag:
            return False
        if c1.ref != c2.ref:
            return False
        if c1.rank != c2.rank:
            return False
        
        # For bound cells, check terms match
        if c1.tag == "bound":
            if not terms_structurally_equal(c1.term, c2.term):
                return False
    
    return True


def terms_structurally_equal(t1: Any, t2: Any) -> bool:
    """Check if two terms are structurally identical."""
    if type(t1) != type(t2):
        return False
    
    if isinstance(t1, Atom):
        return t1.name == t2.name
    elif isinstance(t1, Int):
        return t1.value == t2.value
    elif isinstance(t1, Var):
        return t1.id == t2.id
    elif isinstance(t1, Struct):
        return (t1.functor == t2.functor and
                len(t1.args) == len(t2.args) and
                all(terms_structurally_equal(a1, a2) 
                    for a1, a2 in zip(t1.args, t2.args)))
    elif isinstance(t1, PrologList):
        return (len(t1.items) == len(t2.items) and
                all(terms_structurally_equal(i1, i2)
                    for i1, i2 in zip(t1.items, t2.items)) and
                terms_structurally_equal(t1.tail, t2.tail))
    else:
        return False


# Property 1: Symmetry

@pytest.mark.slow
@pytest.mark.parametrize("seed", range(1000))
def test_unify_symmetry_property(seed):
    """Test that unify(A, B) == unify(B, A) for random terms."""
    rng = random.Random(seed)
    
    # Create two independent stores for the two directions
    store1 = Store()
    store2 = Store()
    trail1: List = []
    trail2: List = []
    
    # Create matching variable pools
    var_pool1: List[int] = []
    var_pool2: List[int] = []
    
    # Generate random terms in store1
    t1 = random_term(store1, var_pool1, 0, rng)
    t2 = random_term(store1, var_pool1, 0, rng)
    
    # Recreate the same vars in store2 and build mapping
    id_map: Dict[int, int] = {}
    for vid1 in var_pool1:
        vid2 = store2.new_var()
        var_pool2.append(vid2)
        id_map[vid1] = vid2
    
    # Translate terms to use store2's variables
    def translate_term(term: Any) -> Any:
        if isinstance(term, Var):
            # Map var from store1 to corresponding var in store2
            return Var(id_map.get(term.id, term.id))
        elif isinstance(term, Struct):
            return Struct(term.functor, tuple(translate_term(a) for a in term.args))
        elif isinstance(term, PrologList):
            return PrologList(
                tuple(translate_term(i) for i in term.items),
                tail=translate_term(term.tail)
            )
        else:
            return term
    
    t1_store2 = translate_term(t1)
    t2_store2 = translate_term(t2)
    
    # Try both directions
    result1 = unify(t1, t2, store1, trail1, occurs_check=True)
    result2 = unify(t2_store2, t1_store2, store2, trail2, occurs_check=True)
    
    # Results should be the same
    assert result1 == result2, f"Symmetry violated: unify(t1,t2)={result1}, unify(t2,t1)={result2}"


# Property 2: Idempotence

@pytest.mark.slow
@pytest.mark.parametrize("seed", range(1000))
def test_unify_idempotence_property(seed):
    """Test that successful unification can be repeated without adding trail entries."""
    rng = random.Random(seed)
    
    store = Store()
    trail: List = []
    var_pool: List[int] = []
    
    # Generate random terms
    t1 = random_term(store, var_pool, 0, rng)
    t2 = random_term(store, var_pool, 0, rng)
    
    # First unification
    result1 = unify(t1, t2, store, trail, occurs_check=True)
    
    if result1:
        # If successful, try again
        mark2 = len(trail)
        result2 = unify(t1, t2, store, trail, occurs_check=True)
        
        # Should still succeed
        assert result2 is True, "Second unification should succeed"
        
        # Should produce no new trail entries
        assert len(trail) == mark2, f"Second unification added {len(trail) - mark2} trail entries"


# Property 3: Trail Invertibility

@pytest.mark.slow
@pytest.mark.parametrize("seed", range(1000))
def test_trail_invertibility_property(seed):
    """Test that undo_to(mark) exactly restores the store state."""
    rng = random.Random(seed)
    
    store = Store()
    trail: List = []
    var_pool: List[int] = []
    
    # Generate some random terms
    terms = [random_term(store, var_pool, 0, rng) for _ in range(5)]
    
    # Take initial snapshot
    initial_snapshot = store_snapshot(store)
    initial_trail_len = len(trail)
    
    # Perform a sequence of unifications
    marks: List[int] = []
    results: List[bool] = []
    
    for i in range(len(terms) - 1):
        marks.append(len(trail))
        results.append(unify(terms[i], terms[i + 1], store, trail, occurs_check=True))
    
    # Take snapshot after all unifications
    final_snapshot = store_snapshot(store)
    
    # Now undo each unification in reverse order
    for mark in reversed(marks):
        undo_to(mark, trail, store)
        # Verify trail is exactly at mark
        assert len(trail) == mark, f"Trail not restored to mark: {len(trail)} != {mark}"
    
    # Store should be back to initial state
    restored_snapshot = store_snapshot(store)
    
    assert restored_snapshot == initial_snapshot, \
        "Store not fully restored after undoing all unifications"
    assert len(trail) == initial_trail_len, \
        "Trail not fully restored after undoing all unifications"
    
    # Redo all unifications should give same final state
    trail.clear()
    for i in range(len(terms) - 1):
        unify(terms[i], terms[i + 1], store, trail, occurs_check=True)
    
    final_snapshot2 = store_snapshot(store)
    assert final_snapshot2 == final_snapshot, \
        "Different final state after redoing unifications"


# Additional property test for partial undo

@pytest.mark.parametrize("seed", range(100))
def test_partial_undo_property(seed):
    """Test that partial undo correctly restores to intermediate states."""
    rng = random.Random(seed)
    
    store = Store()
    trail: List = []
    var_pool: List[int] = []
    
    # Generate random terms
    terms = [random_term(store, var_pool, 0, rng) for _ in range(4)]
    
    # Record state after each unification
    snapshots: List[List[Tuple]] = []
    marks: List[int] = []
    
    snapshots.append(store_snapshot(store))  # Initial state
    
    for i in range(len(terms) - 1):
        marks.append(len(trail))
        unify(terms[i], terms[i + 1], store, trail, occurs_check=True)
        snapshots.append(store_snapshot(store))
    
    # Test undoing to each intermediate state
    for i in range(len(marks) - 1, -1, -1):
        undo_to(marks[i], trail, store)
        current_snapshot = store_snapshot(store)
        
        assert current_snapshot == snapshots[i], \
            f"Undo to position {i} didn't restore expected state"


# Test for occurs check consistency

@pytest.mark.parametrize("seed", range(100))
def test_occurs_check_consistency(seed):
    """Test that occurs check behaves consistently across equivalent terms."""
    rng = random.Random(seed)
    
    store = Store()
    trail: List = []
    var_pool: List[int] = []
    
    # Generate a variable and a term that might contain it
    x = store.new_var()
    var_pool.append(x)
    
    # Generate a term that might reference x
    term = random_term(store, var_pool, 0, rng)
    
    # Try unification with occurs check both on and off
    mark = len(trail)
    result_with_check = unify(Var(x), term, store, trail, occurs_check=True)
    
    # Undo and try without occurs check
    undo_to(mark, trail, store)
    result_without_check = unify(Var(x), term, store, trail, occurs_check=False)
    
    # If it succeeded with occurs check, it should succeed without
    if result_with_check:
        assert result_without_check, \
            "Unification succeeded with occurs check but failed without"
    # If it failed with occurs check, it might succeed without (creating cycle)
    # but we can't assert anything definite in that case


# Test for unification determinism

@pytest.mark.parametrize("seed", range(100))
def test_unification_deterministic(seed):
    """Test that unification always produces the same result for the same inputs."""
    rng = random.Random(seed)
    
    # Generate terms once
    store_template = Store()
    var_pool: List[int] = []
    t1 = random_term(store_template, var_pool, 0, rng)
    t2 = random_term(store_template, var_pool, 0, rng)
    
    # Try unification multiple times with fresh stores
    results = []
    final_states = []
    
    for _ in range(5):
        # Create fresh store with same number of variables
        store = Store()
        trail: List = []
        
        # Pre-create same variables
        for _ in range(len(store_template.cells)):
            store.new_var()
        
        # Unify
        result = unify(t1, t2, store, trail, occurs_check=True)
        results.append(result)
        
        if result:
            # Record final state (excluding trail which might differ in order)
            final_states.append(store_snapshot(store))
    
    # All attempts should have same result
    assert all(r == results[0] for r in results), \
        f"Non-deterministic results: {results}"
    
    # All successful attempts should produce equivalent final states
    if results[0]:
        for i in range(1, len(final_states)):
            assert final_states[i] == final_states[0], \
                f"Different final states in attempts 0 and {i}"


# Property 4: Reified equality after successful unification

@pytest.mark.parametrize("seed", range(200))
def test_reified_terms_equal_on_success(seed):
    """Test that successful unification makes terms structurally equal after reification."""
    rng = random.Random(seed)
    
    store = Store()
    trail: List = []
    var_pool: List[int] = []
    
    # Generate random terms
    t1 = random_term(store, var_pool, 0, rng)
    t2 = random_term(store, var_pool, 0, rng)
    
    mark = len(trail)
    result = unify(t1, t2, store, trail, occurs_check=True)
    
    if result:
        # Reify both terms
        r1 = reify(t1, store)
        r2 = reify(t2, store)
        
        # After unification, the reified terms should be equivalent
        # Note: List([], tail=X) and X unify to make X = List([]), 
        # but they remain structurally different - one is the original
        # list, one is what X was bound to. We need semantic equality.
        # For now, we just verify both reify to ground terms or have
        # the same unbound variables.
        
        # Quick semantic equality check for lists
        def normalize_list(term):
            """Normalize list representation - [] with tail T becomes just T."""
            if isinstance(term, PrologList) and len(term.items) == 0 and term.tail != Atom("[]"):
                return normalize_list(term.tail)
            elif isinstance(term, Struct):
                return Struct(term.functor, tuple(normalize_list(a) for a in term.args))
            elif isinstance(term, PrologList):
                return PrologList(
                    tuple(normalize_list(i) for i in term.items),
                    tail=normalize_list(term.tail)
                )
            else:
                return term
        
        r1_norm = normalize_list(r1)
        r2_norm = normalize_list(r2)
        
        assert terms_structurally_equal(r1_norm, r2_norm), \
            f"Unified terms don't reify to equal structures: {r1_norm} != {r2_norm}"
    
    # Clean up
    undo_to(mark, trail, store)