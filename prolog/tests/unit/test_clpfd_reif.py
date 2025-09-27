"""Unit tests for CLP(FD) reification propagators.

Tests the core reification propagator for B #<==> C, B #==> C, and B #<== C.
"""

from prolog.unify.store import Store
from prolog.unify.trail import Trail
from prolog.unify.unify import bind
from prolog.ast.terms import Int
from prolog.clpfd.domain import Domain
from prolog.clpfd.api import get_domain, set_domain
from prolog.clpfd.boolean import get_boolean_value
from prolog.clpfd.entailment import Entailment
from prolog.clpfd.props.reif import (
    create_reification_propagator,
    create_implication_propagator,
)


class TestReificationPropagatorFactory:
    """Test the create_reification_propagator factory function."""

    def test_factory_creates_callable_propagator(self):
        """Test that factory returns a callable propagator."""

        def mock_check(store, x, y):
            return Entailment.UNKNOWN

        def mock_post(engine, store, trail, x, y):
            return True

        def mock_neg(engine, store, trail, x, y):
            return True

        propagator = create_reification_propagator(
            b_id=0,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=mock_check,
            post_constraint=mock_post,
            post_negation=mock_neg,
        )

        assert callable(propagator)

    def test_propagator_ensures_boolean_domain(self):
        """Test that propagator ensures B has Boolean domain."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        def mock_check(store, x, y):
            return Entailment.UNKNOWN

        def mock_post(engine, store, trail, x, y):
            return True

        def mock_neg(engine, store, trail, x, y):
            return True

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=mock_check,
            post_constraint=mock_post,
            post_negation=mock_neg,
        )

        # Propagator should ensure B has Boolean domain
        result = propagator(store, trail, None, None)
        assert result[0] == "ok"

        b_dom = get_domain(store, b_id)
        assert b_dom is not None
        assert b_dom.min() == 0
        assert b_dom.max() == 1


class TestReificationBidirectionalPropagation:
    """Test bidirectional propagation between B and constraint."""

    def test_entailed_constraint_sets_b_to_1(self):
        """When constraint is entailed, B should become 1."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")
        x_id = store.new_var("X")

        # X has singleton domain {5}
        set_domain(store, x_id, Domain(((5, 5),)), trail)

        def check_x_equals_5(store, x, y):
            # Check X #= 5
            x_dom = get_domain(store, x)
            if x_dom and x_dom.is_singleton() and x_dom.min() == y[1]:
                return Entailment.TRUE
            return Entailment.UNKNOWN

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(x_id, (None, 5)),
            check_entailment=check_x_equals_5,
            post_constraint=lambda *args: True,
            post_negation=lambda *args: True,
        )

        # Run propagator
        result = propagator(store, trail, None, None)
        assert result[0] == "ok"

        # B should be set to 1
        b_value = get_boolean_value(store, b_id)
        assert b_value == 1

    def test_disentailed_constraint_sets_b_to_0(self):
        """When constraint is dis-entailed, B should become 0."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")
        x_id = store.new_var("X")

        # X has domain that excludes 5
        set_domain(store, x_id, Domain(((1, 4), (6, 10))), trail)

        def check_x_equals_5(store, x, y):
            # Check X #= 5
            x_dom = get_domain(store, x)
            if x_dom and not x_dom.contains(y[1]):
                return Entailment.FALSE
            return Entailment.UNKNOWN

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(x_id, (None, 5)),
            check_entailment=check_x_equals_5,
            post_constraint=lambda *args: True,
            post_negation=lambda *args: True,
        )

        # Run propagator
        result = propagator(store, trail, None, None)
        assert result[0] == "ok"

        # B should be set to 0
        b_value = get_boolean_value(store, b_id)
        assert b_value == 0

    def test_b_equals_1_posts_constraint(self):
        """When B=1, the constraint should be posted."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")
        x_id = store.new_var("X")

        # Set B to 1
        set_domain(store, b_id, Domain(((1, 1),)), trail)

        # Track if constraint was posted
        posted = []

        def track_post(engine, store, trail, x, y):
            posted.append(("constraint", x, y))
            return True

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(x_id, (None, 5)),
            check_entailment=lambda *args: Entailment.UNKNOWN,
            post_constraint=track_post,
            post_negation=lambda *args: True,
        )

        # Run propagator
        result = propagator(store, trail, None, None)
        assert result[0] == "ok"

        # Constraint should have been posted
        assert len(posted) == 1
        assert posted[0][0] == "constraint"

    def test_b_equals_0_posts_negation(self):
        """When B=0, the constraint negation should be posted."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")
        x_id = store.new_var("X")

        # Set B to 0
        set_domain(store, b_id, Domain(((0, 0),)), trail)

        # Track if negation was posted
        posted = []

        def track_neg(engine, store, trail, x, y):
            posted.append(("negation", x, y))
            return True

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(x_id, (None, 5)),
            check_entailment=lambda *args: Entailment.UNKNOWN,
            post_constraint=lambda *args: True,
            post_negation=track_neg,
        )

        # Run propagator
        result = propagator(store, trail, None, None)
        assert result[0] == "ok"

        # Negation should have been posted
        assert len(posted) == 1
        assert posted[0][0] == "negation"


class TestReificationLoopPrevention:
    """Test prevention of infinite propagation loops."""

    def test_constraint_posted_only_once(self):
        """Constraint should only be posted once even if propagator runs multiple times."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        # Set B to 1
        set_domain(store, b_id, Domain(((1, 1),)), trail)

        # Count posts
        post_count = [0]

        def counting_post(engine, store, trail, x, y):
            post_count[0] += 1
            return True

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.UNKNOWN,
            post_constraint=counting_post,
            post_negation=lambda *args: True,
        )

        # Run propagator multiple times
        propagator(store, trail, None, None)
        propagator(store, trail, None, None)
        propagator(store, trail, None, None)

        # Should have posted only once
        assert post_count[0] == 1

    def test_negation_posted_only_once(self):
        """Negation should only be posted once even if propagator runs multiple times."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        # Set B to 0
        set_domain(store, b_id, Domain(((0, 0),)), trail)

        # Count posts
        neg_count = [0]

        def counting_neg(engine, store, trail, x, y):
            neg_count[0] += 1
            return True

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.UNKNOWN,
            post_constraint=lambda *args: True,
            post_negation=counting_neg,
        )

        # Run propagator multiple times
        propagator(store, trail, None, None)
        propagator(store, trail, None, None)
        propagator(store, trail, None, None)

        # Should have posted negation only once
        assert neg_count[0] == 1


class TestAdditionalEdgeCases:
    """Test additional edge cases for better coverage."""

    def test_incompatible_boolean_domain(self):
        """When B has incompatible domain (e.g., 2..5), should fail."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        # Set B to incompatible domain
        set_domain(store, b_id, Domain(((2, 5),)), trail)

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.UNKNOWN,
            post_constraint=lambda *args: True,
            post_negation=lambda *args: True,
        )

        result = propagator(store, trail, None, None)
        # Should fail because B can't be constrained to {0,1}
        assert result[0] == "fail"

    def test_bound_boolean_variables(self):
        """Test reification with bound Boolean variables."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        # Bind B to Int(1) instead of using domain
        bind(store, b_id, Int(1), trail)

        # Track if constraint was posted
        posted = []

        def track_post(engine, store, trail, x, y):
            posted.append(("constraint", x, y))
            return True

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.UNKNOWN,
            post_constraint=track_post,
            post_negation=lambda *args: True,
        )

        result = propagator(store, trail, None, None)
        assert result[0] == "ok"
        # Constraint should have been posted because B is bound to 1
        assert len(posted) == 1

    def test_forward_constraint_entailed_no_op(self):
        """B #==> C: When C is already entailed, no forced change to B."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        propagator = create_implication_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.TRUE,
            post_constraint=lambda *args: True,
            forward=True,
        )

        result = propagator(store, trail, None, None)
        assert result[0] == "ok"

        # B should still have full Boolean domain (not forced)
        b_dom = get_domain(store, b_id)
        assert b_dom.contains(0)
        assert b_dom.contains(1)
        # No changes should be returned
        assert result[1] is None


class TestImplicationPropagatorForward:
    """Test forward implication B #==> C."""

    def test_forward_b_1_posts_constraint(self):
        """B #==> C: When B=1, constraint should be posted."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        # Set B to 1
        set_domain(store, b_id, Domain(((1, 1),)), trail)

        posted = []

        def track_post(engine, store, trail, x, y):
            posted.append(("constraint", x, y))
            return True

        propagator = create_implication_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.UNKNOWN,
            post_constraint=track_post,
            forward=True,
        )

        result = propagator(store, trail, None, None)
        assert result[0] == "ok"
        assert len(posted) == 1

    def test_forward_constraint_false_sets_b_0(self):
        """B #==> C: When C is false, B must be 0."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        propagator = create_implication_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.FALSE,
            post_constraint=lambda *args: True,
            forward=True,
        )

        result = propagator(store, trail, None, None)
        assert result[0] == "ok"

        # B should be 0
        b_value = get_boolean_value(store, b_id)
        assert b_value == 0

    def test_forward_b_0_no_constraint(self):
        """B #==> C: When B=0, constraint is not posted."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        # Set B to 0
        set_domain(store, b_id, Domain(((0, 0),)), trail)

        posted = []

        def track_post(engine, store, trail, x, y):
            posted.append(("constraint", x, y))
            return True

        propagator = create_implication_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.UNKNOWN,
            post_constraint=track_post,
            forward=True,
        )

        result = propagator(store, trail, None, None)
        assert result[0] == "ok"

        # Should not have posted constraint
        assert len(posted) == 0


class TestImplicationPropagatorBackward:
    """Test backward implication B #<== C."""

    def test_backward_constraint_true_sets_b_1(self):
        """B #<== C: When C is true, B must be 1."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        propagator = create_implication_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.TRUE,
            post_constraint=lambda *args: True,
            forward=False,
        )

        result = propagator(store, trail, None, None)
        assert result[0] == "ok"

        # B should be 1
        b_value = get_boolean_value(store, b_id)
        assert b_value == 1

    def test_backward_b_0_constraint_true_fails(self):
        """B #<== C: When B=0 and C is entailed, should fail."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        # Set B to 0
        set_domain(store, b_id, Domain(((0, 0),)), trail)

        propagator = create_implication_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.TRUE,
            post_constraint=lambda *args: True,
            forward=False,
        )

        result = propagator(store, trail, None, None)
        # Should fail because B=0 contradicts C being true
        assert result[0] == "fail"

    def test_backward_constraint_false_b_unconstrained(self):
        """B #<== C: When C is false, B can be anything."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        propagator = create_implication_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.FALSE,
            post_constraint=lambda *args: True,
            forward=False,
        )

        result = propagator(store, trail, None, None)
        assert result[0] == "ok"

        # B should still be unconstrained (0 or 1)
        b_dom = get_domain(store, b_id)
        assert b_dom.contains(0)
        assert b_dom.contains(1)


class TestReificationFailureCases:
    """Test failure cases in reification."""

    def test_b_0_contradicts_entailment_true(self):
        """When B=0 but constraint is entailed (true), should fail."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        # Force B to 0
        set_domain(store, b_id, Domain(((0, 0),)), trail)

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.TRUE,  # Constraint is true
            post_constraint=lambda *args: True,
            post_negation=lambda *args: True,
        )

        result = propagator(store, trail, None, None)
        # Should fail because B=0 contradicts entailed constraint
        assert result[0] == "fail"

    def test_b_1_contradicts_entailment_false(self):
        """When B=1 but constraint is disentailed (false), should fail."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        # Force B to 1
        set_domain(store, b_id, Domain(((1, 1),)), trail)

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.FALSE,  # Constraint is false
            post_constraint=lambda *args: True,
            post_negation=lambda *args: True,
        )

        result = propagator(store, trail, None, None)
        # Should fail because B=1 contradicts disentailed constraint
        assert result[0] == "fail"

    def test_posting_constraint_fails(self):
        """When posting constraint fails, propagator should fail."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        # Set B to 1
        set_domain(store, b_id, Domain(((1, 1),)), trail)

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.UNKNOWN,
            post_constraint=lambda *args: False,  # Posting fails
            post_negation=lambda *args: True,
        )

        result = propagator(store, trail, None, None)
        assert result[0] == "fail"

    def test_posting_negation_fails(self):
        """When posting negation fails, propagator should fail."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        # Set B to 0
        set_domain(store, b_id, Domain(((0, 0),)), trail)

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.UNKNOWN,
            post_constraint=lambda *args: True,
            post_negation=lambda *args: False,  # Posting negation fails
        )

        result = propagator(store, trail, None, None)
        assert result[0] == "fail"


class TestChangedVariableTracking:
    """Test that propagators correctly track changed variables."""

    def test_b_changed_returned(self):
        """When B's domain changes, it should be in the changed list."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.TRUE,
            post_constraint=lambda *args: True,
            post_negation=lambda *args: True,
        )

        result = propagator(store, trail, None, None)
        assert result[0] == "ok"
        assert result[1] == [b_id]  # B was changed

    def test_no_change_returns_none(self):
        """When nothing changes, changed list should be None."""

        store = Store()
        trail = Trail()
        b_id = store.new_var("B")

        # B already set to match entailment
        set_domain(store, b_id, Domain(((1, 1),)), trail)

        propagator = create_reification_propagator(
            b_id=b_id,
            constraint_type="#=",
            constraint_args=(1, 2),
            check_entailment=lambda *args: Entailment.TRUE,
            post_constraint=lambda *args: True,
            post_negation=lambda *args: True,
        )

        result = propagator(store, trail, None, None)
        assert result[0] == "ok"
        assert result[1] is None  # Nothing changed
