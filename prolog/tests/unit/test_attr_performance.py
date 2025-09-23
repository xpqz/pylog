"""Performance benchmarks for attributed variables - Issue #114.

Tests that the attributed variables system meets performance targets:
- < 1% overhead when attributes not used
- < 5% overhead with sparse attributes (10% of vars)
- < 10% overhead with dense attributes (90% of vars)
"""

import time
import pytest
from statistics import mean, stdev

from prolog.ast.terms import Atom, Int, Var, Struct
from prolog.unify.store import Store
from prolog.unify.unify import unify
from prolog.engine.engine import Engine
from prolog.ast.clauses import Program


class TestPerformanceTargets:
    """Test that performance targets are met."""

    def measure_time(self, func, iterations=1000):
        """Measure average time for a function over multiple iterations."""
        times = []
        for _ in range(iterations):
            start = time.perf_counter()
            func()
            end = time.perf_counter()
            times.append(end - start)
        return mean(times), stdev(times)

    @pytest.mark.performance
    def test_baseline_no_attributes(self):
        """Establish baseline performance without attributes."""
        def baseline_unification():
            store = Store()
            trail = []

            # Create variables and unify them
            vars = []
            for i in range(100):
                vars.append(store.new_var(f"V{i}"))

            # Perform various unifications
            for i in range(50):
                unify(Var(vars[i], f"V{i}"), Int(i), store, trail)
            for i in range(25):
                unify(Var(vars[i*2], f"V{i*2}"), Var(vars[i*2+1], f"V{i*2+1}"), store, trail)

        baseline_mean, baseline_std = self.measure_time(baseline_unification)
        # Store for comparison
        self.baseline_time = baseline_mean
        print(f"Baseline: {baseline_mean*1000:.3f}ms ± {baseline_std*1000:.3f}ms")

    @pytest.mark.performance
    def test_zero_overhead_no_attrs(self):
        """Test < 1% overhead when attributes not used."""
        baseline_mean, _ = self.measure_time(self._baseline_no_attrs)
        with_system_mean, _ = self.measure_time(self._with_attr_system_no_attrs)

        overhead = ((with_system_mean - baseline_mean) / baseline_mean) * 100
        print(f"Overhead with attr system but no attrs: {overhead:.2f}%")

        assert overhead < 1.0, f"Overhead {overhead:.2f}% exceeds 1% target"

    def _baseline_no_attrs(self):
        """Baseline without attribute system."""
        store = Store()
        # Ensure attrs dict doesn't exist
        if hasattr(store, 'attrs'):
            delattr(store, 'attrs')
        trail = []

        vars = [store.new_var(f"V{i}") for i in range(100)]
        for i in range(50):
            unify(Var(vars[i], f"V{i}"), Int(i), store, trail)

    def _with_attr_system_no_attrs(self):
        """With attribute system but no actual attributes."""
        store = Store()
        # Ensure attrs dict exists but is empty
        store.attrs = {}
        trail = []

        vars = [store.new_var(f"V{i}") for i in range(100)]
        for i in range(50):
            unify(Var(vars[i], f"V{i}"), Int(i), store, trail)

    @pytest.mark.performance
    def test_sparse_attrs_overhead(self):
        """Test < 5% overhead with sparse attributes (10% of vars)."""
        baseline_mean, _ = self.measure_time(self._baseline_no_attrs)
        sparse_mean, _ = self.measure_time(self._with_sparse_attrs)

        overhead = ((sparse_mean - baseline_mean) / baseline_mean) * 100
        print(f"Overhead with sparse attrs (10%): {overhead:.2f}%")

        assert overhead < 5.0, f"Overhead {overhead:.2f}% exceeds 5% target"

    def _with_sparse_attrs(self):
        """With attributes on 10% of variables."""
        store = Store()
        store.attrs = {}
        trail = []

        vars = [store.new_var(f"V{i}") for i in range(100)]

        # Add attributes to 10% of variables
        for i in range(0, 100, 10):
            store.attrs[vars[i]] = {"module": Int(i)}

        # Perform unifications
        for i in range(50):
            unify(Var(vars[i], f"V{i}"), Int(i), store, trail)

    @pytest.mark.performance
    def test_dense_attrs_overhead(self):
        """Test < 10% overhead with dense attributes (90% of vars)."""
        baseline_mean, _ = self.measure_time(self._baseline_no_attrs)
        dense_mean, _ = self.measure_time(self._with_dense_attrs)

        overhead = ((dense_mean - baseline_mean) / baseline_mean) * 100
        print(f"Overhead with dense attrs (90%): {overhead:.2f}%")

        assert overhead < 10.0, f"Overhead {overhead:.2f}% exceeds 10% target"

    def _with_dense_attrs(self):
        """With attributes on 90% of variables."""
        store = Store()
        store.attrs = {}
        trail = []

        vars = [store.new_var(f"V{i}") for i in range(100)]

        # Add attributes to 90% of variables
        for i in range(90):
            store.attrs[vars[i]] = {"module": Int(i)}

        # Perform unifications
        for i in range(50):
            unify(Var(vars[i], f"V{i}"), Int(i), store, trail)

    @pytest.mark.performance
    def test_hook_dispatch_overhead(self):
        """Test overhead of hook dispatch during unification."""
        def with_inactive_hooks():
            """Hooks registered but not triggered."""
            program = Program(())
            engine = Engine(program)

            # Register hook that won't be triggered
            engine.register_attr_hook("unused", lambda e, v, o: True)

            # Query without attributes
            query = "?- X = 42, Y = hello, Z = [1,2,3]."
            list(engine.query(query))

        def with_active_hooks():
            """Hooks registered and actively triggered."""
            program = Program(())
            engine = Engine(program)

            # Register hook that will be triggered
            engine.register_attr_hook("test", lambda e, v, o: True)

            # Query with attributes
            query = "?- put_attr(X, test, 1), X = 42."
            list(engine.query(query))

        inactive_mean, _ = self.measure_time(with_inactive_hooks, iterations=100)
        active_mean, _ = self.measure_time(with_active_hooks, iterations=100)

        overhead = ((active_mean - inactive_mean) / inactive_mean) * 100
        print(f"Hook dispatch overhead: {overhead:.2f}%")

        # Hook dispatch should be efficient
        assert overhead < 15.0, f"Hook overhead {overhead:.2f}% is too high"


class TestScalability:
    """Test scalability with large numbers of attributes."""

    @pytest.mark.performance
    def test_many_modules_one_var(self):
        """Test performance with many modules on one variable."""
        store = Store()
        store.attrs = {}
        trail = []

        var_id = store.new_var("X")

        # Add 100 different modules
        start = time.perf_counter()
        for i in range(100):
            if var_id not in store.attrs:
                store.attrs[var_id] = {}
            store.attrs[var_id][f"module{i}"] = Int(i)
        end = time.perf_counter()

        elapsed = end - start
        print(f"Time to add 100 modules: {elapsed*1000:.3f}ms")

        # Should be reasonably fast
        assert elapsed < 0.01, f"Adding 100 modules took {elapsed}s, too slow"

        # Accessing should also be fast
        start = time.perf_counter()
        for i in range(100):
            _ = store.attrs[var_id].get(f"module{i}")
        end = time.perf_counter()

        access_time = end - start
        print(f"Time to access 100 modules: {access_time*1000:.3f}ms")
        assert access_time < 0.001, f"Accessing 100 modules took {access_time}s"

    @pytest.mark.performance
    def test_many_vars_with_attrs(self):
        """Test performance with many variables having attributes."""
        store = Store()
        store.attrs = {}
        trail = []

        # Create 1000 variables with attributes
        start = time.perf_counter()
        vars = []
        for i in range(1000):
            var_id = store.new_var(f"V{i}")
            vars.append(var_id)
            store.attrs[var_id] = {"test": Int(i)}
        end = time.perf_counter()

        create_time = end - start
        print(f"Time to create 1000 vars with attrs: {create_time*1000:.3f}ms")

        # Should complete in reasonable time
        assert create_time < 0.1, f"Creating 1000 attributed vars took {create_time}s"

        # Unifying them should also be reasonable
        start = time.perf_counter()
        for i in range(500):
            unify(Var(vars[i], f"V{i}"), Int(i), store, trail)
        end = time.perf_counter()

        unify_time = end - start
        print(f"Time to unify 500 attributed vars: {unify_time*1000:.3f}ms")
        assert unify_time < 0.05, f"Unifying 500 attributed vars took {unify_time}s"

    @pytest.mark.performance
    def test_deep_aliasing_chains(self):
        """Test performance with deep chains of aliased variables."""
        store = Store()
        store.attrs = {}
        trail = []

        # Create chain of 100 variables
        vars = []
        for i in range(100):
            var_id = store.new_var(f"V{i}")
            vars.append(var_id)
            # Add attribute to every 10th variable
            if i % 10 == 0:
                store.attrs[var_id] = {"depth": Int(i)}

        # Create deep aliasing chain
        start = time.perf_counter()
        for i in range(99):
            unify(Var(vars[i], f"V{i}"), Var(vars[i+1], f"V{i+1}"), store, trail)
        end = time.perf_counter()

        chain_time = end - start
        print(f"Time to create 100-deep alias chain: {chain_time*1000:.3f}ms")

        # Should handle deep chains efficiently
        assert chain_time < 0.05, f"Creating deep chain took {chain_time}s"

        # Dereferencing through the chain should still be fast
        start = time.perf_counter()
        for var_id in vars:
            _ = store.deref(var_id)
        end = time.perf_counter()

        deref_time = end - start
        print(f"Time to deref 100 vars in chain: {deref_time*1000:.3f}ms")
        assert deref_time < 0.01, f"Dereferencing chain took {deref_time}s"


class TestMemoryUsage:
    """Test memory usage of attributed variables."""

    @pytest.mark.performance
    def test_memory_baseline(self):
        """Measure baseline memory usage without attributes."""
        import sys

        store = Store()
        vars = [store.new_var(f"V{i}") for i in range(1000)]

        # Get size of store without attributes
        baseline_size = sys.getsizeof(store)
        if hasattr(store, 'cells'):
            baseline_size += sys.getsizeof(store.cells)

        print(f"Baseline store size for 1000 vars: {baseline_size} bytes")
        assert baseline_size < 100000, "Baseline memory usage too high"

    @pytest.mark.performance
    def test_memory_with_attrs(self):
        """Measure memory usage with attributes."""
        import sys

        store = Store()
        store.attrs = {}

        vars = [store.new_var(f"V{i}") for i in range(1000)]

        # Add attributes to half the variables
        for i in range(500):
            store.attrs[vars[i]] = {
                "module1": Int(i),
                "module2": Atom(f"atom{i}"),
                "module3": Struct("f", (Int(i), Atom("x")))
            }

        # Calculate total size
        total_size = sys.getsizeof(store)
        if hasattr(store, 'cells'):
            total_size += sys.getsizeof(store.cells)
        total_size += sys.getsizeof(store.attrs)

        # Add size of attribute dictionaries
        for var_attrs in store.attrs.values():
            total_size += sys.getsizeof(var_attrs)
            for module, value in var_attrs.items():
                total_size += sys.getsizeof(module)
                total_size += sys.getsizeof(value)

        print(f"Store size with 500 attributed vars: {total_size} bytes")
        print(f"Average per attributed var: {total_size/500:.1f} bytes")

        # Should have reasonable memory usage
        assert total_size < 500000, f"Memory usage {total_size} bytes is excessive"