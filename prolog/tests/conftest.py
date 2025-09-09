"""Pytest configuration and fixtures for PyLog tests."""

import pytest
from prolog.tests.swi_baseline import swi_count, swi_onevar, is_swipl_available


# Check if SWI-Prolog is available
SWIPL_AVAILABLE = is_swipl_available()


@pytest.fixture
def swi():
    """Fixture providing SWI-Prolog baseline testing utilities.
    
    Usage:
        def test_something(swi):
            count = swi.count("p(1). p(2).", "p(X)")
            assert count == 2
            
            values = swi.onevar("", "member(X, [a,b,c])", "X")
            assert values == ["a", "b", "c"]
    """
    if not SWIPL_AVAILABLE:
        pytest.skip("SWI-Prolog not available")
    
    class SWIHarness:
        """Wrapper class for SWI baseline functions."""
        count = staticmethod(swi_count)
        onevar = staticmethod(swi_onevar)
    
    return SWIHarness


# Marker for tests that require SWI-Prolog
def pytest_configure(config):
    """Register custom markers."""
    config.addinivalue_line(
        "markers", 
        "swi_baseline: mark test as requiring SWI-Prolog for baseline comparison"
    )


# Auto-skip SWI baseline tests if SWI-Prolog is not available
def pytest_collection_modifyitems(config, items):
    """Modify test collection to skip SWI tests when appropriate."""
    if SWIPL_AVAILABLE:
        return
    
    skip_swi = pytest.mark.skip(reason="SWI-Prolog not available")
    for item in items:
        if "swi_baseline" in item.keywords:
            item.add_marker(skip_swi)