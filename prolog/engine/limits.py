"""Implementation limits for the Prolog engine.

Limits that are properties of this implementation rather than of the language.
Kept in their own module so that both the engine and the builtin modules can
import them without a cycle.
"""

# Largest arity functor/3 will construct.
#
# Not an ISO max_arity limit. SWI reports max_arity as unbounded and raises
# error(resource_error(stack), _) when a requested arity cannot be allocated,
# and PyLog follows SWI here in preference to the ISO test suite, which expects
# representation_error(max_arity) at iso_test_js/iso.tst:214.
#
# The threshold itself does diverge from SWI, which serves arities up to around
# 1e8. Every argument of a constructed term is a distinct store cell in PyLog,
# so 1e6 arguments already costs roughly 17s and 540MB; this is set at the edge
# of what PyLog can actually build. Without a bound, the 2^63 arity probed by
# iso.tst:214 never completes.
MAX_ARITY = 1_000_000
