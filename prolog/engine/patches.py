"""Dev mode patches for error handling behavior.

These patches modify the engine behavior for Stage 1 dev mode:
- Uncaught throws fail gracefully instead of raising
- Type errors fail instead of throwing

Note: Dev mode does not mask non-Prolog exceptions; only PrologThrow
exceptions are caught and converted to failures.
"""

from prolog.engine.errors import PrologThrow


def create_dev_engine(base_engine_class):
    """Create a dev mode engine class that handles uncaught throws as failures."""
    
    class DevEngine(base_engine_class):
        """Engine with dev mode error handling."""
        
        def run(self, goals, max_solutions=None):
            """Run with dev mode error handling."""
            try:
                return super().run(goals, max_solutions)
            except PrologThrow:
                # In dev mode, uncaught throws just fail
                # Clean up state
                self.trail.unwind_to(0, self.store)
                self.goal_stack.shrink_to(0)
                self.frame_stack.clear()
                self.cp_stack.clear()
                
                # Reset the trail write stamp and var stamps to avoid
                # "already trailed" suppression on subsequent runs
                if hasattr(self.trail, 'set_current_stamp'):
                    self.trail.set_current_stamp(0)
                if hasattr(self.trail, '_var_stamps'):
                    self.trail._var_stamps.clear()
                    
                return []  # Return empty solutions (failure)
            # All other exceptions are re-raised (not masked)
    
    return DevEngine