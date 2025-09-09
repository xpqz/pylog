"""Dev mode patches for error handling behavior.

These patches modify the engine behavior for Stage 1 dev mode:
- Uncaught throws fail gracefully instead of raising
- Type errors fail instead of throwing
"""

def patch_dev_mode_throw(engine_class):
    """Patch Engine to handle uncaught throws as failures in dev mode."""
    original_run = engine_class.run
    
    def dev_run(self, goals, max_solutions=None):
        """Run with dev mode error handling."""
        try:
            return original_run(self, goals, max_solutions)
        except Exception as e:
            # In dev mode, uncaught throws just fail
            if hasattr(e, '__class__') and e.__class__.__name__ == 'PrologThrow':
                # Clean up state
                self.trail.unwind_to(0, self.store)
                self.goal_stack.shrink_to(0)
                self.frame_stack.clear()
                self.cp_stack.clear()
                return []  # Return empty solutions (failure)
            else:
                # Re-raise other exceptions
                raise
    
    engine_class.run = dev_run
    return engine_class