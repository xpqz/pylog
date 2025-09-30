# Contributing to PyLog

Thank you for your interest in contributing to PyLog! This document outlines the development guidelines and processes.

## ⚠️ CRITICAL: Test Integrity Rules

**Before making any changes, read and understand our test integrity rules.**

### Sacred Principle: NEVER MODIFY TESTS TO MAKE THEM PASS

Tests in PyLog are the executable specification. They define the expected behavior, not the current behavior.

**If a test fails, the implementation is wrong, not the test.**

### Complete Rules
See the "Test Integrity Rules" section in [`CLAUDE.md`](./CLAUDE.md) for the complete set of rules and guidelines.

### Quick Checklist for Contributors:
- ✅ Failing tests indicate missing/broken functionality
- ✅ Fix the implementation to satisfy the test
- ✅ Only change tests for provable errors (with written justification)
- ❌ Never "simplify" tests because they're complex
- ❌ Never change expected values to match actual output
- ❌ Never remove assertions because they fail

## Development Process

### Project Guidelines
Follow the development process outlined in [`CLAUDE.md`](./CLAUDE.md):

1. **Create feature branch**: `{issue-id}-{descriptive-name}`
2. **Write tests first**: Stop for human review before implementation
3. **Implement until tests pass**: Make the tests green
4. **Run complete test suite**: No regressions tolerated
5. **Create PR**: Clean, focused changes
6. **Verify CI**: All tests must pass

### Testing Strategy
- **Unit tests**: `prolog/tests/unit/` - Component-level testing
- **Scenario tests**: `prolog/tests/scenarios/` - Integration testing
- **Benchmarks**: `prolog/tests/benchmarks/` - Performance testing
- **SWI baseline**: Use `@pytest.mark.swi_baseline` for Prolog semantics

### Code Quality
- **No recursion**: Use explicit stacks to avoid recursion limits
- **Centralized mutation**: Only `bind()` mutates the store
- **Stable interfaces**: Maintain compatibility across development stages
- **Dependencies at top**: All imports at file top, fail fast on missing deps

## Architecture Principles

### Core Design Rules
1. **No Python recursion** - All execution uses explicit stacks
2. **Centralized mutation** - Only `bind()` mutates; everything is trailed
3. **Stable interfaces** - Terms, store, trail, and engine hooks remain stable
4. **Engine purity** - CLP(FD) integrates via attributed variables

### Development Stages
PyLog follows staged development where each stage builds on stable interfaces:
- **Stage -1**: Unifier Workbench
- **Stage 0**: Explicit stacks and choicepoints
- **Stage 1**: Minimal ISO builtins
- **Stage 1.5**: Operator support
- **Stage 2**: Indexing for performance
- **Stage 3**: Debug and observability
- **Stage 4**: Attributed variables
- **Stage 5**: CLP(FD) core
- **Stage 5.5**: Reification support
- **Stage 6**: Global constraints

## Pull Request Guidelines

### Before Submitting
- [ ] All tests pass locally
- [ ] New functionality has comprehensive tests
- [ ] No test files were modified without written justification
- [ ] Code follows project conventions
- [ ] Commit messages are clear and descriptive

### PR Description Template
```markdown
## Summary
Brief description of changes

## Testing
- [ ] Added tests for new functionality
- [ ] All existing tests pass
- [ ] No tests were modified (or justified why)

## Related Issues
Fixes #XXX
```

### Review Process
1. **Automated checks**: All CI tests must pass
2. **Code review**: Focus on correctness and adherence to principles
3. **Test validation**: Ensure tests properly verify the implementation
4. **Architecture review**: Confirm changes align with staged development

## Getting Help

- **Issues**: Use GitHub issues for bugs and feature requests
- **Discussions**: For architectural questions and design discussions
- **Documentation**: Check [`CLAUDE.md`](./CLAUDE.md) for detailed guidance

## Remember

**Tests are the specification.** When in doubt about expected behavior, look at the tests. When tests fail, fix the implementation. When tests pass, the feature is complete.

This approach ensures PyLog remains stable, reliable, and true to its design principles throughout development.