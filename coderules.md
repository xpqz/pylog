# Code Rules and Processes for this project

**KEY RULES**: 
- NEVER CLAIM THAT SOMETHING IS COMPLETE IF THERE ARE REGRESSIONS. RUN THE FULL TEST SUITE BEFORE AND AFTER EACH WORK UNIT. 
- NEVER USE --no-verify WITH GIT!

## Test Locations
- **CRITICAL**: Unit tests MUST be placed in `prolog/tests/unit/` (NOT in `tests/unit/`)
- Always use the full path starting with `prolog/`
- Test files should be named `test_*.py`
- Scenario tests go in `prolog/tests/scenarios/`
- NEVER create a `tests/` directory at the repository root

## Developing in Python 

- Never do "fallback" programming in terms of requirements: if you expect module A, fail immediately if it's not present.
- **CRITICAL**: No conditional imports. All imports at the top of files only.
- Use up-to-date Python syntax, version 3.10 and onwards. 
- Backwards compatibility is NOT a goal.
- Use modern type hinting (dict, not Dict)

## Debugging

- **CRITICAL**: Always identify root causes of failures. Do NOT treat the symptoms of failures.

## Process
- Don't back files up by copying! We use git for versioning.
- For each new development stage, create a new git branch first.
- We practice TDD: 
    - Write tests first that demonstrate the desired behaviour
    - Pause for human review of the tests
    - Progress the implementation until the tests succeed. 
    - NEVER tweak a test to "fit" the behaviour, unless the test is demonstrably broken.
    - Once a test set has been reviewed and approved, that's a contract: do NOT skip or change without re-approval. All approved tests MUST pass before PR.
    - Before opening a PR, you MUST ensure that the full test suite is green.
    - Review any skipped, xfailed and xpassed tests.
    - Fix any pytest warnings.
- Maintain progress in docs/TODO-X.md files
- Don't use /tmp and other locations outside the current repository
- If you create temporary scripts for debugging, remove them after use.

## GitHub Workflow

- **NEVER EVER CHANGE THE DEFAULT BRANCH ON GIT OR GITHUB!**
- When creating PRs or commits, **DO NOT** mention Claude, Anthropic, or AI assistance in the message

Follow this process for each GitHub issue:

1. **Pick an issue** - Note its ID number
2. **Create branch** - Name format: `{ID}-{slug-derived-from-issue-title}`
   - Example: `9-parser-grammar-basic-terms`
3. **Write tests FIRST** - STOP after writing tests for human review
4. **Commit approved tests** - Only after review approval
5. **Implement until tests pass** - Make the tests green
6. **Run complete test suite** - No regressions tolerated!
7. **Create PR** - Make an orderly PR, squashing commits if necessary. DON'T  mention Claude or AI in the PR message
8. **Verify CI** - Ensure all CI tests pass fully
9. **Await PR review** - Wait for human review
10. **Merge** - After approval, merge PR and verify that tests complete in CI
11. **Maintain issues and epics** Update the Epic issue where relevant by ticking any boxes
