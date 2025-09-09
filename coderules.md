# Code Rules and Processes for this project

## Process
- Don't back files up by copying! We use git for versioning.
- For each new development stage, create a new git branch first.
- We practice TDD: 
    - Write tests first that demonstrate the desired behaviour
    - Pause for human review of the tests
    - Progress the implementation until the tests succeed. 
    - NEVER tweak a test to "fit" the behaviour, unless the test is demonstrably broken.
- Maintain progress in docs/TODO-X.md files
- Don't use /tmp and other locations outside the current repository

### GitHub Workflow

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
10. **Merge and update** - After approval, merge PR and update the epic
