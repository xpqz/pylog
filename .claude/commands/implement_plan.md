# Implement Plan

You are tasked with implementing an approved technical plan either from `docs/plans/`, or from a GitHub Epic issue, which should map to a plan document in docs/plans. These plans contain phases with specific changes and success criteria. Read docs/ARCH.md (the overarching system architecture) and docs/PLAN.md (the project's global plan) to ensure you understand the project's context.

**CRITICAL**: **ALWAYS** run the full test suite before claiming that you have completed. Regressions are **NOT** tolerated! You may use the "not slow" switch when running the pytest suite:

uv run pytest -m "not slow" -q --tb=short

**CRITICAL**: **NEVER** use the `--no-verify` flag with git!

**CRITICAL**: **NEVER** write to `/tmp`. If you need to generate temporary test files, do so in the repository root, and ensure you remove them after you've finished with them and never commit them to git,

**CRITICAL**: review and accept coderules.md in its entirety. 

To save both tokens and time:

**Review coderules.md now, and stick closely to that.**


## Getting Started

When given a plan path or GitHub Epic issue ID:

- Read the plan completely and check for any existing checkmarks (- [x])
- Read the issues and all files mentioned in the plan
- **Read files fully** - never use limit/offset parameters, you need complete context
- Think deeply about how the pieces fit together
- Create a todo list to track your progress
- Given the plan and your todo-list, create a set of GitHub issues for each of the check-boxed tasks on your todo list
- Pick the first unaddressed task from the todo-list and create a local Git branch {ID}-{kebab-case-description} where {ID} is the GitHub issue id.
- Start implementing if you understand what needs to be done

If no plan path or GitHub issue ID provided, ask for one.

## Implementation Philosophy

Plans are carefully designed, but reality can be messy. Your job is to:
- Follow the plan's intent while adapting to what you find
- Implement each phase fully before moving to the next
- **CRITICAL**: always begin with implementing the unit tests for a task. We practice TDD.
- Verify your work makes sense in the broader codebase context
- Update checkboxes in the plan as you complete sections

When things don't match the plan exactly, think about why and communicate clearly. The plan is your guide, but your judgment matters too.

If you encounter a mismatch:
- STOP and think deeply about why the plan can't be followed
- Present the issue clearly:
  ```
  Issue in Phase [N]:
  Expected: [what the plan says]
  Found: [actual situation]
  Why this matters: [explanation]

  How should I proceed?
  ```

## Testing: TDD

- Implement tests FIRST, before implementation
- STOP for review of the tests
- **CRITICAL**: once tests have been reviewed and approved, they form an agreed CONTRACT. You MAY NOT modify tests to fit observed behaviour, unless the tests are PROVABLY broken: this must be confirmed with a human reviewer

## Verification Approach

After implementing a phase:
- **CRITICAL**: Run the FULL TEST SUITE (usually `uv run pytest -m "not slow" -q --tb=short` is sufficient). Regressions are NOT tolerated
- Fix any issues before proceeding, including anything discovered, but not directly caused by these changes
- Update your progress in both the plan, your todos, and in GitHub issues and epics
- Check off completed items in the plan file itself using Edit

## If You Get Stuck

When something isn't working as expected:
- First, make sure you've read and understood all the relevant code
- Consider if the codebase has evolved since the plan was written
- Present the mismatch clearly and ask for guidance

Use sub-tasks sparingly - mainly for targeted debugging or exploring unfamiliar territory.

## Resuming Work

If the plan has existing checkmarks:
- Trust that completed work is done
- Pick up from the first unchecked item
- Verify previous work only if something seems off

Remember: You're implementing a solution, not just checking boxes. Keep the end goal in mind and maintain forward momentum.
