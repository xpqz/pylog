# Git Hooks

This directory contains git hooks that help maintain code quality and enforce project standards.

## Installation

To use these hooks in your local repository, copy them to your `.git/hooks` directory:

```bash
cp .githooks/commit-msg .git/hooks/commit-msg
chmod +x .git/hooks/commit-msg
```

Or create a symlink:

```bash
ln -s ../../.githooks/commit-msg .git/hooks/commit-msg
```

## Available Hooks

### commit-msg

Prevents commit messages containing AI attribution such as:
- 🤖 Generated with [Claude Code]
- Co-Authored-By: Claude
- Any mention of Claude, Anthropic, or AI assistance

This enforces the project rule: NO AI attribution in commits.

## Why These Hooks Exist

The project specifically prohibits AI attribution in commit messages to maintain clean, professional commit history without unnecessary metadata about tooling used.