# PCP - Progressive Commit Protocol for sage.el

Small, frequent commits that maintain a working state.

## Principles

1. **Atomic Changes**: One logical change per commit
2. **Always Buildable**: Never commit broken code
3. **Test Before Commit**: Run `gmake test` before each commit
4. **Sync Frequently**: `bd sync && git push` after milestones

## Commit Flow

```
Work → Test → Commit → Sync → Repeat
       ↓
   If tests fail → Fix → Test → Commit
```

## Commit Message Format

```
<type>(<scope>): <description>

Types:
- feat: new feature
- fix: bug fix
- docs: documentation
- test: tests
- chore: maintenance
- refactor: code restructuring

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
```

## sage.el Specific Rules

### Before Every Commit

```bash
# 1. Run tests
gmake test

# 2. Check for secrets
grep -rE "sk-|AIza|ghp_|password\s*=" . --include="*.el" --include="*.md"

# 3. Verify no accidental additions
git status
git diff --cached
```

### Staging Rules

```bash
# GOOD: Specific files
git add sage-doctor.el test/sage-doctor-test.el

# BAD: Everything
git add -A  # NEVER use this
git add .   # NEVER use this
```

### Commit Examples

```bash
# Feature
git commit -m "feat(dx): add sage-doctor diagnostic command

Validates Emacs version, packages, provider config, and API keys.

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"

# Fix
git commit -m "fix(tools): handle nil response in read_file tool

Prevents error when file is empty.

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"

# Test
git commit -m "test(context): add compaction strategy tests

Covers sliding-window, importance, and intent strategies.

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

## Integration with Beads

```bash
# After completing work on a bead
bd close gemini-repl-010-xxx
bd sync

git commit -m "feat(dx): implement sage-doctor command

Closes gemini-repl-010-cle

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"

git push
```

## Milestone Checkpoints

Sync beads and push after:
- [ ] Feature complete and tested
- [ ] All tests passing
- [ ] Documentation updated
- [ ] Before context switch (lunch, meeting, end of day)

## Emergency Recovery

If you committed something you shouldn't have:

```bash
# Uncommit (keep changes)
git reset --soft HEAD~1

# Check what's there
git diff --cached

# Remove sensitive file from staging
git reset HEAD <sensitive-file>

# Re-commit without sensitive content
git add <safe-files>
git commit -m "..."
```

**If already pushed**: Contact repo owner immediately. May need force push
(destructive) or BFG repo cleaner for secrets.
