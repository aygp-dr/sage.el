# CONTINUE Protocol for sage.el

Session continuity protocol for autonomous agent work.

## Purpose

Enable seamless handoff between sessions (human or agent) by maintaining
a structured snapshot of current work state.

## File Location

```
/CONTINUE.md           # Root-level session state
/worktrees/*/CONTINUE.md  # Per-worktree state (optional)
```

## Structure

```markdown
# CONTINUE - sage.el [Feature/Area]

## Immediate (Next Session)
- **Active Bead:** `gemini-repl-010-xxx`
- Current focus and any blockers
- Commands to run first: `bd ready`, `gmake test`

## Open Beads
```bash
bd list --status open | grep "relevant-pattern"
```

## Documentation Gaps
- [ ] Missing docs identified during work

## Feature Ideas (P3/P4)
- Future improvements noticed but not urgent

## Validated
- What's confirmed working (test results, manual verification)

## Session Notes
- Pointer to `experiments/notes/session-*.md`
```

## Usage in sage.el

### Session Start
```bash
# 1. Read CONTINUE.md
cat CONTINUE.md

# 2. Check bead status
bd ready

# 3. Run health check
gmake check
```

### Session End
```bash
# 1. Commit any changes (PCP)
git add <specific-files>
git commit -m "feat: description"

# 2. Sync beads
bd sync && git push

# 3. Update CONTINUE.md
# - Move completed items to Validated
# - Add new items to Immediate
# - Update active bead reference
```

### Swarm Integration

Each DX swarm worker has its own AGENT-PROMPT.md in its worktree:
```
worktrees/sage-doctor/AGENT-PROMPT.md
worktrees/compact-importance/AGENT-PROMPT.md
worktrees/sage-agent/AGENT-PROMPT.md
```

The orchestrator (`bin/dx-swarm`) generates these prompts with
CONTINUE-style instructions.

## Example for sage.el

```markdown
# CONTINUE - sage.el DX

## Immediate (Next Session)
- **Bead:** `gemini-repl-010-cle` (sage-doctor)
- Implementing M-x sage-doctor diagnostic command
- Blocked on: deciding error message format

## Open Beads
```bash
bd list | grep "DX:"
```

## Documentation Gaps
- [ ] Provider comparison (gemini vs ollama vs openai)
- [ ] Tool permission model documentation

## Validated
- Core tests: 27/27 passing
- Modules load without errors
- 28 tools registered

## Session Notes
See `experiments/notes/session-2026-01-29-dx.md`
```

## Safety Rules

1. **Never include secrets** in CONTINUE.md
2. **Use bead IDs** not full descriptions for sensitive work
3. **Audit before commit**: `grep -E "key|secret|password" CONTINUE.md`
