# Agent Protocols for sage.el

Documentation for autonomous agent session management.

## Protocols

| Protocol | File | Purpose |
|----------|------|---------|
| [CONTINUE](CONTINUE.md) | Session continuity | Handoff between sessions |
| [PCP](PCP.md) | Progressive Commit | Small, tested commits |
| [EDD](EDD.md) | Experiment-Driven Dev | Document all research |
| [HIVE](HIVE.md) | Multi-Agent Coordination | Parallel agent work |

## Quick Reference

### Session Start
```bash
cat CONTINUE.md           # Read current state
bd ready                  # Find unblocked work
gmake check               # Verify health
```

### During Work
```bash
gmake test                # Before each commit
git add <specific-files>  # Never use -A or .
bd create "Sub: task"     # Track subtasks
```

### Session End
```bash
bd sync && git push       # Sync state
# Update CONTINUE.md with next steps
```

### Swarm Operations
```bash
bin/dx-swarm start        # Launch agents
bin/dx-monitor once       # Check progress
bin/dx-swarm stop         # Shutdown
```

## Safety Checklist

Before every commit:
- [ ] No secrets in staged files
- [ ] Tests pass (`gmake test`)
- [ ] Specific files staged (not `git add -A`)
- [ ] Commit message follows format

```bash
# Quick secret check
grep -rE "sk-|AIza|ghp_|password\s*=" --include="*.el" --include="*.md" .
```

## Related Files

- `/CONTINUE.md` - Current session state
- `/AGENTS.md` - Agent role definitions
- `/bin/dx-swarm` - Swarm orchestration
- `/bin/dx-monitor` - Progress monitoring
