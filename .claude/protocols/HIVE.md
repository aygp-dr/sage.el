# HIVE Protocol for sage.el

Hierarchical Intelligent Virtualized Ensemble - Multi-agent coordination.

## Concept

```
HIVE = Hierarchical Intelligent Virtualized Ensemble

Coordinator (Main Claude Session)
    ├── sage-dx-1: sage-doctor implementation
    ├── sage-dx-2: compact-importance port
    └── sage-dx-3: sage-agent.el module
```

## Principles

1. **Specialization**: Each agent has focused expertise/task
2. **Handoff**: Clear input/output contracts between agents
3. **Parallel Execution**: Independent agents run concurrently
4. **Aggregation**: Coordinator combines and reviews outputs

## sage.el Implementation

### Swarm Scripts

```bash
bin/dx-swarm       # Session orchestration
bin/dx-monitor     # Progress tracking and health
```

### Agent Assignments

| Session | Worktree | Task | Bead Pattern |
|---------|----------|------|--------------|
| sage-dx-1 | sage-doctor | Diagnostic command | sage-doctor |
| sage-dx-2 | compact-importance | Port from guile-sage | compact |
| sage-dx-3 | sage-agent | Agent loop module | sage-agent |

### Coordination Pattern

```
1. Coordinator creates worktrees + beads
2. Agents work in parallel (tmux sessions)
3. Monitor checks progress (bin/dx-monitor)
4. Agents commit to feature branches
5. Coordinator reviews and merges
```

## Agent Communication

### Input Contract (AGENT-PROMPT.md)

Each agent receives a prompt file with:
- Task description
- Bead filter pattern
- Safety rules
- CONTINUE-style instructions

### Output Contract

Agents produce:
- Code in worktree
- Commits following PCP
- Bead updates
- Session notes (EDD)

### Review Gates

Before merge to main:
```bash
# Review beads
bd list | grep "Review:"

# gemini-repl-010-bik: Elisp expert review
# gemini-repl-010-rai: L7 engineer review
```

## Orchestration Commands

```bash
# Start swarm
bin/dx-swarm start

# Check status
bin/dx-swarm status
bin/dx-monitor once

# Watch progress
bin/dx-monitor watch

# Nudge stale agents
bin/dx-monitor nudge

# Attach to agent
bin/dx-swarm attach sage-dx-1

# Send prompt to all
bin/dx-swarm prompt

# Stop swarm
bin/dx-swarm stop
```

## Pipeline Patterns

### Sequential (Review Flow)

```
Implementation → Elisp Review → L7 Review → Merge
     ↓              ↓              ↓
  sage-dx-*    reviewer-1     reviewer-2
```

### Fan-Out (Parallel Development)

```
Coordinator
    ├── sage-dx-1 (sage-doctor)
    ├── sage-dx-2 (compact-importance)
    └── sage-dx-3 (sage-agent)
```

### Fan-In (Merge)

```
sage-dx-1 ─┐
sage-dx-2 ─┼─→ feat/dx → main
sage-dx-3 ─┘
```

## Health Monitoring

### Session Health States

| State | Meaning | Action |
|-------|---------|--------|
| active | Recent activity (<1min) | None |
| idle | No activity (1-5min) | Monitor |
| stale | No activity (>5min) | Nudge |
| dead | Session terminated | Restart |

### Progress Metrics

```bash
bin/dx-monitor once

# Shows:
# - Session status (active/idle/stale/dead)
# - Commits in last hour
# - Uncommitted changes
# - Open beads
```

## Error Handling

### Agent Blocked

1. Agent creates blocker bead: `bd create "Blocked: <reason>" -p 0`
2. Agent switches to different task: `bd ready`
3. Coordinator notified via bead status

### Agent Crashed

1. Monitor detects dead session
2. `bin/dx-monitor nudge` restarts session
3. Agent resumes from worktree state

### Merge Conflict

1. Stop affected agent
2. Coordinator resolves conflict
3. Push resolution
4. Restart agent

## Safety Rules

1. **Never share secrets** between agents
2. **Each agent** has its own worktree (isolation)
3. **Commit frequently** (PCP) to preserve work
4. **Sync beads** before stopping: `bd sync`

## Cleanup

After swarm work:

```bash
# Stop all sessions
bin/dx-swarm stop

# Merge completed work
cd worktrees/feat-dx
git merge ../sage-doctor --no-ff
git merge ../compact-importance --no-ff
git merge ../sage-agent --no-ff

# Clean up worktrees (optional)
git worktree remove worktrees/sage-doctor
git worktree remove worktrees/compact-importance
git worktree remove worktrees/sage-agent

# Final sync
bd sync && git push
```

## Future: Specialized Agents

| Agent | Role | Tools |
|-------|------|-------|
| elisp-expert | Code review, patterns | Read, Grep |
| l7-engineer | API contracts, protocols | Read, WebFetch |
| test-runner | Continuous testing | Bash (gmake) |
| doc-writer | Documentation | Read, Write, Edit |

Configuration planned for `~/.claude/agents/`.
