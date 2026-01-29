# CONTINUE - sage.el DX Next Steps

## Immediate (Next Session)

### Active DX Work
- **Branch:** `feat/dx`
- **Design Docs:** `worktrees/feat-dx/DX-DESIGN.org`, `GUILE-SAGE-FEATURES.org`
- **Swarm Status:** 3 agents running (`bin/dx-monitor`)

### Running Agents
| Session | Worktree | Task |
|---------|----------|------|
| sage-dx-1 | sage-doctor | Implement `M-x sage-doctor` |
| sage-dx-2 | compact-importance | Port from guile-sage |
| sage-dx-3 | sage-agent | Create `sage-agent.el` |

### Open Beads
```bash
bd ready                    # Show unblocked work
bd list | grep "DX:"        # DX-specific tasks
bin/dx-monitor              # Check swarm status
```

## Documentation Gaps

- [ ] Consolidate 20+ org files to 5 focused docs
- [ ] Add `M-x sage-help` with discoverability
- [ ] Provider comparison (gemini vs ollama vs openai)

## Feature Priorities (P1)

### sage-doctor (P0)
- Validates: Emacs version, packages, provider config, API keys, network
- Output: Actionable diagnostics with fix suggestions
- Ref: `lsp-doctor` pattern

### Actionable Errors (P0)
- Replace generic "API error" with specific guidance
- Include URLs to get API keys
- Rate limit messages show wait time + alternatives

### Compaction Strategies (P1)
Port from guile-sage:
- `compact-importance` - keyword scoring
- `compact-intent` - intent preservation
- `evaluate-compaction` - strategy comparison

### Agent Loop (P1)
New `sage-agent.el` module:
- Modes: interactive, autonomous, yolo
- Beads integration for task tracking
- Max iterations safety limit

## Review Gates

Before merging to main:
- [ ] Elisp expert review (bead: gemini-repl-010-bik)
- [ ] L7 engineer review (bead: gemini-repl-010-rai)

## Validated

- Core tests: 27/27 passing
- Modules: 12 files compile clean
- Tools: 28 registered

## Swarm Commands

```bash
# Monitor
bin/dx-monitor              # Quick status
bin/dx-monitor watch        # Continuous monitoring
bin/dx-monitor report       # Generate markdown report

# Control
bin/dx-swarm status         # Session status
bin/dx-swarm attach sage-dx-1  # Watch agent
bin/dx-swarm prompt         # Nudge all agents
bin/dx-swarm stop           # Shutdown
```

## Session Notes

See `experiments/notes/session-*.md` for detailed logs.

---
*Last updated: 2026-01-29*
