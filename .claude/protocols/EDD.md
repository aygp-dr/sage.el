# EDD - Experiment-Driven Development for sage.el

Document all exploration and research, even failures.

## Purpose

- Preserve institutional knowledge
- Avoid repeating failed experiments
- Enable future developers to understand design decisions
- Track research that informs implementation

## Directory Structure

```
experiments/
├── notes/                    # Research and session logs
│   ├── session-YYYY-MM-DD-*.md   # Daily session notes
│   ├── <topic>-research.md       # Topic-specific research
│   └── post-mortem-*.md          # Failure analysis
└── *.el, *.json              # Experiment code and data
```

## Experiment Template

```markdown
# Experiment: <Title>

**Bead:** `gemini-repl-010-xxx`
**Date:** YYYY-MM-DD
**Status:** in_progress | completed | failed | abandoned

## Hypothesis

What we're trying to prove or learn.

## Method

Steps taken, tools used, code written.

## Results

What happened - include error messages, metrics, observations.

## Conclusions

What we learned, whether hypothesis was confirmed/rejected.

## Next Steps

Follow-up work, link to new beads if created.
```

## sage.el Experiment Categories

### Provider Experiments
```markdown
# Experiment: Gemini vs Ollama Response Quality

**Bead:** gemini-repl-010-xxx
**Date:** 2026-01-29
**Status:** completed

## Hypothesis
Gemini 2.0 Flash provides better tool-calling accuracy than Ollama llama3.2.

## Method
1. Created 20 tool-calling scenarios
2. Ran each through both providers
3. Measured: accuracy, latency, token usage

## Results
| Provider | Accuracy | Avg Latency | Tokens/call |
|----------|----------|-------------|-------------|
| Gemini   | 95%      | 1.2s        | 450         |
| Ollama   | 78%      | 0.8s        | 380         |

## Conclusions
Gemini better for complex tool calls. Ollama suitable for simple queries.

## Next Steps
- Create provider selection heuristic based on query complexity
- Bead: gemini-repl-010-yyy
```

### Compaction Strategy Experiments
```markdown
# Experiment: Context Compaction Strategy Comparison

**Bead:** gemini-repl-010-xxx
**Date:** 2026-01-29

## Method
Compared 5 strategies from guile-sage:
- truncate, token-limit, importance, summarize, intent

## Results
See experiments/compaction-benchmark.json
```

### Tool Implementation Experiments
```markdown
# Experiment: Native vs Shell File Operations

**Bead:** gemini-repl-010-xxx
**Status:** completed

## Hypothesis
Native Elisp file operations are more reliable than shell commands.

## Results
Shell failures: 12% (path escaping, permission issues)
Native failures: 2% (only on truly inaccessible files)

## Conclusions
ALWAYS use native Elisp. Document in CLAUDE.md.
```

## Session Notes Format

```markdown
# Session: 2026-01-29 DX Implementation

**Duration:** 2 hours
**Focus:** sage-doctor command

## Accomplished
- [x] Created sage-doctor skeleton
- [x] Added Emacs version check
- [x] Added package dependency checks

## Blocked On
- Error message format (need design decision)

## Discovered
- `package-installed-p` doesn't work for built-in packages
- Need to use `featurep` instead

## Next Session
- Implement provider connectivity check
- Add API key validation (without leaking key)
```

## Linking to Beads

Always reference beads in experiments:

```bash
# Create experiment bead
bd create "Experiment: compaction strategy comparison" -p 2

# Reference in experiment notes
**Bead:** gemini-repl-010-xxx

# Close when experiment complete
bd close gemini-repl-010-xxx
```

## Archiving Experiments

After 30 days, move completed experiments:
```
experiments/notes/ → experiments/archive/2026-01/
```

Keep failed experiments accessible - they're valuable documentation.
