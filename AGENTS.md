# Agent Instructions

This document extends [wal.sh/research/agentic-workflow-2026](https://wal.sh/research/agentic-workflow-2026/) with project-specific patterns discovered building sage.el.

## Quick Reference: Beads (bd)

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

---

## Agent Coordination Patterns

### Multi-Perspective Code Review (Fan-Out)

Use parallel agents with different expertise for comprehensive reviews:

```
                    ┌─────────────────────┐
                    │   Code Changes      │
                    └──────────┬──────────┘
           ┌───────────────────┼───────────────────┐
           ▼                   ▼                   ▼
┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐
│  Domain Expert   │ │   L7/Protocol    │ │   CTO/Architect  │
│  - idioms        │ │  - tool patterns │ │  - security      │
│  - conventions   │ │  - API contracts │ │  - extensibility │
│  - integration   │ │  - error handling│ │  - scale         │
└──────────────────┘ └──────────────────┘ └──────────────────┘
```

**Create review beads in parallel:**
```bash
bd create "Code review: Domain expert - patterns, integration" -p 1
bd create "Code review: L7 - API contracts, error handling" -p 1
bd create "Code review: CTO - architecture, security" -p 1
```

### Background Agent Pattern

For long-running tasks (SSH, screenshots, compilation):

```bash
# Launch: Task tool with run_in_background: true
# Monitor: tail -f /tmp/claude/.../tasks/xxx.output
# Retrieve: Read tool on output file when complete
```

### Explore Agent for Codebase Understanding

Delegate broad searches to explore agents (subagent_type=Explore):
- `quick`: Single search pattern
- `medium`: Multiple strategies
- `very thorough`: Comprehensive cross-reference

---

## Tool Development Lifecycle

### Evolution Pattern

```
Phase 1: Shell-Based (Prototype)
├── Quick implementation using CLI tools (rg, grep)
└── May fail on edge cases

Phase 2: Native Primitives (Production)
├── Uses language-native functions
├── No external dependencies
└── Predictable behavior

Phase 3: Self-Extending (Advanced)
├── Tools can create new tools
├── Persistent across sessions
└── AI-discoverable
```

### Hard Dependencies (Not Fallbacks)

**Why:** Fallbacks create inconsistent behavior that's hard to debug.

```elisp
;; Good: Fail fast with clear message
(unless (require 'magit nil t)
  (error "Git tools require magit. M-x package-install RET magit"))

;; Bad: Silent degradation
(if (require 'magit nil t)
    (magit-git-insert ...)
  (shell-command-to-string "git ..."))  ; Different behavior!
```

### Tool Implementation Checklist

```markdown
- [ ] Uses native primitives (no shell commands)
- [ ] Has JSON schema for parameters
- [ ] Returns structured output
- [ ] Handles errors with clear messages
- [ ] Has unit tests with mocking
- [ ] Has demo/example usage
- [ ] Documented in tool reference
```

---

## Testing Strategies

### Unit Testing with Mocking

Mock external dependencies using `cl-letf`:

```elisp
(ert-deftest test-web-fetch ()
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             #'mock-url-retrieve))
    (should (string-match-p "Expected" (tool-web-fetch ...)))))
```

### Test Hierarchy

| Type | Speed | Dependencies | Example |
|------|-------|--------------|---------|
| Unit | Fast | None (mocked) | `sage-test.el` |
| Integration | Slow | Live services | `sage-integration-test.el` |
| Demo | Visual | GUI | `examples/tool-demos.el` |

### Remote Testing

Test on different environments for compatibility:

```bash
ssh pi.lan "cd ~/project && make test"
ssh pi.lan "emacs -Q -L . -l examples/demo.el -f run-demo"
```

---

## Experimental Development

### Tool Usage Analysis

Mine actual usage to prioritize implementation:

```bash
# Analyze Claude Code tool usage patterns
find ~/.claude/projects -name "*.jsonl" -exec cat {} \; | \
  grep -o '"tool":"[^"]*"' | sort | uniq -c | sort -rn

# Results (this project):
# Bash: 9277  → eval_elisp priority
# Read: 1364  → read_file priority
# Edit: 1267  → edit_file priority
```

### Observation-Driven Fixes

Document the observation → cause → solution chain:

```markdown
## Observation: code_search fails with "unrecognized file type"

**Context:** REPL error output
**Error:** `rg: unrecognized file type: clj`
**Root Cause:** Shell command with hardcoded file types
**Solution:** Replace with `directory-files-recursively` + `string-match-p`
**Bead:** gemini-repl-010-bij (closed)
```

---

## Session Management

### Landing the Plane (Session Completion)

**MANDATORY WORKFLOW - Work is NOT complete until `git push` succeeds:**

1. **File issues for remaining work**
   ```bash
   bd create "Follow-up: incomplete feature X" -p 2
   ```

2. **Run quality gates** (if code changed)
   ```bash
   make test && make lint
   ```

3. **Update issue status**
   ```bash
   bd close <completed-id> "Reason"
   bd update <partial-id> --status blocked --blocker <reason>
   ```

4. **PUSH TO REMOTE** (mandatory)
   ```bash
   git pull --rebase && bd sync && git push
   git status  # MUST show "up to date with origin"
   ```

5. **Hand off** - Update CONTINUE.md with next priorities

### Critical Rules

- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

---

## Key Learnings from This Project

| Insight | Before | After |
|---------|--------|-------|
| Tool implementation | Shell commands (`rg`, `grep`) | Native primitives (`directory-files-recursively`) |
| Error handling | Graceful degradation | Hard dependencies with clear errors |
| Code review | Single perspective | Multi-agent fan-out (3+ perspectives) |
| Testing | Manual verification | ERT + mocking + demos |
| Documentation | Static markdown | Executable org-mode examples |
| Tool system | Fixed set | Self-extending factory |

---

## Related Files

| File | Purpose |
|------|---------|
| `.claude/CLAUDE.md` | Project-specific agent instructions |
| `CONTINUE.md` | Session handoff notes |
| `docs/DEMO.org` | Interactive demonstration |
| `docs/TOOL-AUDIT.org` | Tool implementation status |
| `examples/tool-demos.el` | Automated tool demos |
| `.beads/issues.jsonl` | Issue tracking data |

---

## Updates for Research Page

New patterns discovered that could extend [agentic-workflow-2026](https://wal.sh/research/agentic-workflow-2026/):

1. **Multi-Agent Review Fan-Out** - Parallel specialized reviewers
2. **Tool Evolution Lifecycle** - Shell → Primitives → Self-Extending
3. **Self-Extending Tool Factory** - AI creating persistent tools
4. **Hard Dependencies Strategy** - Why fallbacks harm debugging
5. **Tool Usage Mining** - Analyzing agent logs to prioritize features
6. **Demo-Driven Documentation** - Executable examples as docs

