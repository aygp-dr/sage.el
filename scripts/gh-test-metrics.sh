#!/bin/bash
# gh-test-metrics.sh - Track test counts and coverage from GitHub Actions
#
# Usage:
#   ./scripts/gh-test-metrics.sh          # Show latest run metrics
#   ./scripts/gh-test-metrics.sh history  # Show metrics history
#   ./scripts/gh-test-metrics.sh compare  # Compare main vs PR
#
# Requires: gh CLI, jq

set -euo pipefail

METRICS_FILE="${METRICS_FILE:-.github/test-metrics.json}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[OK]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Get the latest workflow run
get_latest_run() {
    local workflow="${1:-test.yml}"
    gh run list --workflow="$workflow" --limit 1 --json databaseId,conclusion,startedAt,updatedAt -q '.[0]'
}

# Get test counts from a workflow run
get_test_counts() {
    local run_id="$1"
    local logs

    # Get logs and parse test counts
    logs=$(gh run view "$run_id" --log 2>/dev/null || echo "")

    if [ -z "$logs" ]; then
        echo '{"total": 0, "passed": 0, "failed": 0, "skipped": 0}'
        return
    fi

    # Parse ERT output format: "Ran X tests, Y passed, Z failed"
    # Or: "X expected failures" / "Y unexpected results"
    local total=0
    local passed=0
    local failed=0

    # Look for test summary lines
    while IFS= read -r line; do
        # Match "Ran N tests" or "Running N tests"
        if [[ "$line" =~ Ran[[:space:]]+([0-9]+)[[:space:]]+test ]]; then
            total=$((total + ${BASH_REMATCH[1]}))
        fi
        # Match "N passed"
        if [[ "$line" =~ ([0-9]+)[[:space:]]+passed ]]; then
            passed=$((passed + ${BASH_REMATCH[1]}))
        fi
        # Match "N unexpected" (failures)
        if [[ "$line" =~ ([0-9]+)[[:space:]]+unexpected ]]; then
            failed=$((failed + ${BASH_REMATCH[1]}))
        fi
    done <<< "$logs"

    # If we didn't find structured output, try alternative format
    if [ "$total" -eq 0 ]; then
        # Count test file runs
        total=$(echo "$logs" | grep -c "ert-run-tests-batch" || echo "0")
    fi

    echo "{\"total\": $total, \"passed\": $passed, \"failed\": $failed, \"skipped\": 0}"
}

# Get workflow run status
get_run_status() {
    local run_id="$1"
    gh run view "$run_id" --json conclusion,status -q '{conclusion: .conclusion, status: .status}'
}

# Show latest run metrics
show_latest() {
    log_info "Fetching latest test run..."

    local run_info
    run_info=$(get_latest_run "test.yml")

    if [ -z "$run_info" ] || [ "$run_info" = "null" ]; then
        log_error "No workflow runs found"
        exit 1
    fi

    local run_id
    run_id=$(echo "$run_info" | jq -r '.databaseId')
    local conclusion
    conclusion=$(echo "$run_info" | jq -r '.conclusion')
    local started_at
    started_at=$(echo "$run_info" | jq -r '.startedAt')

    echo ""
    echo "=== Latest Test Run ==="
    echo "Run ID:     $run_id"
    echo "Status:     $conclusion"
    echo "Started:    $started_at"
    echo ""

    # Get test counts
    log_info "Parsing test output..."
    local counts
    counts=$(get_test_counts "$run_id")

    local total passed failed
    total=$(echo "$counts" | jq -r '.total')
    passed=$(echo "$counts" | jq -r '.passed')
    failed=$(echo "$counts" | jq -r '.failed')

    echo "=== Test Metrics ==="
    echo "Total tests:  $total"
    if [ "$passed" -gt 0 ]; then
        echo -e "Passed:       ${GREEN}$passed${NC}"
    fi
    if [ "$failed" -gt 0 ]; then
        echo -e "Failed:       ${RED}$failed${NC}"
    else
        echo -e "Failed:       ${GREEN}0${NC}"
    fi
    echo ""

    # Coverage (placeholder - not yet implemented)
    echo "=== Coverage ==="
    log_warn "Coverage tracking not yet configured"
    echo "To enable: add undercover.el to test setup"
    echo ""

    # Local test count for comparison
    echo "=== Local Test Count ==="
    if command -v emacs &> /dev/null; then
        local local_count
        local_count=$(make test-all 2>&1 | grep -c "passed" || echo "0")
        echo "Local test suites: $local_count"
    else
        echo "Emacs not available for local check"
    fi
}

# Show metrics history
show_history() {
    log_info "Fetching workflow history..."

    echo ""
    echo "=== Test Run History (Last 10) ==="
    echo ""
    printf "%-12s %-12s %-25s %s\n" "RUN ID" "STATUS" "DATE" "COMMIT"
    echo "-------------------------------------------------------------"

    gh run list --workflow="test.yml" --limit 10 --json databaseId,conclusion,startedAt,headSha \
        | jq -r '.[] | [.databaseId, .conclusion, .startedAt, .headSha[0:7]] | @tsv' \
        | while IFS=$'\t' read -r id status date sha; do
            if [ "$status" = "success" ]; then
                status_colored="${GREEN}success${NC}"
            elif [ "$status" = "failure" ]; then
                status_colored="${RED}failure${NC}"
            else
                status_colored="${YELLOW}$status${NC}"
            fi
            printf "%-12s " "$id"
            echo -en "$status_colored"
            printf "      %-25s %s\n" "$date" "$sha"
        done

    echo ""
}

# Compare main vs current branch
compare_branches() {
    log_info "Comparing test results..."

    echo ""
    echo "=== Branch Comparison ==="

    # Get main branch latest
    local main_run
    main_run=$(gh run list --workflow="test.yml" --branch=main --limit 1 --json databaseId,conclusion -q '.[0]')

    # Get current branch latest
    local current_branch
    current_branch=$(git branch --show-current)
    local current_run
    current_run=$(gh run list --workflow="test.yml" --branch="$current_branch" --limit 1 --json databaseId,conclusion -q '.[0]')

    echo ""
    echo "Main branch:"
    if [ -n "$main_run" ] && [ "$main_run" != "null" ]; then
        local main_id main_status
        main_id=$(echo "$main_run" | jq -r '.databaseId')
        main_status=$(echo "$main_run" | jq -r '.conclusion')
        echo "  Run: $main_id  Status: $main_status"
    else
        echo "  No runs found"
    fi

    echo ""
    echo "Current branch ($current_branch):"
    if [ -n "$current_run" ] && [ "$current_run" != "null" ]; then
        local cur_id cur_status
        cur_id=$(echo "$current_run" | jq -r '.databaseId')
        cur_status=$(echo "$current_run" | jq -r '.conclusion')
        echo "  Run: $cur_id  Status: $cur_status"
    else
        echo "  No runs found (push to trigger CI)"
    fi
    echo ""
}

# Save metrics to file for tracking
save_metrics() {
    local run_id="$1"
    local counts="$2"
    local timestamp
    timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    local entry
    entry=$(jq -n \
        --arg ts "$timestamp" \
        --arg id "$run_id" \
        --argjson counts "$counts" \
        '{timestamp: $ts, run_id: $id, tests: $counts}')

    if [ -f "$METRICS_FILE" ]; then
        local existing
        existing=$(cat "$METRICS_FILE")
        echo "$existing" | jq --argjson new "$entry" '. + [$new]' > "$METRICS_FILE"
    else
        echo "[$entry]" > "$METRICS_FILE"
    fi

    log_success "Metrics saved to $METRICS_FILE"
}

# Main
main() {
    local cmd="${1:-latest}"

    case "$cmd" in
        latest|"")
            show_latest
            ;;
        history)
            show_history
            ;;
        compare)
            compare_branches
            ;;
        save)
            local run_info
            run_info=$(get_latest_run "test.yml")
            local run_id
            run_id=$(echo "$run_info" | jq -r '.databaseId')
            local counts
            counts=$(get_test_counts "$run_id")
            save_metrics "$run_id" "$counts"
            ;;
        help|--help|-h)
            echo "Usage: $0 [command]"
            echo ""
            echo "Commands:"
            echo "  latest   Show latest test run metrics (default)"
            echo "  history  Show test run history"
            echo "  compare  Compare main vs current branch"
            echo "  save     Save metrics to $METRICS_FILE"
            echo "  help     Show this help"
            ;;
        *)
            log_error "Unknown command: $cmd"
            echo "Run '$0 help' for usage"
            exit 1
            ;;
    esac
}

main "$@"
