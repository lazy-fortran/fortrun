#!/bin/bash
# Parallel test runner for FPM tests
# Splits tests across CPU cores and runs them in parallel

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Get number of CPU cores
NCORES=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

# Parse arguments
VERBOSE=0
FILTER=""
while [[ $# -gt 0 ]]; do
    case $1 in
        -j|--jobs)
            NCORES="$2"
            shift 2
            ;;
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        --filter)
            FILTER="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS] [FILTER]"
            echo "Options:"
            echo "  -j, --jobs N     Use N parallel jobs (default: CPU count)"
            echo "  -v, --verbose    Show detailed test output"
            echo "  --filter PATTERN Filter tests by pattern"
            echo "  -h, --help       Show this help"
            exit 0
            ;;
        *)
            # Assume it's a test filter
            FILTER="$1"
            shift
            ;;
    esac
done

echo "Running tests on $NCORES cores"

# Build all tests first
echo "Building tests..."
if ! fpm test --runner echo >/dev/null 2>&1; then
    echo -e "${RED}ERROR: Failed to build tests${NC}"
    exit 1
fi

# Discover all test executables
echo "Discovering tests..."
ALL_TESTS=$(fpm test --runner echo 2>/dev/null | grep -E '^build/' | sort | uniq || true)

# Apply filter if provided
if [ -n "$FILTER" ]; then
    TEST_EXECUTABLES=$(echo "$ALL_TESTS" | grep -i "$FILTER" || true)
    if [ -z "$TEST_EXECUTABLES" ]; then
        echo -e "${YELLOW}No tests match filter: $FILTER${NC}"
        exit 0
    fi
else
    TEST_EXECUTABLES="$ALL_TESTS"
fi

# Count tests
TOTAL_TESTS=0
if [ -n "$TEST_EXECUTABLES" ]; then
    TOTAL_TESTS=$(echo "$TEST_EXECUTABLES" | wc -l)
fi

if [ "$TOTAL_TESTS" -eq 0 ]; then
    echo "No tests found"
    exit 0
fi

echo "Found $TOTAL_TESTS tests to run"

# Convert to array safely
mapfile -t TEST_ARRAY <<< "$TEST_EXECUTABLES"

# Calculate tests per core
TESTS_PER_CORE=$(( (TOTAL_TESTS + NCORES - 1) / NCORES ))

# Create temporary directory for results
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

# Track timing
START_TIME=$(date +%s)

# Function to run a subset of tests
run_test_subset() {
    local core_id=$1
    local start_idx=$2
    local end_idx=$3
    local results_file="$TEMP_DIR/core_${core_id}.results"
    local output_file="$TEMP_DIR/core_${core_id}.out"
    local error_file="$TEMP_DIR/core_${core_id}.err"

    [ "$VERBOSE" -eq 1 ] && echo "[Core $core_id] Processing tests $((start_idx + 1))-$((end_idx + 1))" >&2

    for ((i=start_idx; i<=end_idx && i<${#TEST_ARRAY[@]}; i++)); do
        local test_name="${TEST_ARRAY[$i]}"
        [ -z "$test_name" ] && continue

        # Extract just the test name for display
        local display_name=$(basename "$test_name")

        # Run test and capture output
        local test_output="$TEMP_DIR/test_${core_id}_${i}.out"

        if fpm test "$test_name" > "$test_output" 2>&1; then
            echo "$i|PASS|$display_name|$test_name" >> "$results_file"
            [ "$VERBOSE" -eq 1 ] && cat "$test_output" >> "$output_file"
        else
            echo "$i|FAIL|$display_name|$test_name" >> "$results_file"
            echo "=== Failed: $display_name ===" >> "$error_file"
            cat "$test_output" >> "$error_file"
            echo "" >> "$error_file"
        fi

        rm -f "$test_output"
    done
}

# Launch parallel test runs
echo "Running tests..."
for ((core=0; core<NCORES; core++)); do
    start_idx=$((core * TESTS_PER_CORE))
    end_idx=$((start_idx + TESTS_PER_CORE - 1))

    run_test_subset "$core" "$start_idx" "$end_idx" &
done

# Show progress
if [ "$VERBOSE" -eq 0 ]; then
    while true; do
        completed=$(find "$TEMP_DIR" -name "*.results" -exec cat {} \; 2>/dev/null | wc -l)
        printf "\rProgress: %d/%d tests completed" "$completed" "$TOTAL_TESTS"
        [ "$completed" -ge "$TOTAL_TESTS" ] && break
        sleep 0.5
    done
    echo ""
fi

# Wait for all background jobs
wait

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# Collect and display results
echo ""
echo "=== Test Results ==="

# Merge all results and sort by original order
TEMP_ALL="$TEMP_DIR/all_results.txt"
cat "$TEMP_DIR"/core_*.results 2>/dev/null | sort -n > "$TEMP_ALL" || true

# Count and display results
TOTAL_PASSED=0
TOTAL_FAILED=0

while IFS='|' read -r idx status display_name full_name; do
    if [ "$status" = "PASS" ]; then
        echo -e " ${GREEN}✓ PASS${NC}: $display_name"
        ((TOTAL_PASSED++))
    else
        echo -e " ${RED}✗ FAIL${NC}: $display_name"
        ((TOTAL_FAILED++))
    fi
done < "$TEMP_ALL"

# Show verbose output
if [ "$VERBOSE" -eq 1 ] && [ -n "$(find "$TEMP_DIR" -name "core_*.out" -size +0 2>/dev/null)" ]; then
    echo ""
    echo "=== Detailed Output ==="
    cat "$TEMP_DIR"/core_*.out 2>/dev/null || true
fi

# Show error details
if [ "$TOTAL_FAILED" -gt 0 ] && [ -n "$(find "$TEMP_DIR" -name "core_*.err" -size +0 2>/dev/null)" ]; then
    echo ""
    echo "=== Failed Test Details ==="
    cat "$TEMP_DIR"/core_*.err 2>/dev/null || true
fi

# Summary
echo ""
echo "=== Summary ==="
echo "Tests run: $TOTAL_TESTS"
echo -e "Passed: ${GREEN}$TOTAL_PASSED${NC}"
echo -e "Failed: ${RED}$TOTAL_FAILED${NC}"
echo "Time: ${DURATION}s (using $NCORES cores)"

# Final status
if [ "$TOTAL_FAILED" -eq 0 ]; then
    echo -e "\n${GREEN}${BOLD}All tests passed!${NC}"
    exit 0
else
    echo -e "\n${RED}${BOLD}$TOTAL_FAILED test(s) failed!${NC}"
    exit 1
fi
