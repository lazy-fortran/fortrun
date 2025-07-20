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
DEBUG=0
OUTPUT_DIR=""
FULL_OUTPUT=0
QUIET=0
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
        -d|--debug)
            DEBUG=1
            shift
            ;;
        --full-output)
            FULL_OUTPUT=1
            shift
            ;;
        -q|--quiet)
            QUIET=1
            shift
            ;;
        --output-dir)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        --filter)
            FILTER="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS] [FILTER]"
            echo "Options:"
            echo "  -j, --jobs N      Use N parallel jobs (default: CPU count)"
            echo "  -v, --verbose     Show all test names (default: only failures)"
            echo "  -d, --debug       Show debug information"
            echo "  -q, --quiet       Suppress progress output"
            echo "  --full-output     Show full test output (like fpm test)"
            echo "  --output-dir DIR  Save individual test outputs to DIR"
            echo "  --filter PATTERN  Filter tests by pattern"
            echo "  -h, --help        Show this help"
            echo ""
            echo "Default behavior:"
            echo "  - Shows only failed test names and their output"
            echo "  - Passed tests are counted but not displayed"
            echo "  - Use -v to see all test results"
            exit 0
            ;;
        *)
            # Assume it's a test filter
            FILTER="$1"
            shift
            ;;
    esac
done

[ "$QUIET" -eq 0 ] && echo "Running tests on $NCORES cores"

# Create output directory if specified
if [ -n "$OUTPUT_DIR" ]; then
    mkdir -p "$OUTPUT_DIR"
    [ "$QUIET" -eq 0 ] && echo "Saving test outputs to: $OUTPUT_DIR"
fi

# Build all tests first
[ "$QUIET" -eq 0 ] && echo "Building tests..."
if [ "$FULL_OUTPUT" -eq 1 ]; then
    # Show build output in full mode
    if ! fpm build --tests; then
        echo -e "${RED}ERROR: Failed to build tests${NC}"
        exit 1
    fi
else
    # Quiet build in normal mode
    if ! fpm test --runner echo >/dev/null 2>&1; then
        echo -e "${RED}ERROR: Failed to build tests${NC}"
        exit 1
    fi
fi

# Discover all test executables
[ "$QUIET" -eq 0 ] && echo "Discovering tests..."
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

        # Run test using just the basename
        local exit_code
        if [ "$FULL_OUTPUT" -eq 1 ]; then
            # In full output mode, capture everything
            fpm test "$(basename "$test_name")" > "$test_output" 2>&1
            exit_code=$?
        else
            # Normal mode - just capture output
            fpm test "$(basename "$test_name")" > "$test_output" 2>&1
            exit_code=$?
        fi

        if [ $exit_code -eq 0 ]; then
            echo "$i|PASS|$display_name|$test_name" >> "$results_file"

            # Save output if requested
            if [ "$VERBOSE" -eq 1 ] || [ "$FULL_OUTPUT" -eq 1 ]; then
                cat "$test_output" >> "$output_file"
            fi

            # Save to output directory if specified
            if [ -n "$OUTPUT_DIR" ]; then
                cp "$test_output" "$OUTPUT_DIR/${display_name}.out"
            fi
        else
            echo "$i|FAIL|$display_name|$test_name" >> "$results_file"
            echo "=== Failed: $display_name ===" >> "$error_file"
            cat "$test_output" >> "$error_file"
            echo "" >> "$error_file"

            # Save failed test output to directory if specified
            if [ -n "$OUTPUT_DIR" ]; then
                cp "$test_output" "$OUTPUT_DIR/${display_name}.FAILED.out"
            fi
        fi

        rm -f "$test_output"
    done
}

# Launch parallel test runs
echo "Running tests..."
pids=()
for ((core=0; core<NCORES; core++)); do
    start_idx=$((core * TESTS_PER_CORE))
    end_idx=$((start_idx + TESTS_PER_CORE - 1))

    run_test_subset "$core" "$start_idx" "$end_idx" &
    pids+=($!)
done

# Show progress
if [ "$VERBOSE" -eq 0 ] && [ "$QUIET" -eq 0 ] && [ "$FULL_OUTPUT" -eq 0 ]; then
    while true; do
        completed=$(find "$TEMP_DIR" -name "*.results" -exec cat {} \; 2>/dev/null | wc -l)
        printf "\rProgress: %d/%d tests completed" "$completed" "$TOTAL_TESTS" >&2
        [ "$completed" -ge "$TOTAL_TESTS" ] && break
        sleep 0.5
    done
    echo "" >&2
fi

# Wait for all background jobs with error checking
for pid in "${pids[@]}"; do
    wait "$pid" || true
done

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

# Process all results
if [ -s "$TEMP_ALL" ]; then
    [ "$DEBUG" -eq 1 ] && echo "Debug: Processing $(wc -l < "$TEMP_ALL") test results"

    # In full output mode, show everything as we go
    if [ "$FULL_OUTPUT" -eq 1 ]; then
        while IFS='|' read -r idx status display_name full_name; do
            if [ -n "$status" ]; then
                echo ""
                echo "=== Running $display_name ==="

                # Find and display the output for this test
                for core_out in "$TEMP_DIR"/core_*.out; do
                    if [ -f "$core_out" ] && grep -q "$display_name" "$core_out" 2>/dev/null; then
                        # Show the test's output
                        cat "$core_out" 2>/dev/null || true
                    fi
                done

                if [ "$status" = "PASS" ]; then
                    echo -e "${GREEN}PASSED${NC}: $display_name"
                    ((TOTAL_PASSED++))
                else
                    echo -e "${RED}FAILED${NC}: $display_name"
                    ((TOTAL_FAILED++))

                    # Show failure details immediately
                    for err_file in "$TEMP_DIR"/core_*.err; do
                        if [ -f "$err_file" ] && grep -q "$display_name" "$err_file" 2>/dev/null; then
                            grep -A50 "=== Failed: $display_name ===" "$err_file" | head -50
                        fi
                    done
                fi
            fi
        done < "$TEMP_ALL"
    else
        # Default mode - show pass/fail summary, but only show output for failed tests
        while IFS='|' read -r idx status display_name full_name; do
            if [ -n "$status" ]; then
                if [ "$status" = "PASS" ]; then
                    if [ "$VERBOSE" -eq 1 ]; then
                        echo -e " ${GREEN}✓ PASS${NC}: $display_name"
                    fi
                    ((TOTAL_PASSED++))
                else
                    echo -e " ${RED}✗ FAIL${NC}: $display_name"
                    ((TOTAL_FAILED++))
                fi
            fi
        done < "$TEMP_ALL"
    fi
else
    echo "No test results found!"
    [ "$DEBUG" -eq 1 ] && ls -la "$TEMP_DIR"
fi

# Ensure all output is flushed
sync
sleep 0.1

# Show verbose output
if [ "$VERBOSE" -eq 1 ] && [ -n "$(find "$TEMP_DIR" -name "core_*.out" -size +0 2>/dev/null)" ]; then
    echo ""
    echo "=== Detailed Output ==="
    cat "$TEMP_DIR"/core_*.out 2>/dev/null || true
fi

# Show error details (only if not already shown in full output mode)
if [ "$FULL_OUTPUT" -eq 0 ] && [ "$TOTAL_FAILED" -gt 0 ] && [ -n "$(find "$TEMP_DIR" -name "core_*.err" -size +0 2>/dev/null)" ]; then
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
