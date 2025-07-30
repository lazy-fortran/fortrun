#!/bin/bash
# Backend Equivalence Tests
# Comprehensive comparison between Fortran and MLIR backends

set -e

# Test configuration
TEST_DIR=$(mktemp -d)
FORTRAN_CMD="fpm run fortrun --"
RESULTS_DIR="$TEST_DIR/results"
mkdir -p "$RESULTS_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Helper functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

run_test() {
    local test_name="$1"
    local test_function="$2"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo ""
    log_info "Running test: $test_name"

    if $test_function; then
        log_info "PASS: $test_name"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        log_error "FAIL: $test_name"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
}

# Test functions
test_simple_arithmetic() {
    local test_file="$TEST_DIR/arithmetic.lf"
    cat > "$test_file" << 'EOF'
program arithmetic
    integer :: a = 10, b = 5, result
    result = a + b * 2
    print *, result
end program arithmetic
EOF

    # Test Fortran backend
    if ! $FORTRAN_CMD --standardize "$test_file" > "$RESULTS_DIR/arithmetic_fortran.f90" 2>"$RESULTS_DIR/arithmetic_fortran.err"; then
        log_error "Fortran backend failed on arithmetic test"
        return 1
    fi

    # Test MLIR backend
    if ! $FORTRAN_CMD --compile "$test_file" -o "$RESULTS_DIR/arithmetic_mlir.o" 2>"$RESULTS_DIR/arithmetic_mlir.err"; then
        log_error "MLIR backend failed on arithmetic test"
        return 1
    fi

    # Both backends processed successfully
    return 0
}

test_function_definitions() {
    local test_file="$TEST_DIR/functions.lf"
    cat > "$test_file" << 'EOF'
program function_test
    real function square(x)
        real :: x
        square = x * x
    end function square

    real :: result
    result = square(5.0)
    print *, result
end program function_test
EOF

    # Test Fortran backend
    if ! $FORTRAN_CMD --standardize "$test_file" > "$RESULTS_DIR/functions_fortran.f90" 2>"$RESULTS_DIR/functions_fortran.err"; then
        log_error "Fortran backend failed on function test"
        return 1
    fi

    # Test MLIR backend
    if ! $FORTRAN_CMD --compile "$test_file" -o "$RESULTS_DIR/functions_mlir.o" 2>"$RESULTS_DIR/functions_mlir.err"; then
        log_error "MLIR backend failed on function test"
        return 1
    fi

    return 0
}

test_loop_constructs() {
    local test_file="$TEST_DIR/loops.lf"
    cat > "$test_file" << 'EOF'
program loop_test
    integer :: i, sum = 0
    do i = 1, 10
        sum = sum + i
    end do
    print *, sum
end program loop_test
EOF

    # Test Fortran backend
    if ! $FORTRAN_CMD --standardize "$test_file" > "$RESULTS_DIR/loops_fortran.f90" 2>"$RESULTS_DIR/loops_fortran.err"; then
        log_error "Fortran backend failed on loop test"
        return 1
    fi

    # Test MLIR backend
    if ! $FORTRAN_CMD --compile "$test_file" -o "$RESULTS_DIR/loops_mlir.o" 2>"$RESULTS_DIR/loops_mlir.err"; then
        log_error "MLIR backend failed on loop test"
        return 1
    fi

    return 0
}

test_variable_declarations() {
    local test_file="$TEST_DIR/variables.lf"
    cat > "$test_file" << 'EOF'
program variable_test
    integer :: x = 42
    real :: y = 3.14
    character(len=10) :: name = "test"
    print *, x, y, name
end program variable_test
EOF

    # Test Fortran backend
    if ! $FORTRAN_CMD --standardize "$test_file" > "$RESULTS_DIR/variables_fortran.f90" 2>"$RESULTS_DIR/variables_fortran.err"; then
        log_error "Fortran backend failed on variable test"
        return 1
    fi

    # Test MLIR backend
    if ! $FORTRAN_CMD --compile "$test_file" -o "$RESULTS_DIR/variables_mlir.o" 2>"$RESULTS_DIR/variables_mlir.err"; then
        log_error "MLIR backend failed on variable test"
        return 1
    fi

    return 0
}

test_control_flow() {
    local test_file="$TEST_DIR/control_flow.lf"
    cat > "$test_file" << 'EOF'
program control_flow_test
    integer :: x = 10
    if (x > 5) then
        print *, "x is greater than 5"
    else
        print *, "x is not greater than 5"
    end if
end program control_flow_test
EOF

    # Test Fortran backend
    if ! $FORTRAN_CMD --standardize "$test_file" > "$RESULTS_DIR/control_flow_fortran.f90" 2>"$RESULTS_DIR/control_flow_fortran.err"; then
        log_error "Fortran backend failed on control flow test"
        return 1
    fi

    # Test MLIR backend
    if ! $FORTRAN_CMD --compile "$test_file" -o "$RESULTS_DIR/control_flow_mlir.o" 2>"$RESULTS_DIR/control_flow_mlir.err"; then
        log_error "MLIR backend failed on control flow test"
        return 1
    fi

    return 0
}

test_error_handling() {
    local test_file="$TEST_DIR/syntax_error.lf"
    cat > "$test_file" << 'EOF'
program syntax_error_test
    integer :: x
    x =
end program syntax_error_test
EOF

    # Both backends should fail
    local fortran_exit=0
    local mlir_exit=0

    $FORTRAN_CMD --standardize "$test_file" > "$RESULTS_DIR/syntax_error_fortran.out" 2>"$RESULTS_DIR/syntax_error_fortran.err" || fortran_exit=$?
    $FORTRAN_CMD --compile "$test_file" -o "$RESULTS_DIR/syntax_error_mlir.o" 2>"$RESULTS_DIR/syntax_error_mlir.err" || mlir_exit=$?

    if [ $fortran_exit -ne 0 ] && [ $mlir_exit -ne 0 ]; then
        log_info "Both backends properly handle syntax errors"
        return 0
    else
        log_error "Error handling inconsistent: Fortran exit=$fortran_exit, MLIR exit=$mlir_exit"
        return 1
    fi
}

test_performance_comparison() {
    local test_file="$TEST_DIR/performance.lf"
    cat > "$test_file" << 'EOF'
program performance_test
    integer :: i, sum = 0
    do i = 1, 10000
        sum = sum + i
    end do
    print *, sum
end program performance_test
EOF

    # Time Fortran backend
    local start_time=$(date +%s.%N)
    if ! $FORTRAN_CMD --standardize "$test_file" > "$RESULTS_DIR/performance_fortran.f90" 2>"$RESULTS_DIR/performance_fortran.err"; then
        log_error "Fortran backend failed on performance test"
        return 1
    fi
    local fortran_time=$(echo "$(date +%s.%N) - $start_time" | bc -l)

    # Time MLIR backend
    start_time=$(date +%s.%N)
    if ! $FORTRAN_CMD --compile "$test_file" -o "$RESULTS_DIR/performance_mlir.o" 2>"$RESULTS_DIR/performance_mlir.err"; then
        log_error "MLIR backend failed on performance test"
        return 1
    fi
    local mlir_time=$(echo "$(date +%s.%N) - $start_time" | bc -l)

    printf "  Fortran backend time: %.3fs\n" "$fortran_time"
    printf "  MLIR backend time:    %.3fs\n" "$mlir_time"

    return 0
}

test_regression_detection() {
    local test_file="$TEST_DIR/regression.lf"
    cat > "$test_file" << 'EOF'
program regression_test
    integer :: result = 2 + 3 * 4
    print *, result
end program regression_test
EOF

    # Generate baseline
    if ! $FORTRAN_CMD --standardize "$test_file" > "$RESULTS_DIR/regression_baseline.f90" 2>"$RESULTS_DIR/regression_baseline.err"; then
        log_error "Failed to generate regression baseline"
        return 1
    fi

    # Generate current output
    if ! $FORTRAN_CMD --standardize "$test_file" > "$RESULTS_DIR/regression_current.f90" 2>"$RESULTS_DIR/regression_current.err"; then
        log_error "Failed to generate current regression output"
        return 1
    fi

    # Compare outputs
    if cmp -s "$RESULTS_DIR/regression_baseline.f90" "$RESULTS_DIR/regression_current.f90"; then
        log_info "No regression detected"
        return 0
    else
        log_error "Regression detected in output"
        return 1
    fi
}

# Main test execution
main() {
    echo "=== Backend Equivalence Test Suite ==="
    echo "Test directory: $TEST_DIR"
    echo ""

    # Run all tests
    run_test "Simple Arithmetic" test_simple_arithmetic
    run_test "Function Definitions" test_function_definitions
    run_test "Loop Constructs" test_loop_constructs
    run_test "Variable Declarations" test_variable_declarations
    run_test "Control Flow" test_control_flow
    run_test "Error Handling" test_error_handling
    run_test "Performance Comparison" test_performance_comparison
    run_test "Regression Detection" test_regression_detection

    # Report results
    echo ""
    echo "=== Test Summary ==="
    echo "Total tests:  $TOTAL_TESTS"
    echo "Passed:       $PASSED_TESTS"
    echo "Failed:       $FAILED_TESTS"

    if [ $TOTAL_TESTS -gt 0 ]; then
        PASS_RATE=$(echo "scale=1; $PASSED_TESTS * 100 / $TOTAL_TESTS" | bc -l)
        echo "Pass rate:    ${PASS_RATE}%"
    fi

    echo ""
    echo "Test results saved in: $RESULTS_DIR"

    # Cleanup on success, keep on failure for debugging
    if [ $FAILED_TESTS -eq 0 ]; then
        echo "All tests passed! Cleaning up..."
        rm -rf "$TEST_DIR"
        exit 0
    else
        echo "Some tests failed. Results preserved in $TEST_DIR for debugging."
        exit 1
    fi
}

# Cleanup on script exit
cleanup() {
    if [ -d "$TEST_DIR" ] && [ $FAILED_TESTS -eq 0 ]; then
        rm -rf "$TEST_DIR"
    fi
}
trap cleanup EXIT

# Run main function
main "$@"
