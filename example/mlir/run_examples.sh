#!/bin/bash
# Script to run all MLIR backend examples and demonstrate their capabilities

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_section() {
    echo -e "${BLUE}=== $1 ===${NC}"
}

# Configuration
FORTRAN_CMD="fmp run fortran --"
EXAMPLES_DIR="$(dirname "$0")"
OUTPUT_DIR="$EXAMPLES_DIR/output"

# Create output directory
mkdir -p "$OUTPUT_DIR"

log_section "MLIR Backend Examples"
echo "This script demonstrates the MLIR backend capabilities"
echo "Output files will be saved to: $OUTPUT_DIR"
echo ""

# Function to run an example
run_example() {
    local category="$1"
    local example="$2"
    local description="$3"

    local input_file="$EXAMPLES_DIR/$category/$example.f90"
    local mlir_output="$OUTPUT_DIR/${example}.mlir"
    local object_output="$OUTPUT_DIR/${example}.o"
    local exec_output="$OUTPUT_DIR/${example}"

    if [ ! -f "$input_file" ]; then
        log_error "Example file not found: $input_file"
        return 1
    fi

    log_info "Running $category/$example: $description"

    # Generate MLIR output
    echo "  Generating MLIR..."
    if $FORTRAN_CMD --compile --debug-codegen "$input_file" > "$mlir_output" 2>&1; then
        echo "    MLIR generation: SUCCESS"
    else
        echo "    MLIR generation: FAILED"
        cat "$mlir_output"
        return 1
    fi

    # Validate MLIR if mlir-opt is available
    if command -v mlir-opt >/dev/null 2>&1; then
        echo "  Validating MLIR..."
        if mlir-opt --verify-each "$mlir_output" >/dev/null 2>&1; then
            echo "    MLIR validation: SUCCESS"
        else
            echo "    MLIR validation: FAILED"
            log_warn "MLIR validation failed for $example"
        fi
    else
        echo "    MLIR validation: SKIPPED (mlir-opt not available)"
    fi

    # Compile to object file
    echo "  Compiling to object..."
    if $FORTRAN_CMD --compile "$input_file" -o "$object_output" >/dev/null 2>&1; then
        echo "    Object compilation: SUCCESS"
    else
        echo "    Object compilation: FAILED"
        log_warn "Object compilation failed for $example"
    fi

    # Compile to executable (basic examples only)
    if [ "$category" == "basic" ]; then
        echo "  Compiling to executable..."
        if $FORTRAN_CMD --compile -o "$exec_output" "$input_file" >/dev/null 2>&1; then
            echo "    Executable compilation: SUCCESS"

            # Try to run the executable
            if [ -x "$exec_output" ]; then
                echo "  Running executable..."
                if timeout 10 "$exec_output" > "$OUTPUT_DIR/${example}_output.txt" 2>&1; then
                    echo "    Execution: SUCCESS"
                else
                    echo "    Execution: FAILED or TIMEOUT"
                fi
            fi
        else
            echo "    Executable compilation: FAILED"
        fi
    fi

    echo ""
}

# Function to analyze MLIR output
analyze_mlir() {
    local mlir_file="$1"
    local example_name="$2"

    if [ ! -f "$mlir_file" ]; then
        return 1
    fi

    echo "MLIR Analysis for $example_name:"

    # Count different types of operations
    local func_count=$(grep -c "func.func" "$mlir_file" 2>/dev/null || echo "0")
    local arith_count=$(grep -c "arith\." "$mlir_file" 2>/dev/null || echo "0")
    local scf_count=$(grep -c "scf\." "$mlir_file" 2>/dev/null || echo "0")
    local alloca_count=$(grep -c "alloca" "$mlir_file" 2>/dev/null || echo "0")

    echo "  Functions: $func_count"
    echo "  Arithmetic ops: $arith_count"
    echo "  Control flow ops: $scf_count"
    echo "  Memory allocations: $alloca_count"

    # Check for optimization markers
    if grep -q "optimization passes" "$mlir_file" 2>/dev/null; then
        echo "  ✓ Optimization passes applied"
    fi

    if grep -q "enzyme.differentiable" "$mlir_file" 2>/dev/null; then
        echo "  ✓ AD attributes found"
    fi

    echo ""
}

# Run basic examples
log_section "Basic Examples"
run_example "basic" "hello_world" "Simple hello world program"
run_example "basic" "variables" "Variable declarations and assignments"
run_example "basic" "arithmetic" "Arithmetic operations"
run_example "basic" "functions" "Function definitions and calls"
run_example "basic" "loops" "Loop constructs"
run_example "basic" "conditionals" "Conditional statements"
run_example "basic" "arrays" "Array operations"

# Run autodiff examples
log_section "Automatic Differentiation Examples"
log_info "Note: AD examples require --enable-ad flag for full functionality"
run_example "autodiff" "simple_gradient" "Basic gradient computation"
run_example "autodiff" "polynomial" "Polynomial differentiation"
run_example "autodiff" "mathematical_functions" "Mathematical function derivatives"
run_example "autodiff" "optimization" "Gradient-based optimization"
run_example "autodiff" "neural_network" "Neural network with backpropagation"

# Run optimization examples
log_section "Optimization Examples"
run_example "optimization" "constant_folding" "Constant folding optimization"
run_example "optimization" "dead_code_elimination" "Dead code elimination"
run_example "optimization" "loop_optimization" "Loop optimizations"
run_example "optimization" "function_inlining" "Function inlining"
run_example "optimization" "algebraic_simplification" "Algebraic simplifications"

# Generate summary report
log_section "Analysis Summary"

echo "Generated files in $OUTPUT_DIR:"
ls -la "$OUTPUT_DIR" | grep -E '\.(mlir|o|txt)$' || echo "No output files found"

echo ""
echo "MLIR Analysis Summary:"
for mlir_file in "$OUTPUT_DIR"/*.mlir; do
    if [ -f "$mlir_file" ]; then
        example_name=$(basename "$mlir_file" .mlir)
        analyze_mlir "$mlir_file" "$example_name"
    fi
done

log_section "Example Run Complete"
echo "All examples have been processed."
echo "Check the output directory for generated MLIR, object files, and execution results."
echo ""
echo "To manually run individual examples:"
echo "  # Generate MLIR"
echo "  fpm run fortran -- --compile --debug-codegen example.f90"
echo ""
echo "  # Compile to executable"
echo "  fpm run fortran -- --compile -o example example.f90"
echo ""
echo "  # Enable automatic differentiation"
echo "  fpm run fortran -- --compile --enable-ad example.f90"
