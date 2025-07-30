# MLIR Debugging Techniques

This guide covers debugging techniques for the MLIR backend, including common issues, diagnostic tools, and troubleshooting strategies.

## Debug Output Options

### Enable Debug Output

The compiler provides several debug output options:

```bash
# Debug AST generation
fortran --debug-ast input.f90

# Debug semantic analysis
fortran --debug-semantic input.f90

# Debug code generation (MLIR output)
fortran --compile --debug-codegen input.f90

# Combine multiple debug options
fortran --debug-ast --debug-semantic --compile --debug-codegen input.f90
```

### Debug Output Format

Debug output is generated in JSON format for programmatic processing:

```json
{
  "input_file": "test.f90",
  "generated_code": [
    "module {",
    "  func.func @main() {",
    "    return",
    "  }",
    "}"
  ]
}
```

## MLIR Tool Integration

### Validation with mlir-opt

Validate generated MLIR syntax and semantics:

```bash
# Basic validation
mlir-opt --verify-each output.mlir

# With specific passes
mlir-opt --convert-func-to-llvm --verify-each output.mlir

# Show all available passes
mlir-opt --help
```

### MLIR to LLVM IR Translation

Convert MLIR to LLVM IR for further analysis:

```bash
# Direct translation
mlir-translate --mlir-to-llvmir output.mlir -o output.ll

# With optimization passes first
mlir-opt --convert-func-to-llvm --finalize-memref-to-llvm output.mlir | \
mlir-translate --mlir-to-llvmir -o optimized.ll
```

### LLVM Tool Integration

Use LLVM tools on generated IR:

```bash
# Compile to object file
llc output.ll -o output.o

# Disassemble for inspection
llvm-dis output.bc -o output.ll

# Optimize with LLVM
opt -O2 output.ll -o optimized.ll
```

## Common MLIR Issues

### 1. SSA Value Errors

**Symptom**: `error: use of undeclared SSA value '%1'`

**Cause**: Using SSA values that haven't been defined or using them out of scope.

**Debug Steps**:
1. Check SSA value generation:
   ```fortran
   ssa_val = backend%next_ssa_value()  ! Generates unique %N
   ```

2. Verify value is defined before use:
   ```mlir
   %1 = arith.constant 42 : i32  // Define first
   %2 = arith.addi %1, %1 : i32  // Then use
   ```

3. Check scope boundaries (function/block limits)

**Fix**: Ensure proper SSA value lifecycle and scoping.

### 2. Type Mismatch Errors

**Symptom**: `error: type mismatch between operand and result types`

**Cause**: Operations with incompatible operand/result types.

**Debug Steps**:
1. Examine type generation:
   ```fortran
   mlir_type = fortran_to_mlir_type(node%type_name, node%kind_value)
   ```

2. Check operation type constraints:
   ```mlir
   // Correct: matching types
   %1 = arith.addi %arg0, %arg1 : i32

   // Incorrect: mismatched types
   %1 = arith.addi %arg0, %arg1 : i32, f32  // Error!
   ```

3. Verify dialect operation signatures

**Fix**: Ensure type consistency across operations.

### 3. Dialect Import Issues

**Symptom**: `error: dialect 'func' is not registered`

**Cause**: Missing dialect declarations in MLIR module.

**Debug Steps**:
1. Check module header includes required dialects
2. Verify dialect operations are used correctly
3. Ensure proper dialect initialization

**Fix**: Add proper dialect declarations and use correct operations.

### 4. Function Signature Mismatches

**Symptom**: `error: function signature mismatch`

**Cause**: Function calls don't match declared signatures.

**Debug Steps**:
1. Compare function declaration and call sites:
   ```mlir
   // Declaration
   func.func @square(%arg0: f32) -> f32

   // Call (must match)
   %result = func.call @square(%input) : (f32) -> f32
   ```

2. Check parameter types and counts
3. Verify return type consistency

**Fix**: Align function signatures with call sites.

## Debugging Workflow

### 1. Incremental Testing

Start with simple test cases and gradually increase complexity:

```fortran
! Step 1: Simple program
program simple
    integer :: x = 42
    print *, x
end program simple

! Step 2: Add functions
program with_function
    integer function add(a, b)
        integer :: a, b
        add = a + b
    end function add

    print *, add(1, 2)
end program with_function

! Step 3: Add loops and control flow
! ... continue incrementally
```

### 2. Isolation Testing

Test individual components separately:

```bash
# Test only AST generation
fortran --debug-ast simple.f90

# Test semantic analysis
fortran --debug-semantic simple.f90

# Test MLIR generation
fortran --compile --debug-codegen simple.f90

# Test with specific options
fortran --compile --enable-ad --debug-codegen simple.f90
```

### 3. Comparative Analysis

Compare outputs between backends:

```bash
# Fortran backend output
fortran --standardize input.f90 > fortran_output.f90

# MLIR backend output
fortran --compile --debug-codegen input.f90 > mlir_output.json

# Compare semantic analysis
fortran --debug-semantic input.f90 > semantic_output.json
```

## Performance Debugging

### 1. Compilation Time Analysis

Use the benchmarking framework:

```bash
# Run performance benchmarks
make benchmark

# Keep results for analysis
make benchmark-keep

# Profile specific components
time fortran --compile input.f90
```

### 2. Memory Usage Profiling

Monitor memory usage during compilation:

```bash
# Using time command (if available)
/usr/bin/time -v fortran --compile input.f90

# Using valgrind for detailed analysis
valgrind --tool=massif fortran --compile input.f90
```

### 3. MLIR Generation Profiling

Profile MLIR generation performance:

```fortran
! Add timing to generation functions
call system_clock(start_time)
mlir = generate_mlir_node(backend, arena, node_index, indent_level)
call system_clock(end_time)
! Log timing information
```

## Automated Debugging

### 1. Test Harness Integration

Use the MLIR test harness for systematic debugging:

```bash
# Run all tests with validation
make test-mlir-validate

# Run with verbose output for debugging
make test-mlir-verbose

# Filter specific test categories
./test/mlir/run_mlir_tests.sh --filter basic
```

### 2. Continuous Integration Debugging

Configure CI for comprehensive debugging:

```yaml
# Example CI configuration
- name: MLIR Debug Tests
  run: |
    make test-mlir-validate
    make benchmark
    # Upload debug artifacts
```

### 3. Regression Detection

Use regression tests to catch issues:

```bash
# Run regression test suite
fpm test test_regression_suite

# Compare with baseline
fpm test test_backend_comparison
```

## Advanced Debugging Techniques

### 1. MLIR Pass Debugging

Debug specific MLIR passes:

```bash
# Debug specific passes
mlir-opt --debug --convert-func-to-llvm input.mlir

# Print IR before/after passes
mlir-opt --print-ir-before-all --print-ir-after-all input.mlir
```

### 2. LLVM IR Analysis

Analyze generated LLVM IR:

```bash
# Generate and analyze LLVM IR
mlir-translate --mlir-to-llvmir input.mlir -o output.ll
llvm-as output.ll -o output.bc
llvm-dis output.bc  # Re-disassemble for verification
```

### 3. Custom Debug Assertions

Add debug assertions to MLIR generation:

```fortran
! Example debug assertion
if (len_trim(error_msg) > 0) then
    print *, "DEBUG: Error in MLIR generation:", trim(error_msg)
    print *, "DEBUG: Current node type:", node%node_type
    print *, "DEBUG: SSA counter:", backend%ssa_counter
    stop 1
end if
```

## Troubleshooting Checklist

### Before Debugging
- [ ] Verify input Fortran code is syntactically correct
- [ ] Check that required MLIR tools are installed
- [ ] Ensure test environment is properly set up
- [ ] Review recent changes that might have introduced issues

### During Debugging
- [ ] Start with simplest possible test case
- [ ] Use incremental complexity approach
- [ ] Enable all relevant debug outputs
- [ ] Compare with known working examples
- [ ] Check for consistent SSA value usage
- [ ] Verify type consistency throughout pipeline

### After Debugging
- [ ] Add regression test for the fixed issue
- [ ] Update documentation if needed
- [ ] Verify fix doesn't break existing functionality
- [ ] Consider if fix reveals need for architectural changes

## Getting Help

### Internal Resources
1. Check existing tests in `test/mlir/` for working examples
2. Review backend implementation guide
3. Examine similar AST node implementations
4. Use the test harness for systematic validation

### External Resources
1. [MLIR Discourse Forum](https://discourse.llvm.org/c/mlir/31)
2. [LLVM Developer Documentation](https://llvm.org/docs/)
3. [FIR Documentation](https://flang.llvm.org/docs/)
4. [Enzyme AD Documentation](https://enzyme.mit.edu/getting_started/)

### Reporting Issues
When reporting issues, include:
- Minimal reproducing example
- Complete error messages
- Debug output from all stages
- Environment information (OS, MLIR version, etc.)
- Steps taken to debug the issue

Remember: Most MLIR debugging issues stem from SSA form violations, type mismatches, or incorrect dialect usage. Start with these common causes before diving into complex debugging scenarios.
