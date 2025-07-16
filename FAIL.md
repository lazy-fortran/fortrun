# Test Suite Failure Report

## Date: 2025-01-17

## Summary
Multiple test failures occurred during full test suite execution. The stdin blocking issue has been fixed, but numerous other tests are failing.

## Test Execution Results

### 1. Frontend AST JSON I/O Tests
**Test**: `test_ast_json_io`  
**Status**: FAILED  
**Error**: `ERROR STOP AST JSON I/O tests failed!`  
**Location**: `test/frontend/test_ast_json_io.f90:35`  
**Exit Code**: 1

### 2. Frontend Lexer Number Tests
**Test**: `test_frontend_lexer_numbers`  
**Status**: RUNTIME ERROR  
**Error**: `Substring out of bounds: upper bound (6) of 'source' exceeds string length (2)`  
**Location**: `src/frontend/lexer/lexer_core.f90:346` in `scan_logical_token`  
**Context**: Error occurs during `test_real_literals` at `test/frontend/lexer/test_frontend_lexer_numbers.f90:98`  
**Exit Code**: 2

### 3. Frontend Lexer Operator Tests
**Test**: `test_frontend_lexer_operators`  
**Status**: RUNTIME ERROR  
**Error**: `Substring out of bounds: upper bound (6) of 'source' exceeds string length (1)`  
**Location**: `src/frontend/lexer/lexer_core.f90:346` in `scan_logical_token`  
**Context**: Error occurs during `test_delimiters` at `test/frontend/lexer/test_frontend_lexer_operators.f90:179`  
**Exit Code**: 2

### 4. Frontend Semantic Array Type Inference Tests
**Test**: `test_frontend_semantic_array_type_inference`  
**Status**: FAILED  
**Error**: `FAIL: Parameters get intent(in) by default`  
**Details**: `FAILED: 1 tests failed`  
**Exit Code**: 1

### 5. Frontend Test Cases
**Test**: `test_frontend_test_cases`  
**Status**: FAILED  
**Failures**:
  - `function_call_inference` - output mismatch
    - Expected: `result = compute(5.0d0)`
    - Actual: `result = ???`
  - `function_with_param` - output mismatch
    - Expected: `result = add_numbers(5.0d0, 3.0d0)`
    - Actual: `result = ???`
**Details**: Tests: 13/18 passed, 5 tests failed  
**Exit Code**: 1

### 6. Examples Tests
**Test**: `test_examples`  
**Status**: FAILED  
**Exit Code**: 1

### 7. Step1 Integration Tests
**Test**: `test_step1_integration`  
**Status**: FAILED  
**Exit Code**: 1

### 8. Notebook Examples Tests
**Test**: `test_notebook_examples`  
**Status**: FAILED  
**Exit Code**: 1

### 9. Notebook System End-to-End Tests
**Test**: `test_notebook_system_end2end`  
**Status**: FAILED  
**Exit Code**: 1

### 10. Registry Enhancement Tests
**Test**: `test_registry_enhancement`  
**Status**: FAILED  
**Error**: `Expected text "pyplot-fortran" not found in output`  
**Exit Code**: 1

### 11. Compilation Failures
**Objects failing compilation**:
  - `app_main.f90.o` (multiple instances)
  - `test_runner_empty`
**Error**: `undefined reference to 'main'` during linking  
**Details**: `/usr/bin/ld: /lib/x86_64-linux-gnu/crt1.o: in function '_start': (.text+0x17): undefined reference to 'main'`

### 12. System Errors
**Directory Creation Errors**:
  - `mkdir: cannot create directory '/dev/null': Not a directory`
  - `mkdir: cannot create directory '': No such file or directory`

### 13. CLI System Test
**Test**: `test_cli_system`  
**Error**: `Help output not found in /tmp/test_no_args_13EA4A2/cli_test_output.txt`

## Critical Issues

### 1. Lexer Substring Bounds Checking
The lexer is attempting to access characters beyond the string length when scanning for logical tokens. This affects:
- Number literal parsing
- Operator/delimiter parsing

### 2. Type Inference for Function Calls
The semantic analyzer is generating `???` placeholders instead of proper function calls for:
- Simple function calls: `compute(5.0d0)`
- Function calls with parameters: `add_numbers(5.0d0, 3.0d0)`

### 3. AST JSON Serialization/Deserialization
Complete failure of the AST JSON I/O functionality, which is critical for the JSON pipeline features.

### 4. Build System Issues
Missing main function in some test compilations, particularly affecting `test_runner_empty`.

## Fixed Issues

### Stdin Blocking Issue
**Status**: FIXED  
**Solution**: Added `check_stdin_available` check before attempting to read from stdin in `handle_stdin_input` function at `src/runner/runner.f90:854-858`

## Impact
- Multiple core functionalities are broken
- Type inference system has critical bugs
- Lexer has bounds checking issues
- JSON pipeline functionality is compromised

## Next Steps
1. Fix lexer substring bounds checking in `scan_logical_token`
2. Debug type inference for function calls
3. Fix AST JSON serialization/deserialization
4. Resolve build system issues for test executables
5. Address registry and notebook test failures