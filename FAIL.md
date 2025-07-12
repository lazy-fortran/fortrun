# Test Failures - FPM Test Run

## Current Status (2025-07-12)

### ❌ **Current Failing Tests**

#### 1. ~~test_step1_single_file~~ - ✅ FIXED
- Fixed: Parameter type enhancement with intent(in)
- Now expects parameters on separate lines (better practice)

#### 2. test_preprocessor_function_integration - 0/3 passed (3 failures)
- **FAIL**: Function parameter type inference
- **FAIL**: Function return type inference  
- **FAIL**: Nested function call inference
- **Issue**: Tests are producing correct output but test framework may have issues

#### 3. test_preprocessor_integration - 3/5 passed (2 failures)
- **FAIL**: math.f failed to run
- **FAIL**: subroutines.f failed to run
- **Issue**: Duplicate variable declarations when explicit declarations exist

#### 4. test_examples - Has failures
- **FAIL**: example/step1_explicit_types/step1_demo.f 
  - Compilation error: Symbol 'square' already has basic type of REAL
  - Likely a cache issue

## Main Issues

### 1. **Duplicate Variable Declarations** (math.f, subroutines.f)
The preprocessor generates duplicate declarations when:
- Variables are explicitly declared (`real :: result`)
- They're also assigned values (`result = func()`)
- Type inference generates a declaration before seeing the explicit one
- Result: Both auto-generated and enhanced explicit declarations appear

### 2. **Function Inference Test Framework**
- The preprocessor is generating correct output for function tests
- But tests are still failing - possibly test framework issues
- Need to investigate why `check_output_contains` isn't finding the expected strings

## Progress Made

✅ Fixed type inference integration tests (integer(4) vs integer)
✅ Fixed function return variable declarations for untyped functions  
✅ Fixed duplicate parameter declarations in functions
✅ Fixed "Parameter type enhancement with intent(in)" test