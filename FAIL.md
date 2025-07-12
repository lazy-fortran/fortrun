# Test Failures - FPM Test Run

## Current Status (2025-07-12)

### ❌ **Current Failing Tests**

#### 1. test_preprocessor_function_integration - 0/3 passed (3 failures)
- **FAIL**: Function parameter type inference
- **FAIL**: Function return type inference  
- **FAIL**: Nested function call inference
- **Issue**: Tests are producing correct output but test framework may have issues

#### 2. test_preprocessor_integration - 3/5 passed (2 failures)
- **FAIL**: math.f - duplicate declaration of `result`, missing declarations in `add` function
- **FAIL**: subroutines.f - syntax error in declaration
- **Status**: Partially fixed - `end function` lines now correct, but other issues remain

#### 3. test_examples - Has failures
- **FAIL**: example/step1_explicit_types/step1_demo.f 
  - Compilation error: Symbol 'square' already has basic type of REAL
  - Likely a cache issue

## Main Issues

### 1. **Duplicate Variable Declarations** (math.f)
- Variables explicitly declared in main scope get both:
  - Auto-generated declaration (e.g., line 5: `real(8) :: result`)
  - Enhanced explicit declaration (e.g., line 8: `real(8) :: x, y, result`)
- Need to prevent auto-generation when variables are explicitly declared

### 2. **Missing Declarations in Functions** (math.f `add` function)
- When we skip parameter declarations (`real :: a, b`), we don't inject:
  - Auto-generated declarations comment
  - Parameter declarations with intent
  - Function return variable declaration
- The `multiply` function works because it has no explicit parameter declarations

### 3. **Function Inference Test Framework**
- The preprocessor is generating correct output for function tests
- But tests are still failing - possibly test framework issues

## Progress Made

✅ Fixed type inference integration tests (integer(4) vs integer)
✅ Fixed function return variable declarations for untyped functions  
✅ Fixed duplicate parameter declarations in functions
✅ Fixed "Parameter type enhancement with intent(in)" test
✅ Fixed `real(8) end function` syntax error - now outputs correct `end function`
✅ Fixed missing variable declarations by not skipping explicit declarations in main scope