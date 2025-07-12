# Test Failures - FPM Test Run

## Current Status (2025-07-12)

### ❌ **Current Failing Tests**

#### 1. test_step1_single_file - 5/6 passed (1 failure)
- **FAIL**: Parameter type enhancement with intent(in)

#### 2. test_preprocessor_function_integration - 0/3 passed (3 failures)
- **FAIL**: Function parameter type inference
- **FAIL**: Function return type inference  
- **FAIL**: Nested function call inference

#### 3. test_preprocessor_integration - 3/5 passed (2 failures)
- **FAIL**: math.f failed to run
- **FAIL**: subroutines.f failed to run

#### 4. test_examples - Has failures
- **FAIL**: example/step1_explicit_types/step1_demo.f 
  - Compilation error: Symbol 'square' already has basic type of REAL

## Main Issues

1. **Compilation error with step1_demo.f** - duplicate type declaration for 'square'
   - The cached version may not be regenerated properly
   
2. **Function-related type inference tests** are failing
   - Function parameter and return type inference not working as expected
   
3. **Preprocessor integration tests** for math.f and subroutines.f are failing
   - Need to investigate specific issues with these files