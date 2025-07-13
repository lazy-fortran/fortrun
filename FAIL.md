# FAIL.md

This file tracks the current test failures and their status.

## Status: All Major Issues Resolved ✅

All major test failures have been resolved. The remaining failures are known issues with advanced type inference features that are not yet implemented.

## Fixed Issues ✅

### 1. **test_cli_system** - FIXED ✅
- **Root Cause**: Malformed cache files in `/tmp` causing compilation failures
- **Solution**: Cleared cache and temporary files
- **Status**: All CLI tests now passing

### 2. **test_preprocessor** - FIXED ✅  
- **Root Cause**: Stale build artifacts causing phantom program statements
- **Solution**: Clean rebuild with `fpm clean`
- **Status**: All preprocessor tests passing

### 3. **Subroutine Parameters Missing Intent** - FIXED ✅
- **Issue**: Subroutine parameters were not getting intent specifications
- **Root Cause**: Subroutine handling code didn't extract parameters like function handling did
- **Fix**: Added `extract_procedure_name_from_line` and parameter extraction to subroutine processing
- **Status**: Both functions and subroutines now properly enforce intent(in) by default

### 4. **Literal Type Inference Not Working** - FIXED ✅
- **Issue**: Assignments like `x = 5.0` were not generating type declarations
- **Root Cause**: Scope management bug - current_scope was not reset after USE statement collection pass
- **Symptom**: Main program lines were processed in scope 3 instead of scope 1
- **Fix**: Added `current_scope = 1` reset after rewinding for actual processing pass
- **Status**: Literal assignments now correctly generate type declarations

### 5. **test_step1_single_file** - FIXED ✅
- **Status**: All 6/6 tests passing:
  - ✅ Function signature enhancement (real → real(8))
  - ✅ Parameter type enhancement with intent(in)
  - ✅ Forward type propagation
  - ✅ Multiple functions in single file
  - ✅ Mixed explicit and implicit types
  - ✅ Nested function calls

## Known Issues (Not Yet Implemented)

These failures are expected as they test features planned for future phases:

1. **Advanced type inference examples**:
   - `example/advanced_inference/arrays.f` - Array type inference (Phase 6)
   - `example/advanced_inference/derived_types.f` - Derived type inference (Phase 6)
   - `example/advanced_inference/function_returns.f` - Complex function return inference
   - `example/notebook/arrays_loops_simple.f` - Loop array inference
   - `example/calculator/calculator.f` - Known preprocessor limitation

2. **Nested function type inference** - Some edge cases with deeply nested function calls still fail

## Summary

The preprocessor now correctly:
- ✅ Enforces `intent(in)` as default for both function AND subroutine parameters
- ✅ Infers types from literal assignments (e.g., `x = 5.0` → `real(8) :: x`)
- ✅ Manages scopes correctly during multi-pass processing
- ✅ Handles multiple functions in a single file
- ✅ Propagates types from function return values
- ✅ Enhances explicit type declarations (real → real(8))

The core functionality is working as designed. The remaining failures are for advanced features planned for future development phases.