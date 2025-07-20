# Test Failures Summary

This document tracks current test failures and their root causes.

## Summary

- **Total Tests**: 87
- **Failing Tests**: 7

## Failing Tests

### 1. test_frontend_test_cases
**Status**: FAILED  
**Issue**: Minor whitespace difference in code generation
```
FAIL: function_call_inference - output mismatch
--- expected: compute = x*x
+++ actual:   compute = x * x
```
**Root Cause**: Code generator adds spaces around operators
**Fix**: Either update expected output or standardize spacing in codegen

### 2. test_artifact_cache
**Status**: FAILED  
**Issue**: Directory creation error
```
<ERROR> *mkdir*:directory creation failed
```
**Root Cause**: Trying to create cache directory with invalid path containing spaces
**Fix**: Fix path handling in cache module

### 3. test_json_workflows
**Status**: FAILED  
**Issue**: JSON workflow tests failing
**Root Cause**: JSON serialization/deserialization issues
**Fix**: Debug JSON round-trip functionality

### 4. test_json_workflows_simple
**Status**: FAILED  
**Issue**: Simple JSON workflow tests failing
**Root Cause**: Related to test_json_workflows
**Fix**: Same as above

### 5. test_notebook_system_end2end
**Status**: FAILED  
**Issue**: Notebook system tests failing
**Root Cause**: Unknown - needs investigation
**Fix**: Debug notebook integration

### 6. test_registry_enhancement
**Status**: FAILED  
**Issue**: Registry enhancement tests failing
**Root Cause**: Unknown - needs investigation
**Fix**: Debug registry module

### 7. test_runner_comprehensive
**Status**: FAILED  
**Issue**: Compilation failures in test cases
```
<ERROR> Compilation failed for object " app_main.f90.o "
```
**Root Cause**: Test cases with intentional errors not handled properly
**Fix**: Update test expectations or error handling

## Known Issues

1. **Parallel Test Runner**: Hangs on certain tests in non-verbose mode
   - Workaround: Use `-v` flag or run specific tests with `fpm test`

2. **Whitespace Inconsistencies**: Code generation adds spaces inconsistently
   - Affects: function_call_inference test
   - Need: Standardized formatting module

## Next Steps

1. Fix whitespace formatting in code generation (high priority)
2. Fix cache directory path handling (high priority)
3. Debug JSON workflow issues (medium priority)
4. Fix test runner hanging issue (high priority)
5. Investigate notebook and registry test failures (medium priority)
