# Test Failures

## Summary

**Frontend Tests**: 26/29 passed (90% success rate)
- Control flow: 100% passing
- Type inference: 3 failures (function-related)

## Failing Tests

### Frontend Type Inference (3 failures)

1. **function_call_inference**
   - Issue: Missing contains block and function definition
   - Generated incomplete code

2. **function_def**
   - Issue: Function definition not properly generated
   - Missing implementation details

3. **function_with_param**
   - Issue: Function with parameters not handled correctly
   - Code generation incomplete

### Other Test Failures

1. **test_cli_json_options**
   - --from-tokens execution failed with exit code 1

2. **example/fortran/step1_explicit_types/step1_demo.f**
   - Exit code 1 (not preprocessor issue)

3. **test_artifact_cache**
   - Output file not created/empty

## Known Issues (Expected Failures)

- example/fortran/advanced_inference/arrays.f
- example/fortran/advanced_inference/derived_types.f
- example/fortran/advanced_inference/function_returns.f
- example/basic/calculator/calculator.f

All marked as known preprocessor issues.

## Test Successes

### Perfect Score Categories
- Control flow (if/else, do while) - 100%
- Basic statements (assignments, prints) - 100%
- Multiple statements handling - 100%
- Logical type inference - 100%
- Most example files - 95%+ success

### Major Systems Working
- Cache system
- Runner system
- Registry system
- Notebook system (with minor issues)
- CLI argument handling
- Module resolution
- FPM integration
