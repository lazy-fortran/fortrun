# Test Suite Organization

This directory contains the comprehensive test suite for the Fortran CLI project. Tests are organized by category to enable efficient parallel execution and rapid feedback during development.

## Test Categories

### Quick Tests (< 5 seconds)
- **core**: Core language features (lexer, parser, semantic analysis)
- **utilities**: Utility modules (logger, string utils, system utils)
- **cache**: Cache functionality tests
- **runner**: Test runner and execution tests

### Medium Tests (5-30 seconds)
- **cli**: Command-line interface tests
- **notebook**: Jupyter notebook support tests
- **fpm**: FPM integration tests
- **module**: Module scanning and resolution tests

### Heavy Tests (> 30 seconds)
- **frontend**: Comprehensive frontend tests
- **integration**: End-to-end integration tests

## Running Tests

### Local Development

```bash
# Run all tests
make test

# Run quick tests for rapid feedback
make test-quick

# Run specific test category
make test-core
make test-utilities
make test-cache
# ... etc

# Run multiple categories
./scripts/run-test-group.sh core utilities cache

# Run a single test
make test-single ARGS="test_name"
```

### CI/CD

Tests are automatically run in parallel across multiple GitHub Actions runners:
- Unit tests run on both Ubuntu and Windows
- Integration tests run with appropriate dependencies
- Heavy tests run on Ubuntu with more resources

See `.github/workflows/test-parallel.yml` for the parallel test configuration.

## Adding New Tests

1. Place your test in the appropriate category directory
2. Name it following the pattern: `test_<module>_<description>.f90`
3. Add it to the appropriate test group in:
   - `scripts/run-test-group.sh` for local testing
   - `.github/workflows/test-parallel.yml` for CI

## Test Naming Convention

### File Naming
All test files must follow this pattern: `test_<category>_<feature>.f90`

Examples:
- `test_lexer_keywords.f90` - Tests for lexer keyword recognition
- `test_parser_expressions.f90` - Tests for parser expression handling
- `test_cache_isolation.f90` - Tests for cache isolation features

### Test Function Naming
Test functions within files should be descriptive and follow a consistent pattern:
- Start with the feature being tested
- Include the specific behavior or edge case
- Use underscores to separate words

Examples:
```fortran
! Good test function names
subroutine test_array_literal_type_inference()
subroutine test_where_construct_single_line()
subroutine test_optional_parameter_with_present()

! Avoid vague names like:
! test1(), test_basic(), test_it_works()
```

## Test Best Practices

1. **Keep tests focused**: Each test should verify one specific behavior
2. **Use descriptive names**: Test names should clearly indicate what they test
3. **Clean up resources**: Always clean up temporary files and directories
4. **Use cross-platform paths**: Ensure tests work on both Unix and Windows
5. **Minimize dependencies**: Tests should be as isolated as possible
6. **Use error messages**: Include descriptive error messages in assertions
7. **Test edge cases**: Don't just test the happy path
8. **Isolate tests**: Use isolated cache/temp directories for parallel execution

## Performance Tips

- Use `make test-quick` during development for rapid feedback
- Run full test suite before committing
- Heavy tests should be marked and placed in appropriate categories
- Consider using test fixtures for expensive setup operations