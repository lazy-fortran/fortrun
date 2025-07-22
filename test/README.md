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

## Test Best Practices

1. **Keep tests focused**: Each test should verify one specific behavior
2. **Use descriptive names**: Test names should clearly indicate what they test
3. **Clean up resources**: Always clean up temporary files and directories
4. **Use cross-platform paths**: Ensure tests work on both Unix and Windows
5. **Minimize dependencies**: Tests should be as isolated as possible

## Performance Tips

- Use `make test-quick` during development for rapid feedback
- Run full test suite before committing
- Heavy tests should be marked and placed in appropriate categories
- Consider using test fixtures for expensive setup operations