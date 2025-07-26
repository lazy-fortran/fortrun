# Fortran Compiler Project Guidelines

## Testing Guidelines

### Test Execution
The correct command to run tests is:
```bash
OMP_NUM_THREADS=24 fpm run fortran -- --test
```

### Test Isolation Requirements
**IMPORTANT**: Each test MUST run in its own isolated cache and temp directory to avoid race conditions during parallel execution.

The codebase provides tooling for test isolation:

1. **fortran_with_isolated_cache**: Creates a command that runs fortran with an isolated cache directory
   ```fortran
   use temp_utils, only: fortran_with_isolated_cache
   
   ! Create command with isolated cache
   command = fortran_with_isolated_cache('test_name') // ' <args>'
   ```

2. **create_test_cache_dir**: Creates a unique test cache directory
   ```fortran
   use temp_utils, only: create_test_cache_dir, ensure_cache_structure
   
   ! Create isolated cache for test
   test_cache_dir = create_test_cache_dir('test_name')
   call ensure_cache_structure(test_cache_dir, success)
   ```

3. **temp_dir_manager**: Manages temporary directories with automatic cleanup
   ```fortran
   use temp_utils, only: temp_dir_manager
   
   type(temp_dir_manager) :: temp_mgr
   call temp_mgr%create('test_prefix')
   ! Use temp_mgr%get_file_path('filename') to get paths
   ! Automatically cleaned up when temp_mgr goes out of scope
   ```

### Common Test Patterns

When writing tests that use caching or temporary files:
1. Create isolated cache/temp directories for each test function
2. Use `temp_dir_manager` for automatic cleanup
3. Never share cache directories between tests
4. Use `fortran_with_isolated_cache` when running fortran subprocess commands

### Test Directory Structure

**IMPORTANT**: All test files MUST be organized in subdirectories by category.

#### Directory Structure:
```
test/
├── benchmarks/      # Performance and benchmark tests
├── cache/           # Cache system tests
├── cli/             # CLI interface tests
├── config/          # Configuration tests
├── examples/        # Example program tests
├── fpm/             # FPM integration tests
├── frontend/        # Frontend tests (lexer, parser, semantic)
│   ├── lexer/       # Lexer-specific tests
│   ├── parser/      # Parser-specific tests
│   └── semantic/    # Semantic analysis tests
├── integration/     # End-to-end integration tests
├── misc/            # Miscellaneous tests
├── module/          # Module scanner tests
├── notebook/        # Notebook feature tests
├── registry/        # Registry tests
├── runner/          # Runner tests
├── standard_fortran/# Standard Fortran compatibility tests
├── utilities/       # Utility module tests
└── utils/           # Test utilities (not actual tests)
```

#### Test Naming Conventions:
1. All test files MUST start with `test_` prefix
2. Test files MUST have `.f90` extension
3. Test file names should be descriptive: `test_<feature>_<aspect>.f90`
4. Examples:
   - `test_cache_isolation.f90`
   - `test_parser_expressions.f90`
   - `test_cli_json_options.f90`

#### Test Groups for CI:
Tests are organized into groups for parallel CI execution:
- **core**: Lexer, parser, semantic analysis tests
- **utilities**: String utils, logger, system utils tests
- **cache**: All cache-related tests
- **runner**: Test runner functionality
- **cli**: CLI interface tests
- **notebook**: Notebook feature tests
- **fpm**: FPM integration tests
- **module**: Module scanner tests
- **frontend**: Heavy frontend tests (run with OMP_NUM_THREADS=4)
- **integration**: Heavy integration tests (run with OMP_NUM_THREADS=4)
- **quick**: Subset of fast tests for rapid feedback

#### Running Test Groups:
```bash
# Run specific test group
make test-core
make test-cache
make test-utilities

# Run all tests in parallel
make test

# Generate coverage for specific group
make coverage-group GROUP=cache
```

## Code Style Guidelines

- No move-alloc is allowed in Fortran - find other safe ways
- Always overload assignment operators for types with allocatable components to ensure deep copying
- Follow TDD, SOLID, KISS, SRP and DRY principles
- Keep work units small and manageable
- Work sequentially, never do multiple things in parallel
- Never leave commented out sections of code in source files

## CI/CD Guidelines

### Parallel Test Execution
The CI system runs test groups in parallel across multiple jobs:
- Each test group runs in its own GitHub Actions job
- Coverage is collected separately for each group
- Coverage reports are merged at the end

### Adding New Tests
When adding new tests:
1. Place the test in the appropriate subdirectory
2. Follow the `test_*.f90` naming convention
3. Add the test to the appropriate group in `scripts/run-test-group.sh`
4. Update the CI workflow if creating a new test group
5. Ensure the test uses proper cache isolation

### Coverage Requirements
- Minimum coverage threshold: 45%
- Coverage is collected using gcovr
- HTML coverage reports are generated and uploaded as artifacts
- Coverage data from all test groups is merged for the final report