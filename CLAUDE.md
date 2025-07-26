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

## Code Style Guidelines

- No move-alloc is allowed in Fortran - find other safe ways
- Always overload assignment operators for types with allocatable components to ensure deep copying
- Follow TDD, SOLID, KISS, SRP and DRY principles
- Keep work units small and manageable
- Work sequentially, never do multiple things in parallel
- Never leave commented out sections of code in source files