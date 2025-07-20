# Parallel Test Runner

⚠️ **CRITICAL: This is the ONLY approved method for running multiple tests** ⚠️

The `run_tests_parallel.sh` script runs FPM tests in parallel across multiple CPU cores to speed up test execution. It provides all the functionality of `fpm test` with better performance and additional features.

**Default behavior**: Shows only failed tests and their output. Passed tests are counted but not displayed for cleaner output.

## Usage

```bash
./test/run_tests_parallel.sh [OPTIONS] [FILTER]
```

## Options

- `-j, --jobs N`: Use N parallel jobs (default: CPU count)
- `-v, --verbose`: Show all test names, not just failures (default: only show failed tests)
- `-q, --quiet`: Suppress progress output (only show summary)
- `-d, --debug`: Show debug information
- `--full-output`: Show full test output exactly like `fpm test`
- `--output-dir DIR`: Save individual test outputs to directory
- `--filter PATTERN`: Filter tests by pattern
- `-h, --help`: Show help

## Examples

Run all tests (default - shows only failures):
```bash
./test/run_tests_parallel.sh
```

Run all tests showing all results:
```bash
./test/run_tests_parallel.sh -v
```

Run with full output (exactly like `fpm test`):
```bash
./test/run_tests_parallel.sh --full-output
```

Run tests matching "frontend" on 8 cores:
```bash
./test/run_tests_parallel.sh -j 8 --filter frontend
```

Debug failing tests with saved outputs:
```bash
./test/run_tests_parallel.sh --full-output --output-dir failures/ --filter "failing_test"
```

Quiet mode for CI/CD:
```bash
./test/run_tests_parallel.sh -q
```

Run specific test with verbose output:
```bash
./test/run_tests_parallel.sh -v --filter test_parse_multi_decl
```

## How it works

1. **Build Phase**: Builds all tests using `fpm test --runner echo`
2. **Discovery**: Finds all test executables
3. **Distribution**: Evenly splits tests across available cores
4. **Execution**: Runs test subsets in parallel
5. **Collection**: Merges results maintaining original test order
6. **Reporting**: Shows results in same format as regular `fpm test`

## Performance

On a typical 8-core machine, this can reduce test time by 6-7x compared to sequential execution.

## Usage Policy

⚠️ **IMPORTANT**:
- **ALWAYS** use this script for running multiple tests
- **NEVER** use `fpm test` without arguments (it's too slow)
- **ONLY** use `fpm test test_name` for running a single specific test

This ensures consistent test execution and optimal performance across the project.

## Requirements

- Bash 4.0+
- FPM (Fortran Package Manager)
- Standard Unix utilities (nproc, mktemp, etc.)
