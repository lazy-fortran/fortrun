# Parallel Test Runner

The `run_tests_parallel.sh` script runs FPM tests in parallel across multiple CPU cores to speed up test execution.

## Usage

```bash
./test/run_tests_parallel.sh [OPTIONS] [FILTER]
```

## Options

- `-j, --jobs N`: Use N parallel jobs (default: CPU count)
- `-v, --verbose`: Show detailed test output
- `--filter PATTERN`: Filter tests by pattern
- `-h, --help`: Show help

## Examples

Run all tests using all available cores:
```bash
./test/run_tests_parallel.sh
```

Run tests matching "frontend" on 8 cores:
```bash
./test/run_tests_parallel.sh -j 8 --filter frontend
```

Run specific test with verbose output:
```bash
./test/run_tests_parallel.sh -v test_parse_multi_decl
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

## Requirements

- Bash 4.0+
- FPM (Fortran Package Manager)
- Standard Unix utilities (nproc, mktemp, etc.)
