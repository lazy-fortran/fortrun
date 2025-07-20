# Testing Guide

The Fortran CLI includes a high-performance parallel test runner built with OpenMP that provides efficient test execution with smart load balancing.

## Quick Start

```bash
# Run all tests
fortran --test

# Run tests matching a pattern
fortran --test frontend
fortran --test --filter cache

# Control output verbosity
fortran --test -v    # Verbose (show all test output)
fortran --test -q    # Quiet (only failures and summary)

# Control parallelization
fortran --test -j 8  # Use 8 threads (default: auto-detect all cores)

# Get help
fortran --test --help
```

## Features

### 🚀 **High Performance**
- **OpenMP parallel execution** with dynamic work queue
- **Smart load balancing** across CPU cores
- **Real-time progress reporting** showing thread assignments
- **Direct FPM API integration** (no shell command overhead)

### 🎯 **Smart Test Discovery**
- Automatic test discovery using FPM API
- Filters tests by executable scope (test vs app vs library)
- Supports pattern-based filtering for targeted test runs

### 📊 **Rich Output Options**
- **Default**: Shows only failed tests and summary
- **Verbose (`-v`)**: Shows detailed output for all tests
- **Quiet (`-q`)**: Minimal output (failures and summary only)
- **Real-time progress**: Thread assignments and completion status

### 🛡️ **Robust Execution**
- **Timeout handling**: Tests are automatically killed after 60 seconds
- **Error isolation**: Failed tests don't affect other test execution
- **Thread safety**: All operations are properly synchronized

## Command Reference

### Basic Usage
```bash
fortran --test                    # Run all tests
fortran --test [PATTERN]          # Run tests matching pattern
```

### Options
```bash
-v, --verbose     Show detailed output for all tests
-q, --quiet       Minimal output (only failures and summary)
--filter PATTERN  Run only tests matching the pattern
-j, --jobs N      Use N threads (default: auto-detect)
-h, --help        Show help message
```

### Examples
```bash
# Run all frontend tests with verbose output
fortran --test --filter frontend -v

# Run cache tests quietly using 4 threads
fortran --test cache -q -j 4

# Run all tests with maximum verbosity
fortran --test -v

# Get detailed help
fortran --test --help
```

## Architecture

The test runner consists of several modular components:

- **`test_discovery.f90`**: FPM API integration for test discovery
- **`test_execution.f90`**: Individual test execution with timeout handling
- **`test_runner.f90`**: OpenMP parallel coordination and work queue
- **`test_cli.f90`**: Command-line interface and argument parsing

## Performance

The parallel test runner provides significant performance benefits:

- **Dynamic work distribution**: Tests are assigned to threads as they become available
- **No idle time**: Threads immediately pick up new work when they finish
- **Optimal resource usage**: Automatically detects and uses all available CPU cores
- **Minimal overhead**: Direct API calls avoid shell command overhead

## Error Handling

The test runner includes comprehensive error handling:

- **Build failures**: Clear error messages if test compilation fails
- **Test timeouts**: Automatic termination of hanging tests after 60 seconds
- **Missing executables**: Graceful handling of discovery vs execution mismatches
- **Thread safety**: All shared state is properly protected with OpenMP locks
