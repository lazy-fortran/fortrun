# Code Coverage Guide

This document explains how code coverage is collected and reported in the Fortran CLI project.

## Overview

We use a parallel test execution strategy to speed up CI/CD while maintaining comprehensive coverage reporting. Coverage data from multiple test jobs is collected and merged to provide a complete picture.

## CI/CD Coverage Collection

### Parallel Workflow

The `.github/workflows/test-parallel-coverage.yml` workflow:

1. **Runs tests in parallel** across multiple jobs:
   - Unit tests (core, utilities, cache, runner)
   - Integration tests (cli, notebook, fpm, module)
   - Heavy tests (frontend, integration)

2. **Collects coverage data** from each job:
   - Each job generates `.gcda` and `.gcno` files
   - Coverage data is converted to JSON format
   - Artifacts are uploaded for merging

3. **Merges coverage** in a final job:
   - Downloads all coverage artifacts
   - Restores original file structure
   - Merges JSON reports using `gcovr`
   - Uploads to Codecov

### Coverage Flow

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│ Unit Tests  │     │Integration  │     │ Heavy Tests │
│   (4 jobs)  │     │  (4 jobs)   │     │  (2 jobs)   │
└──────┬──────┘     └──────┬──────┘     └──────┬──────┘
       │                   │                   │
       │ coverage-*.json   │ coverage-*.json  │ coverage-*.json
       └───────────────────┴───────────────────┘
                           │
                    ┌──────▼──────┐
                    │ Merge Job   │
                    │  - gcovr    │
                    │  - Codecov  │
                    └─────────────┘
```

## Local Coverage Collection

### Quick Coverage Check

For a specific test group:
```bash
make coverage-group GROUP=utilities
```

### Full Coverage Report

Run all tests with coverage:
```bash
make coverage
```

This uses `scripts/coverage-local.sh` which:
- Runs test groups in parallel (if GNU parallel is available)
- Generates individual JSON reports
- Merges them into a single report

### Coverage Reports

Three formats are generated:
- **XML** (`coverage.xml`): For Codecov upload
- **HTML** (`coverage-reports/index.html`): For detailed browsing
- **Text** (`coverage.txt`): For quick terminal viewing

## Best Practices

### Writing Tests for Coverage

1. **Test edge cases**: Ensure error paths are covered
2. **Use platform checks**: Test both Windows and Unix paths
3. **Clean up resources**: Prevent test interference
4. **Group related tests**: Keep test groups focused

### Improving Coverage

To identify uncovered code:
```bash
# Generate HTML report
make coverage-html

# Open in browser
open coverage-reports/index.html
```

Look for:
- Red lines: Uncovered code
- Yellow lines: Partially covered (some branches missed)
- Green lines: Fully covered

### Coverage Thresholds

- Current threshold: 45% (enforced in CI)
- Target: 70%+ for good coverage
- Critical modules should have 80%+ coverage

## Troubleshooting

### Missing Coverage Data

If coverage seems low:
1. Ensure tests actually execute the code
2. Check for conditional compilation (`#ifdef`)
3. Verify `.gcda` files are generated
4. Look for test failures that skip coverage

### Merging Issues

If coverage merge fails:
1. Check artifact upload/download logs
2. Verify JSON files are valid
3. Ensure directory structure is preserved
4. Check gcovr version compatibility

### Performance

To speed up local coverage:
1. Install GNU parallel: `sudo apt-get install parallel`
2. Run specific groups: `make coverage-group GROUP=core`
3. Use `make test-quick` for rapid feedback without coverage