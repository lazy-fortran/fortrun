# Precision Examples

Demonstrates the Fortran CLI tool's modern defaults for precision handling.

## Files

- **`precision_test.f90`**: Tests default precision settings
- **`precision_compare.f90`**: Compares single vs double precision
- **`real_default_test.f90`**: Verifies that `real` defaults to `real(8)` (double precision)

## Modern Defaults

The Fortran CLI tool enforces modern defaults:
- `implicit none` is enforced automatically
- `real` variables default to `real(8)` (double precision)
- Compiler flags: `-fdefault-real-8 -fdefault-double-8`

## Running

```bash
fortran precision_test.f90
fortran precision_compare.f90
fortran real_default_test.f90
```

This tests the Fortran CLI tool's ability to:
- Apply modern compiler defaults
- Ensure double precision by default
- Enforce `implicit none` through fpm.toml configuration