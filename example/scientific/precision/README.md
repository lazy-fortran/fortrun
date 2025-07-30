# Precision Examples

Demonstrates the Fortran CLI tool's different behaviors for .f90 and .lf files.

## Files

**Simplified syntax (.lf files):**
- **`precision_test.lf`**: Tests opinionated precision settings (real(8) default)
- **`precision_compare.lf`**: Compares single vs double precision with .lf behavior
- **`real_default_test.lf`**: Verifies `real` becomes `real(8)` with .lf files

**Standard Fortran (.f90 files):**
- **`precision_test.f90`**: Tests standard Fortran precision settings
- **`precision_compare.f90`**: Compares single vs double precision  
- **`real_default_test.f90`**: Verifies standard `real` behavior

## Precision Behavior

The Fortran CLI tool handles precision differently for different file types:

### Standard Fortran (.f90 files)
- Uses standard Fortran defaults
- `real` variables use default single precision
- No special compiler flags applied
- `implicit none` enforced only through fpm.toml configuration

### Lowercase Fortran (.lf files)  
- Uses opinionated modern defaults
- `real` variables default to `real(8)` (double precision)
- Compiler flags: `-fdefault-real-8 -fdefault-double-8`
- `implicit none` added automatically during preprocessing

## Running

**Simplified syntax (opinionated double precision):**
```bash
fortran precision_test.lf
fortran precision_compare.lf
fortran real_default_test.lf
```

**Standard Fortran (standard single precision):**
```bash
fortran precision_test.f90
fortran precision_compare.f90
fortran real_default_test.f90
```

This demonstrates:
- Standard Fortran behavior for .f90 files (single precision by default)
- Modern opinionated defaults for .lf files (double precision by default)
- How file extension affects compilation behavior
