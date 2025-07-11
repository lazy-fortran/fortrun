# Calculator Example

Demonstrates local module usage with a simple calculator program.

## Files

- **`calculator.f90`**: Main program that uses math utilities
- **`math_module.f90`**: Module containing mathematical functions (add, multiply)

## Module Dependencies

```
calculator.f90
└── math_utils (from math_module.f90)
```

## Running

```bash
fortran calculator.f90
```

This tests the Fortran CLI tool's ability to:
- Detect local module dependencies
- Copy local module files to build directory
- Build programs with local module dependencies