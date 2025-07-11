# Interdependent Modules Example

This example demonstrates multiple local Fortran modules with interdependencies:

## Module Dependencies

```
main.f90
├── geometry.f90
│   ├── constants.f90
│   └── geometry.f90 (self-dependency for calculate_area)
└── input_output.f90
    └── constants.f90
```

## Files

- **`main.f90`**: Main program that uses geometry and input_output modules
- **`constants.f90`**: Mathematical constants (π, e)
- **`geometry.f90`**: Geometry calculations (depends on constants)
- **`input_output.f90`**: Output formatting (depends on constants)

## Dependency Chain

1. `constants.f90` - Base module with no dependencies
2. `geometry.f90` - Uses constants, has internal function dependencies
3. `input_output.f90` - Uses constants
4. `main.f90` - Uses geometry and input_output

## Running

```bash
fortran main.f90
```

This tests the Fortran CLI tool's ability to:
- Detect multiple local module files
- Resolve interdependencies between modules
- Copy all required files to the temporary build directory
- Build successfully with correct module ordering