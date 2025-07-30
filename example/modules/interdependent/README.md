# Interdependent Modules Example

Demonstrates multiple local Fortran modules with interdependencies in both syntax styles.

## Module Dependencies

```
main.lf / main.f90
├── geometry.lf / geometry.f90
│   ├── constants.lf / constants.f90
│   └── geometry.f90 (self-dependency for calculate_area)
└── input_output.lf / input_output.f90
    └── constants.lf / constants.f90
```

## Files

**Simplified syntax (.lf files):**
- **`main.lf`**: Main program with type inference
- **`constants.lf`**: Mathematical constants (π, e) with type inference
- **`geometry.lf`**: Geometry calculations with simplified syntax
- **`input_output.lf`**: Output formatting with simplified syntax

**Standard Fortran (.f90 files):**
- **`main.f90`**: Standard program with explicit types
- **`constants.f90`**: Mathematical constants with explicit real(8)
- **`geometry.f90`**: Standard geometry calculations
- **`input_output.f90`**: Standard output formatting

## Dependency Chain

1. `constants.lf/f90` - Base module with no dependencies
2. `geometry.lf/f90` - Uses constants, has internal function dependencies  
3. `input_output.lf/f90` - Uses constants
4. `main.lf/f90` - Uses geometry and input_output

## Running

**Simplified syntax:**
```bash
fortran main.lf
```

**Standard Fortran:**
```bash
fortran main.f90
```

## Preprocessor Example

**Simplified `.lf` version:**
```fortran
! main.lf
use geometry, only: calculate_area, calculate_volume
use input_output, only: print_results

radius = 5.0    ! Type inference: real(8)
height = 10.0   ! Type inference: real(8)
area = calculate_area(radius)
volume = calculate_volume(radius, height)
call print_results(radius, height, area, volume)
```

**Generated `.f90` equivalent:**
```fortran
! main.f90  
program main
  use geometry, only: calculate_area, calculate_volume
  use input_output, only: print_results
  implicit none

  real(8) :: radius, height, area, volume  ! Explicit declarations

  radius = 5.0_8
  height = 10.0_8
  area = calculate_area(radius)
  volume = calculate_volume(radius, height)
  call print_results(radius, height, area, volume)
end program main
```

This demonstrates the Fortran CLI tool's ability to:
- Detect multiple local module files
- Resolve interdependencies between modules
- Handle both simplified and standard syntax
- Automatically generate type declarations for complex dependency chains

## Current Status
- ✅ **main.f90**: Fully working with interdependent modules
- ⚠️ **main.lf**: Known limitation - USE statements after variable declarations not yet supported
- ✅ **All .f90 modules**: Standard module format working perfectly
- 🔄 **All .lf modules**: Module preprocessing not yet implemented

**Workaround**: Use .f90 for all modules, only main programs can use .lf syntax currently.
