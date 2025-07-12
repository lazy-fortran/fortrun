# Getting Started

**Make Python Fortran again** - Run Fortran programs directly without compilation hassles.

## Installation

**Prerequisites**: FPM and a Fortran compiler (gfortran, ifort, etc.)

```bash
# Clone and build
git clone https://github.com/krystophny/fortran
cd fortran
fpm build --profile release

# Add to PATH or copy binary
cp build/gfortran_*/app/fortran /usr/local/bin/
```

## First Run

```bash
# Create hello.f90
echo 'print *, "Hello, World!"' > hello.f90

# Run it
fortran hello.f90
```

Output:
```
Hello, World!
```

That's it. No makefiles, no manual compilation.

## Basic Examples

**Simple calculation:**
```fortran
! calc.f90
real :: x = 3.14159
real :: area = x * x  
print *, 'Area:', area
```

```bash
fortran calc.f90
# Area: 9.869604
```

**With modules:**
```fortran
! math_utils.f90  
module math_utils
    implicit none
contains
    function square(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x * x
    end function
end module

! main.f90
program main
    use math_utils
    print *, square(5.0)
end program
```

```bash
fortran main.f90
# 25.0
```

The tool automatically finds `math_utils.f90`, compiles it, and links everything.

## Key Features

- **Zero configuration** - No build files needed
- **Smart dependency detection** - Finds local modules automatically  
- **Package registry** - Resolves external dependencies from FPM registry
- **Fast caching** - Recompiles only when source changes
- **Modern defaults** - `implicit none`, double precision by default

## What Makes This Different?

**Traditional Fortran:**
```bash
gfortran -c math_utils.f90
gfortran -c main.f90  
gfortran -o main main.o math_utils.o
./main
```

**With fortran tool:**
```bash
fortran main.f90
```

**Python-like workflow:**
```bash
python script.py    # Python
fortran script.f90  # Fortran - same simplicity
```

## Common Usage

```bash
# Basic execution
fortran myprogram.f90

# Verbose output (see what's happening)
fortran -v myprogram.f90

# Very verbose (debug mode)  
fortran -vv myprogram.f90

# Custom cache location
fortran --cache-dir /tmp/cache myprogram.f90

# Help
fortran --help
```

## Next Steps

- See [[Manual|Manual]] for complete CLI reference
- Browse [[Examples|Examples]] for more usage patterns
- Check out the [example/](example/) directory for working code

The goal: Make Fortran development as frictionless as Python.
