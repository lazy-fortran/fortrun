# fortran 2507 Standard (*Lowercase Fortran*)

This document describes the "lowercase 2507" standard - our experimental *lowercase fortran* dialect that pushes beyond all alternative scientific computing languages. It explores how far we can evolve Fortran to surpass Python, Julia, MATLAB, and others in both performance and expressiveness. The name uses lowercase to distinguish it from standard Fortran, and "2507" represents our vision of what Fortran could be in the far future (25th century).

## Table of Contents

1. [Philosophy](#philosophy)
2. [Bare File Execution](#bare-file-execution)
3. [Automatic Type Inference](#automatic-type-inference)
4. [Modern Defaults](#modern-defaults)
5. [Function Safety](#function-safety)
6. [Zero Configuration](#zero-configuration)
7. [Notebook Mode](#notebook-mode)
8. [Comparison with Standard Fortran](#comparison-with-standard-fortran)

## Philosophy

*lowercase fortran* (lowercase 2507) follows these core principles:

1. **Beyond Alternatives**: Push beyond Python, Julia, MATLAB, and others in scientific computing
2. **Performance + Expressiveness**: Combine Fortran's speed with modern language features
3. **Safe by Default**: Prevent common errors through opinionated defaults
4. **Zero Boilerplate**: Write equations directly, minimal ceremony
5. **Gradual Adoption**: Mix with standard Fortran seamlessly
6. **Strict Superset**: **lowercase 2507 is a strict superset of standard Fortran** - any valid Fortran 95/2003/2008/2018/2023 program is also a valid lowercase 2507 program and passes through the compiler frontend unchanged

## Bare File Execution

### No Program Declaration Required

In lowercase 2507, you can write executable code directly:

```fortran
! hello.f - Complete valid program
print *, "Hello, World!"
```

This automatically becomes:
```fortran
program main
    implicit none
    print *, "Hello, World!"
end program main
```

### Direct Calculations

```fortran
! calculate.f
x = 5.0
y = 3.0
z = sqrt(x**2 + y**2)
print *, "Distance:", z
```

Transforms to:
```fortran
program main
    implicit none
    real(8) :: x, y, z

    x = 5.0
    y = 3.0
    z = sqrt(x**2 + y**2)
    print *, "Distance:", z
end program main
```

## Automatic Type Inference

### Basic Types

lazy 2507 infers types from first assignment:

```fortran
! Type inference examples
x = 5.0          ! real(8) :: x
i = 42           ! integer :: i
name = "Alice"   ! character(len=5) :: name
flag = .true.    ! logical :: flag
```

### Complex Expressions

```fortran
! Expression type inference
radius = 5.0
area = 3.14159 * radius**2    ! real(8) :: area

! Function call inference
angle = sin(1.57)              ! real(8) :: angle
length = len_trim("  hello  ") ! integer :: length
```

### Array Inference

```fortran
! Array declarations inferred from usage
data = [1.0, 2.0, 3.0, 4.0]   ! real(8) :: data(4)

! 2D array from reshape
matrix = reshape([1, 2, 3, 4, 5, 6], [2, 3])  ! integer :: matrix(2,3)

! Array operations
result = data * 2.0 + 1.0      ! real(8) :: result(4)
```

## Modern Defaults

### Real Means Double Precision

All real numbers default to 64-bit precision:

```fortran
! In lazy 2507
x = 1.0    ! real(8) :: x

! Equivalent standard Fortran
real(kind=8) :: x = 1.0
```

### Implicit None by Default

No need to write `implicit none`:

```fortran
! lazy 2507
x = 5.0
y = x + 1

! Attempting to use undefined variable gives error
! z = w + 1  ! ERROR: 'w' not defined
```

### Integer Division Protection

Integer division automatically promotes to real when needed:

```fortran
! Careful with integer division
average = (a + b + c) / 3  ! If a,b,c are integers, result is real(8)
```

## Function Safety

### Automatic intent(in)

Function and subroutine parameters default to `intent(in)`:

```fortran
! lazy 2507
subroutine process(x, y)
    real :: x, y  ! Automatically intent(in)
    ! x = 5.0    ! ERROR: Cannot modify intent(in)
end subroutine

! To allow modification, explicitly declare
subroutine modify(x, y)
    real :: x  ! Automatically intent(in)
    real, intent(out) :: y  ! Explicit intent(out)
    y = x * 2.0
end subroutine
```

### Automatic Contains Section

Functions and subroutines automatically go in contains:

```fortran
! main.f
x = 5.0
y = square(x)
print *, y

real function square(x)
    real :: x
    square = x * x
end function
```

Becomes:
```fortran
program main
    implicit none
    real(8) :: x, y

    x = 5.0
    y = square(x)
    print *, y

contains

    real function square(x)
        real, intent(in) :: x
        square = x * x
    end function

end program main
```

## Zero Configuration

### Automatic Module Resolution

Use modules without explicit paths:

```fortran
! main.f
use pyplot_module  ! Automatically finds and builds dependency
call plot(x, y)
```

The tool automatically:
1. Searches local directory
2. Checks module registry
3. Downloads and builds if needed
4. Links everything together

### Smart Caching

```fortran
! First run: builds everything
$ fortran simulation.f  # Takes 2 seconds

! Subsequent runs: uses cache
$ fortran simulation.f  # Takes 0.1 seconds
```

## Notebook Mode

### Interactive Execution

Run lazy 2507 files as notebooks:

```fortran
! analysis.f
! # Data Analysis Example

! ## Load data
data = [1.2, 3.4, 5.6, 7.8, 9.0]
print *, "Data points:", size(data)

! ## Calculate statistics  
mean = sum(data) / size(data)
print *, "Mean:", mean

! ## Visualize
use pyplot_module
call plot(data)
call savefig("data_plot.png")  ! Automatically captured
```

Run with:
```bash
fortran --notebook analysis.f
```

Produces markdown output with:
- Code cells with syntax highlighting
- Output captured after each cell
- Figures automatically embedded
- Markdown cells preserved

## Comparison with Standard Fortran

| Feature | Standard Fortran | lazy 2507 |
|---------|------------------|--------------|
| **File Extension** | `.f90` | `.f` |
| **Program Declaration** | Required | Optional |
| **Implicit None** | Must declare | Automatic |
| **Type Declarations** | Required | Inferred |
| **Real Precision** | Default 32-bit | Default 64-bit |
| **Parameter Intent** | Default inout | Default in |
| **Module Building** | Manual | Automatic |
| **Execution** | Compile then run | Direct run |

### Example Comparison

**Standard Fortran (.f90)**:
```fortran
program calculate_distance
    implicit none
    real :: x, y, distance

    x = 3.0
    y = 4.0
    distance = sqrt(x**2 + y**2)
    print *, "Distance:", distance

end program calculate_distance
```

**lazy 2507 (.f)**:
```fortran
x = 3.0
y = 4.0
distance = sqrt(x**2 + y**2)
print *, "Distance:", distance
```

Both produce the same result, but lazy 2507:
- Uses 64-bit precision automatically
- Requires no boilerplate
- Infers all types
- Runs directly with `fortran calculate.f`

## Implementation Status

### Fully Implemented Features ✅

The AST-based frontend with full type inference is **production-ready**:

- ✅ **Bare file execution** - No program/end program required
- ✅ **Hindley-Milner type inference** - Complete implementation with Algorithm W
- ✅ **Automatic type inference** - Variables, expressions, function returns
- ✅ **Automatic real(8)** - All reals default to double precision
- ✅ **Implicit none by default** - No need to declare
- ✅ **Automatic variable declarations** - Inferred from usage
- ✅ **Module dependency resolution** - Automatic discovery and building
- ✅ **Smart caching system** - 2-4x performance improvements
- ✅ **Notebook mode** - Interactive execution with figure capture
- ✅ **4-Phase compiler** - Lexer → Parser → Semantic → Codegen
- ✅ **JSON intermediate representations** - Debug and compose pipelines
- ✅ **Comprehensive test suite** - 30+ frontend tests

### Currently Supported Type Inference

- ✅ Basic types (integer, real, logical, character)
- ✅ Arithmetic expressions with type promotion
- ✅ Function calls and return types
- ✅ Array literals and operations
- ✅ Control flow (if/then/else, do loops)
- ✅ Type unification across branches
- ✅ Automatic contains section for functions/subroutines

## Summary

fortran 2507 (*lowercase fortran*) is not a new language - it's a complete compiler frontend that makes standard Fortran easier to write while maintaining 100% compatibility. The production-ready AST-based system with Hindley-Milner type inference allows you to:

- Write equations directly without boilerplate
- Let the compiler infer all types automatically
- Use modern defaults (real(8), implicit none)
- Run files directly without manual compilation
- Leverage automatic module resolution and caching

Write less, compute more, with the same performance as standard Fortran.
