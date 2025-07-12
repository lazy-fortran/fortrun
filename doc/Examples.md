title: Examples
---

# Examples

Practical examples demonstrating `fortran` tool features. All examples are in the [example/](https://github.com/krystophny/fortran/tree/main/example) directory and can be run directly.

## Basic Usage

### Hello World
**Location:** [example/hello/](https://github.com/krystophny/fortran/tree/main/example/hello)

```bash
# Create and run
echo 'print *, "Hello, World!"' > hello.f90
fortran hello.f90
```

Simple program execution - the most basic use case.

### Calculator with Local Modules
**Location:** [example/calculator/](https://github.com/krystophny/fortran/tree/main/example/calculator)

```fortran
! math_module.f90
module math_module
    implicit none
contains
    function add(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a + b
    end function
end module

! calculator.f90  
program calculator
    use math_module
    print *, add(5.0, 3.0)
end program
```

```bash
fortran calculator.f90  # Automatically finds and builds math_module.f90
```

Demonstrates automatic local module detection and compilation.

## Advanced Features

### Interdependent Modules
**Location:** [example/interdependent/](https://github.com/krystophny/fortran/tree/main/example/interdependent)

Complex module relationships where modules depend on each other:

```fortran
! constants.f90
module constants
    real, parameter :: pi = 3.14159
end module

! geometry.f90
module geometry
    use constants
    ! ... uses pi from constants
end module

! main.f90
program main
    use geometry
    use input_output
    ! ...
end program
```

```bash
fortran main.f90  # Resolves all interdependencies automatically
```

Shows how the tool handles complex dependency graphs.

### Type Inference (.f files)
**Location:** [example/type_inference/](https://github.com/krystophny/fortran/tree/main/example/type_inference)

```fortran
! calculate.f (note: .f extension)
x = 5
y = 3.14
result = x * y
print *, result
```

```bash
fortran calculate.f  # Automatically infers types and wraps in program
```

Demonstrates the preprocessor that adds type declarations and program structure.

### Preprocessor Features  
**Location:** [example/preprocessor/](https://github.com/krystophny/fortran/tree/main/example/preprocessor)

```fortran
! math.f
function square(x)
    square = x * x
end

! calc.f
y = square(5.0)
print *, y
```

```bash
fortran calc.f  # Automatically wraps functions and adds program structure
```

Shows automatic program wrapping and function handling.

## Specialized Use Cases

### Modern Precision Defaults
**Location:** [example/precision/](https://github.com/krystophny/fortran/tree/main/example/precision)

```fortran
! precision_test.f90
program precision_test
    real :: x = 1.0/3.0
    print *, 'Default real precision:', precision(x)
    print *, 'Value:', x
end program
```

```bash
fortran precision_test.f90
# Output shows standard single precision for .f90 files
```

Demonstrates standard Fortran precision behavior for .f90 files.

### Notebook-Style Execution
**Location:** [example/notebook/](https://github.com/krystophny/fortran/tree/main/example/notebook)

**Simple mathematical computations:**
```fortran
! simple_math.f (simplified syntax)
x = 10
y = 20  
sum = x + y
print *, 'Sum:', sum
```

**Array operations and loops:**
```fortran
! arrays_loops_simple.f (simplified syntax)
do i = 1, 5
    numbers(i) = i**2
end do
print *, "Array of squares:", numbers
```

**Control flow examples:**
```fortran
! control_flow_simple.f (simplified syntax)
temperature = 25.5
if (temperature < 30.0) then
    print *, "It's mild"
end if
```

**Scientific computing with visualization:**
```fortran
! plotting_demo.f (notebook with multiple cells)
! %% [markdown]
! # Scientific Computing with Fortran

! %%
n_points = 100
do i = 1, n_points
    x_data(i) = (i-1) * 6.28318 / (n_points - 1)
    y_sin(i) = sin(x_data(i))
end do

! Create plots with fortplotlib - auto-converted to base64 PNG in notebook mode
use fortplotlib
call figure()
call plot(x_data, y_sin, 'b-', label='sin(x)')
call xlabel('x')
call ylabel('y')
call title('Sine Wave')
call show()  ! Auto-converted to base64 PNG image
```

```bash
fortran simple_math.f
fortran arrays_loops_simple.f  
fortran control_flow_simple.f
fortran plotting_demo.f
```

Demonstrates script-like execution for exploratory programming and notebook-style analysis.

### External Dependencies
**Location:** [example/plotting/](https://github.com/krystophny/fortran/tree/main/example/plotting)

```fortran
! plot_demo.f90
program plot_demo
    use pyplot_module  ! External dependency
    ! ... plotting code
end program
```

```bash
# First ensure registry is configured
echo '[packages.pyplot-fortran]' >> ~/.config/fortran/registry.toml
echo 'git = "https://github.com/jacobwilliams/pyplot-fortran"' >> ~/.config/fortran/registry.toml

fortran plot_demo.f90  # Downloads and builds pyplot-fortran automatically
```

Shows integration with external FPM packages.

### Advanced Type Inference
**Location:** [example/advanced_inference/](https://github.com/krystophny/fortran/tree/main/example/advanced_inference)

```fortran
! arrays.f
data = [1, 2, 3, 4, 5]
do i = 1, size(data)
    print *, data(i)**2
end do

! derived_types.f  
type :: point
    real :: x, y
end type
p = point(1.0, 2.0)
print *, p%x, p%y
```

```bash
fortran arrays.f       # Infers array types
fortran derived_types.f # Handles derived types
```

Advanced preprocessing with complex type inference.

## Running Examples

**Run all examples:**
```bash
cd example/
for dir in */; do
    echo "Running $dir..."
    cd "$dir"
    fortran *.f90 2>/dev/null || fortran *.f 2>/dev/null || echo "Skipped"
    cd ..
done
```

**Run specific example:**
```bash
cd example/calculator/
fortran calculator.f90
```

**With verbose output:**
```bash
fortran -v example/hello/hello.f90
```

## Example Output

**Hello World:**
```
$ fortran example/hello/hello.f90
Hello, World!
```

**Calculator:**
```
$ fortran example/calculator/calculator.f90  
8.0
```

**Type Inference:**
```
$ fortran example/type_inference/calculate.f
15.7079999
```

**Precision Test:**
```
$ fortran example/precision/precision_test.f90
Default real precision: 15
Value: 0.33333333333333331
```

## Testing Examples

All examples are automatically tested:
```bash
fpm test test_examples  # Runs all example validations
```

Each example includes:
- Source code
- README.md with explanation  
- Expected output validation
- Error handling tests

## Next Steps

- Explore the [example/](https://github.com/krystophny/fortran/tree/main/example) directory
- Try modifying examples to see how caching works
- Add your own examples following the same structure
- Check the [Manual](index.html) for detailed CLI options
