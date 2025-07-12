title: Examples
---

# Examples

Practical examples demonstrating `fortran` tool features. All examples are in the [example/](../example/) directory and can be run directly.

## Basic Usage

### Hello World
**Location:** [example/hello/](../example/hello/)

```bash
# Create and run
echo 'print *, "Hello, World!"' > hello.f90
fortran hello.f90
```

Simple program execution - the most basic use case.

### Calculator with Local Modules
**Location:** [example/calculator/](../example/calculator/)

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
**Location:** [example/interdependent/](../example/interdependent/)

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
**Location:** [example/type_inference/](../example/type_inference/)

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
**Location:** [example/preprocessor/](../example/preprocessor/)

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
**Location:** [example/precision/](../example/precision/)

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
# Output shows double precision by default
```

Demonstrates the tool's modern precision defaults.

### Notebook-Style Execution
**Location:** [example/notebook/](../example/notebook/)

```fortran
! simple_math.f
x = 10
y = 20  
sum = x + y
print *, 'Sum:', sum

! arrays_loops.f
integer :: i
do i = 1, 5
    print *, i*i
end do
```

```bash
fortran simple_math.f
fortran arrays_loops.f
```

Quick script-like execution for exploratory programming.

### External Dependencies
**Location:** [example/plotting/](../example/plotting/)

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
**Location:** [example/advanced_inference/](../example/advanced_inference/)

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

- Explore the [example/](../example/) directory
- Try modifying examples to see how caching works
- Add your own examples following the same structure
- Check the [Manual](index.html) for detailed CLI options
