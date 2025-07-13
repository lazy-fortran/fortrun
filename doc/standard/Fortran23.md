# Fortran 2023 Standard Summary

This document summarizes the new features and changes in Fortran 2023 (ISO/IEC 1539-1:2023) compared to Fortran 2018. Fortran 2023 is a minor revision that adds features for generic programming and corrects errors in Fortran 2018.

## Table of Contents

1. [Conditional Expressions](#conditional-expressions)
2. [Conditional Arguments](#conditional-arguments)
3. [Type Inference Features](#type-inference-features)
4. [Enumeration Types](#enumeration-types)
5. [DO CONCURRENT Enhancements](#do-concurrent-enhancements)
6. [Source Form Enhancements](#source-form-enhancements)
7. [I/O Enhancements](#io-enhancements)
8. [C Interoperability](#c-interoperability)
9. [Other Language Enhancements](#other-language-enhancements)

## Conditional Expressions

Conditional expressions allow inline selection between values based on conditions.

### Basic Syntax

```fortran
! Basic conditional expression
real :: x, y, max_value
max_value = (x > y ? x : y)

! With different types (must match)
integer :: i, j, max_int
max_int = (i > j ? i : j)

! Character conditional
character(len=10) :: status
logical :: is_valid
status = (is_valid ? "VALID" : "INVALID")
```

### Nested Conditional Expressions

```fortran
! Multi-way selection
real :: x, sign_value
sign_value = (x > 0.0 ? 1.0 : x < 0.0 ? -1.0 : 0.0)

! Equivalent to nested if-then-else
character(len=10) :: grade
integer :: score
grade = (score >= 90 ? "A" : &
         score >= 80 ? "B" : &
         score >= 70 ? "C" : &
         score >= 60 ? "D" : "F")
```

### Important Properties

```fortran
! Only selected expression is evaluated
real :: x, y
! If x <= 0, log(x) is never evaluated
y = (x > 0.0 ? log(x) : 0.0)

! Array conditional expressions
real :: a(100), b(100), c(100)
! Element-wise conditional
c = (a > b ? a : b)  ! Element-wise maximum

! Must have matching type, kind, and rank
real(real64) :: d1, d2, d3
real(real32) :: s1
! d3 = (condition ? d1 : s1)  ! ERROR: kind mismatch
d3 = (condition ? d1 : d2)    ! OK
```

## Conditional Arguments

Select actual arguments conditionally, including optional arguments.

### Basic Conditional Arguments

```fortran
! Select between actual arguments
subroutine process(data, use_fast)
    real, intent(inout) :: data(:)
    logical, intent(in) :: use_fast
    
    ! Call with conditional argument
    call algorithm(data, (use_fast ? fast_method : slow_method))
end subroutine

! Conditional array arguments
real :: array1(100), array2(100), work_array(100)
logical :: use_first
call process_array((use_first ? array1 : array2), work_array)
```

### Optional Arguments with .nil.

```fortran
! .nil. indicates absent optional argument
subroutine compute(x, y, optional_param)
    real, intent(in) :: x, y
    real, intent(in), optional :: optional_param
    ! Implementation
end subroutine

! Conditional optional argument
logical :: include_correction
real :: correction_factor
call compute(1.0, 2.0, (include_correction ? correction_factor : .nil.))

! Multiple optional arguments
subroutine advanced_calc(a, b, opt1, opt2, opt3)
    real, intent(in) :: a, b
    real, intent(in), optional :: opt1, opt2, opt3
end subroutine

logical :: use_defaults
real :: custom1, custom2, custom3
call advanced_calc(x, y, &
    (use_defaults ? .nil. : custom1), &
    (use_defaults ? .nil. : custom2), &
    (use_defaults ? .nil. : custom3))
```

## Type Inference Features

### TYPEOF Type Specifier

```fortran
! Declare variables with same type as existing entity
real(real64) :: template
typeof(template) :: x, y, z  ! All are real(real64)

! With derived types
type :: my_type
    integer :: component
end type
type(my_type) :: original
typeof(original) :: copy  ! Same type as original

! Arrays preserve type but not shape
real :: matrix(10,10)
typeof(matrix) :: vector(100)  ! real, but different shape

! With type parameters
character(len=20) :: name
typeof(name) :: other_name  ! character(len=20)
```

### CLASSOF Type Specifier

```fortran
! For polymorphic entities
type :: shape
    real :: area
end type shape

type, extends(shape) :: circle
    real :: radius
end type circle

class(shape), allocatable :: s1
allocate(circle :: s1)

! Declare with same dynamic type
classof(s1), allocatable :: s2  ! Can be shape or any extension

! Function results
function clone_shape(original) result(copy)
    class(shape), intent(in) :: original
    classof(original), allocatable :: copy
    
    ! Allocate with same dynamic type
    allocate(copy, source=original)
end function
```

### Implications for Generic Programming

```fortran
! Generic swap using typeof
subroutine swap(a, b)
    class(*), intent(inout) :: a, b
    typeof(a) :: temp
    
    temp = a
    a = b
    b = temp
end subroutine

! Template-like function
function make_array(template, n) result(array)
    class(*), intent(in) :: template
    integer, intent(in) :: n
    typeof(template), allocatable :: array(:)
    
    allocate(array(n))
end function
```

## Enumeration Types

True enumeration types with C interoperability.

### Basic Enumerations

```fortran
! Define enumeration type
enum, bind(c)
    enumerator :: red = 1
    enumerator :: green = 2
    enumerator :: blue = 4
    enumerator :: yellow  ! = 5 (auto-increment)
end enum

! Use enumeration
integer(kind(red)) :: color
color = green

! With named enum type
enum, bind(c) :: color_enum
    enumerator :: color_red = 1
    enumerator :: color_green
    enumerator :: color_blue
end enum

integer(color_enum) :: my_color
my_color = color_blue
```

### Enumeration in Modules

```fortran
module traffic_light_module
    implicit none
    
    enum, bind(c), public :: traffic_state
        enumerator :: stop_light = 0
        enumerator :: caution_light = 1
        enumerator :: go_light = 2
    end enum
    
contains
    
    function next_state(current) result(next)
        integer(traffic_state), intent(in) :: current
        integer(traffic_state) :: next
        
        select case (current)
        case (stop_light)
            next = go_light
        case (go_light)
            next = caution_light
        case (caution_light)
            next = stop_light
        end select
    end function
    
end module
```

### C Interoperable Enumerations

```fortran
! Matches C enum
enum, bind(c) :: c_errors
    enumerator :: success = 0
    enumerator :: invalid_input = -1
    enumerator :: out_of_memory = -2
    enumerator :: io_error = -3
end enum

! Interface to C function
interface
    function c_function(flag) bind(c, name="process")
        import :: c_errors
        integer(c_errors) :: c_function
        integer(c_errors), value :: flag
    end function
end interface
```

## DO CONCURRENT Enhancements

### REDUCE Locality Specifier

```fortran
! Reduction in DO CONCURRENT
real :: sum, product
real :: array(1000)

sum = 0.0
product = 1.0

do concurrent (i = 1:size(array)) &
    reduce(+:sum) &
    reduce(*:product)
    
    sum = sum + array(i)
    product = product * array(i)
end do

! Multiple reductions
integer :: count
real :: total, max_val

count = 0
total = 0.0
max_val = -huge(max_val)

do concurrent (i = 1:n, array(i) > 0.0) &
    reduce(+:count, total) &
    reduce(max:max_val)
    
    count = count + 1
    total = total + array(i)
    max_val = max(max_val, array(i))
end do
```

### User-Defined Reductions

```fortran
! Custom reduction operation
interface operator(.sum_squares.)
    module procedure sum_squares_op
end interface

do concurrent (i = 1:n) &
    reduce(.sum_squares.:result)
    
    result = result .sum_squares. array(i)
end do

! Where sum_squares_op is defined as:
pure function sum_squares_op(a, b) result(c)
    real, intent(in) :: a, b
    real :: c
    c = a + b**2
end function
```

## Source Form Enhancements

### Extended Line Length

```fortran
! Lines can now be up to 10,000 characters
character(len=10000) :: very_long_string = "This is a very long string that can now be written on a single line..."

! No limit on continuation lines (total statement limit: 1 million characters)
real :: result = very_long_expression_1 + very_long_expression_2 + &
                very_long_expression_3 + very_long_expression_4 + &
                very_long_expression_5 + very_long_expression_6 + &
                ! ... can continue as needed up to 1 million total characters
```

### Include Line Enhancements

```fortran
! INCLUDE can now use preprocessor syntax
include "config.inc"
#include "macros.inc"  ! Now standard

! Conditional includes with preprocessor
#ifdef DEBUG
#include "debug_routines.inc"
#endif
```

## I/O Enhancements

### AT Format Descriptor

Automatically trim trailing whitespace:

```fortran
character(len=20) :: name = "John Doe"
character(len=30) :: output

! Traditional - includes trailing spaces
write(output, '(a)') name  ! "John Doe            "

! With AT descriptor - trims trailing spaces
write(output, '(at)') name  ! "John Doe"

! In list-directed I/O
write(*, '(at, ", ", at)') "First   ", "Last    "
! Output: "First, Last"

! With width specification
write(*, '(at20)') "Hello"  ! Right-justified in field of 20
```

### Namelist Enhancements

```fortran
! Namelist comments
namelist /config/ x, y, z

! Can now include comments in namelist input
! &config
!   x = 1.0  ! X coordinate
!   y = 2.0  ! Y coordinate  
!   z = 3.0  ! Z coordinate
! /
```

## C Interoperability

### C_F_POINTER Lower Bounds

```fortran
use, intrinsic :: iso_c_binding
type(c_ptr) :: cptr
real, pointer :: fptr(:,:)
integer :: lower_bounds(2)

! Get C pointer to 2D array
cptr = get_c_array()

! Create Fortran pointer with custom lower bounds
lower_bounds = [0, 0]  ! C-style 0-based indexing
call c_f_pointer(cptr, fptr, shape=[100, 200], lower_bounds=lower_bounds)

! Now fptr(0:99, 0:199) maps to C array[100][200]
```

### Enhanced C Descriptors

```fortran
! Better support for assumed-rank interoperability
interface
    subroutine c_process(array) bind(c)
        use, intrinsic :: iso_c_binding
        type(*), dimension(..), intent(inout) :: array
    end subroutine
end interface

! Can pass any type and rank
real :: scalar, vector(10), matrix(5,5)
call c_process(scalar)
call c_process(vector)
call c_process(matrix)
```

## Other Language Enhancements

### Half-Precision Real

```fortran
! If supported by processor
use, intrinsic :: iso_fortran_env
integer, parameter :: real16 = selected_real_kind(3)  ! ~3 decimal digits

real(real16) :: half_precision_var
real(real16) :: tiny_array(1000)  ! Save memory

! Conversion
real(real32) :: single
real(real16) :: half
half = real(single, real16)
```

### Public/Private Enhancements

```fortran
module enhanced_module
    implicit none
    private  ! Default
    
    ! Can now have multiple public statements
    public :: type1, proc1
    public :: type2, proc2
    
    ! Protected components in types
    type :: protected_type
        integer :: public_read
        private
        integer :: hidden
    end type
end module
```

### Integer Array Specifiers

```fortran
! New syntax for array operations using integer arrays
integer :: indices(3) = [2, 5, 8]
real :: source(10), selected(3)

! Select elements using integer array
selected = source(indices)

! Bounds specification
integer :: lower(2) = [0, 1]
integer :: upper(2) = [10, 20]
real, allocatable :: array(:,:)

allocate(array(lower(1):upper(1), lower(2):upper(2)))
```

### Simple Procedures

```fortran
! Simplified procedure definitions for common patterns
elemental real function square(x)
    real, intent(in) :: x
    square = x * x
end function

! Implicit interface for module procedures
module procs
contains
    ! No interface block needed
    simple subroutine say_hello(name)
        character(*), intent(in) :: name
        print *, "Hello, ", name
    end subroutine
end module
```

## Summary

Fortran 2023 adds features for modern programming practices:

1. **Conditional expressions** - Inline value selection with (condition ? true_value : false_value)
2. **Conditional arguments** - Dynamic argument selection including .nil. for optional arguments
3. **TYPEOF/CLASSOF** - Type inference for generic programming
4. **Enumerations** - True enum types with C compatibility
5. **DO CONCURRENT REDUCE** - Built-in reductions for parallel loops
6. **Extended source form** - 10,000 character lines, unlimited continuations
7. **AT format** - Automatic whitespace trimming
8. **Enhanced C interop** - Better pointer and descriptor handling

These features improve expressiveness and generic programming capabilities while maintaining Fortran's computational efficiency.