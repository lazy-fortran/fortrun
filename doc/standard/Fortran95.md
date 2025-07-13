# Fortran 95 Standard Summary

This document summarizes the key features and syntax of the Fortran 95 standard (ISO/IEC 1539-1:1997) for reference during parser implementation.

## Table of Contents

1. [Overview](#overview)
2. [Lexical Elements](#lexical-elements)
3. [Data Types](#data-types)
4. [Declarations](#declarations)
5. [Expressions and Assignment](#expressions-and-assignment)
6. [Control Constructs](#control-constructs)
7. [Program Units](#program-units)
8. [Procedures](#procedures)
9. [Arrays](#arrays)
10. [Modules](#modules)
11. [Key Differences from Fortran 90](#key-differences-from-fortran-90)

## Overview

Fortran 95 is a minor revision of Fortran 90, adding features for parallel computing and resolving outstanding issues. The standard maintains backward compatibility while introducing modern programming constructs.

## Lexical Elements

### Character Set
- Letters: A-Z, a-z (case insensitive)
- Digits: 0-9
- Special characters: + - * / ( ) [ ] { } , . : ; ! " % & ' = < > $ _ space

### Source Form
- **Fixed form**: Columns 1-6 for labels/continuation, 7-72 for statements
- **Free form**: Lines up to 132 characters, & for continuation
- Comments: ! to end of line (free form), C or * in column 1 (fixed form)

### Tokens
- **Keywords**: `program`, `end`, `function`, `subroutine`, `if`, `then`, `else`, `do`, `while`, etc.
- **Identifiers**: Start with letter, up to 31 characters, alphanumeric + underscore
- **Literals**: Integer, real, complex, logical, character constants
- **Operators**: Arithmetic, relational, logical

## Data Types

### Intrinsic Types
1. **INTEGER**: Whole numbers
   - Default kind typically 32-bit
   - Literal: `42`, `123_int64`

2. **REAL**: Floating-point numbers
   - Default kind typically 32-bit
   - Literal: `3.14`, `1.0e-10`, `2.5_real64`

3. **COMPLEX**: Complex numbers
   - Pair of real numbers
   - Literal: `(1.0, 2.0)`

4. **LOGICAL**: Boolean values
   - Values: `.true.`, `.false.`

5. **CHARACTER**: Character strings
   - Fixed or assumed length
   - Literal: `"string"`, `'string'`

### Type Parameters
- **KIND**: Selects precision/range for numeric types
- **LEN**: Length for character types

### Derived Types
```fortran
type :: person
    character(len=30) :: name
    integer :: age
    real :: height
end type person
```

## Declarations

### Variable Declarations
```fortran
integer :: i, j, k
real(kind=8) :: x, y
character(len=80) :: message
logical :: flag = .true.
type(person) :: student
```

### Implicit Typing
- Default: I-N are INTEGER, others are REAL
- Override: `implicit none` (recommended)
- Custom: `implicit real(a-h, o-z)`

### Attributes
- `parameter`: Named constant
- `dimension`: Array specification
- `allocatable`: Dynamic allocation
- `pointer`: Pointer variable
- `target`: Can be pointed to
- `intent(in/out/inout)`: Dummy argument intent
- `optional`: Optional dummy argument
- `save`: Retains value between calls

## Expressions and Assignment

### Operators (precedence high to low)
1. `**` (exponentiation)
2. `*`, `/` (multiplication, division)
3. `+`, `-` (addition, subtraction, unary)
4. `//` (string concatenation)
5. `.eq.`, `.ne.`, `.lt.`, `.le.`, `.gt.`, `.ge.` or `==`, `/=`, `<`, `<=`, `>`, `>=`
6. `.not.`
7. `.and.`
8. `.or.`
9. `.eqv.`, `.neqv.` (logical equivalence)

### Assignment
```fortran
variable = expression
array(:) = 0.0  ! Array assignment
where (array > 0.0) array = sqrt(array)  ! Masked assignment
```

## Control Constructs

### IF Construct
```fortran
if (condition) then
    ! statements
else if (another_condition) then
    ! statements
else
    ! statements
end if
```

### CASE Construct
```fortran
select case (expression)
case (value1)
    ! statements
case (value2:value3)
    ! statements
case default
    ! statements
end select
```

### DO Loops
```fortran
! Counting loop
do i = 1, 10, 2
    ! statements
end do

! While loop
do while (condition)
    ! statements
end do

! Infinite loop with exit
do
    ! statements
    if (condition) exit
end do
```

### WHERE Construct (Fortran 95)
```fortran
where (array > 0.0)
    array = sqrt(array)
elsewhere
    array = 0.0
end where
```

### FORALL Construct (Fortran 95)
```fortran
forall (i = 1:n, j = 1:n, i /= j)
    a(i,j) = 1.0 / (i - j)
end forall
```

## Program Units

### Main Program
```fortran
program name
    implicit none
    ! declarations
    ! executable statements
contains
    ! internal procedures
end program name
```

### External Procedures
```fortran
function func_name(args) result(return_var)
    ! declarations
    ! statements
end function func_name

subroutine sub_name(args)
    ! declarations
    ! statements
end subroutine sub_name
```

### Block Data
```fortran
block data name
    ! common block initializations
end block data name
```

## Procedures

### Function Definition
```fortran
real function square(x)
    real, intent(in) :: x
    square = x * x
end function square
```

### Subroutine Definition
```fortran
subroutine swap(a, b)
    real, intent(inout) :: a, b
    real :: temp
    temp = a
    a = b
    b = temp
end subroutine swap
```

### Pure and Elemental (Fortran 95)
```fortran
pure function add(x, y) result(sum)
    real, intent(in) :: x, y
    real :: sum
    sum = x + y
end function add

elemental function square(x) result(y)
    real, intent(in) :: x
    real :: y
    y = x * x
end function square
```

### Interface Blocks
```fortran
interface
    function external_func(x)
        real :: external_func
        real, intent(in) :: x
    end function external_func
end interface
```

## Arrays

### Declaration
```fortran
real :: vector(10)                    ! 1D array, bounds 1:10
real :: matrix(3,3)                   ! 2D array
real :: cube(-5:5, 0:10, 2:8)       ! 3D with explicit bounds
real, dimension(:), allocatable :: dynamic_array
real, dimension(:,:), pointer :: array_ptr
```

### Array Operations
```fortran
! Whole array operations
a = b + c                  ! Element-wise addition
d = matmul(a, b)          ! Matrix multiplication
e = dot_product(x, y)     ! Dot product

! Array sections
a(1:5) = b(6:10)          ! Copy section
a(:,3) = 0.0              ! Set column to zero
a(1:10:2) = 1.0           ! Set odd elements

! Array intrinsics
size(array [, dim])       ! Array size
shape(array)              ! Array shape
lbound(array [, dim])     ! Lower bounds
ubound(array [, dim])     ! Upper bounds
```

### Dynamic Arrays
```fortran
real, allocatable :: array(:,:)
allocate(array(100, 200))
! ... use array
deallocate(array)  ! Automatic in Fortran 95 when out of scope
```

## Modules

### Module Definition
```fortran
module module_name
    implicit none
    private  ! Default accessibility
    
    ! Public interface
    public :: public_proc, public_type
    
    ! Module variables
    real :: module_var
    
    ! Derived types
    type :: my_type
        ! components
    end type my_type
    
contains
    
    ! Module procedures
    subroutine module_proc()
        ! ...
    end subroutine module_proc
    
end module module_name
```

### Module Usage
```fortran
program main
    use module_name
    use other_module, only: specific_item
    implicit none
    ! ...
end program main
```

## Key Differences from Fortran 90

### New Features in Fortran 95
1. **FORALL construct**: Parallel array assignments
2. **Nested WHERE**: Enhanced masked assignments
3. **PURE and ELEMENTAL procedures**: Side-effect free functions
4. **Automatic deallocation**: ALLOCATABLE arrays deallocated on scope exit
5. **Enhanced intrinsics**: Additional arguments (e.g., DIM in MAXLOC)
6. **CPU_TIME intrinsic**: Processor time measurement
7. **NULL() intrinsic**: Initialize pointers

### Clarifications and Fixes
1. More precise specifications for numeric model
2. Better defined initialization expressions
3. Clarified pointer association status
4. Enhanced interoperability specifications

## Intrinsic Procedures

### Commonly Used Intrinsics
- **Numeric**: `abs`, `sqrt`, `exp`, `log`, `sin`, `cos`, `tan`
- **Type conversion**: `int`, `real`, `cmplx`, `char`, `ichar`
- **Array**: `sum`, `product`, `maxval`, `minval`, `maxloc`, `minloc`
- **Array manipulation**: `reshape`, `transpose`, `spread`, `pack`, `unpack`
- **Array inquiry**: `size`, `shape`, `lbound`, `ubound`, `allocated`
- **String**: `len`, `len_trim`, `trim`, `adjustl`, `adjustr`, `index`
- **Bit manipulation**: `iand`, `ior`, `ieor`, `not`, `ishft`, `ibits`

## Statement Order

Required order in program units:
1. PROGRAM, FUNCTION, SUBROUTINE, MODULE, or BLOCK DATA statement
2. USE statements
3. IMPLICIT statements
4. PARAMETER statements and derived type definitions
5. Type declaration statements
6. Statement function statements
7. Executable constructs
8. CONTAINS statement
9. Internal procedures
10. END statement

## Deprecated Features

Features obsolescent in Fortran 95:
- Arithmetic IF
- Shared DO termination
- Alternate RETURN
- Computed GOTO
- Statement functions (use internal procedures)
- DATA statements among executables
- Assumed character length functions
- Fixed source form (informally)

## References

- ISO/IEC 1539-1:1997 - Fortran 95 Standard
- ISO/IEC 1539-2:2000 - Varying Length Character Strings
- High Performance Fortran (HPF) extensions