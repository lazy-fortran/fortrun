# Fortran 2003 Standard Summary

This document summarizes the new features and changes in Fortran 2003 (ISO/IEC 1539-1:2004) compared to Fortran 95. Fortran 2003 was a major revision that introduced object-oriented programming and many other modern features.

## Table of Contents

1. [Object-Oriented Programming](#object-oriented-programming)
2. [C Interoperability](#c-interoperability)
3. [IEEE Arithmetic Support](#ieee-arithmetic-support)
4. [Data Manipulation Enhancements](#data-manipulation-enhancements)
5. [I/O Enhancements](#io-enhancements)
6. [Procedure Enhancements](#procedure-enhancements)
7. [Other New Features](#other-new-features)

## Object-Oriented Programming

### Type Extension and Inheritance

Fortran 2003 introduces type extension using the `EXTENDS` clause:

```fortran
type :: shape
    integer :: color
    logical :: filled
    integer :: x, y
end type shape

type, extends(shape) :: rectangle
    integer :: width, height
end type rectangle

type, extends(rectangle) :: square
end type square
```

### Type-Bound Procedures

Procedures can be bound to derived types for encapsulation:

```fortran
type :: shape
    integer :: color
    logical :: filled
    integer :: x, y
contains
    procedure :: initialize => shape_initialize
    procedure :: draw => shape_draw
    procedure :: move => shape_move
end type shape
```

#### Procedure Binding Attributes
- `PASS`: Specifies which dummy argument receives the passed object (default: first)
- `NOPASS`: Procedure doesn't receive the passed object
- `DEFERRED`: Deferred binding (requires ABSTRACT interface)
- `NON_OVERRIDABLE`: Cannot be overridden in extensions
- `PRIVATE`/`PUBLIC`: Access control

### Polymorphism with CLASS

The `CLASS` keyword enables polymorphism:

```fortran
class(shape), allocatable :: my_shape
class(shape), pointer :: shape_ptr
class(shape), allocatable :: shapes(:)  ! Polymorphic array

! Allocation with dynamic type
allocate(rectangle :: my_shape)
```

### SELECT TYPE Construct

Runtime type identification and safe downcasting:

```fortran
select type (object => my_shape)
type is (shape)
    ! object is exactly type shape
    print *, "Basic shape"
type is (rectangle)
    ! object is exactly type rectangle
    print *, "Rectangle:", object%width, object%height
class is (rectangle)
    ! object is rectangle or any extension of rectangle
    print *, "Rectangle or derived"
class default
    ! object is some other extension
    print *, "Unknown shape type"
end select
```

### Abstract Types and Deferred Bindings

```fortran
type, abstract :: shape
    integer :: x, y
contains
    procedure(shape_area), deferred :: area
end type shape

abstract interface
    real function shape_area(this)
        import :: shape
        class(shape), intent(in) :: this
    end function shape_area
end interface
```

### Type Parameters

Parameterized derived types (PDTs):

```fortran
type :: matrix(k, rows, cols)
    integer, kind :: k = kind(0.0)  ! Type parameter
    integer, len :: rows, cols       ! Length parameters
    real(k) :: data(rows, cols)
end type matrix

! Usage
type(matrix(kind=real64, :, :)), allocatable :: my_matrix
allocate(matrix(kind=real64, rows=100, cols=200) :: my_matrix)
```

### Finalization

Automatic cleanup with final procedures:

```fortran
type :: resource
    integer :: handle
contains
    final :: cleanup_resource
end type resource

subroutine cleanup_resource(this)
    type(resource), intent(inout) :: this
    ! Release resources
end subroutine cleanup_resource
```

## C Interoperability

### ISO_C_BINDING Module

Standard interface to C:

```fortran
use, intrinsic :: iso_c_binding

! C types
integer(c_int) :: c_integer
real(c_float) :: c_real
type(c_ptr) :: c_pointer
character(kind=c_char) :: c_string(100)

! C function interface
interface
    function c_function(x, n) bind(c, name="c_func")
        import :: c_int, c_double
        real(c_double) :: c_function
        real(c_double), value :: x
        integer(c_int), value :: n
    end function c_function
end interface
```

### BIND(C) Attribute

```fortran
! Interoperable procedure
subroutine fortran_sub(x, n) bind(c, name="fort_sub")
    real(c_double), intent(inout) :: x(*)
    integer(c_int), value :: n
end subroutine fortran_sub

! Interoperable derived type
type, bind(c) :: c_struct
    real(c_float) :: x, y, z
    integer(c_int) :: id
end type c_struct
```

### C Pointer Interoperability

```fortran
type(c_ptr) :: cptr
real(c_double), pointer :: fptr(:)
integer(c_int) :: n

! C to Fortran pointer
call c_f_pointer(cptr, fptr, [n])

! Fortran to C pointer
cptr = c_loc(fptr)
```

## IEEE Arithmetic Support

### IEEE Modules

```fortran
use, intrinsic :: ieee_exceptions
use, intrinsic :: ieee_arithmetic
use, intrinsic :: ieee_features

! Check for IEEE support
if (ieee_support_datatype(x)) then
    ! IEEE arithmetic is supported for x's type
end if

! Exception handling
call ieee_set_halting_mode(ieee_overflow, .false.)
call ieee_set_flag(ieee_overflow, .false.)

! Perform calculation
y = huge(y) * 2.0

! Check for overflow
if (ieee_get_flag(ieee_overflow)) then
    print *, "Overflow occurred"
end if
```

### Rounding Modes

```fortran
type(ieee_round_type) :: round_mode

! Save current rounding mode
call ieee_get_rounding_mode(round_mode)

! Set rounding mode
call ieee_set_rounding_mode(ieee_up)
! ... calculations ...

! Restore rounding mode
call ieee_set_rounding_mode(round_mode)
```

## Data Manipulation Enhancements

### Allocatable Enhancements

#### Allocatable Components
```fortran
type :: flexible_array
    real, allocatable :: data(:,:)
    character(:), allocatable :: name
end type flexible_array
```

#### Allocatable Dummy Arguments
```fortran
subroutine process_data(array)
    real, allocatable, intent(inout) :: array(:)

    ! Can reallocate
    if (size(array) < 100) then
        deallocate(array)
        allocate(array(100))
    end if
end subroutine process_data
```

#### Allocatable Function Results
```fortran
function create_array(n) result(array)
    integer, intent(in) :: n
    real, allocatable :: array(:)

    allocate(array(n))
    array = 0.0
end function create_array
```

### ASSOCIATE Construct

Create aliases for expressions:

```fortran
associate (x => real_part(complex_array), &
          y => aimag(complex_array), &
          r => sqrt(x**2 + y**2))
    ! Use x, y, r as variables
    where (r > 1.0)
        x = x / r
        y = y / r
    end where
end associate
```

### Enhanced Array Features

#### MOVE_ALLOC Intrinsic
```fortran
real, allocatable :: from(:), to(:)
allocate(from(1000))
! ... use from ...
call move_alloc(from, to)  ! Efficient transfer
! from is now deallocated, to has the data
```

#### Reallocation on Assignment
```fortran
real, allocatable :: array(:)
array = [1.0, 2.0, 3.0]  ! Automatic allocation
array = [real :: ]       ! Reallocate to size 0
```

## I/O Enhancements

### Stream I/O

```fortran
open(unit=10, file="data.bin", access="stream", form="unformatted")
write(10) x, y, z  ! No record markers
read(10, pos=100) value  ! Position to byte 100
```

### Asynchronous I/O

```fortran
real :: large_array(1000000)
integer :: id

open(unit=10, file="data.dat", asynchronous="yes")
write(10, asynchronous="yes", id=id) large_array

! Do other work while I/O proceeds
call cpu_intensive_calculation()

! Wait for I/O to complete
wait(10, id=id)
```

### FLUSH Statement

```fortran
write(10, *) "Important data"
flush(10)  ! Ensure data is written to disk
```

### Enhanced FORMAT Features

```fortran
! Unlimited format item repetition
write(*, '(*(i0, :, ", "))') array  ! Outputs: 1, 2, 3, 4, 5

! G0 descriptor for minimal width
write(*, '(g0)') 123.45  ! Outputs: 123.45
```

## Procedure Enhancements

### Procedure Pointers

```fortran
! Procedure pointer declaration
procedure(real_func), pointer :: func_ptr => null()
real, external :: sin, cos

! Interface block
abstract interface
    real function real_func(x)
        real, intent(in) :: x
    end function real_func
end interface

! Assignment
func_ptr => sin
y = func_ptr(x)  ! Calls sin(x)

func_ptr => cos
y = func_ptr(x)  ! Calls cos(x)
```

### Procedure Pointer Components

```fortran
type :: callback_handler
    procedure(callback_interface), pointer, nopass :: callback => null()
end type callback_handler

type(callback_handler) :: handler
handler%callback => my_callback
call handler%callback(data)
```

### IMPORT Statement

Make host entities accessible in interface blocks:

```fortran
module my_module
    type :: my_type
        integer :: value
    end type my_type

    interface
        subroutine external_proc(obj)
            import :: my_type  ! Import from host
            type(my_type) :: obj
        end subroutine external_proc
    end interface
end module my_module
```

### ABSTRACT Interfaces

```fortran
abstract interface
    subroutine callback(x, status)
        real, intent(in) :: x
        integer, intent(out) :: status
    end subroutine callback
end interface

procedure(callback) :: user_callback
```

## Other New Features

### VOLATILE Attribute

For variables that may change outside program control:

```fortran
real, volatile :: hardware_register
integer, volatile :: shared_counter
```

### Enhanced Intrinsic Procedures

#### New Intrinsics
- `COMMAND_ARGUMENT_COUNT()`: Number of command-line arguments
- `GET_COMMAND_ARGUMENT(number, value, length, status)`: Retrieve arguments
- `GET_COMMAND(command, length, status)`: Get entire command line
- `GET_ENVIRONMENT_VARIABLE(name, value, length, status, trim_name)`: Environment access
- `IS_IOSTAT_END(i)`, `IS_IOSTAT_EOR(i)`: I/O status inquiry
- `NEW_LINE(c)`: Newline character
- `SELECTED_CHAR_KIND(name)`: Character kind selection

#### Enhanced Existing Intrinsics
- `MAXLOC`/`MINLOC`: Added BACK argument
- `NULL()`: Can specify MOLD for type

### SOURCE Allocation

Allocate with source expression:

```fortran
type(person) :: template
type(person), allocatable :: people(:)

! Allocate and initialize from source
allocate(people(100), source=template)

! For polymorphic allocation
class(shape), allocatable :: new_shape
allocate(new_shape, source=existing_shape)
```

### Character Enhancements

#### Allocatable Character Length
```fortran
character(:), allocatable :: string
string = "Hello, World!"  ! Automatic allocation
string = trim(string) // " Extended"  ! Reallocation
```

#### KIND Parameter for Characters
```fortran
integer, parameter :: ucs4 = selected_char_kind("ISO_10646")
character(len=100, kind=ucs4) :: unicode_string
```

### Enhanced Accessibility Control

```fortran
module my_module
    private  ! Default accessibility

    type :: my_type
        private
        integer :: hidden_component
        integer, public :: visible_component
    end type my_type

    ! Selective exposure
    public :: my_type, public_procedure

contains
    ! Procedures...
end module my_module
```

### Enumerations

```fortran
enum, bind(c)
    enumerator :: red = 1
    enumerator :: green = 2
    enumerator :: blue = 4
    enumerator :: yellow = 3
end enum

integer(kind(red)) :: color
color = green
```

### PROTECTED Attribute

Read-only access outside defining module:

```fortran
module constants
    real, protected :: pi = 3.14159265359
    real, protected :: e = 2.71828182846
end module constants
```

## Removed Features

The following features were deleted from Fortran 95:
- Real and double precision DO control variables
- Branching to an END IF from outside its IF block
- PAUSE statement
- ASSIGN statement and assigned GOTO
- Assigned FORMAT specifiers
- H edit descriptor

## Summary

Fortran 2003 transformed Fortran into a modern programming language with:
- Full object-oriented programming support
- Seamless C interoperability
- IEEE arithmetic control
- Dynamic memory management improvements
- Modern I/O capabilities
- Enhanced modularity and encapsulation

These features maintain Fortran's strengths in numerical computation while providing the tools needed for large-scale software engineering.
