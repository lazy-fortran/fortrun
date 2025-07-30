# Fortran 2008 Standard Summary

This document summarizes the new features and changes in Fortran 2008 (ISO/IEC 1539-1:2010) compared to Fortran 2003. Fortran 2008 is a minor revision that adds parallel programming support through coarrays and other enhancements.

## Table of Contents

1. [Coarrays and Parallel Programming](#coarrays-and-parallel-programming)
2. [Submodules](#submodules)
3. [DO CONCURRENT](#do-concurrent)
4. [BLOCK Construct](#block-construct)
5. [CONTIGUOUS Attribute](#contiguous-attribute)
6. [Data Declaration Enhancements](#data-declaration-enhancements)
7. [Control Construct Enhancements](#control-construct-enhancements)
8. [Intrinsic Procedures](#intrinsic-procedures)
9. [I/O Enhancements](#io-enhancements)
10. [Module Enhancements](#module-enhancements)
11. [Performance Enhancements](#performance-enhancements)

## Coarrays and Parallel Programming

Coarrays provide a simple parallel programming model using Single Program Multiple Data (SPMD) approach.

### Basic Coarray Syntax

```fortran
! Scalar coarray
real :: temperature[*]  ! One copy per image

! Array coarray
real :: data(100)[*]    ! 100-element array on each image

! Multi-dimensional coarrays
real :: matrix(50,50)[2,*]  ! 2D array with 2D coarray dimensions

! Coarray with allocatable component
type :: container
    real, allocatable :: values(:)
end type container
type(container) :: work[*]

! Derived type coarray
type(particle) :: particles(1000)[*]
```

### Coarray References

```fortran
! Local reference (current image)
temperature = 25.0

! Remote reference (image 3)
temperature[3] = 30.0

! Array section from image 2
local_data(:) = data(:)[2]

! Using image index variables
integer :: img
img = 4
value = temperature[img]
```

### Image Control Statements

#### SYNC ALL
Synchronize all images:
```fortran
real :: data[*]
data = this_image() * 10.0
sync all  ! Wait for all images
! Now all images have updated their data

! With error handling
integer :: sync_stat
sync all(stat=sync_stat)
if (sync_stat /= 0) error stop "Sync failed"
```

#### SYNC IMAGES
Synchronize specific images:
```fortran
! Synchronize with neighboring images
integer :: neighbors(2)
neighbors = [this_image()-1, this_image()+1]
if (neighbors(1) >= 1 .and. neighbors(2) <= num_images()) then
    sync images(neighbors)
end if

! Synchronize with single image
sync images(1)  ! All images sync with image 1

! Synchronize a list
sync images([2, 4, 6, 8])
```

#### SYNC MEMORY
Memory synchronization fence:
```fortran
! Ensure memory consistency
shared_data[2] = value
sync memory  ! Ensure write is visible
! Other images can now safely read shared_data[2]
```

### Critical Sections and Locks

```fortran
use, intrinsic :: iso_fortran_env
type(lock_type) :: resource_lock[*]

! Critical section
critical
    ! Only one image executes this at a time
    shared_counter[1] = shared_counter[1] + 1
end critical

! Named critical section
critical (update_database)
    ! Database update code
end critical

! Lock/unlock for finer control
lock(resource_lock[1])
    ! Access shared resource
unlock(resource_lock[1])
```

### Image Inquiry Functions

```fortran
integer :: me, total, coords(3)

me = this_image()        ! Current image index
total = num_images()     ! Total number of images

! For multi-dimensional coarray organization
integer :: codims[2,4,*]
coords = this_image(codims)  ! Get coordinates in coarray grid
```

### ERROR STOP Statement

Enhanced termination for parallel programs:
```fortran
if (error_condition) then
    error stop "Fatal error in parallel computation"
    ! All images terminate
end if

! With error code
error stop 42  ! Stop all images with exit code 42
```

## Submodules

Submodules provide better encapsulation and separate compilation:

### Module with Module Procedures

```fortran
module points
    implicit none

    type :: point
        real :: x, y
    end type point

    interface
        ! Module procedure interface
        module subroutine point_distance(p1, p2, dist)
            type(point), intent(in) :: p1, p2
            real, intent(out) :: dist
        end subroutine point_distance

        module function point_add(p1, p2) result(p3)
            type(point), intent(in) :: p1, p2
            type(point) :: p3
        end function point_add
    end interface

end module points
```

### Submodule Implementation

```fortran
submodule (points) points_impl
    implicit none

contains

    ! Implementation of module procedures
    module subroutine point_distance(p1, p2, dist)
        type(point), intent(in) :: p1, p2
        real, intent(out) :: dist

        dist = sqrt((p2%x - p1%x)**2 + (p2%y - p1%y)**2)
    end subroutine point_distance

    module function point_add(p1, p2) result(p3)
        type(point), intent(in) :: p1, p2
        type(point) :: p3

        p3%x = p1%x + p2%x
        p3%y = p1%y + p2%y
    end function point_add

end submodule points_impl
```

### Nested Submodules

```fortran
! Parent submodule
submodule (points) points_base
    implicit none
contains
    ! Base implementations
end submodule points_base

! Child submodule
submodule (points:points_base) points_extended
    implicit none
contains
    ! Extended implementations
end submodule points_extended
```

## DO CONCURRENT

Parallel loop execution without explicit synchronization:

```fortran
! Basic DO CONCURRENT
do concurrent (i = 1:n)
    a(i) = b(i) + c(i)  ! No dependencies between iterations
end do

! With local variables
do concurrent (i = 1:n) local(temp)
    temp = b(i) * c(i)
    a(i) = sqrt(temp)
end do

! Multiple indices
do concurrent (i = 1:n, j = 1:m)
    matrix(i,j) = i * j
end do

! With mask
do concurrent (i = 1:n, b(i) > 0.0)
    a(i) = log(b(i))
end do

! Locality specifiers
real :: sum
do concurrent (i = 1:n) local(temp) shared(sum) default(none)
    temp = a(i) * b(i)
    sum = sum + temp  ! Note: potential race condition
end do
```

## BLOCK Construct

Local scope within procedures:

```fortran
subroutine process_data(n)
    integer, intent(in) :: n
    real :: global_array(n)

    ! Create local scope
    block
        real :: temp(n), average
        integer :: i

        ! temp and average only exist in this block
        temp = global_array * 2.0
        average = sum(temp) / n

        block
            ! Nested block
            real :: std_dev
            std_dev = sqrt(sum((temp - average)**2) / n)
            print *, "Standard deviation:", std_dev
        end block
        ! std_dev no longer exists here

    end block
    ! temp and average no longer exist here

end subroutine process_data

! BLOCK with name
outer: block
    integer :: i
    do i = 1, 10
        inner: block
            real :: x
            x = real(i)
            if (x > 5.0) exit outer
        end block inner
    end do
end block outer
```

## CONTIGUOUS Attribute

Guarantees array elements are contiguous in memory:

```fortran
! Contiguous dummy arguments
subroutine fast_process(array)
    real, contiguous, intent(inout) :: array(:,:)
    ! Compiler can optimize assuming contiguity
end subroutine fast_process

! Contiguous pointers
real, pointer, contiguous :: ptr(:,:)
real, target :: data(100,100)
ptr => data  ! Valid: data is contiguous

! Contiguous pointer assignment
real, pointer, contiguous :: p1(:), p2(:)
real, target :: array(100)
p1 => array(1:50:1)   ! Valid: stride 1
p2 => array(1:50:2)   ! Invalid: stride 2, not contiguous

! Function result
function get_diagonal(matrix) result(diag)
    real, intent(in) :: matrix(:,:)
    real, pointer, contiguous :: diag(:)
    ! Implementation must ensure contiguous result
end function get_diagonal

! Type components
type :: fast_array
    real, allocatable, contiguous :: data(:,:)
end type fast_array
```

## Data Declaration Enhancements

### Maximum Rank Increased

Arrays can now have up to 15 dimensions (was 7):
```fortran
real :: hypercube(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)  ! 15D array
```

### Implied-shape Arrays

```fortran
! Implied-shape named constant
integer, parameter :: primes(*) = [2, 3, 5, 7, 11, 13, 17, 19]
real, parameter :: matrix(2,*) = reshape([1.0, 2.0, 3.0, 4.0], [2,2])
```

### Type-bound Procedure Enhancements

```fortran
type :: mytype
contains
    ! Generic type-bound procedure with multiple specifics
    generic :: write(formatted) => write_formatted, write_unformatted
    procedure :: write_formatted
    procedure :: write_unformatted
end type mytype
```

## Control Construct Enhancements

### EXIT and CYCLE with Constructs

```fortran
! EXIT from named block
process: block
    integer :: i
    do i = 1, 100
        if (error_condition) exit process
    end do
    ! More processing
end block process

! CYCLE in DO CONCURRENT
do concurrent (i = 1:n)
    if (a(i) < 0) cycle  ! Skip negative values
    b(i) = sqrt(a(i))
end do
```

### STOP and ERROR STOP Enhancements

```fortran
! STOP with integer code
stop 42  ! Exit code 42

! STOP with expression
integer :: error_code = 100
stop error_code + 5  ! Exit code 105

! ERROR STOP with message
error stop "Unrecoverable error occurred"
```

## Intrinsic Procedures

### New Mathematical Intrinsics

```fortran
! Bessel functions
real :: x = 2.5
real :: j0, j1, y0, y1
j0 = bessel_j0(x)   ! Bessel function of first kind, order 0
j1 = bessel_j1(x)   ! Order 1
y0 = bessel_y0(x)   ! Bessel function of second kind, order 0
y1 = bessel_y1(x)   ! Order 1

! Array of Bessel functions
real :: jn(0:5)
jn = bessel_jn(0, 5, x)  ! Orders 0 through 5

! Error functions
real :: erf_val, erfc_val
erf_val = erf(x)     ! Error function
erfc_val = erfc(x)   ! Complementary error function

! Gamma functions
real :: gamma_val, log_gamma_val
gamma_val = gamma(x)
log_gamma_val = log_gamma(x)

! Euclidean norm
real :: vector(3) = [3.0, 4.0, 0.0]
real :: norm
norm = norm2(vector)  ! Returns 5.0

! Hypot without overflow
real :: hypot_val
hypot_val = hypot(3.0, 4.0)  ! Returns 5.0
```

### Bit Intrinsics

```fortran
! Combined bit shift
integer :: i, j
i = 42
j = dshiftl(i, i, 3)  ! Left shift combining two values
j = dshiftr(i, i, 3)  ! Right shift combining two values

! Masked bit operations
integer :: mask = int(b'11110000')
j = iand(i, mask)
j = maskl(4)   ! Creates mask: 11110000...
j = maskr(4)   ! Creates mask: ...00001111

! Bit merge
integer :: a = int(b'10101010')
integer :: b = int(b'01010101')
integer :: m = int(b'11110000')
j = merge_bits(a, b, m)  ! Result: 10100101
```

### Array and Transformational Intrinsics

```fortran
! Find location going backwards
integer :: array(10) = [1,2,3,2,1,2,3,2,1,0]
integer :: loc
loc = findloc(array, 2, back=.true.)  ! Returns 8

! Parity (true if odd number of .true. values)
logical :: flags(5) = [.true., .false., .true., .true., .false.]
logical :: odd_count
odd_count = parity(flags)  ! Returns .true.

! Storage size
integer :: bits
bits = storage_size(1.0_real64)  ! Returns 64
```

### Intrinsic Module Enhancements

```fortran
! ISO_FORTRAN_ENV additions
use, intrinsic :: iso_fortran_env
integer :: unit
real(real128) :: quad_precision  ! 128-bit real if available
integer(int64) :: big_int

! Compiler version information
print *, compiler_version()
print *, compiler_options()

! Atomic operations
use, intrinsic :: iso_fortran_env
integer :: atom[*], val
call atomic_define(atom[2], 42)
call atomic_ref(val, atom[2])
```

## I/O Enhancements

### NEWUNIT Specifier

Automatic unit number allocation:
```fortran
integer :: unit
open(newunit=unit, file='data.txt')
! unit now contains a unique unit number
write(unit, *) "Data"
close(unit)
```

### G0 and G0.d Format Descriptors

Minimal width output:
```fortran
real :: x = 123.456
integer :: i = 42
character(20) :: name = "Fortran"

! G0 format uses minimal width
print '(g0)', x      ! Outputs: 123.456
print '(g0)', i      ! Outputs: 42
print '(g0)', name   ! Outputs: Fortran

! G0.d for reals with d decimal places
print '(g0.2)', x    ! Outputs: 123.46
```

### Unlimited Format Item

```fortran
integer :: array(5) = [1, 2, 3, 4, 5]

! Unlimited repeat count
print '(*(i0, :, ", "))', array
! Outputs: 1, 2, 3, 4, 5
```

### I/O Enhancements

```fortran
! ENCODING specifier
open(unit, file='utf8.txt', encoding='UTF-8')

! Decimal comma for output
open(unit, file='european.txt', decimal='comma')
write(unit, '(f10.2)') 123.45  ! Outputs: 123,45

! RC and RD rounding modes
real :: x = 2.125
write(*, '(rc, f5.2)') x  ! Round compatible: 2.12
write(*, '(rd, f5.2)') x  ! Round down: 2.12
```

## Module Enhancements

### Impure Elemental Procedures

```fortran
! Elemental procedures can now be impure
impure elemental subroutine debug_log(x)
    real, intent(in) :: x
    write(*,*) "Processing value:", x  ! I/O makes it impure
end subroutine debug_log

real :: array(100)
call debug_log(array)  ! Applies to all elements
```

### Generic Resolution Enhancements

```fortran
module overloads
    interface operator(+)
        module procedure add_custom_types
    end interface

    ! Can now distinguish by kind type parameters
    interface process
        module procedure process_real32
        module procedure process_real64
    end interface

contains

    subroutine process_real32(x)
        real(real32), intent(in) :: x
        ! Processing for 32-bit reals
    end subroutine

    subroutine process_real64(x)
        real(real64), intent(in) :: x
        ! Processing for 64-bit reals
    end subroutine

end module overloads
```

### Separate Module Procedures

```fortran
! In module
module procedures_module
    interface
        module subroutine separate_impl(x)
            real, intent(inout) :: x
        end subroutine
    end interface
end module

! In submodule
submodule (procedures_module) implementations
contains
    module procedure separate_impl
        x = x * 2.0
    end procedure
end submodule
```

## Performance Enhancements

### Compiler Directives

```fortran
! CONCURRENT attribute for FORALL and DO CONCURRENT
do concurrent (i = 1:n) local(temp)
    temp = a(i) + b(i)
    c(i) = temp ** 2
end do

! VALUE attribute for better optimization
function cube(x) result(y)
    real, value :: x  ! x is passed by value
    real :: y
    y = x ** 3
end function
```

### Asynchronous Enhancements

```fortran
real :: large_array(1000000)
integer :: id

! Asynchronous I/O with multiple operations
open(10, file='data1.dat', asynchronous='yes')
open(20, file='data2.dat', asynchronous='yes')

read(10, asynchronous='yes', id=id) large_array
! Do work while reading
call compute_something_else()

wait(10)  ! Wait for read to complete
```

## Summary

Fortran 2008 adds significant parallel programming capabilities through coarrays while maintaining backward compatibility. Key additions include:

1. **Coarrays** - Simple SPMD parallel programming model
2. **Submodules** - Better encapsulation and separate compilation
3. **DO CONCURRENT** - Compiler-optimizable parallel loops
4. **BLOCK construct** - Local scoping within procedures
5. **CONTIGUOUS attribute** - Performance optimization for arrays
6. **Enhanced intrinsics** - Mathematical, bit manipulation, and parallel support
7. **I/O improvements** - NEWUNIT, G0 format, better UTF-8 support
8. **Module enhancements** - Impure elemental procedures, better generic resolution

These features make Fortran 2008 suitable for modern parallel architectures while maintaining its strengths in numerical computation.
