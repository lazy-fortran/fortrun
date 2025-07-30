# Fortran 2018 Standard Summary

This document summarizes the new features and changes in Fortran 2018 (ISO/IEC 1539-1:2018) compared to Fortran 2008. Fortran 2018 incorporates two Technical Specifications: Enhanced C interoperability (TS 29113) and Additional Parallel Features (TS 18508).

## Table of Contents

1. [Coarray Teams](#coarray-teams)
2. [Failed Image Handling](#failed-image-handling)
3. [Collective Subroutines](#collective-subroutines)
4. [Coarray Events](#coarray-events)
5. [Enhanced C Interoperability](#enhanced-c-interoperability)
6. [Intrinsic Procedures](#intrinsic-procedures)
7. [I/O Enhancements](#io-enhancements)
8. [Language Feature Enhancements](#language-feature-enhancements)
9. [Deleted and Obsolescent Features](#deleted-and-obsolescent-features)

## Coarray Teams

Teams provide a way to organize images into groups for structured parallel computation.

### TEAM_TYPE and Basic Operations

```fortran
use, intrinsic :: iso_fortran_env
type(team_type) :: team_odd, team_even

! Form teams based on image number
if (mod(this_image(), 2) == 0) then
    form team (1, team_even)
else
    form team (2, team_odd)
end if

! Change to team context
change team (team_even)
    ! Inside here, only even-numbered images exist
    ! this_image() and num_images() are relative to the team

    ! Team-specific computation
    real :: data[*]
    data = this_image() * 2.0
    sync all  ! Synchronizes only team members

end team

! Back to parent team context
```

### Nested Teams

```fortran
type(team_type) :: color_team, row_team
integer :: color, row

! First level: divide by color
color = mod(this_image() - 1, 3) + 1
form team (color, color_team)

change team (color_team)
    ! Second level: divide by row within color team
    row = (this_image() - 1) / 4 + 1
    form team (row, row_team)

    change team (row_team)
        ! Work within row team
        call process_row_data()
    end team

end team
```

### Team Inquiry Functions

```fortran
type(team_type) :: my_team, parent_team
integer :: team_num

! Get current team
my_team = get_team()

! Get parent team
parent_team = get_team(parent_team)

! Get team number
team_num = team_number()

! Image index in parent team
integer :: parent_index
parent_index = this_image(team=parent_team)
```

### Team Synchronization

```fortran
! Sync all images in current team
sync team (my_team)

! Sync with optional stat
integer :: sync_stat
sync team (my_team, stat=sync_stat)
if (sync_stat == stat_failed_image) then
    ! Handle failed images
end if
```

## Failed Image Handling

Robust handling of image failures in coarray programs.

### Failed Image Detection

```fortran
use, intrinsic :: iso_fortran_env
integer :: failed_count, stopped_count
integer, allocatable :: failed_list(:), stopped_list(:)

! Get failed images
failed_list = failed_images()
failed_count = size(failed_list)

! Get stopped images
stopped_list = stopped_images()
stopped_count = size(stopped_list)

! Check specific image status
integer :: img_status
img_status = image_status(5)

select case (img_status)
case (0)
    print *, "Image 5 is active"
case (stat_failed_image)
    print *, "Image 5 has failed"
case (stat_stopped_image)
    print *, "Image 5 has stopped"
end select
```

### Handling Failed Images in Operations

```fortran
real :: remote_data
integer :: stat

! Coarray reference with failure handling
remote_data = data[5, stat=stat]
if (stat == stat_failed_image) then
    print *, "Cannot access data on failed image 5"
    ! Use default or alternative value
    remote_data = 0.0
end if

! Lock with failed image handling
type(lock_type) :: lock[*]
lock(lock[3], stat=stat)
if (stat == stat_unlocked_failed_image) then
    print *, "Lock on failed image 3"
end if
```

### Reforming Teams After Failure

```fortran
type(team_type) :: active_team
integer :: stat

! Form team with only active images
form team (1, active_team, stat=stat)
if (stat == stat_failed_image) then
    print *, "Some images failed during team formation"
end if

! Continue with active images only
change team (active_team)
    ! Failed images are excluded from this team
    call continue_computation()
end team
```

## Collective Subroutines

Intrinsic procedures for collective operations across team images.

### CO_SUM

```fortran
real :: local_sum, total_sum
integer :: sum_on_image

local_sum = calculate_local_sum()

! Sum across all images, result on all images
call co_sum(local_sum)
! Now local_sum contains the total on all images

! Sum with result on specific image
local_sum = calculate_local_sum()
sum_on_image = 1
call co_sum(local_sum, result_image=sum_on_image)
! Now only image 1 has the total sum

! Array sum
real :: array(100), total_array(100)
array = local_computation()
call co_sum(array)  ! Element-wise sum
```

### CO_MIN and CO_MAX

```fortran
real :: local_min, global_min
real :: local_max, global_max
integer :: min_location[*]

! Find minimum across all images
local_min = find_local_minimum()
call co_min(local_min)

! Find maximum with result on image 2
local_max = find_local_maximum()
call co_max(local_max, result_image=2)

! Character minimum (lexicographic)
character(len=20) :: name, first_name
name = get_local_name()
call co_min(name)
```

### CO_REDUCE

```fortran
! User-defined reduction operation
interface
    pure function my_reduction(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
    end function my_reduction
end interface

real :: value
value = local_value()

! Reduce using custom operation
call co_reduce(value, my_reduction)

! Example: product reduction
pure function product_op(a, b) result(c)
    real, intent(in) :: a, b
    real :: c
    c = a * b
end function product_op

real :: partial_product
partial_product = calculate_partial_product()
call co_reduce(partial_product, product_op)
```

### CO_BROADCAST

```fortran
real :: data
integer :: source_image

source_image = 3
if (this_image() == source_image) then
    data = 42.0
end if

! Broadcast from source to all images
call co_broadcast(data, source_image)
! Now all images have data = 42.0

! Broadcast array
real :: matrix(100, 100)
if (this_image() == 1) then
    matrix = read_input_matrix()
end if
call co_broadcast(matrix, 1)
```

## Coarray Events

Event variables for fine-grained synchronization between images.

### EVENT_TYPE

```fortran
use, intrinsic :: iso_fortran_env
type(event_type) :: ready[*], done[*]
integer :: count

! Post event to signal readiness
event post (ready[2])  ! Signal image 2

! Wait for event
event wait (ready)     ! Wait for local event

! Query event count
call event_query(ready, count)
print *, "Event posted", count, "times"

! Wait with threshold
event wait (ready, until_count=3)  ! Wait for 3 posts
```

### Producer-Consumer Pattern

```fortran
type(event_type) :: data_ready[*], data_consumed[*]
real :: buffer[*]

! Producer (image 1)
if (this_image() == 1) then
    do i = 1, 100
        buffer[2] = produce_data()
        event post (data_ready[2])
        event wait (data_consumed)
    end do
end if

! Consumer (image 2)
if (this_image() == 2) then
    do i = 1, 100
        event wait (data_ready)
        call consume_data(buffer)
        event post (data_consumed[1])
    end do
end if
```

## Enhanced C Interoperability

Extended from TS 29113 for better C interaction.

### Assumed-Rank Arrays

```fortran
! Interface for C function accepting any rank array
interface
    subroutine c_process_array(array, rank) bind(c)
        use, intrinsic :: iso_c_binding
        real(c_float), intent(inout) :: array(..)  ! Assumed-rank
        integer(c_int), value :: rank
    end subroutine c_process_array
end interface

! Fortran procedure with assumed-rank dummy
subroutine process_any_rank(array)
    real, intent(inout) :: array(..)
    integer :: r

    r = rank(array)

    select rank (array)
    rank (0)  ! Scalar
        array = array * 2.0
    rank (1)  ! 1D array
        array(:) = array(:) * 2.0
    rank (2)  ! 2D array
        array(:,:) = array(:,:) * 2.0
    rank default
        error stop "Unsupported rank"
    end select
end subroutine
```

### C Descriptor

```fortran
! Assumed-shape/rank arrays use C descriptors
subroutine fortran_proc(array) bind(c)
    use, intrinsic :: iso_c_binding
    real(c_float), intent(inout) :: array(:,:)
    ! Array descriptor passed to C
end subroutine

! C_SIZEOF for assumed-rank
subroutine get_size(array, bytes)
    use, intrinsic :: iso_c_binding
    real :: array(..)
    integer(c_size_t) :: bytes

    bytes = c_sizeof(array)
end subroutine
```

### C_PTRDIFF_T

```fortran
use, intrinsic :: iso_c_binding
integer(c_ptrdiff_t) :: offset
type(c_ptr) :: ptr1, ptr2

! Calculate pointer difference
offset = c_ptr_diff(ptr2, ptr1)
```

### Optional Arguments

```fortran
! C interface with optional argument
interface
    subroutine c_func(required, optional) bind(c)
        use, intrinsic :: iso_c_binding
        integer(c_int), value :: required
        integer(c_int), optional, value :: optional
    end subroutine c_func
end interface

! Call with/without optional
call c_func(42)
call c_func(42, 100)
```

## Intrinsic Procedures

### COSHAPE

```fortran
real :: scalar[*], array(10,20)[2,3:5,*]
integer :: coshape_scalar(1), coshape_array(3)

! Get coshape
coshape_scalar = coshape(scalar)     ! Returns [num_images()]
coshape_array = coshape(array)       ! Returns [2, 3, *]

! Get cobound
integer :: lower_cobounds(3), upper_cobounds(3)
lower_cobounds = lcobound(array)     ! Returns [1, 3, 1]
upper_cobounds = ucobound(array)     ! Returns [2, 5, *]
```

### RANDOM_INIT

```fortran
! Initialize random number generator
logical :: repeatable, image_distinct

! Same sequence every run, different across images
call random_init(repeatable=.true., image_distinct=.true.)

! Different sequence every run, same across images
call random_init(repeatable=.false., image_distinct=.false.)
```

### OUT_OF_RANGE

```fortran
real :: x
integer :: i
logical :: overflow

! Check if real->integer conversion would overflow
x = 1.0e10
overflow = out_of_range(x, i)
if (.not. overflow) then
    i = int(x)
end if

! Check for different integer kinds
integer(int8) :: small_int
overflow = out_of_range(huge(0), small_int)
```

### REDUCE

```fortran
! Array reduction with user-defined operation
real :: array(100)
real :: result

interface
    pure function my_op(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
    end function my_op
end interface

! Reduce array to scalar
result = reduce(array, my_op)

! Reduce with identity value
result = reduce(array, my_op, identity=0.0)

! Reduce along dimension
real :: matrix(10,20), row_results(10)
row_results = reduce(matrix, my_op, dim=2)

! Ordered reduction
result = reduce(array, my_op, ordered=.true.)
```

## I/O Enhancements

### Hexadecimal Floating-Point I/O

```fortran
real :: x = 3.14159
character(len=20) :: hex_string

! Write floating-point as hexadecimal
write(hex_string, '(ex20.12)') x
! Result: "0X1.921FB54442D18P+1"

! Read hexadecimal floating-point
read(hex_string, *) x

! Formatted hex output
write(*, '(ex0)') 1.0  ! Minimal width: 0X1P+0
write(*, '(ex15.8)') x ! Width 15, 8 hex digits after point
```

### Round-to-Nearest Edit Descriptor

```fortran
real :: x = 2.5, y = 3.5

! RN - round to nearest (ties to even)
write(*, '(rn, f5.1)') x  ! Output: 2.0
write(*, '(rn, f5.1)') y  ! Output: 4.0

! Comparison with other modes
write(*, '(ru, f5.1)') 2.5  ! Round up: 3.0
write(*, '(rd, f5.1)') 2.5  ! Round down: 2.0
write(*, '(rz, f5.1)') 2.5  ! Round to zero: 2.0
```

### SIZE= for Non-advancing I/O

```fortran
character(len=100) :: buffer
integer :: chars_transferred
integer :: unit

! Read with size information
read(unit, '(a)', advance='no', size=chars_transferred) buffer
print *, "Read", chars_transferred, "characters"

! Partial record handling
do
    read(unit, '(a)', advance='no', size=chars_transferred, &
         iostat=ios, eor=100) buffer
    ! Process partial data
    cycle
    100 continue  ! End of record
    exit
end do
```

## Language Feature Enhancements

### IMPORT in Internal Procedures

```fortran
module my_module
    type :: my_type
        real :: value
    end type
end module

program main
    use my_module
    type(my_type) :: obj

    call internal_sub()

contains
    subroutine internal_sub()
        import :: my_type  ! Selective import
        type(my_type) :: local_obj
        ! Can now use my_type
    end subroutine
end program

! IMPORT in BLOCK
subroutine process()
    real :: x

    block
        import :: x  ! Import specific host variable
        x = x * 2.0
    end block
end subroutine
```

### Implied-DO in DATA Statements

```fortran
! More flexible implied-DO in DATA
integer :: array(10), matrix(5,5)
data (array(i), i=1,10,2) / 5*1 /  ! Odd elements
data ((matrix(i,j), i=j,5), j=1,5) / 15*1 /  ! Upper triangle
```

### STOP and ERROR STOP Enhancements

```fortran
! Suppress STOP output
logical :: quiet
quiet = .true.
stop "Normal termination", quiet=quiet  ! No output if quiet

! Non-constant stop code
integer :: error_code
error_code = get_error_code()
error stop error_code  ! Dynamic error code

! Conditional output
error stop "Critical error", quiet=(verbosity < 2)
```

### Recursive I/O

```fortran
! Allowed in Fortran 2018
type :: node
    integer :: value
    type(node), pointer :: next => null()
contains
    procedure :: print => print_node
end type

recursive subroutine print_node(this, unit)
    class(node), intent(in) :: this
    integer, intent(in) :: unit

    write(unit, *) this%value
    if (associated(this%next)) then
        call this%next%print(unit)  ! Recursive I/O
    end if
end subroutine
```

## Deleted and Obsolescent Features

### Deleted Features

```fortran
! DELETED - Arithmetic IF (was obsolescent in Fortran 90)
! if (expr) 10, 20, 30  ! NO LONGER VALID

! DELETED - Non-block DO constructs
! do 10 i = 1, n         ! NO LONGER VALID
!     x(i) = i
! 10 continue            ! Must use END DO
```

### New Obsolescent Features

The following are now obsolescent (may be deleted in future):

```fortran
! OBSOLESCENT - COMMON blocks
common /mycommon/ x, y, z  ! Use modules instead

! OBSOLESCENT - EQUIVALENCE
real :: a(100)
real :: b(50)
equivalence (a(1), b(1))  ! Use pointers or other methods

! OBSOLESCENT - BLOCK DATA
block data init_common
    common /mycommon/ x, y, z
    data x, y, z / 1.0, 2.0, 3.0 /
end block data

! OBSOLESCENT - Labeled DO loops
do 100 i = 1, n
    ! loop body
100 continue  ! Use DO/END DO instead

! OBSOLESCENT - Specific intrinsic names
y = dsin(x)   ! Use generic sin(x)
z = iabs(i)   ! Use generic abs(i)

! OBSOLESCENT - FORALL construct
forall (i = 1:n, j = 1:m, a(i,j) /= 0.0)
    a(i,j) = 1.0 / a(i,j)
end forall
! Use DO CONCURRENT instead
```

## Summary

Fortran 2018 enhances parallel programming capabilities with:

1. **Teams** - Hierarchical organization of coarray images
2. **Failed images** - Robust handling of image failures
3. **Collective subroutines** - Built-in reduction operations
4. **Events** - Fine-grained synchronization
5. **Enhanced C interoperability** - Assumed-rank arrays, better descriptors
6. **I/O improvements** - Hexadecimal floats, SIZE= for non-advancing I/O
7. **Language cleanups** - Removal of arithmetic IF, non-block DO

These features make Fortran 2018 more suitable for resilient parallel programming on modern HPC systems while maintaining backward compatibility with most Fortran 2008 code.
