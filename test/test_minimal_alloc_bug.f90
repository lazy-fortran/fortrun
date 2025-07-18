program test_minimal_alloc_bug
    implicit none

    type :: my_type
        integer :: x
    end type

    type(my_type), allocatable :: result

    print *, "Test 1: Returning allocated"
    result = get_allocated()
    print *, "Success! allocated = ", allocated(result)

    print *, ""
    print *, "Test 2: Returning unallocated"
    if (allocated(result)) deallocate (result)
    result = get_unallocated()  ! This will likely crash
    print *, "Success! allocated = ", allocated(result)

contains

    function get_allocated() result(mt)
        type(my_type), allocatable :: mt
        allocate (mt)
        mt%x = 42
    end function

    function get_unallocated() result(mt)
        type(my_type), allocatable :: mt
        ! Return without allocating
    end function

end program test_minimal_alloc_bug
