! Test basic types in .f90 for comparison
program simple_types_test
    implicit none
    
    integer :: count
    real :: pi
    character(len=10) :: name
    logical :: ready
    
    count = 42
    pi = 3.14159
    name = "Hello"
    ready = .true.

    print *, "Count:", count
    print *, "Pi:", pi
    print *, "Name:", name
    print *, "Ready:", ready
end program