program variables
    ! Demonstrate variable declarations and assignments
    integer :: i = 42
    real :: x = 3.14
    logical :: flag = .true.
    character(len=10) :: name = "Fortran"

    ! Variable assignments
    i = i + 1
    x = x*2.0
    flag = .not. flag

    ! Print values
    print *, "Integer:", i
    print *, "Real:", x
    print *, "Logical:", flag
    print *, "String:", name
end program variables
