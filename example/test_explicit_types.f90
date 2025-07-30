function add(real(8) :: x, y) result(z)
    real(8) :: z
    z = x + y
end function add

subroutine process(real(8), intent(in) :: a, b)
    print *, a + b
end subroutine process

program test_explicit_types
    implicit none
    real(8) :: result

    ! Test the function
    result = add(1.0d0, 2.0d0)
    print *, 'Result:', result

    ! Test the subroutine
    call process(3.0d0, 4.0d0)
end program test_explicit_types
