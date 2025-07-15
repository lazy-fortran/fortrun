program main
    implicit none
    real(8) :: result

    result = square(5.0d0)
contains
    real(8) function square(x)
    implicit none
    real(8), intent(in) :: x
    square = x * x
end function square
    
end program main
