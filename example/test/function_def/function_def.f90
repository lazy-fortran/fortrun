program main
    implicit none
    real(8) :: x
    real(8) :: result

    x = 5.0
    result = square(x)
    print *, "Result:", result

contains

    real function square(a)
        real :: a
        square = a * a
    end function square

end program main