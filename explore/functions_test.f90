! Test function definitions in .f90 for comparison
program functions_test
    implicit none
    
    real :: x, y, z
    real :: square, cube
    
    x = 5.0
    y = square(x)
    z = cube(x)

    print *, "x =", x
    print *, "x^2 =", y  
    print *, "x^3 =", z

contains
    function square(val) result(res)
        real, intent(in) :: val
        real :: res
        res = val * val
    end function

    function cube(val) result(res)
        real, intent(in) :: val
        real :: res
        res = val * val * val
    end function
end program