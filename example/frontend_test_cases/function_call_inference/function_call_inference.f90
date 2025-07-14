program main
    implicit none
    real :: result
    real :: compute
    result = compute(5.0)
contains
    real function compute(x)
        real :: x
        compute = x * x
    end function compute
end program main