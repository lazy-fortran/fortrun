program simple_gradient
    ! Simple gradient computation example
    ! Function: f(x) = x^2
    ! Derivative: f'(x) = 2*x

    real :: x = 3.0
    real :: y, analytical_grad, numerical_grad
    real, parameter :: tolerance = 1.0e-6

    ! Compute function value
    y = square_function(x)
    print *, "f(", x, ") =", y

    ! Analytical gradient
    analytical_grad = 2.0*x
    print *, "Analytical gradient:", analytical_grad

    ! With AD enabled, the MLIR backend will generate gradient computation
    ! This would be computed by Enzyme AD
    print *, "Expected AD result:", analytical_grad

    ! Numerical gradient for comparison (finite differences)
    numerical_grad = numerical_derivative(x)
    print *, "Numerical gradient:", numerical_grad

    ! Validate results
    if (abs(analytical_grad - numerical_grad) < tolerance) then
        print *, "Gradient validation: PASSED"
    else
        print *, "Gradient validation: FAILED"
        print *, "Difference:", abs(analytical_grad - numerical_grad)
    end if

contains

    real function square_function(x)
        ! Function to differentiate: f(x) = x^2
        real, intent(in) :: x
        square_function = x*x
    end function square_function

    real function numerical_derivative(x)
        ! Numerical derivative using finite differences
        real, intent(in) :: x
        real, parameter :: h = 1.0e-8
        numerical_derivative = (square_function(x + h) - square_function(x - h))/(2.0*h)
    end function numerical_derivative

end program simple_gradient
