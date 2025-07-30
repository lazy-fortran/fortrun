program polynomial
    ! Polynomial differentiation example
    ! Function: f(x) = 2*x^3 + 3*x^2 - 4*x + 1
    ! Derivative: f'(x) = 6*x^2 + 6*x - 4

    real :: x = 2.5
    real :: y, analytical_grad, numerical_grad
    real, parameter :: tolerance = 1.0e-5

    ! Evaluate polynomial
    y = polynomial_function(x)
    print *, "f(", x, ") =", y

    ! Analytical derivative
    analytical_grad = polynomial_derivative(x)
    print *, "Analytical gradient:", analytical_grad

    ! Numerical derivative for validation
    numerical_grad = numerical_derivative(x)
    print *, "Numerical gradient:", numerical_grad

    ! Test multiple points
    print *, ""
    print *, "Testing at multiple points:"
    call test_gradient_at_point(-1.0)
    call test_gradient_at_point(0.0)
    call test_gradient_at_point(1.5)

contains

    real function polynomial_function(x)
        ! Polynomial: f(x) = 2*x^3 + 3*x^2 - 4*x + 1
        real, intent(in) :: x
        polynomial_function = 2.0*x**3 + 3.0*x**2 - 4.0*x + 1.0
    end function polynomial_function

    real function polynomial_derivative(x)
        ! Analytical derivative: f'(x) = 6*x^2 + 6*x - 4
        real, intent(in) :: x
        polynomial_derivative = 6.0*x**2 + 6.0*x - 4.0
    end function polynomial_derivative

    real function numerical_derivative(x)
        ! Numerical derivative using finite differences
        real, intent(in) :: x
        real, parameter :: h = 1.0e-8
numerical_derivative = (polynomial_function(x + h) - polynomial_function(x - h))/(2.0*h)
    end function numerical_derivative

    subroutine test_gradient_at_point(test_x)
        real, intent(in) :: test_x
        real :: analytical, numerical, error

        analytical = polynomial_derivative(test_x)
        numerical = numerical_derivative(test_x)
        error = abs(analytical - numerical)

        print *, "x =", test_x
        print *, "  Analytical:", analytical
        print *, "  Numerical: ", numerical
        print *, "  Error:     ", error

        if (error < tolerance) then
            print *, "  Status: PASSED"
        else
            print *, "  Status: FAILED"
        end if
        print *, ""
    end subroutine test_gradient_at_point

end program polynomial
