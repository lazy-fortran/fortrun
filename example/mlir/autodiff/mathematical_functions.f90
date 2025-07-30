program mathematical_functions
    ! Mathematical function differentiation example
    ! Tests: sin(x), cos(x), exp(x), ln(x)

    real :: x = 1.0
    real, parameter :: tolerance = 1.0e-5

    print *, "Testing mathematical function derivatives at x =", x
    print *, ""

    ! Test sine function
    call test_sine(x)

    ! Test cosine function
    call test_cosine(x)

    ! Test exponential function
    call test_exponential(x)

    ! Test natural logarithm
    call test_logarithm(x)

contains

    subroutine test_sine(test_x)
        ! f(x) = sin(x), f'(x) = cos(x)
        real, intent(in) :: test_x
        real :: analytical, numerical, error

        print *, "Testing sin(x) at x =", test_x
        analytical = cos(test_x)  ! Analytical derivative
        numerical = numerical_sine_derivative(test_x)
        error = abs(analytical - numerical)

        print *, "  sin(", test_x, ") =", sin(test_x)
        print *, "  Analytical d/dx sin(x) = cos(x) =", analytical
        print *, "  Numerical derivative =", numerical
        print *, "  Error =", error

        if (error < tolerance) then
            print *, "  Status: PASSED"
        else
            print *, "  Status: FAILED"
        end if
        print *, ""
    end subroutine test_sine

    subroutine test_cosine(test_x)
        ! f(x) = cos(x), f'(x) = -sin(x)
        real, intent(in) :: test_x
        real :: analytical, numerical, error

        print *, "Testing cos(x) at x =", test_x
        analytical = -sin(test_x)  ! Analytical derivative
        numerical = numerical_cosine_derivative(test_x)
        error = abs(analytical - numerical)

        print *, "  cos(", test_x, ") =", cos(test_x)
        print *, "  Analytical d/dx cos(x) = -sin(x) =", analytical
        print *, "  Numerical derivative =", numerical
        print *, "  Error =", error

        if (error < tolerance) then
            print *, "  Status: PASSED"
        else
            print *, "  Status: FAILED"
        end if
        print *, ""
    end subroutine test_cosine

    subroutine test_exponential(test_x)
        ! f(x) = exp(x), f'(x) = exp(x)
        real, intent(in) :: test_x
        real :: analytical, numerical, error

        print *, "Testing exp(x) at x =", test_x
        analytical = exp(test_x)  ! Analytical derivative
        numerical = numerical_exp_derivative(test_x)
        error = abs(analytical - numerical)

        print *, "  exp(", test_x, ") =", exp(test_x)
        print *, "  Analytical d/dx exp(x) = exp(x) =", analytical
        print *, "  Numerical derivative =", numerical
        print *, "  Error =", error

        if (error < tolerance) then
            print *, "  Status: PASSED"
        else
            print *, "  Status: FAILED"
        end if
        print *, ""
    end subroutine test_exponential

    subroutine test_logarithm(test_x)
        ! f(x) = log(x), f'(x) = 1/x
        real, intent(in) :: test_x
        real :: analytical, numerical, error

        if (test_x <= 0.0) then
            print *, "Skipping log(x) test: x must be positive"
            return
        end if

        print *, "Testing log(x) at x =", test_x
        analytical = 1.0/test_x  ! Analytical derivative
        numerical = numerical_log_derivative(test_x)
        error = abs(analytical - numerical)

        print *, "  log(", test_x, ") =", log(test_x)
        print *, "  Analytical d/dx log(x) = 1/x =", analytical
        print *, "  Numerical derivative =", numerical
        print *, "  Error =", error

        if (error < tolerance) then
            print *, "  Status: PASSED"
        else
            print *, "  Status: FAILED"
        end if
        print *, ""
    end subroutine test_logarithm

    ! Numerical derivative functions using finite differences
    real function numerical_sine_derivative(x)
        real, intent(in) :: x
        real, parameter :: h = 1.0e-8
        numerical_sine_derivative = (sin(x + h) - sin(x - h))/(2.0*h)
    end function numerical_sine_derivative

    real function numerical_cosine_derivative(x)
        real, intent(in) :: x
        real, parameter :: h = 1.0e-8
        numerical_cosine_derivative = (cos(x + h) - cos(x - h))/(2.0*h)
    end function numerical_cosine_derivative

    real function numerical_exp_derivative(x)
        real, intent(in) :: x
        real, parameter :: h = 1.0e-8
        numerical_exp_derivative = (exp(x + h) - exp(x - h))/(2.0*h)
    end function numerical_exp_derivative

    real function numerical_log_derivative(x)
        real, intent(in) :: x
        real, parameter :: h = 1.0e-8
        numerical_log_derivative = (log(x + h) - log(x - h))/(2.0*h)
    end function numerical_log_derivative

end program mathematical_functions
