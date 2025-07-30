program optimization
    ! Gradient-based optimization example
    ! Minimize: f(x) = (x - 2)^2 + 1
    ! Minimum at x = 2, f(2) = 1

    real :: x = 0.0  ! Starting point
    real :: learning_rate = 0.1
    integer, parameter :: max_iterations = 100
    real, parameter :: tolerance = 1.0e-6
    integer :: iter
    real :: f_val, gradient, step_size

    print *, "Gradient-based optimization example"
    print *, "Function: f(x) = (x - 2)^2 + 1"
    print *, "Analytical minimum: x = 2, f(2) = 1"
    print *, ""
    print *, "Starting optimization from x =", x
    print *, ""

    ! Optimization loop
    do iter = 1, max_iterations
        ! Compute function value and gradient
        f_val = objective_function(x)
        gradient = objective_gradient(x)

        ! Print current state
        if (mod(iter, 10) == 1 .or. iter <= 5) then
         print *, "Iteration", iter, ": x =", x, ", f(x) =", f_val, ", grad =", gradient
        end if

        ! Check convergence
        if (abs(gradient) < tolerance) then
            print *, "Converged at iteration", iter
            exit
        end if

        ! Gradient descent step
        step_size = learning_rate*gradient
        x = x - step_size

        ! Adaptive learning rate (simple decay)
        if (iter > 20) then
            learning_rate = learning_rate*0.99
        end if
    end do

    print *, ""
    print *, "Final results:"
    print *, "  x =", x
    print *, "  f(x) =", objective_function(x)
    print *, "  gradient =", objective_gradient(x)
    print *, "  iterations =", iter

    ! Compare with analytical solution
    print *, ""
    print *, "Comparison with analytical solution:"
    print *, "  Error in x:", abs(x - 2.0)
    print *, "  Error in f(x):", abs(objective_function(x) - 1.0)

    if (abs(x - 2.0) < 0.01 .and. abs(objective_function(x) - 1.0) < 0.01) then
        print *, "  Optimization: SUCCESSFUL"
    else
        print *, "  Optimization: FAILED"
    end if

contains

    real function objective_function(x)
        ! Function to minimize: f(x) = (x - 2)^2 + 1
        real, intent(in) :: x
        objective_function = (x - 2.0)**2 + 1.0
    end function objective_function

    real function objective_gradient(x)
        ! Analytical gradient: f'(x) = 2(x - 2)
        real, intent(in) :: x
        objective_gradient = 2.0*(x - 2.0)
    end function objective_gradient

    real function numerical_gradient(x)
        ! Numerical gradient for validation
        real, intent(in) :: x
        real, parameter :: h = 1.0e-8
    numerical_gradient = (objective_function(x + h) - objective_function(x - h))/(2.0*h)
    end function numerical_gradient

end program optimization
