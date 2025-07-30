program function_inlining
    ! Demonstrates function inlining optimization
    ! Small functions should be inlined to eliminate call overhead

    real :: x = 5.0
    real :: y = 3.0
    real :: result
    integer :: i
    real :: sum = 0.0

    print *, "Function Inlining Optimization Example"
    print *, ""

    ! Small utility functions - good candidates for inlining
    result = add_two_numbers(x, y)
    print *, "Addition result:", result

    result = square(x)
    print *, "Square result:", result

    result = simple_max(x, y)
    print *, "Max result:", result

    ! Function called in a loop - strong inlining candidate
    print *, "Function calls in loop:"
    do i = 1, 10
        sum = sum + increment_and_square(real(i))
    end do
    print *, "Loop sum:", sum

    ! More complex inlining scenarios
    call demonstrate_complex_inlining()

    print *, ""
    print *, "Expected MLIR optimizations:"
    print *, "- Small functions inlined"
    print *, "- Function call overhead eliminated"
    print *, "- Better optimization opportunities after inlining"
    print *, "- Reduced stack frame operations"

contains

    ! Simple functions - excellent inlining candidates
    real function add_two_numbers(a, b)
        real, intent(in) :: a, b
        add_two_numbers = a + b
    end function add_two_numbers

    real function square(x)
        real, intent(in) :: x
        square = x*x
    end function square

    real function simple_max(a, b)
        real, intent(in) :: a, b
        if (a > b) then
            simple_max = a
        else
            simple_max = b
        end if
    end function simple_max

    real function increment_and_square(x)
        real, intent(in) :: x
        real :: temp
        temp = x + 1.0
        increment_and_square = temp*temp
    end function increment_and_square

    ! Slightly larger function - may or may not be inlined
    real function polynomial_eval(x)
        real, intent(in) :: x
        polynomial_eval = 2.0*x**3 + 3.0*x**2 - x + 1.0
    end function polynomial_eval

    ! Function with multiple return paths
    integer function classify_number(x)
        real, intent(in) :: x
        if (x > 0.0) then
            classify_number = 1
        else if (x < 0.0) then
            classify_number = -1
        else
            classify_number = 0
        end if
    end function classify_number

    subroutine demonstrate_complex_inlining()
        real :: test_values(5) = [1.0, 2.0, 3.0, 4.0, 5.0]
        real :: poly_results(5)
        integer :: class_results(5)
        integer :: i

        print *, "Complex inlining scenarios:"

        ! Functions called with array elements
        do i = 1, 5
            poly_results(i) = polynomial_eval(test_values(i))
            class_results(i) = classify_number(test_values(i) - 3.0)
        end do

        print *, "Polynomial results:", poly_results
        print *, "Classification results:", class_results

        ! Nested function calls - opportunities for multiple inlining
        do i = 1, 3
            result = square(add_two_numbers(test_values(i), test_values(i + 1)))
            print *, "Nested call result", i, ":", result
        end do

        ! Function composition
        result = simple_max(square(2.0), polynomial_eval(1.0))
        print *, "Composed functions result:", result
    end subroutine demonstrate_complex_inlining

    ! Recursive function - typically not inlined
    integer function factorial(n)
        integer, intent(in) :: n
        if (n <= 1) then
            factorial = 1
        else
            factorial = n*factorial(n - 1)
        end if
    end function factorial

    ! Large function - unlikely to be inlined
    real function expensive_computation(x)
        real, intent(in) :: x
        real :: result_val, temp
        integer :: i

        result_val = x
        do i = 1, 100
            temp = sin(result_val) + cos(result_val)
            result_val = result_val + temp*0.01
        end do

        expensive_computation = result_val
    end function expensive_computation

end program function_inlining
