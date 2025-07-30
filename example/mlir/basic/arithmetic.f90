program arithmetic
    ! Demonstrate arithmetic operations
    integer :: a = 10
    integer :: b = 3
    integer :: result
    real :: x = 5.5
    real :: y = 2.5
    real :: real_result

    ! Integer arithmetic
    result = a + b    ! Addition
    print *, "Addition:", result

    result = a - b    ! Subtraction
    print *, "Subtraction:", result

    result = a*b    ! Multiplication
    print *, "Multiplication:", result

    result = a/b    ! Integer division
    print *, "Integer division:", result

    ! Real arithmetic
    real_result = x + y
    print *, "Real addition:", real_result

    real_result = x*y
    print *, "Real multiplication:", real_result

    real_result = x/y
    print *, "Real division:", real_result

    ! Mixed arithmetic
    real_result = real(a) + x
    print *, "Mixed arithmetic:", real_result
end program arithmetic
