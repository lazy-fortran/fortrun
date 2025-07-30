program functions
    ! Demonstrate function definitions and calls
    integer :: result
    real :: x = 3.0
    real :: area

    ! Call integer function
    result = add_integers(5, 7)
    print *, "Sum:", result

    ! Call real function
    area = circle_area(x)
    print *, "Circle area:", area

contains

    integer function add_integers(a, b)
        integer, intent(in) :: a, b
        add_integers = a + b
    end function add_integers

    real function circle_area(radius)
        real, intent(in) :: radius
        real, parameter :: pi = 3.14159
        circle_area = pi*radius*radius
    end function circle_area

end program functions
