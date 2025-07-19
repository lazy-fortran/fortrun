! Demo: Explicit parameter type declarations (parsed from source)
! This demonstrates the parser's ability to handle explicit type
! declarations in function/subroutine parameter lists

function compute(real(8), intent(in) :: x, y, integer :: n)
    real(8) :: compute
    compute = (x + y)*n
end function

subroutine display(character(*), intent(in) :: msg, real(8) :: value)
    print *, trim(msg), value
end subroutine

program demo
    implicit none
    real(8) :: result

    result = compute(1.5d0, 2.5d0, 3)
    call display("Result = ", result)
end program
