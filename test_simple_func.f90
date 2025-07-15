program main
    implicit none
contains
    real(8) function add_one(x)
    implicit none
    real(8), intent(in) :: x
    real(8) :: x
    add_one = x + 1.0d0
end function add_one
end program main
