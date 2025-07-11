module shared_utils
  implicit none
  private
  public :: compute_square
contains
  pure function compute_square(x) result(y)
    real, intent(in) :: x
    real :: y
    y = x * x
  end function compute_square
end module shared_utils
