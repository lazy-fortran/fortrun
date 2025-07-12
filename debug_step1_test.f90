program main
  implicit none
  
  ! Auto-generated variable declarations:
  integer(4) :: y
  
  y = compute(3, 4)

contains
integer(4) function compute(a, b)
  implicit none
  
  ! Auto-generated variable declarations:
  
integer(4), intent(in) :: a, b
  compute = a + b
end function
end program main
