program test_subroutine_intent
  implicit none
  real(8) :: x, y, result
  
  x = 3.0d0
  y = 4.0d0
  
  call my_subroutine(x, y, result)
  print *, 'Result:', result
  
contains
  
  subroutine my_subroutine(a, b, c)
    real(8) :: a, b, c
    c = a + b
  end subroutine my_subroutine
  
end program test_subroutine_intent