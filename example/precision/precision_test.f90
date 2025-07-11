program precision_test
  implicit none
  real :: x, y
  
  x = 1.0 / 3.0
  y = x * 3.0
  
  print '(a,f20.15)', 'x = 1/3 = ', x
  print '(a,f20.15)', 'y = x*3 = ', y
  print '(a,i0)', 'Precision digits: ', precision(x)
  
end program precision_test