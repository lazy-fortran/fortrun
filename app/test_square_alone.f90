program test_square_alone
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test just the function alone
  open(10, file='test_square_alone.f', status='replace')
  write(10, '(a)') 'function square(val)'
  write(10, '(a)') '    square = val * val'
  write(10, '(a)') 'end function'
  close(10)
  
  print *, "Testing function alone:"
  call preprocess_file('test_square_alone.f', 'test_square_alone_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, "Success! Checking output:"
    call system('cat -n test_square_alone_out.f90')
    print *, ""
    print *, "Checking for square declaration:"
    call system('grep ":: square" test_square_alone_out.f90 || echo "NOT FOUND"')
  else
    print *, "Error: ", trim(error_msg)
  end if
  
  ! Also run it through the fortran command to see what happens
  print *, ""
  print *, "Running through fortran command:"
  call system('fpm run fortran -- test_square_alone.f 2>&1 | grep -E ":: square|:: val" || echo "Declarations not shown"')
  
  call system('rm -f test_square_alone.f test_square_alone_out.f90')
  
end program test_square_alone