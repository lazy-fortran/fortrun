program debug_math_fix
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test the exact structure of math.f
  open(10, file='test_math_structure.f', status='replace')
  write(10, '(a)') '! Math example with functions but no contains statement'
  write(10, '(a)') 'real :: x, y, result'
  write(10, '(a)') ''
  write(10, '(a)') 'x = 5.0'
  write(10, '(a)') 'y = 3.0'
  write(10, '(a)') ''
  write(10, '(a)') 'result = add(x, y)'
  write(10, '(a)') 'print *, result'
  close(10)
  
  print *, "Testing math.f structure:"
  call preprocess_file('test_math_structure.f', 'test_math_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, "Output:"
    call system('cat -n test_math_out.f90')
    print *, ""
    print *, "Checking for duplicate declarations:"
    call system('grep ":: x" test_math_out.f90 | nl')
    call system('grep ":: result" test_math_out.f90 | nl')
  else
    print *, "Error: ", trim(error_msg)
  end if
  
  call system('rm -f test_math_structure.f test_math_out.f90')
  
end program debug_math_fix