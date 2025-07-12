program test_step1_explicit
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test from the failing test case
  open(10, file='test_step1.f', status='replace')
  write(10, '(a)') 'result = square(5.0)'
  write(10, '(a)') 'print *, "Square of 5.0 is:", result'
  write(10, '(a)') ''
  write(10, '(a)') 'real function square(x)'
  write(10, '(a)') '  real :: x'
  write(10, '(a)') '  square = x * x'
  write(10, '(a)') 'end function'
  close(10)
  
  print *, "Preprocessing step1 example:"
  call preprocess_file('test_step1.f', 'test_step1_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, "Output:"
    call system('cat -n test_step1_out.f90')
  else
    print *, "Error: ", trim(error_msg)
  end if
  
  call system('rm -f test_step1.f test_step1_out.f90')
  
end program test_step1_explicit