program debug_explicit_decl
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test: Function with explicit parameter declaration
  open(10, file='test_explicit.f', status='replace')
  write(10, '(a)') 'real function square(x)'
  write(10, '(a)') '  real :: x'
  write(10, '(a)') '  square = x * x'
  write(10, '(a)') 'end function'
  close(10)
  
  print *, "Testing function with explicit parameter declaration:"
  call preprocess_file('test_explicit.f', 'test_explicit_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, "Output:"
    call system('cat -n test_explicit_out.f90 | grep -v "^$"')
    print *, ""
    print *, "Variable declarations:"
    call system('grep -E "real.*:: x" test_explicit_out.f90')
  else
    print *, "Error: ", trim(error_msg)
  end if
  
  call system('rm -f test_explicit.f test_explicit_out.f90')
  
end program debug_explicit_decl