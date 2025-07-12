program debug_func_param_inference
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test from the failing test case
  open(10, file='test_func_param.f', status='replace')
  write(10, '(a)') 'x = 5.0'
  write(10, '(a)') 'y = square(x)'
  write(10, '(a)') ''
  write(10, '(a)') 'function square(val)'
  write(10, '(a)') '    square = val * val'
  write(10, '(a)') 'end function'
  close(10)
  
  print *, "Testing function parameter type inference:"
  call preprocess_file('test_func_param.f', 'test_func_param_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, "Output:"
    call system('cat -n test_func_param_out.f90')
    print *, ""
    print *, "Checking for expected declarations:"
    print *, "Looking for 'real(8), intent(in) :: val':"
    call system('grep "intent.*val" test_func_param_out.f90 || echo "  NOT FOUND"')
    print *, "Looking for 'real(8) :: square':"
    call system('grep "real.*:: square" test_func_param_out.f90 || echo "  NOT FOUND"')
  else
    print *, "Error: ", trim(error_msg)
  end if
  
  call system('rm -f test_func_param.f test_func_param_out.f90')
  
end program debug_func_param_inference