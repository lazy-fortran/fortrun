program debug_func_vars
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Create the exact test case from the failing test
  open(10, file='test_func_param.f', status='replace')
  write(10, '(a)') 'x = 5.0'
  write(10, '(a)') 'y = square(x)'
  write(10, '(a)') ''
  write(10, '(a)') 'function square(val)'
  write(10, '(a)') '    square = val * val'
  write(10, '(a)') 'end function'
  close(10)
  
  call preprocess_file('test_func_param.f', 'test_func_param_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, "Full output:"
    call system('cat -n test_func_param_out.f90')
    print *, ""
    print *, "Expected: 'real(8) :: square' in function body"
    print *, "Actual:"
    call system('grep -n ":: square" test_func_param_out.f90 || echo "NO square declaration found"')
  else
    print *, "Error: ", trim(error_msg)
  end if
  
  call system('rm -f test_func_param.f test_func_param_out.f90')
  
end program debug_func_vars