program test_return_type
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test from the failing test case
  open(10, file='test_logical.f', status='replace')
  write(10, '(a)') 'result = is_positive(3.14)'
  write(10, '(a)') ''
  write(10, '(a)') 'function is_positive(x)'
  write(10, '(a)') '    is_positive = x > 0'
  write(10, '(a)') 'end function'
  close(10)
  
  call preprocess_file('test_logical.f', 'test_logical_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, "Output:"
    call system('cat -n test_logical_out.f90')
    print *, ""
    print *, "Expected: 'logical :: is_positive'"
    print *, "Actual:"
    call system('grep ":: is_positive" test_logical_out.f90')
  else
    print *, "Error: ", trim(error_msg)
  end if
  
  call system('rm -f test_logical.f test_logical_out.f90')
  
end program test_return_type