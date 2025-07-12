program debug_func_type_check
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test 1: Check if "function square" is being enhanced to typed function
  open(10, file='test_func.f', status='replace')
  write(10, '(a)') 'function square(x)'
  write(10, '(a)') '  square = x * x'
  write(10, '(a)') 'end function'
  close(10)
  
  print *, "Checking function enhancement:"
  call preprocess_file('test_func.f', 'test_func_out.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, "First line of function:"
    call system('grep -n "function square" test_func_out.f90')
    print *, ""
    print *, "All declarations in function:"
    call system('sed -n "/function square/,/end function/p" test_func_out.f90 | grep "::"')
  else
    print *, "Error: ", trim(error_msg)
  end if
  
  call system('rm -f test_func.f test_func_out.f90')
  
end program debug_func_type_check