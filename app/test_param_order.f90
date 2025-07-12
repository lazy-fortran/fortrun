program test_param_order
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test: Function using parameter in expression
  open(10, file='test_param.f', status='replace')
  write(10, '(a)') 'function double(x)'
  write(10, '(a)') '  double = x * 2'
  write(10, '(a)') 'end function'
  close(10)
  
  print *, "Function using parameter:"
  call preprocess_file('test_param.f', 'test_param_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    print *, "Checking declarations:"
    call system('grep -A10 "Auto-generated" test_param_out.f90 | tail -9 | grep "::"')
  end if
  call system('rm -f test_param.f test_param_out.f90')
  
  ! Test: Function using literal then parameter
  open(10, file='test_mixed.f', status='replace')
  write(10, '(a)') 'function mixed(x)'
  write(10, '(a)') '  temp = 2'      ! Literal first
  write(10, '(a)') '  mixed = x * temp'  ! Then use parameter
  write(10, '(a)') 'end function'
  close(10)
  
  print *, ""
  print *, "Function with literal first:"
  call preprocess_file('test_mixed.f', 'test_mixed_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    print *, "Checking declarations:"
    call system('grep -A10 "Auto-generated" test_mixed_out.f90 | tail -9 | grep "::"')
  end if
  call system('rm -f test_mixed.f test_mixed_out.f90')
  
end program test_param_order