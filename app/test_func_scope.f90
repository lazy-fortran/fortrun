program test_func_scope
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test 1: Simple literal assignment in function
  open(10, file='test1.f', status='replace')
  write(10, '(a)') 'function test1()'
  write(10, '(a)') '  x = 42'
  write(10, '(a)') '  test1 = x'
  write(10, '(a)') 'end function'
  close(10)
  
  print *, "Test 1: Literal assignment in function"
  call preprocess_file('test1.f', 'test1_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    call system('grep -A5 "Auto-generated" test1_out.f90 | grep -v "Auto-generated"')
  end if
  call system('rm -f test1.f test1_out.f90')
  
  ! Test 2: Multiple variables
  open(10, file='test2.f', status='replace')
  write(10, '(a)') 'function test2(n)'
  write(10, '(a)') '  a = 1.0'
  write(10, '(a)') '  b = 2.0'
  write(10, '(a)') '  test2 = a + b'
  write(10, '(a)') 'end function'
  close(10)
  
  print *, ""
  print *, "Test 2: Multiple variables in function"
  call preprocess_file('test2.f', 'test2_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    call system('grep -A8 "Auto-generated" test2_out.f90 | tail -7 | grep "::"')
  end if
  call system('rm -f test2.f test2_out.f90')
  
end program test_func_scope