program test_first_assign
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test 1: Function name is first assignment
  open(10, file='test1.f', status='replace')
  write(10, '(a)') 'function first(n)'
  write(10, '(a)') '  first = n * 2'  ! First and only assignment
  write(10, '(a)') 'end function'
  close(10)
  
  print *, "Test 1: Function name is first assignment"
  call preprocess_file('test1.f', 'test1_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    call system('grep ":: first" test1_out.f90 || echo "first NOT declared"')
  end if
  call system('rm -f test1.f test1_out.f90')
  
  ! Test 2: Other variable first, then function
  open(10, file='test2.f', status='replace')
  write(10, '(a)') 'function second(n)'
  write(10, '(a)') '  temp = n * 2'    ! Other variable first
  write(10, '(a)') '  second = temp'   ! Then function name
  write(10, '(a)') 'end function'
  close(10)
  
  print *, ""
  print *, "Test 2: Other variable first"
  call preprocess_file('test2.f', 'test2_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    call system('grep ":: temp" test2_out.f90 || echo "temp NOT declared"')
    call system('grep ":: second" test2_out.f90 || echo "second NOT declared"')
  end if
  call system('rm -f test2.f test2_out.f90')
  
end program test_first_assign