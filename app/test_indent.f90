program test_indent
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test with 4-space indent (like the failing test)
  open(10, file='test_4space.f', status='replace')
  write(10, '(a)') 'function square(val)'
  write(10, '(a)') '    square = val * val'  ! 4 spaces
  write(10, '(a)') 'end function'
  close(10)
  
  print *, "Test with 4-space indent:"
  call preprocess_file('test_4space.f', 'test_4space_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    call system('grep ":: square" test_4space_out.f90 || echo "square NOT declared"')
    call system('grep ":: val" test_4space_out.f90 || echo "val NOT declared"')
  end if
  call system('rm -f test_4space.f test_4space_out.f90')
  
  ! Test with 2-space indent
  open(10, file='test_2space.f', status='replace')
  write(10, '(a)') 'function square2(val)'
  write(10, '(a)') '  square2 = val * val'  ! 2 spaces
  write(10, '(a)') 'end function'
  close(10)
  
  print *, ""
  print *, "Test with 2-space indent:"
  call preprocess_file('test_2space.f', 'test_2space_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    call system('grep ":: square2" test_2space_out.f90 || echo "square2 NOT declared"')
    call system('grep ":: val" test_2space_out.f90 || echo "val NOT declared"')
  end if
  call system('rm -f test_2space.f test_2space_out.f90')
  
end program test_indent