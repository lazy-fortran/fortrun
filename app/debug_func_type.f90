program debug_func_type
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Create simple untyped function
  open(10, file='test_untyped.f', status='replace')
  write(10, '(a)') 'x = 10'
  write(10, '(a)') 'y = double(x)'
  write(10, '(a)') ''
  write(10, '(a)') 'function double(n)'
  write(10, '(a)') '  double = 2 * n'
  write(10, '(a)') 'end function'
  close(10)
  
  ! Preprocess it
  print *, "Testing untyped function..."
  call preprocess_file('test_untyped.f', 'test_untyped_output.f90', error_msg)
  
  if (len_trim(error_msg) > 0) then
    print *, "Error: ", trim(error_msg)
  else
    print *, "Output:"
    call system('cat -n test_untyped_output.f90 | grep -E "(function double|:: double)"')
  end if
  
  ! Also test typed function
  open(10, file='test_typed.f', status='replace')
  write(10, '(a)') 'x = 10'
  write(10, '(a)') 'y = triple(x)'
  write(10, '(a)') ''
  write(10, '(a)') 'integer function triple(n)'
  write(10, '(a)') '  integer :: n'
  write(10, '(a)') '  triple = 3 * n'
  write(10, '(a)') 'end function'
  close(10)
  
  print *, ""
  print *, "Testing typed function..."
  call preprocess_file('test_typed.f', 'test_typed_output.f90', error_msg)
  
  if (len_trim(error_msg) == 0) then
    print *, "Output:"
    call system('cat -n test_typed_output.f90 | grep -E "(function triple|:: triple)"')
  end if
  
  ! Clean up
  call system('rm -f test_untyped.f test_untyped_output.f90 test_typed.f test_typed_output.f90')
  
end program debug_func_type