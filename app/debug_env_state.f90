program debug_env_state
  use preprocessor, only: preprocess_file
  implicit none
  
  character(len=256) :: error_msg
  
  ! Test 1: Simple assignment to see type inference
  open(10, file='test_simple.f', status='replace')
  write(10, '(a)') 'x = 2.0'
  write(10, '(a)') 'y = x * x'
  close(10)
  
  print *, "Test 1: Simple variable inference"
  call preprocess_file('test_simple.f', 'test_simple_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    call system('grep ":: x\|:: y" test_simple_out.f90')
  end if
  call system('rm -f test_simple.f test_simple_out.f90')
  
  ! Test 2: Function with val * val
  open(10, file='test_expr.f', status='replace')
  write(10, '(a)') 'function square(val)'
  write(10, '(a)') '  temp = val * val'
  write(10, '(a)') '  square = temp'
  write(10, '(a)') 'end function'
  close(10)
  
  print *, ""
  print *, "Test 2: Function with intermediate variable"
  call preprocess_file('test_expr.f', 'test_expr_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    call system('grep ":: val\|:: temp\|:: square" test_expr_out.f90')
  end if
  call system('rm -f test_expr.f test_expr_out.f90')
  
  ! Test 3: Direct assignment with literal
  open(10, file='test_direct.f', status='replace')
  write(10, '(a)') 'function cube(x)'
  write(10, '(a)') '  cube = 2.0'
  write(10, '(a)') 'end function'
  close(10)
  
  print *, ""
  print *, "Test 3: Direct assignment with literal"
  call preprocess_file('test_direct.f', 'test_direct_out.f90', error_msg)
  if (len_trim(error_msg) == 0) then
    call system('grep ":: x\|:: cube" test_direct_out.f90')
  end if
  call system('rm -f test_direct.f test_direct_out.f90')
  
end program debug_env_state