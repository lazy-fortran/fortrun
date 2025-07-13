program test_preprocessor_integration
  implicit none
  
  integer :: test_count, pass_count
  character(len=1024) :: output
  integer :: exit_status
  
  test_count = 0
  pass_count = 0
  
  print '(a)', 'Testing preprocessor integration...'
  
  ! Test 1: Run simple .f file
  test_count = test_count + 1
  call execute_command_line('fpm run fortran -- example/preprocessor/hello.f', &
                           exitstat=exit_status)
  if (exit_status == 0) then
    pass_count = pass_count + 1
    print '(a)', 'PASS: hello.f runs successfully'
  else
    print '(a)', 'FAIL: hello.f failed to run'
  end if
  
  ! Test 2: Run .f file with functions
  test_count = test_count + 1
  call execute_command_line('fpm run fortran -- example/preprocessor/math.f', &
                           exitstat=exit_status)
  if (exit_status == 0) then
    pass_count = pass_count + 1
    print '(a)', 'PASS: math.f runs successfully'
  else
    print '(a)', 'FAIL: math.f failed to run'
  end if
  
  ! Test 3: Run .f file with subroutines
  test_count = test_count + 1
  call execute_command_line('fpm run fortran -- example/preprocessor/subroutines.f', &
                           exitstat=exit_status)
  if (exit_status == 0) then
    pass_count = pass_count + 1
    print '(a)', 'PASS: subroutines.f runs successfully'
  else
    print '(a)', 'FAIL: subroutines.f failed to run'
  end if
  
  ! Test 4: Verbose mode shows preprocessing message
  test_count = test_count + 1
  call execute_command_line('fpm run fortran -- -v example/preprocessor/hello.f ' // &
                           '2>&1 | grep -q "Preprocessing"', exitstat=exit_status)
  if (exit_status == 0) then
    pass_count = pass_count + 1
    print '(a)', 'PASS: Verbose mode shows preprocessing message'
  else
    print '(a)', 'FAIL: Verbose mode should show preprocessing message'
  end if
  
  ! Test 5: Regular .f90 files still work
  test_count = test_count + 1
  call execute_command_line('fpm run fortran -- example/hello/hello.f90', &
                           exitstat=exit_status)
  if (exit_status == 0) then
    pass_count = pass_count + 1
    print '(a)', 'PASS: Regular .f90 files still work'
  else
    print '(a)', 'FAIL: Regular .f90 files should still work'
  end if
  
  print '(a)', ''
  print '(a,i0,a,i0,a)', 'Preprocessor integration tests: ', pass_count, '/', test_count, ' passed'
  
  if (pass_count /= test_count) then
    error stop 'Some integration tests failed!'
  end if
  
end program test_preprocessor_integration