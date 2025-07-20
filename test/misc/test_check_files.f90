program test_check_files
    implicit none

    character(len=1024) :: cmd_output
    integer :: ios, unit

    ! List all .f and .f90 files in current directory
    print '(a)', 'Files before test:'
  call execute_command_line('ls -la *.f *.f90 2>/dev/null || echo "No files found"', wait=.true.)

    ! Run the preprocessor test
    print '(a)', ''
    print '(a)', 'Running preprocessor test...'
  call execute_command_line('fpm test test_preprocessor 2>&1 | grep -E "(FAIL|Wrong|program statements)"', wait=.true.)

    print '(a)', ''
    print '(a)', 'Files after test:'
  call execute_command_line('ls -la *.f *.f90 2>/dev/null || echo "No files found"', wait=.true.)

    stop 0
end program test_check_files
