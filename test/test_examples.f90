program test_examples
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  
  character(len=256), dimension(:), allocatable :: example_files
  character(len=1024) :: output
  integer :: n_examples, i, exit_code
  integer :: n_passed, n_failed
  logical :: file_exists
  
  ! List of example files to test
  n_examples = 6
  allocate(example_files(n_examples))
  example_files(1) = 'example/hello/hello.f90'
  example_files(2) = 'example/calculator/calculator.f90'
  example_files(3) = 'example/precision/precision_test.f90'
  example_files(4) = 'example/precision/precision_compare.f90'
  example_files(5) = 'example/precision/real_default_test.f90'
  example_files(6) = 'example/interdependent/main.f90'
  
  n_passed = 0
  n_failed = 0
  
  print '(a)', '='//repeat('=', 60)
  print '(a)', 'Running Fortran CLI Example Tests'
  print '(a)', '='//repeat('=', 60)
  print *
  
  do i = 1, n_examples
    ! Check if file exists
    inquire(file=example_files(i), exist=file_exists)
    
    if (.not. file_exists) then
      print '(a,a,a)', 'SKIP: ', trim(example_files(i)), ' (file not found)'
      cycle
    end if
    
    ! Run the example
    print '(a,a,a)', 'Running: ', trim(example_files(i)), '...'
    call run_example(example_files(i), output, exit_code)
    
    if (exit_code == 0) then
      print '(a,a,a)', '  ✓ PASS: ', trim(example_files(i)), ' (exit code 0)'
      n_passed = n_passed + 1
      
      ! Show output for specific examples
      select case (trim(example_files(i)))
      case ('example/hello/hello.f90')
        if (index(output, 'Hello from fortran CLI!') == 0) then
          print '(a)', '    WARNING: Expected output not found'
        end if
      case ('example/precision/real_default_test.f90')
        if (index(output, 'sizeof(real) =                     8  bytes') > 0) then
          print '(a)', '    ✓ Double precision default confirmed'
        else
          print '(a)', '    WARNING: Double precision not working as expected'
        end if
      case ('example/interdependent/main.f90')
        if (index(output, 'Cylinder Calculations') > 0) then
          print '(a)', '    ✓ Interdependent modules working correctly'
        else
          print '(a)', '    WARNING: Interdependent modules may not be working'
        end if
      end select
    else
      print '(a,a,a,i0,a)', '  ✗ FAIL: ', trim(example_files(i)), &
                             ' (exit code ', exit_code, ')'
      n_failed = n_failed + 1
      ! Show error output
      if (len_trim(output) > 0) then
        print '(a)', '    Error output:'
        print '(a,a)', '    ', trim(output)
      end if
    end if
    print *
  end do
  
  ! Test incremental compilation (caching)
  call test_incremental_compilation(n_passed, n_failed)
  
  ! Summary
  print '(a)', '='//repeat('=', 60)
  print '(a)', 'Test Summary'
  print '(a)', '='//repeat('=', 60)
  print '(a,i0)', 'Total examples: ', n_examples
  print '(a,i0)', 'Passed: ', n_passed
  print '(a,i0)', 'Failed: ', n_failed
  print '(a,i0)', 'Skipped: ', n_examples - n_passed - n_failed
  
  if (n_failed > 0) then
    print *
    print '(a)', 'OVERALL: FAILED'
    stop 1
  else
    print *
    print '(a)', 'OVERALL: PASSED'
  end if
  
contains

  subroutine run_example(filename, output, exit_code)
    character(len=*), intent(in) :: filename
    character(len=*), intent(out) :: output
    integer, intent(out) :: exit_code
    
    character(len=512) :: command
    character(len=256) :: cache_pattern
    integer :: unit, iostat, j
    character(len=1024) :: line
    
    ! Extract base name for cache cleanup
    j = index(filename, '/', back=.true.)
    if (j > 0) then
      cache_pattern = filename(j+1:)
    else
      cache_pattern = filename
    end if
    j = index(cache_pattern, '.f90')
    if (j > 0) then
      cache_pattern = cache_pattern(1:j-1)
    end if
    
    ! Clean cache for this example
    command = 'rm -rf ~/.cache/fortran/' // trim(cache_pattern) // '_*'
    call execute_command_line(trim(command))
    
    ! Run the example
    command = './build/gfortran_*/app/fortran ' // trim(filename) // &
              ' > test_output.tmp 2>&1'
    call execute_command_line(trim(command), exitstat=exit_code)
    
    ! Read output
    output = ''
    open(newunit=unit, file='test_output.tmp', status='old', iostat=iostat)
    if (iostat == 0) then
      do
        read(unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit
        if (len_trim(output) > 0) then
          output = trim(output) // ' | ' // trim(adjustl(line))
        else
          output = trim(adjustl(line))
        end if
      end do
      close(unit)
    end if
    
    ! Extract just the program output (after FPM messages)
    j = index(output, '[100%] Project compiled successfully.')
    if (j > 0) then
      output = output(j+37:)  ! Skip the FPM message
    end if
    
    ! Clean up
    call execute_command_line('rm -f test_output.tmp')
    
  end subroutine run_example
  
  subroutine test_incremental_compilation(n_passed, n_failed)
    integer, intent(inout) :: n_passed, n_failed
    character(len=1024) :: output1, output2
    integer :: exit_code1, exit_code2
    character(len=*), parameter :: test_file = 'example/hello/hello.f90'
    character(len=256) :: temp_cache_dir
    character(len=512) :: cleanup_command
    
    print '(a)', '='//repeat('=', 60)
    print '(a)', 'Testing Incremental Compilation (Caching)'
    print '(a)', '='//repeat('=', 60)
    print *
    
    ! Create temporary cache directory
    temp_cache_dir = '/tmp/fortran_test_cache_' // get_test_timestamp()
    print '(a,a)', 'Using temporary cache: ', trim(temp_cache_dir)
    
    ! First run - should compile
    print '(a)', 'First run (should compile)...'
    call run_example_with_cache(test_file, temp_cache_dir, output1, exit_code1)
    
    if (exit_code1 /= 0) then
      print '(a)', '  ✗ FAIL: First run failed'
      print '(a,a)', '    Output: ', trim(output1)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Check that compilation occurred
    if (index(output1, '[  0%]') > 0 .and. index(output1, '[100%]') > 0) then
      print '(a)', '  ✓ First run compiled files as expected'
    else
      print '(a)', '  ✗ FAIL: Expected compilation messages not found in first run'
      print '(a,a)', '    Output: ', trim(output1)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Verify cache miss message
    if (index(output1, 'Cache miss: Setting up new build') > 0) then
      print '(a)', '  ✓ Cache miss detected on first run'
    else
      print '(a)', '  ✗ FAIL: Cache miss not detected on first run'
      print '(a,a)', '    Output: ', trim(output1)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Second run - should use cache
    print '(a)', 'Second run (should use cache)...'
    call run_example_with_cache(test_file, temp_cache_dir, output2, exit_code2)
    
    if (exit_code2 /= 0) then
      print '(a)', '  ✗ FAIL: Second run failed'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Check that cache was hit
    if (index(output2, 'Cache hit: Using existing build') > 0) then
      print '(a)', '  ✓ Cache hit detected on second run'
    else
      print '(a)', '  ✗ FAIL: Cache hit not detected on second run'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Check that FPM detected no compilation needed
    if (index(output2, 'Project is up to date') > 0) then
      print '(a)', '  ✓ FPM detected no compilation needed'
    else
      print '(a)', '  ✗ FAIL: FMP "Project is up to date" message not found'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Verify no compilation percentages in second run
    if (index(output2, '[  0%]') == 0 .and. index(output2, '[100%]') == 0) then
      print '(a)', '  ✓ No compilation occurred on second run'
    else
      print '(a)', '  ✗ FAIL: Compilation occurred on second run (no caching)'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    print '(a)', '  ✓ PASS: Incremental compilation working correctly'
    n_passed = n_passed + 1
    
999 continue
    ! Clean up temporary cache directory
    cleanup_command = 'rm -rf "' // trim(temp_cache_dir) // '"'
    call execute_command_line(trim(cleanup_command))
    print '(a)', 'Cleaned up temporary cache directory'
    print *
    
  end subroutine test_incremental_compilation
  
  subroutine run_example_with_cache(filename, cache_dir, output, exit_code)
    character(len=*), intent(in) :: filename, cache_dir
    character(len=*), intent(out) :: output
    integer, intent(out) :: exit_code
    
    character(len=512) :: command
    integer :: unit, iostat
    character(len=1024) :: line
    
    ! Run with verbose flag and custom cache directory
    command = './build/gfortran_*/app/fortran -v --cache-dir "' // trim(cache_dir) // &
              '" ' // trim(filename) // ' > test_cache_output.tmp 2>&1'
    call execute_command_line(trim(command), exitstat=exit_code)
    
    ! Read full output including verbose messages
    output = ''
    open(newunit=unit, file='test_cache_output.tmp', status='old', iostat=iostat)
    if (iostat == 0) then
      do
        read(unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit
        if (len_trim(output) > 0) then
          output = trim(output) // ' | ' // trim(adjustl(line))
        else
          output = trim(adjustl(line))
        end if
      end do
      close(unit)
    end if
    
    ! Clean up
    call execute_command_line('rm -f test_cache_output.tmp')
    
  end subroutine run_example_with_cache
  
  function get_test_timestamp() result(timestamp)
    character(len=16) :: timestamp
    integer :: time_values(8)
    
    call date_and_time(values=time_values)
    write(timestamp, '(i4.4,i2.2,i2.2,a1,i2.2,i2.2,i2.2)') &
          time_values(1), time_values(2), time_values(3), '_', &
          time_values(5), time_values(6), time_values(7)
          
  end function get_test_timestamp
  
end program test_examples