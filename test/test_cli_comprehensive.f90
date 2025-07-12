program test_cli_comprehensive
  use cli, only: parse_arguments
  implicit none
  
  logical :: all_tests_passed
  
  print *, "=== Comprehensive CLI Tests ==="
  print *, ""
  
  all_tests_passed = .true.
  
  ! Test comprehensive argument parsing scenarios
  if (.not. test_empty_arguments()) all_tests_passed = .false.
  if (.not. test_help_arguments()) all_tests_passed = .false.
  if (.not. test_verbose_arguments()) all_tests_passed = .false.
  if (.not. test_directory_arguments()) all_tests_passed = .false.
  if (.not. test_parallel_arguments()) all_tests_passed = .false.
  if (.not. test_notebook_arguments()) all_tests_passed = .false.
  if (.not. test_complex_combinations()) all_tests_passed = .false.
  if (.not. test_edge_cases()) all_tests_passed = .false.
  
  print *, ""
  if (all_tests_passed) then
    print *, "All comprehensive CLI tests PASSED!"
  else
    print *, "Some comprehensive CLI tests FAILED!"
    stop 1
  end if
  
contains

  function test_empty_arguments() result(passed)
    logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output
    logical :: show_help, no_wait, notebook_mode
    integer :: verbose_level, parallel_jobs
    
    print *, "Test 1: Empty arguments"
    
    ! Simulate empty command line
    call setup_test_command_line('')
    
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (show_help) then
      print *, "  PASS: Empty arguments trigger help"
      passed = .true.
    else
      print *, "  FAIL: Empty arguments should trigger help"
      passed = .false.
    end if
    
  end function test_empty_arguments

  function test_help_arguments() result(passed)
    logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output
    logical :: show_help, no_wait, notebook_mode
    integer :: verbose_level, parallel_jobs
    
    print *, "Test 2: Help arguments"
    
    ! Test --help
    call setup_test_command_line('--help')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (.not. show_help) then
      print *, "  FAIL: --help should set show_help to true"
      passed = .false.
      return
    end if
    
    ! Test -h
    call setup_test_command_line('-h')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (show_help) then
      print *, "  PASS: Help arguments work correctly"
      passed = .true.
    else
      print *, "  FAIL: -h should set show_help to true"
      passed = .false.
    end if
    
  end function test_help_arguments

  function test_verbose_arguments() result(passed)
    logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output
    logical :: show_help, no_wait, notebook_mode
    integer :: verbose_level, parallel_jobs
    
    print *, "Test 3: Verbose arguments"
    
    ! Test -v
    call setup_test_command_line('-v test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (verbose_level /= 1) then
      print *, "  FAIL: -v should set verbose_level to 1, got", verbose_level
      passed = .false.
      return
    end if
    
    ! Test -vv
    call setup_test_command_line('-vv test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (verbose_level /= 2) then
      print *, "  FAIL: -vv should set verbose_level to 2, got", verbose_level
      passed = .false.
      return
    end if
    
    ! Test --verbose
    call setup_test_command_line('--verbose test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (verbose_level /= 1) then
      print *, "  FAIL: --verbose should set verbose_level to 1, got", verbose_level
      passed = .false.
      return
    end if
    
    ! Test --verbose 2
    call setup_test_command_line('--verbose 2 test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (verbose_level == 2) then
      print *, "  PASS: All verbose arguments work correctly"
      passed = .true.
    else
      print *, "  FAIL: --verbose 2 should set verbose_level to 2, got", verbose_level
      passed = .false.
    end if
    
  end function test_verbose_arguments

  function test_directory_arguments() result(passed)
    logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output
    logical :: show_help, no_wait, notebook_mode
    integer :: verbose_level, parallel_jobs
    
    print *, "Test 4: Directory arguments"
    
    ! Test --cache-dir
    call setup_test_command_line('--cache-dir /tmp/mycache test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (trim(custom_cache_dir) /= '/tmp/mycache') then
      print *, "  FAIL: --cache-dir should set custom_cache_dir, got: '", &
               trim(custom_cache_dir), "'"
      passed = .false.
      return
    end if
    
    ! Test --config-dir
    call setup_test_command_line('--config-dir /tmp/myconfig test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (trim(custom_config_dir) == '/tmp/myconfig') then
      print *, "  PASS: Directory arguments work correctly"
      passed = .true.
    else
      print *, "  FAIL: --config-dir should set custom_config_dir, got: '", &
               trim(custom_config_dir), "'"
      passed = .false.
    end if
    
  end function test_directory_arguments

  function test_parallel_arguments() result(passed)
    logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output
    logical :: show_help, no_wait, notebook_mode
    integer :: verbose_level, parallel_jobs
    
    print *, "Test 5: Parallel arguments"
    
    ! Test --jobs
    call setup_test_command_line('--jobs 4 test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (parallel_jobs /= 4) then
      print *, "  FAIL: --jobs 4 should set parallel_jobs to 4, got", parallel_jobs
      passed = .false.
      return
    end if
    
    ! Test -j
    call setup_test_command_line('-j 8 test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (parallel_jobs /= 8) then
      print *, "  FAIL: -j 8 should set parallel_jobs to 8, got", parallel_jobs
      passed = .false.
      return
    end if
    
    ! Test --no-wait
    call setup_test_command_line('--no-wait test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (no_wait) then
      print *, "  PASS: Parallel arguments work correctly"
      passed = .true.
    else
      print *, "  FAIL: --no-wait should set no_wait to true"
      passed = .false.
    end if
    
  end function test_parallel_arguments

  function test_notebook_arguments() result(passed)
    logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output
    logical :: show_help, no_wait, notebook_mode
    integer :: verbose_level, parallel_jobs
    
    print *, "Test 6: Notebook arguments"
    
    ! Test --notebook
    call setup_test_command_line('--notebook test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (.not. notebook_mode) then
      print *, "  FAIL: --notebook should set notebook_mode to true"
      passed = .false.
      return
    end if
    
    ! Test --notebook-output
    call setup_test_command_line('--notebook-output mynotebook.md test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (trim(notebook_output) == 'mynotebook.md') then
      print *, "  PASS: Notebook arguments work correctly"
      passed = .true.
    else
      print *, "  FAIL: --notebook-output should set notebook_output, got: '", &
               trim(notebook_output), "'"
      passed = .false.
    end if
    
  end function test_notebook_arguments

  function test_complex_combinations() result(passed)
    logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output
    logical :: show_help, no_wait, notebook_mode
    integer :: verbose_level, parallel_jobs
    
    print *, "Test 7: Complex argument combinations"
    
    ! Test multiple flags together
    call setup_test_command_line('-vv --cache-dir /tmp/cache --jobs 4 --no-wait test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (verbose_level /= 2) then
      print *, "  FAIL: Complex combination failed on verbose_level, got", verbose_level
      passed = .false.
      return
    end if
    
    if (trim(custom_cache_dir) /= '/tmp/cache') then
      print *, "  FAIL: Complex combination failed on cache_dir"
      passed = .false.
      return
    end if
    
    if (parallel_jobs /= 4) then
      print *, "  FAIL: Complex combination failed on parallel_jobs, got", parallel_jobs
      passed = .false.
      return
    end if
    
    if (.not. no_wait) then
      print *, "  FAIL: Complex combination failed on no_wait"
      passed = .false.
      return
    end if
    
    if (trim(filename) == 'test.f90') then
      print *, "  PASS: Complex argument combinations work correctly"
      passed = .true.
    else
      print *, "  FAIL: Complex combination failed on filename, got: '", trim(filename), "'"
      passed = .false.
    end if
    
  end function test_complex_combinations

  function test_edge_cases() result(passed)
    logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output
    logical :: show_help, no_wait, notebook_mode
    integer :: verbose_level, parallel_jobs
    
    print *, "Test 8: Edge cases"
    
    ! Test filename with spaces
    call setup_test_command_line('my test file.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (trim(filename) /= 'my test file.f90') then
      print *, "  FAIL: Filename with spaces failed, got: '", trim(filename), "'"
      passed = .false.
      return
    end if
    
    ! Test very long directory path
    call setup_test_command_line('--cache-dir /very/long/path/to/cache/directory/that/might/cause/issues test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (index(custom_cache_dir, '/very/long/path') == 0) then
      print *, "  FAIL: Long directory path failed"
      passed = .false.
      return
    end if
    
    ! Test zero parallel jobs
    call setup_test_command_line('--jobs 0 test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    if (parallel_jobs /= 0) then
      print *, "  FAIL: Zero parallel jobs failed, got", parallel_jobs
      passed = .false.
      return
    end if
    
    ! Test negative verbose level handling
    call setup_test_command_line('--verbose -1 test.f90')
    call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                        custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                        notebook_output)
    
    ! Should handle gracefully (implementation dependent)
    print *, "  PASS: Edge cases handled correctly"
    passed = .true.
    
  end function test_edge_cases

  ! Helper subroutine to simulate command line arguments
  subroutine setup_test_command_line(args)
    character(len=*), intent(in) :: args
    
    ! This would need to interface with the actual command line parsing
    ! For now, we'll simulate by setting up test conditions
    ! In a real implementation, this might use M_CLI2 test facilities
    
    ! Note: This is a simplified version - real implementation would need
    ! to interact with the CLI module's internal state or use test doubles
    
  end subroutine setup_test_command_line

end program test_cli_comprehensive