program test_runner_comprehensive
  use runner, only: run_fortran_file
  implicit none
  
  logical :: all_tests_passed
  
  print *, "=== Comprehensive Runner Tests ==="
  print *, ""
  
  ! Clear cache before running tests to avoid module conflicts
  call clear_test_cache()
  
  all_tests_passed = .true.
  
  ! Test 1: File not found error
  if (.not. test_file_not_found()) all_tests_passed = .false.
  
  ! Test 2: Invalid file extension
  if (.not. test_invalid_extension()) all_tests_passed = .false.
  
  ! Test 3: Basic .f90 file execution
  if (.not. test_basic_f90_execution()) all_tests_passed = .false.
  
  ! Test 4: .f file preprocessing
  if (.not. test_f_file_preprocessing()) all_tests_passed = .false.
  
  ! Test 5: Cache hit scenario
  if (.not. test_cache_hit()) all_tests_passed = .false.
  
  ! Test 6: Verbose modes
  if (.not. test_verbose_modes()) all_tests_passed = .false.
  
  ! Test 7: Custom cache directory
  if (.not. test_custom_cache_dir()) all_tests_passed = .false.
  
  ! Test 8: Custom config directory
  if (.not. test_custom_config_dir()) all_tests_passed = .false.
  
  ! Test 9: Parallel jobs flag
  if (.not. test_parallel_jobs()) all_tests_passed = .false.
  
  ! Test 10: No-wait locking
  if (.not. test_no_wait_locking()) all_tests_passed = .false.
  
  ! Test 11: Local modules
  if (.not. test_local_modules()) all_tests_passed = .false.
  
  ! Test 12: Error handling paths
  if (.not. test_error_paths()) all_tests_passed = .false.
  
  print *, ""
  if (all_tests_passed) then
    print *, "All runner tests PASSED!"
  else
    print *, "Some runner tests FAILED!"
    stop 1
  end if
  
contains

  subroutine clear_test_cache()
    ! Clear cache before running tests to avoid module conflicts
    call execute_command_line('rm -rf ~/.cache/fortran/*')
  end subroutine clear_test_cache

  function test_file_not_found() result(passed)
    logical :: passed
    integer :: exit_code
    
    print *, "Test 1: File not found error"
    
    call run_fortran_file('nonexistent_file.f90', exit_code, 0, '', '', 0, .false.)
    
    if (exit_code == 1) then
      print *, "  PASS: File not found error handled correctly"
      passed = .true.
    else
      print *, "  FAIL: Expected exit code 1, got ", exit_code
      passed = .false.
    end if
    
  end function test_file_not_found

  function test_invalid_extension() result(passed)
    logical :: passed
    integer :: exit_code
    character(len=256) :: test_file
    integer :: unit
    
    print *, "Test 2: Invalid file extension"
    
    ! Create a test file with invalid extension
    test_file = '/tmp/test_invalid.txt'
    open(newunit=unit, file=test_file)
    write(unit, '(a)') 'program test'
    write(unit, '(a)') 'print *, "hello"'
    write(unit, '(a)') 'end program'
    close(unit)
    
    call run_fortran_file(test_file, exit_code, 0, '', '', 0, .false.)
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_file))
    
    if (exit_code == 1) then
      print *, "  PASS: Invalid extension error handled correctly"
      passed = .true.
    else
      print *, "  FAIL: Expected exit code 1, got ", exit_code
      passed = .false.
    end if
    
  end function test_invalid_extension

  function test_basic_f90_execution() result(passed)
    logical :: passed
    integer :: exit_code
    character(len=256) :: test_file
    integer :: unit
    
    print *, "Test 3: Basic .f90 file execution"
    
    ! Create a simple test file
    test_file = '/tmp/test_basic.f90'
    open(newunit=unit, file=test_file)
    write(unit, '(a)') 'program test_basic'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "Hello from runner test"'
    write(unit, '(a)') 'end program test_basic'
    close(unit)
    
    call run_fortran_file(test_file, exit_code, 0, '', '', 0, .false.)
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_file))
    
    if (exit_code == 0) then
      print *, "  PASS: Basic .f90 execution successful"
      passed = .true.
    else
      print *, "  FAIL: Expected exit code 0, got ", exit_code
      passed = .false.
    end if
    
  end function test_basic_f90_execution

  function test_f_file_preprocessing() result(passed)
    logical :: passed
    integer :: exit_code
    character(len=256) :: test_file
    integer :: unit
    
    print *, "Test 4: .f file preprocessing"
    
    ! Create a .f file that needs preprocessing
    test_file = '/tmp/test_preprocess.f'
    open(newunit=unit, file=test_file)
    write(unit, '(a)') 'x = 42'
    write(unit, '(a)') 'print *, x'
    close(unit)
    
    call run_fortran_file(test_file, exit_code, 1, '', '', 0, .false.)
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_file))
    call execute_command_line('rm -f ' // trim(test_file) // '90') ! Remove preprocessed file
    
    if (exit_code == 0) then
      print *, "  PASS: .f file preprocessing successful"
      passed = .true.
    else
      print *, "  FAIL: Expected exit code 0, got ", exit_code
      passed = .false.
    end if
    
  end function test_f_file_preprocessing

  function test_cache_hit() result(passed)
    logical :: passed
    integer :: exit_code
    character(len=256) :: test_file, cache_dir
    integer :: unit
    
    print *, "Test 5: Cache hit scenario"
    
    ! Create test file and cache directory
    test_file = '/tmp/test_cache_hit.f90'
    cache_dir = '/tmp/test_runner_cache'
    
    open(newunit=unit, file=test_file)
    write(unit, '(a)') 'program test_cache'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "Cache test"'
    write(unit, '(a)') 'end program test_cache'
    close(unit)
    
    ! First run (cache miss)
    call run_fortran_file(test_file, exit_code, 1, cache_dir, '', 0, .false.)
    
    if (exit_code /= 0) then
      print *, "  FAIL: First run failed with exit code ", exit_code
      passed = .false.
      return
    end if
    
    ! Second run (should be cache hit)
    call run_fortran_file(test_file, exit_code, 1, cache_dir, '', 0, .false.)
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_file))
    call execute_command_line('rm -rf ' // trim(cache_dir))
    
    if (exit_code == 0) then
      print *, "  PASS: Cache hit scenario successful"
      passed = .true.
    else
      print *, "  FAIL: Cache hit failed with exit code ", exit_code
      passed = .false.
    end if
    
  end function test_cache_hit

  function test_verbose_modes() result(passed)
    logical :: passed
    integer :: exit_code
    character(len=256) :: test_file
    integer :: unit
    
    print *, "Test 6: Verbose modes"
    
    ! Create test file
    test_file = '/tmp/test_verbose.f90'
    open(newunit=unit, file=test_file)
    write(unit, '(a)') 'program test_verbose'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "Verbose test"'
    write(unit, '(a)') 'end program test_verbose'
    close(unit)
    
    ! Test verbose level 0 (quiet)
    call run_fortran_file(test_file, exit_code, 0, '', '', 0, .false.)
    if (exit_code /= 0) then
      print *, "  FAIL: Verbose level 0 failed"
      passed = .false.
      return
    end if
    
    ! Test verbose level 1 (normal)
    call run_fortran_file(test_file, exit_code, 1, '', '', 0, .false.)
    if (exit_code /= 0) then
      print *, "  FAIL: Verbose level 1 failed"
      passed = .false.
      return
    end if
    
    ! Test verbose level 2 (very verbose)
    call run_fortran_file(test_file, exit_code, 2, '', '', 0, .false.)
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_file))
    
    if (exit_code == 0) then
      print *, "  PASS: All verbose modes successful"
      passed = .true.
    else
      print *, "  FAIL: Verbose level 2 failed"
      passed = .false.
    end if
    
  end function test_verbose_modes

  function test_custom_cache_dir() result(passed)
    logical :: passed
    integer :: exit_code
    character(len=256) :: test_file, custom_cache
    integer :: unit
    
    print *, "Test 7: Custom cache directory"
    
    ! Create test file and custom cache directory
    test_file = '/tmp/test_custom_cache.f90'
    custom_cache = '/tmp/my_custom_cache'
    
    open(newunit=unit, file=test_file)
    write(unit, '(a)') 'program test_custom'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "Custom cache test"'
    write(unit, '(a)') 'end program test_custom'
    close(unit)
    
    call run_fortran_file(test_file, exit_code, 1, custom_cache, '', 0, .false.)
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_file))
    call execute_command_line('rm -rf ' // trim(custom_cache))
    
    if (exit_code == 0) then
      print *, "  PASS: Custom cache directory successful"
      passed = .true.
    else
      print *, "  FAIL: Custom cache directory failed with exit code ", exit_code
      passed = .false.
    end if
    
  end function test_custom_cache_dir

  function test_custom_config_dir() result(passed)
    logical :: passed
    integer :: exit_code
    character(len=256) :: test_file, custom_config
    integer :: unit
    
    print *, "Test 8: Custom config directory"
    
    ! Create test file and custom config directory
    test_file = '/tmp/test_custom_config.f90'
    custom_config = '/tmp/my_custom_config'
    
    ! Ensure custom config directory exists
    call execute_command_line('mkdir -p ' // trim(custom_config))
    
    open(newunit=unit, file=test_file)
    write(unit, '(a)') 'program test_config'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "Custom config test"'
    write(unit, '(a)') 'end program test_config'
    close(unit)
    
    call run_fortran_file(test_file, exit_code, 1, '', custom_config, 0, .false.)
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_file))
    call execute_command_line('rm -rf ' // trim(custom_config))
    
    if (exit_code == 0) then
      print *, "  PASS: Custom config directory successful"
      passed = .true.
    else
      print *, "  FAIL: Custom config directory failed with exit code ", exit_code
      passed = .false.
    end if
    
  end function test_custom_config_dir

  function test_parallel_jobs() result(passed)
    logical :: passed
    integer :: exit_code
    character(len=256) :: test_file
    integer :: unit
    
    print *, "Test 9: Parallel jobs flag"
    
    ! Create test file
    test_file = '/tmp/test_parallel.f90'
    open(newunit=unit, file=test_file)
    write(unit, '(a)') 'program test_parallel'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "Parallel jobs test"'
    write(unit, '(a)') 'end program test_parallel'
    close(unit)
    
    ! Test with parallel jobs (should show warning about unsupported feature)
    call run_fortran_file(test_file, exit_code, 1, '', '', 4, .false.)
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_file))
    
    if (exit_code == 0) then
      print *, "  PASS: Parallel jobs flag handling successful"
      passed = .true.
    else
      print *, "  FAIL: Parallel jobs failed with exit code ", exit_code
      passed = .false.
    end if
    
  end function test_parallel_jobs

  function test_no_wait_locking() result(passed)
    logical :: passed
    integer :: exit_code
    character(len=256) :: test_file
    integer :: unit
    
    print *, "Test 10: No-wait locking"
    
    ! Create test file
    test_file = '/tmp/test_no_wait.f90'
    open(newunit=unit, file=test_file)
    write(unit, '(a)') 'program test_no_wait'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "No-wait test"'
    write(unit, '(a)') 'end program test_no_wait'
    close(unit)
    
    ! Test with no-wait flag
    call run_fortran_file(test_file, exit_code, 1, '', '', 0, .true.)
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_file))
    
    if (exit_code == 0) then
      print *, "  PASS: No-wait locking successful"
      passed = .true.
    else
      print *, "  FAIL: No-wait locking failed with exit code ", exit_code
      passed = .false.
    end if
    
  end function test_no_wait_locking

  function test_local_modules() result(passed)
    logical :: passed
    integer :: exit_code
    character(len=256) :: test_file, module_file, test_dir
    integer :: unit
    
    print *, "Test 11: Local modules"
    
    ! Create test directory and files
    test_dir = '/tmp/test_local_modules'
    call execute_command_line('mkdir -p ' // trim(test_dir))
    
    ! Create a local module
    module_file = trim(test_dir) // '/my_module.f90'
    open(newunit=unit, file=module_file)
    write(unit, '(a)') 'module my_module'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  public :: hello'
    write(unit, '(a)') 'contains'
    write(unit, '(a)') '  subroutine hello()'
    write(unit, '(a)') '    print *, "Hello from module"'
    write(unit, '(a)') '  end subroutine hello'
    write(unit, '(a)') 'end module my_module'
    close(unit)
    
    ! Create main file that uses the module
    test_file = trim(test_dir) // '/main.f90'
    open(newunit=unit, file=test_file)
    write(unit, '(a)') 'program test_modules'
    write(unit, '(a)') '  use my_module'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  call hello()'
    write(unit, '(a)') 'end program test_modules'
    close(unit)
    
    call run_fortran_file(test_file, exit_code, 1, '', '', 0, .false.)
    
    ! Clean up
    call execute_command_line('rm -rf ' // trim(test_dir))
    
    if (exit_code == 0) then
      print *, "  PASS: Local modules handling successful"
      passed = .true.
    else
      print *, "  FAIL: Local modules failed with exit code ", exit_code
      passed = .false.
    end if
    
  end function test_local_modules

  function test_error_paths() result(passed)
    logical :: passed
    integer :: exit_code
    character(len=256) :: test_file
    integer :: unit
    
    print *, "Test 12: Error handling paths"
    
    ! Create a file with compilation errors
    test_file = '/tmp/test_error.f90'
    open(newunit=unit, file=test_file)
    write(unit, '(a)') 'program test_error'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer :: x'
    write(unit, '(a)') '  x = "this should cause an error"'  ! Type mismatch
    write(unit, '(a)') 'end program test_error'
    close(unit)
    
    ! Test error handling in quiet mode
    call run_fortran_file(test_file, exit_code, 0, '', '', 0, .false.)
    
    if (exit_code /= 0) then
      print *, "  PASS: Error handling in quiet mode successful"
    else
      print *, "  FAIL: Expected compilation error but got success"
      passed = .false.
      return
    end if
    
    ! Test error handling in verbose mode
    call run_fortran_file(test_file, exit_code, 1, '', '', 0, .false.)
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_file))
    
    if (exit_code /= 0) then
      print *, "  PASS: Error handling paths successful"
      passed = .true.
    else
      print *, "  FAIL: Expected compilation error but got success"
      passed = .false.
    end if
    
  end function test_error_paths

end program test_runner_comprehensive