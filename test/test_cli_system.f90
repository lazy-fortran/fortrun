program test_cli_system
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  
  character(len=512) :: command, test_file
  character(len=1024) :: output_file, expected_output
  integer :: exit_code, unit, iostat
  logical :: test_passed
  
  print *, '=== System CLI Tests ==='
  print *
  
  ! Create test file
  call create_test_file()
  
  ! Test 1: No arguments (should show help)
  call test_no_arguments()
  
  ! Test 2: --help flag
  call test_help_flag()
  
  ! Test 3: -h flag  
  call test_h_flag()
  
  ! Test 4: Basic execution
  call test_basic_execution()
  
  ! Test 5: -v flag
  call test_v_flag()
  
  ! Test 6: -vv flag
  call test_vv_flag()
  
  ! Test 7: --verbose flag (no argument)
  call test_verbose_flag()
  
  ! Test 8: --verbose 1
  call test_verbose_1()
  
  ! Test 9: --verbose 2
  call test_verbose_2()
  
  ! Test 10: --cache-dir
  call test_cache_dir()
  
  ! Test 11: --config-dir
  call test_config_dir()
  
  ! Test 12: Invalid arguments
  call test_invalid_arguments()
  
  ! Test 13: .f file execution
  call test_f_file_execution()
  
  ! Test 14: Invalid file extension
  call test_invalid_extension()
  
  ! Cleanup
  call cleanup_test_files()
  
  print *
  print *, 'All system CLI tests passed!'
  
contains

  subroutine create_test_file()
    integer :: unit
    
    test_file = '/tmp/test_system_cli.f90'
    
    open(newunit=unit, file=test_file, status='replace')
    write(unit, '(a)') 'program test_cli_system'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "CLI System Test Output"'
    write(unit, '(a)') 'end program test_cli_system'
    close(unit)
    
    print *, 'Created test file: ', trim(test_file)
  end subroutine create_test_file

  subroutine test_no_arguments()
    print *, 'Test 1: No arguments (should show help)'
    
    command = './build/gfortran_*/app/fortran > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command)
    
    call check_help_output('/tmp/cli_test_output.txt', test_passed)
    call check_exit_code('/tmp/cli_test_exit.txt', 0, test_passed)
    
    if (.not. test_passed) then
      write(error_unit, *) 'FAIL: No arguments test failed'
      stop 1
    end if
    
    print *, 'PASS: No arguments shows help'
    print *
  end subroutine test_no_arguments

  subroutine test_help_flag()
    print *, 'Test 2: --help flag'
    
    command = './build/gfortran_*/app/fortran --help > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command)
    
    call check_help_output('/tmp/cli_test_output.txt', test_passed)
    call check_exit_code('/tmp/cli_test_exit.txt', 0, test_passed)
    
    if (.not. test_passed) then
      write(error_unit, *) 'FAIL: --help flag test failed'
      stop 1
    end if
    
    print *, 'PASS: --help flag works'
    print *
  end subroutine test_help_flag

  subroutine test_h_flag()
    print *, 'Test 3: -h flag'
    
    command = './build/gfortran_*/app/fortran -h > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command)
    
    call check_help_output('/tmp/cli_test_output.txt', test_passed)
    call check_exit_code('/tmp/cli_test_exit.txt', 0, test_passed)
    
    if (.not. test_passed) then
      write(error_unit, *) 'FAIL: -h flag test failed'
      stop 1
    end if
    
    print *, 'PASS: -h flag works'
    print *
  end subroutine test_h_flag

  subroutine test_basic_execution()
    print *, 'Test 4: Basic execution'
    
    command = './build/gfortran_*/app/fortran ' // trim(test_file) // ' > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command)
    
    call check_program_output('/tmp/cli_test_output.txt', 'CLI System Test Output', test_passed)
    call check_exit_code('/tmp/cli_test_exit.txt', 0, test_passed)
    
    if (.not. test_passed) then
      write(error_unit, *) 'FAIL: Basic execution test failed'
      stop 1
    end if
    
    print *, 'PASS: Basic execution works'
    print *
  end subroutine test_basic_execution

  subroutine test_v_flag()
    print *, 'Test 5: -v flag'
    
    ! Clean cache to force rebuild and see verbose output
    call execute_command_line('rm -rf ~/.cache/fortran/test_system_cli_*')
    
    command = './build/gfortran_*/app/fortran -v ' // trim(test_file) // ' > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command)
    
    call check_program_output('/tmp/cli_test_output.txt', 'CLI System Test Output', test_passed)
    call check_verbose_output('/tmp/cli_test_output.txt', 1, test_passed)
    call check_exit_code('/tmp/cli_test_exit.txt', 0, test_passed)
    
    if (.not. test_passed) then
      write(error_unit, *) 'FAIL: -v flag test failed'
      stop 1
    end if
    
    print *, 'PASS: -v flag works'
    print *
  end subroutine test_v_flag

  subroutine test_vv_flag()
    print *, 'Test 6: -vv flag'
    
    command = './build/gfortran_*/app/fortran -vv ' // trim(test_file) // ' > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command)
    
    call check_program_output('/tmp/cli_test_output.txt', 'CLI System Test Output', test_passed)
    call check_verbose_output('/tmp/cli_test_output.txt', 2, test_passed)
    call check_exit_code('/tmp/cli_test_exit.txt', 0, test_passed)
    
    if (.not. test_passed) then
      write(error_unit, *) 'FAIL: -vv flag test failed'
      stop 1
    end if
    
    print *, 'PASS: -vv flag works'
    print *
  end subroutine test_vv_flag

  subroutine test_verbose_flag()
    print *, 'Test 7: --verbose flag (no argument)'
    
    ! Clean cache to force rebuild and see verbose output
    call execute_command_line('rm -rf ~/.cache/fortran/test_system_cli_*')
    
    command = './build/gfortran_*/app/fortran --verbose ' // trim(test_file) // ' > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command)
    
    call check_program_output('/tmp/cli_test_output.txt', 'CLI System Test Output', test_passed)
    call check_verbose_output('/tmp/cli_test_output.txt', 1, test_passed)
    call check_exit_code('/tmp/cli_test_exit.txt', 0, test_passed)
    
    if (.not. test_passed) then
      write(error_unit, *) 'FAIL: --verbose flag test failed'
      stop 1
    end if
    
    print *, 'PASS: --verbose flag works'
    print *
  end subroutine test_verbose_flag

  subroutine test_verbose_1()
    print *, 'Test 8: --verbose 1'
    
    ! Clean cache to force rebuild and see verbose output
    call execute_command_line('rm -rf ~/.cache/fortran/test_system_cli_*')
    
    command = './build/gfortran_*/app/fortran --verbose 1 ' // trim(test_file) // ' > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command)
    
    call check_program_output('/tmp/cli_test_output.txt', 'CLI System Test Output', test_passed)
    call check_verbose_output('/tmp/cli_test_output.txt', 1, test_passed)
    call check_exit_code('/tmp/cli_test_exit.txt', 0, test_passed)
    
    if (.not. test_passed) then
      write(error_unit, *) 'FAIL: --verbose 1 test failed'
      stop 1
    end if
    
    print *, 'PASS: --verbose 1 works'
    print *
  end subroutine test_verbose_1

  subroutine test_verbose_2()
    print *, 'Test 9: --verbose 2'
    
    command = './build/gfortran_*/app/fortran --verbose 2 ' // trim(test_file) // ' > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command)
    
    call check_program_output('/tmp/cli_test_output.txt', 'CLI System Test Output', test_passed)
    call check_verbose_output('/tmp/cli_test_output.txt', 2, test_passed)
    call check_exit_code('/tmp/cli_test_exit.txt', 0, test_passed)
    
    if (.not. test_passed) then
      write(error_unit, *) 'FAIL: --verbose 2 test failed'
      stop 1
    end if
    
    print *, 'PASS: --verbose 2 works'
    print *
  end subroutine test_verbose_2

  subroutine test_cache_dir()
    print *, 'Test 10: --cache-dir'
    
    command = './build/gfortran_*/app/fortran --cache-dir /tmp/custom_cache ' // trim(test_file) // ' > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command)
    
    call check_program_output('/tmp/cli_test_output.txt', 'CLI System Test Output', test_passed)
    call check_exit_code('/tmp/cli_test_exit.txt', 0, test_passed)
    
    ! Check that custom cache directory was created
    call check_directory_exists('/tmp/custom_cache', test_passed)
    
    if (.not. test_passed) then
      write(error_unit, *) 'FAIL: --cache-dir test failed'
      stop 1
    end if
    
    print *, 'PASS: --cache-dir works'
    print *
  end subroutine test_cache_dir

  subroutine test_config_dir()
    print *, 'Test 11: --config-dir'
    
    ! Create a custom config directory with registry
    call execute_command_line('mkdir -p /tmp/custom_config')
    call execute_command_line('cp registry.toml /tmp/custom_config/')
    
    command = './build/gfortran_*/app/fortran --config-dir /tmp/custom_config ' // trim(test_file) // ' > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command)
    
    call check_program_output('/tmp/cli_test_output.txt', 'CLI System Test Output', test_passed)
    call check_exit_code('/tmp/cli_test_exit.txt', 0, test_passed)
    
    if (.not. test_passed) then
      write(error_unit, *) 'FAIL: --config-dir test failed'
      stop 1
    end if
    
    print *, 'PASS: --config-dir works'
    print *
  end subroutine test_config_dir

  subroutine test_invalid_arguments()
    print *, 'Test 12: Invalid arguments'
    
    ! Test missing cache directory argument
    command = './build/gfortran_*/app/fortran --cache-dir > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command)
    
    call check_help_output('/tmp/cli_test_output.txt', test_passed)
    call check_exit_code('/tmp/cli_test_exit.txt', 0, test_passed)
    
    if (.not. test_passed) then
      write(error_unit, *) 'FAIL: Invalid arguments test failed'
      stop 1
    end if
    
    print *, 'PASS: Invalid arguments handled correctly'
    print *
  end subroutine test_invalid_arguments

  subroutine check_help_output(output_file, passed)
    character(len=*), intent(in) :: output_file
    logical, intent(out) :: passed
    character(len=512) :: line
    integer :: unit, iostat
    logical :: found_usage
    
    passed = .false.
    found_usage = .false.
    
    open(newunit=unit, file=output_file, status='old', iostat=iostat)
    if (iostat /= 0) then
      write(error_unit, *) 'Error: Cannot open output file: ', trim(output_file)
      return
    end if
    
    do
      read(unit, '(a)', iostat=iostat) line
      if (iostat /= 0) exit
      
      if (index(line, 'Usage: fortran') > 0) then
        found_usage = .true.
        exit
      end if
    end do
    
    close(unit)
    
    if (found_usage) then
      passed = .true.
    else
      write(error_unit, *) 'Error: Help output not found in ', trim(output_file)
    end if
    
  end subroutine check_help_output

  subroutine check_program_output(output_file, expected, passed)
    character(len=*), intent(in) :: output_file, expected
    logical, intent(out) :: passed
    character(len=512) :: line
    integer :: unit, iostat
    logical :: found_output
    
    passed = .false.
    found_output = .false.
    
    open(newunit=unit, file=output_file, status='old', iostat=iostat)
    if (iostat /= 0) then
      write(error_unit, *) 'Error: Cannot open output file: ', trim(output_file)
      return
    end if
    
    do
      read(unit, '(a)', iostat=iostat) line
      if (iostat /= 0) exit
      
      if (index(line, trim(expected)) > 0) then
        found_output = .true.
        exit
      end if
    end do
    
    close(unit)
    
    if (found_output) then
      passed = .true.
    else
      write(error_unit, *) 'Error: Expected output "', trim(expected), '" not found'
    end if
    
  end subroutine check_program_output

  subroutine check_verbose_output(output_file, level, passed)
    character(len=*), intent(in) :: output_file
    integer, intent(in) :: level
    logical, intent(out) :: passed
    character(len=512) :: line
    integer :: unit, iostat
    logical :: found_verbose
    
    passed = .false.
    found_verbose = .false.
    
    open(newunit=unit, file=output_file, status='old', iostat=iostat)
    if (iostat /= 0) then
      write(error_unit, *) 'Error: Cannot open output file: ', trim(output_file)
      return
    end if
    
    do
      read(unit, '(a)', iostat=iostat) line
      if (iostat /= 0) exit
      
      if (level == 1) then
        ! Level 1 should show build progress like "[  0%]" or "done."
        if (index(line, '[') > 0 .and. index(line, '%]') > 0) then
          found_verbose = .true.
          exit
        end if
        if (index(line, 'done.') > 0) then
          found_verbose = .true.
          exit
        end if
      else if (level == 2) then
        ! Level 2 should show detailed FPM info like "<INFO>"
        if (index(line, '<INFO>') > 0) then
          found_verbose = .true.
          exit
        end if
      end if
    end do
    
    close(unit)
    
    if (found_verbose) then
      passed = .true.
    else
      write(error_unit, *) 'Error: Verbose level ', level, ' output not found'
    end if
    
  end subroutine check_verbose_output

  subroutine check_exit_code(exit_file, expected, passed)
    character(len=*), intent(in) :: exit_file
    integer, intent(in) :: expected
    logical, intent(out) :: passed
    integer :: unit, iostat, actual_exit
    
    passed = .false.
    
    open(newunit=unit, file=exit_file, status='old', iostat=iostat)
    if (iostat /= 0) then
      write(error_unit, *) 'Error: Cannot open exit code file: ', trim(exit_file)
      return
    end if
    
    read(unit, *, iostat=iostat) actual_exit
    close(unit)
    
    if (iostat /= 0) then
      write(error_unit, *) 'Error: Cannot read exit code from ', trim(exit_file)
      return
    end if
    
    if (actual_exit == expected) then
      passed = .true.
    else
      write(error_unit, *) 'Error: Expected exit code ', expected, ' but got ', actual_exit
    end if
    
  end subroutine check_exit_code

  subroutine check_directory_exists(directory, passed)
    character(len=*), intent(in) :: directory
    logical, intent(out) :: passed
    logical :: exists
    
    inquire(file=directory, exist=exists)
    passed = exists
    
    if (.not. exists) then
      write(error_unit, *) 'Error: Directory does not exist: ', trim(directory)
    end if
    
  end subroutine check_directory_exists

  subroutine test_f_file_execution()
    character(len=512) :: test_f_file
    logical :: success, exit_ok
    integer :: unit
    
    print *, 'Test 13: .f file execution'
    
    ! Create a simple .f test file
    test_f_file = '/tmp/test_simple.f'
    open(newunit=unit, file=test_f_file, status='replace')
    write(unit, '(a)') "print *, 'Hello from .f file'"
    close(unit)
    
    command = './build/gfortran_*/app/fortran ' // trim(test_f_file) // &
              ' > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command, exitstat=exit_code)
    
    call check_exit_code('/tmp/cli_test_exit.txt', 0, exit_ok)
    call check_program_output('/tmp/cli_test_output.txt', 'Hello from .f file', success)
    
    if (exit_ok .and. success) then
      print *, 'PASS: .f file executed successfully'
    else
      print *, 'FAIL: .f file execution failed'
    end if
    
    ! Cleanup
    call execute_command_line('rm -f ' // trim(test_f_file))
  end subroutine test_f_file_execution
  
  subroutine test_invalid_extension()
    logical :: exit_ok, has_error
    integer :: unit
    character(len=256) :: test_txt_file
    
    print *, 'Test 14: Invalid file extension'
    
    ! Create a temporary .txt file
    test_txt_file = '/tmp/test_invalid.txt'
    open(newunit=unit, file=test_txt_file, status='replace')
    write(unit, '(a)') "This is not a Fortran file"
    close(unit)
    
    command = './build/gfortran_*/app/fortran ' // trim(test_txt_file) // &
              ' > /tmp/cli_test_output.txt 2>&1; echo $? > /tmp/cli_test_exit.txt'
    call execute_command_line(command, exitstat=exit_code)
    
    call check_exit_code('/tmp/cli_test_exit.txt', 1, exit_ok)
    call check_program_output('/tmp/cli_test_output.txt', &
                              'must have .f90, .F90, .f, or .F extension', has_error)
    
    if (exit_ok .and. has_error) then
      print *, 'PASS: Invalid extension rejected'
    else
      print *, 'FAIL: Should reject invalid extensions'
    end if
    
    ! Cleanup
    call execute_command_line('rm -f ' // trim(test_txt_file))
  end subroutine test_invalid_extension

  subroutine cleanup_test_files()
    call execute_command_line('rm -f /tmp/test_system_cli.f90')
    call execute_command_line('rm -f /tmp/cli_test_output.txt')
    call execute_command_line('rm -f /tmp/cli_test_exit.txt')
    call execute_command_line('rm -rf /tmp/custom_cache')
    call execute_command_line('rm -rf /tmp/custom_config')
    print *, 'Cleaned up test files'
  end subroutine cleanup_test_files

end program test_cli_system