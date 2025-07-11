program test_different_directories
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  
  character(len=512) :: command
  character(len=256) :: test_dir, sub_dir
  integer :: unit
  
  print *, '=== Different Directories Tests ===\'
  
  ! Create test directory structure
  test_dir = '/tmp/test_different_dirs'
  sub_dir = trim(test_dir) // '/subdir'
  
  call execute_command_line('mkdir -p ' // trim(sub_dir))
  
  ! Create a simple Fortran file in subdirectory
  call create_test_file(trim(sub_dir) // '/hello.f90')
  
  ! Test 1: Run from parent directory using absolute path
  call test_absolute_path()
  
  ! Test 2: Run from parent directory using relative path
  call test_relative_path()
  
  ! Test 3: Run from different directory entirely
  call test_different_directory()
  
  ! Clean up
  call execute_command_line('rm -rf ' // trim(test_dir))
  
  print *, 'All different directory tests passed!'
  
contains

  subroutine create_test_file(file_path)
    character(len=*), intent(in) :: file_path
    integer :: unit
    
    open(newunit=unit, file=file_path, status='replace')
    write(unit, '(a)') 'program hello_from_subdir'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "Hello from subdirectory!"'
    write(unit, '(a)') 'end program hello_from_subdir'
    close(unit)
    
  end subroutine create_test_file

  subroutine test_absolute_path()
    character(len=512) :: command
    character(len=256) :: abs_path
    
    print *, 'Test 1: Run from parent directory using absolute path'
    
    abs_path = trim(sub_dir) // '/hello.f90'
    
    ! Change to parent directory and run with absolute path
    command = 'ORIGINAL_DIR=$(pwd) && cd ' // trim(test_dir) // ' && $ORIGINAL_DIR/build/gfortran_*/app/fortran "' // &
              trim(abs_path) // '" > /tmp/abs_output.txt 2>&1; echo $? > /tmp/abs_exit.txt'
    call execute_command_line(command)
    
    ! Check that it succeeded
    call check_exit_code('/tmp/abs_exit.txt', 0)
    call check_output_contains('/tmp/abs_output.txt', 'Hello from subdirectory!')
    
    ! Clean up
    call execute_command_line('rm -f /tmp/abs_output.txt /tmp/abs_exit.txt')
    
    print *, 'PASS: Absolute path works from different directory'
    print *
    
  end subroutine test_absolute_path

  subroutine test_relative_path()
    character(len=512) :: command
    
    print *, 'Test 2: Run from parent directory using relative path'
    
    ! Change to parent directory and run with relative path
    command = 'ORIGINAL_DIR=$(pwd) && cd ' // trim(test_dir) // &
              ' && $ORIGINAL_DIR/build/gfortran_*/app/fortran subdir/hello.f90 ' // &
              '> /tmp/rel_output.txt 2>&1; echo $? > /tmp/rel_exit.txt'
    call execute_command_line(command)
    
    ! Check that it succeeded
    call check_exit_code('/tmp/rel_exit.txt', 0)
    call check_output_contains('/tmp/rel_output.txt', 'Hello from subdirectory!')
    
    ! Clean up
    call execute_command_line('rm -f /tmp/rel_output.txt /tmp/rel_exit.txt')
    
    print *, 'PASS: Relative path works from different directory'
    print *
    
  end subroutine test_relative_path

  subroutine test_different_directory()
    character(len=512) :: command
    character(len=256) :: abs_path
    
    print *, 'Test 3: Run from completely different directory'
    
    abs_path = trim(sub_dir) // '/hello.f90'
    
    ! Change to /tmp and run with absolute path
    command = 'ORIGINAL_DIR=$(pwd) && cd /tmp && $ORIGINAL_DIR/build/gfortran_*/app/fortran "' // &
              trim(abs_path) // '" > /tmp/diff_output.txt 2>&1; echo $? > /tmp/diff_exit.txt'
    call execute_command_line(command)
    
    ! Check that it succeeded
    call check_exit_code('/tmp/diff_exit.txt', 0)
    call check_output_contains('/tmp/diff_output.txt', 'Hello from subdirectory!')
    
    ! Clean up
    call execute_command_line('rm -f /tmp/diff_output.txt /tmp/diff_exit.txt')
    
    print *, 'PASS: Works from completely different directory'
    print *
    
  end subroutine test_different_directory

  subroutine check_exit_code(exit_file, expected_code)
    character(len=*), intent(in) :: exit_file
    integer, intent(in) :: expected_code
    integer :: unit, iostat, actual_code
    
    open(newunit=unit, file=exit_file, status='old', iostat=iostat)
    if (iostat /= 0) then
      write(error_unit, *) 'Error: Cannot open exit code file: ', trim(exit_file)
      stop 1
    end if
    
    read(unit, *, iostat=iostat) actual_code
    close(unit)
    
    if (iostat /= 0) then
      write(error_unit, *) 'Error: Cannot read exit code from file: ', trim(exit_file)
      stop 1
    end if
    
    if (actual_code /= expected_code) then
      write(error_unit, *) 'Error: Expected exit code ', expected_code, ' but got ', actual_code
      stop 1
    end if
    
  end subroutine check_exit_code

  subroutine check_output_contains(output_file, expected_text)
    character(len=*), intent(in) :: output_file, expected_text
    character(len=512) :: line
    integer :: unit, iostat
    logical :: found
    
    found = .false.
    
    open(newunit=unit, file=output_file, status='old', iostat=iostat)
    if (iostat /= 0) then
      write(error_unit, *) 'Error: Cannot open output file: ', trim(output_file)
      stop 1
    end if
    
    do
      read(unit, '(a)', iostat=iostat) line
      if (iostat /= 0) exit
      
      if (index(line, trim(expected_text)) > 0) then
        found = .true.
        exit
      end if
    end do
    
    close(unit)
    
    if (.not. found) then
      write(error_unit, *) 'Error: Expected text "', trim(expected_text), '" not found in output'
      stop 1
    end if
    
  end subroutine check_output_contains

end program test_different_directories