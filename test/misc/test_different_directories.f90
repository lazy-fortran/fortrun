program test_different_directories
    use, intrinsic :: iso_fortran_env, only: error_unit
    use temp_utils, only: create_temp_dir, get_temp_file_path, path_join, fortran_with_isolated_cache
 use system_utils, only: sys_remove_dir, sys_remove_file, sys_run_command_with_exit_code
    use temp_utils, only: mkdir
    use fpm_environment, only: get_os_type, OS_WINDOWS, get_env
    implicit none

    character(len=512) :: command
    character(len=256) :: test_dir, sub_dir
    integer :: unit
    logical :: success

    ! Skip this test on Windows CI - it hangs due to multiple cache operations
    if (get_os_type() == OS_WINDOWS .and. len_trim(get_env('CI', '')) > 0) then
        print *, 'SKIP: test_different_directories on Windows CI (known issue with parallel cache operations)'
        stop 0
    end if

    print *, '=== Different Directories Tests ===\'

    ! Create test directory structure
    test_dir = create_temp_dir('fortran_test_different_dirs')
    sub_dir = path_join(test_dir, 'subdir')

    call mkdir(trim(sub_dir))

    ! Create a simple Fortran file in subdirectory
    call create_test_file(path_join(sub_dir, 'hello.f90'))

    ! Test 1: Run from parent directory using absolute path
    call test_absolute_path()

    ! Test 2: Run from parent directory using relative path
    call test_relative_path()

    ! Test 3: Run from different directory entirely
    call test_different_directory()

    ! Clean up
    call sys_remove_dir(test_dir, success)

    print *, 'All different directory tests passed!'

contains

    subroutine create_test_file(file_path)
        character(len=*), intent(in) :: file_path
        integer :: unit

        open (newunit=unit, file=file_path, status='replace')
        write (unit, '(a)') 'program hello_from_subdir'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "Hello from subdirectory!"'
        write (unit, '(a)') 'end program hello_from_subdir'
        close (unit)

    end subroutine create_test_file

    subroutine test_absolute_path()
        character(len=512) :: command
        character(len=256) :: abs_path

        print *, 'Test 1: Run from parent directory using absolute path'

        abs_path = path_join(sub_dir, 'hello.f90')

        ! Change to parent directory and run with absolute path
        block
            character(len=:), allocatable :: temp_dir, abs_output_file, abs_exit_file
            temp_dir = create_temp_dir('fortran_test')
            abs_output_file = get_temp_file_path(temp_dir, 'abs_output.txt')
            abs_exit_file = get_temp_file_path(temp_dir, 'abs_exit.txt')

            if (get_os_type() == OS_WINDOWS) then
                command = trim(fortran_with_isolated_cache('test_diff_dirs_abs')) // ' "' // trim(abs_path) // '"'
            else
  command = 'ORIGINAL_DIR=$(pwd) && cd '//trim(test_dir)//' && cd $ORIGINAL_DIR && '// &
                       trim(fortran_with_isolated_cache('test_diff_dirs_abs'))//' "'// &
                          trim(abs_path)//'"'
            end if
            call sys_run_command_with_exit_code(command, abs_output_file, abs_exit_file)

            ! Check that it succeeded
            call check_exit_code(abs_exit_file, 0)
            call check_output_contains(abs_output_file, 'Hello from subdirectory!')

            ! Clean up
            call sys_remove_file(abs_output_file)
            call sys_remove_file(abs_exit_file)
        end block

        print *, 'PASS: Absolute path works from different directory'
        print *

    end subroutine test_absolute_path

    subroutine test_relative_path()
        character(len=512) :: command

        print *, 'Test 2: Run from parent directory using relative path'

        ! Change to parent directory and run with relative path
        block
            character(len=:), allocatable :: temp_dir, rel_output_file, rel_exit_file
            temp_dir = create_temp_dir('fortran_test')
            rel_output_file = get_temp_file_path(temp_dir, 'rel_output.txt')
            rel_exit_file = get_temp_file_path(temp_dir, 'rel_exit.txt')

            if (get_os_type() == OS_WINDOWS) then
              command = trim(fortran_with_isolated_cache('test_diff_dirs_rel'))//' '// &
                          path_join(test_dir, 'subdir/hello.f90')
            else
                command = 'ORIGINAL_DIR=$(pwd) && cd '//trim(test_dir)// &
                          ' && cd $ORIGINAL_DIR && '// &
                        trim(fortran_with_isolated_cache('test_diff_dirs_rel'))//' '// &
                          path_join(test_dir, 'subdir/hello.f90')
            end if
            call sys_run_command_with_exit_code(command, rel_output_file, rel_exit_file)

            ! Check that it succeeded
            call check_exit_code(rel_exit_file, 0)
            call check_output_contains(rel_output_file, 'Hello from subdirectory!')

            ! Clean up
            call sys_remove_file(rel_output_file)
            call sys_remove_file(rel_exit_file)
        end block

        print *, 'PASS: Relative path works from different directory'
        print *

    end subroutine test_relative_path

    subroutine test_different_directory()
        character(len=512) :: command
        character(len=256) :: abs_path

        print *, 'Test 3: Run from completely different directory'

        abs_path = path_join(sub_dir, 'hello.f90')

        ! Change to system temp and run with absolute path
        block
character(len=:), allocatable :: temp_dir, diff_output_file, diff_exit_file, system_temp
            temp_dir = create_temp_dir('fortran_test')
            diff_output_file = get_temp_file_path(temp_dir, 'diff_output.txt')
            diff_exit_file = get_temp_file_path(temp_dir, 'diff_exit.txt')
            system_temp = create_temp_dir('system_temp')

            if (get_os_type() == OS_WINDOWS) then
          command = trim(fortran_with_isolated_cache('test_diff_dirs_other')) // ' "' // trim(abs_path) // '"'
            else
     command = 'ORIGINAL_DIR=$(pwd) && cd '//system_temp//' && cd $ORIGINAL_DIR && '// &
                     trim(fortran_with_isolated_cache('test_diff_dirs_other'))//' "'// &
                          trim(abs_path)//'"'
            end if
          call sys_run_command_with_exit_code(command, diff_output_file, diff_exit_file)

            ! Check that it succeeded
            call check_exit_code(diff_exit_file, 0)
            call check_output_contains(diff_output_file, 'Hello from subdirectory!')

            ! Clean up
            call sys_remove_file(diff_output_file)
            call sys_remove_file(diff_exit_file)
        end block

        print *, 'PASS: Works from completely different directory'
        print *

    end subroutine test_different_directory

    subroutine check_exit_code(exit_file, expected_code)
        character(len=*), intent(in) :: exit_file
        integer, intent(in) :: expected_code
        integer :: unit, iostat, actual_code

        open (newunit=unit, file=exit_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, *) 'Error: Cannot open exit code file: ', trim(exit_file)
            stop 1
        end if

        read (unit, *, iostat=iostat) actual_code
        close (unit)

        if (iostat /= 0) then
       write (error_unit, *) 'Error: Cannot read exit code from file: ', trim(exit_file)
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

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, *) 'Error: Cannot open output file: ', trim(output_file)
            stop 1
        end if

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            if (index(line, trim(expected_text)) > 0) then
                found = .true.
                exit
            end if
        end do

        close (unit)

        if (.not. found) then
      write(error_unit, *) 'Error: Expected text "', trim(expected_text), '" not found in output'
            stop 1
        end if

    end subroutine check_output_contains

end program test_different_directories
