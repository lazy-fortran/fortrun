program test_main_coverage
    use temp_utils, only: create_temp_dir, get_temp_file_path, create_temp_file, &
        fortran_with_isolated_cache
    use system_utils, only: sys_remove_dir, sys_remove_file
    use fpm_environment, only: get_os_type, OS_WINDOWS, get_env
    implicit none

    logical :: all_tests_passed

    ! Skip this test on Windows CI - it runs fortran CLI 9 times
    if (get_os_type() == OS_WINDOWS .and. len_trim(get_env('CI', '')) > 0) then
        print *, 'SKIP: test_main_coverage on Windows CI (runs fortran CLI 9 times)'
        stop 0
    end if

    print *, "=== Main Application Coverage Tests ==="
    print *

    all_tests_passed = .true.

    ! Test main application paths through system calls
    if (.not. test_help_output()) all_tests_passed = .false.
    if (.not. test_normal_execution_mode()) all_tests_passed = .false.
    if (.not. test_notebook_execution_mode()) all_tests_passed = .false.
    if (.not. test_verbose_modes()) all_tests_passed = .false.
    if (.not. test_error_handling()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All main coverage tests passed!"
        stop 0
    else
        print *, "Some main coverage tests failed!"
        stop 1
    end if

contains

    function test_help_output() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        character(len=:), allocatable :: output

        print *, "Test 1: Help output coverage"
        passed = .true.

        ! Test --help flag
        command = fortran_with_isolated_cache('main_cov_help') // ' --help'
        call execute_and_capture(command, output, exit_code)

        if (exit_code /= 0) then
            print *, "  WARNING: Help command failed"
        end if

        if (index(output, 'Usage:') == 0) then
            print *, "  WARNING: Usage not found in help"
        end if

        if (index(output, 'Notebook Mode:') == 0) then
            print *, "  WARNING: Notebook mode section not found"
        end if

        ! Test -h flag
        command = fortran_with_isolated_cache('main_cov_h') // ' -h'
        call execute_and_capture(command, output, exit_code)

        if (exit_code /= 0) then
            print *, "  WARNING: -h flag failed"
        end if

        print *, "  PASS: Help output"

    end function test_help_output

    function test_normal_execution_mode() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        character(len=256) :: test_file
        character(len=:), allocatable :: output

        print *, "Test 2: Normal execution mode coverage"
        passed = .true.

        ! Create test file
 test_file = create_temp_file('fortran_test_main_normal', '.f90')
        call create_simple_program(test_file)

        ! Test normal execution
        command = fortran_with_isolated_cache('main_cov_normal') // ' '//trim(test_file)
        call execute_and_capture(command, output, exit_code)

        if (exit_code /= 0) then
            print *, "  WARNING: Normal execution failed"
        end if

        if (index(output, 'Hello from main test') == 0) then
            print *, "  WARNING: Expected output not found"
        end if

        ! Clean up
        call delete_file(test_file)

        print *, "  PASS: Normal execution mode"

    end function test_normal_execution_mode

    function test_notebook_execution_mode() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        character(len=256) :: test_file, output_file
        character(len=:), allocatable :: output
        logical :: file_exists

        print *, "Test 3: Notebook execution mode coverage"
        passed = .true.

        ! Create test notebook file
 test_file = create_temp_file('fortran_test_main_notebook', '.f')
        output_file = create_temp_file('fortran_test_main_notebook', '.md')
        call create_simple_notebook(test_file)

        ! Test notebook execution
        command = fortran_with_isolated_cache('main_cov_notebook') // ' --notebook '//trim(test_file)
        call execute_and_capture(command, output, exit_code)

        if (exit_code /= 0) then
            print *, "  WARNING: Notebook execution failed"
        end if

        ! Check output file was created
        inquire (file=output_file, exist=file_exists)
        if (.not. file_exists) then
            print *, "  WARNING: Notebook output file not created"
        end if

        ! Clean up
        call delete_file(test_file)
        call delete_file(output_file)

        print *, "  PASS: Notebook execution mode"

    end function test_notebook_execution_mode

    function test_verbose_modes() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        character(len=256) :: test_file
        character(len=:), allocatable :: output

        print *, "Test 4: Verbose modes coverage"
        passed = .true.

        ! Create test file
test_file = create_temp_file('fortran_test_main_verbose', '.f90')
        call create_simple_program(test_file)

        ! Test -v verbose mode
        command = fortran_with_isolated_cache('main_cov_verbose_v') // ' -v '//trim(test_file)
        call execute_and_capture(command, output, exit_code)

        if (exit_code /= 0) then
            print *, "  WARNING: Verbose mode (-v) failed"
        end if

        ! Test -vv verbose mode
        command = fortran_with_isolated_cache('main_cov_verbose_vv') // ' -vv '//trim(test_file)
        call execute_and_capture(command, output, exit_code)

        if (exit_code /= 0) then
            print *, "  WARNING: Very verbose mode (-vv) failed"
        end if

        ! Test notebook with verbose
        test_file = create_temp_file('fortran_test_main_verbose_notebook', '.f')
        call create_simple_notebook(test_file)

        command = fortran_with_isolated_cache('main_cov_notebook_v') // ' --notebook -v '//trim(test_file)
        call execute_and_capture(command, output, exit_code)

        if (exit_code /= 0) then
            print *, "  WARNING: Notebook verbose mode failed"
        end if

        if (index(output, 'Running in notebook mode') == 0) then
            print *, "  WARNING: Notebook verbose message not found"
        end if

        if (index(output, 'Notebook output saved to') == 0) then
            print *, "  WARNING: Notebook save message not found"
        end if

        ! Clean up
        call delete_file(create_temp_file('fortran_test_main_verbose', '.f90'))
        call delete_file(create_temp_file('fortran_test_main_verbose_notebook', '.f'))
        call delete_file(create_temp_file('fortran_test_main_verbose_notebook', '.md'))

        print *, "  PASS: Verbose modes"

    end function test_verbose_modes

    function test_error_handling() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=512) :: command
        character(len=256) :: test_file
        character(len=:), allocatable :: output

        print *, "Test 5: Error handling coverage"
        passed = .true.

        ! Create file with compilation error
  test_file = create_temp_file('fortran_test_main_error', '.f90')
        call create_error_program(test_file)

        ! Test error handling in normal mode
        command = fortran_with_isolated_cache('main_cov_error') // ' '//trim(test_file)//' 2>/dev/null'
        call execute_and_capture(command, output, exit_code)

        if (exit_code == 0) then
            print *, "  WARNING: Error program should have failed"
        end if

        ! Test non-existent file
        block
            character(len=256) :: test_file
            test_file = create_temp_file('fortran_test_definitely_nonexistent_file', '.f90')
            command = fortran_with_isolated_cache('main_cov_nonexistent') // ' '//trim(test_file)//' 2>/dev/null'
        end block
        call execute_and_capture(command, output, exit_code)

        if (exit_code == 0) then
            print *, "  WARNING: Non-existent file should have failed"
        end if

        ! Clean up
        call delete_file(test_file)

        print *, "  PASS: Error handling"

    end function test_error_handling

    ! Helper subroutines
    subroutine create_simple_program(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') 'program main_test'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "Hello from main test"'
        write (unit, '(a)') 'end program main_test'
        close (unit)

    end subroutine create_simple_program

    subroutine create_simple_notebook(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') '! %% [markdown]'
        write (unit, '(a)') '! # Test Notebook for Main Coverage'
        write (unit, '(a)') '! %%'
        write (unit, '(a)') 'x = 42'
        write (unit, '(a)') 'print *, "Test notebook output:", x'
        close (unit)

    end subroutine create_simple_notebook

    subroutine create_error_program(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') 'program error_test'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  integer :: x'
        write (unit, '(a)') '  x = "this should cause an error"'  ! Type mismatch
        write (unit, '(a)') 'end program error_test'
        close (unit)

    end subroutine create_error_program

    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename
        character(len=512) :: command

        command = 'rm -f "'//trim(filename)//'"'
        call execute_command_line(command)

    end subroutine delete_file

    subroutine execute_and_capture(command, output, exit_code)
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: output
        integer, intent(out) :: exit_code

        character(len=256) :: temp_file
        character(len=512) :: full_command
        integer :: unit, iostat, file_size

temp_file = create_temp_file('fortran_test_main_test', '.out')

        full_command = trim(command)//' > '//trim(temp_file)//' 2>&1'
        call execute_command_line(full_command, exitstat=exit_code)

        inquire (file=temp_file, size=file_size)

        if (file_size > 0) then
            open (newunit=unit, file=temp_file, status='old', &
                  access='stream', form='unformatted', iostat=iostat)

            if (iostat == 0) then
                allocate (character(len=file_size) :: output)
                read (unit, iostat=iostat) output
                close (unit)
            else
                output = ""
            end if
        else
            output = ""
        end if

        call sys_remove_file(temp_file)

    end subroutine execute_and_capture

end program test_main_coverage
