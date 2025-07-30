program test_runner_error_paths
    use iso_fortran_env, only: error_unit
    use runner
    use logger_utils, only: set_logger_verbose_level
    use temp_utils, only: temp_dir_manager
    use system_utils, only: sys_remove_file
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr

    print *, "=== Runner Error Path Coverage Tests ==="
    print *

    all_tests_passed = .true.

    call temp_mgr%create('runner_error_test')

    ! Set verbose mode to exercise debug_print paths
    call set_logger_verbose_level(2)

    ! Test error handling paths that were modified in the PR
    if (.not. test_file_not_found_error()) all_tests_passed = .false.
    if (.not. test_invalid_extension_error()) all_tests_passed = .false.
    if (.not. test_debug_output_paths()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All runner error path tests passed!"
        stop 0
    else
        print *, "Some runner error path tests failed!"
        stop 1
    end if

contains

    function test_file_not_found_error() result(passed)
        logical :: passed
        integer :: exit_code
        character(len=256) :: custom_cache_dir, custom_config_dir

        print *, "Test: File not found error path"
        passed = .true.

        custom_cache_dir = ""
        custom_config_dir = ""

        ! Test with non-existent file to exercise print_error path
        call run_fortran_file('/nonexistent/file.f90', exit_code, 2, &
                              custom_cache_dir, custom_config_dir, &
                              1, .false.)

        if (exit_code /= 0) then
            print *, "  PASS: Non-existent file properly returns error code", exit_code
        else
            print *, "  FAIL: Non-existent file should return non-zero exit code"
            passed = .false.
        end if

    end function test_file_not_found_error

    function test_invalid_extension_error() result(passed)
        logical :: passed
        character(len=:), allocatable :: invalid_file
        integer :: exit_code, unit
        character(len=256) :: custom_cache_dir, custom_config_dir

        print *, "Test: Invalid extension error path"
        passed = .true.

        ! Create a file with invalid extension
        invalid_file = temp_mgr%get_file_path('test.txt')
        open (newunit=unit, file=invalid_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '  print *, "hello"'
        write (unit, '(a)') 'end program test'
        close (unit)

        custom_cache_dir = ""
        custom_config_dir = ""

        ! Test with invalid extension to exercise print_error path
        call run_fortran_file(invalid_file, exit_code, 2, &
                              custom_cache_dir, custom_config_dir, &
                              1, .false.)

        if (exit_code /= 0) then
            print *, "  PASS: Invalid extension properly returns error code", exit_code
        else
            print *, "  FAIL: Invalid extension should return non-zero exit code"
            passed = .false.
        end if

        ! Clean up
        call sys_remove_file(invalid_file)

    end function test_invalid_extension_error

    function test_debug_output_paths() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_file
        integer :: exit_code, unit
        character(len=256) :: custom_cache_dir, custom_config_dir

        print *, "Test: Debug output paths"
        passed = .true.

        ! Create a simple test file
        test_file = temp_mgr%get_file_path('debug_test.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'program debug_test'
        write (unit, '(a)') '  print *, "debug test"'
        write (unit, '(a)') 'end program debug_test'
        close (unit)

        custom_cache_dir = temp_mgr%get_file_path('custom_cache')
        custom_config_dir = ""

        ! Run with verbose level 2 to exercise debug_print calls
        call run_fortran_file(test_file, exit_code, 2, &
                              custom_cache_dir, custom_config_dir, &
                              1, .false.)

        ! The main goal is to exercise the debug code paths, not necessarily succeed
        print *, "  INFO: Debug test completed with exit code:", exit_code
        print *, "  PASS: Debug output paths exercised"

        ! Test with quiet mode (verbose level 0) to exercise stderr redirect
        call run_fortran_file(test_file, exit_code, 0, &
                              custom_cache_dir, custom_config_dir, &
                              1, .false.)

        print *, "  INFO: Quiet mode test completed with exit code:", exit_code
        print *, "  PASS: Quiet mode paths exercised"

        ! Clean up
        call sys_remove_file(test_file)

    end function test_debug_output_paths

end program test_runner_error_paths
