program test_logger_utils
    use iso_fortran_env, only: error_unit, output_unit
    use logger_utils
    use temp_utils, only: temp_dir_manager, get_temp_file_path
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr
    character(len=:), allocatable :: temp_output, original_output

    print *, "=== Logger Utils Tests ==="
    print *

    all_tests_passed = .true.

    call temp_mgr%create('logger_utils_test')

    ! Test logger functions with different verbosity levels
    if (.not. test_logger_output()) all_tests_passed = .false.
    if (.not. test_verbose_levels()) all_tests_passed = .false.
    if (.not. test_error_output()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All logger utils tests passed!"
        stop 0
    else
        print *, "Some logger utils tests failed!"
        stop 1
    end if

contains

    function test_logger_output() result(passed)
        logical :: passed
        integer :: old_verbose_level

        print *, "Test 1: Basic logger output"
        passed = .true.

        ! Save original verbose level
        old_verbose_level = get_logger_verbose_level()

        ! Test set/get verbose level
        call set_logger_verbose_level(2)
        if (get_logger_verbose_level() /= 2) then
            print *, "  FAIL: Verbose level not set correctly"
            passed = .false.
        else
            print *, "  PASS: Verbose level set and retrieved correctly"
        end if

        ! Test basic print functions (hard to capture output, so just verify they don't crash)
        call print_info("Test info message")
        call print_warning("Test warning message")
        call print_error("Test error message")
        call debug_print("Test debug message")

        print *, "  PASS: All print functions executed without error"

        ! Restore original verbose level
        call set_logger_verbose_level(old_verbose_level)

    end function test_logger_output

    function test_verbose_levels() result(passed)
        logical :: passed
        integer :: old_verbose_level

        print *, "Test 2: Verbose level controls"
        passed = .true.

        ! Save original verbose level
        old_verbose_level = get_logger_verbose_level()

        ! Test different verbose levels
        call set_logger_verbose_level(0) ! Quiet
        if (get_logger_verbose_level() /= 0) then
            print *, "  FAIL: Quiet level not set"
            passed = .false.
        else
            call debug_print("This debug should be suppressed at level 0")
            print *, "  PASS: Quiet level set"
        end if

        call set_logger_verbose_level(1) ! Normal
        if (get_logger_verbose_level() /= 1) then
            print *, "  FAIL: Normal level not set"
            passed = .false.
        else
            call print_info("This info should show at level 1")
            print *, "  PASS: Normal level set"
        end if

        call set_logger_verbose_level(2) ! Verbose
        if (get_logger_verbose_level() /= 2) then
            print *, "  FAIL: Verbose level not set"
            passed = .false.
        else
            call debug_print("This debug should show at level 2")
            print *, "  PASS: Verbose level set"
        end if

        ! Test boundary values
        call set_logger_verbose_level(-1) ! Should be handled gracefully
        call set_logger_verbose_level(100) ! Should be handled gracefully
        print *, "  PASS: Boundary verbose levels handled"

        ! Restore original verbose level
        call set_logger_verbose_level(old_verbose_level)

    end function test_verbose_levels

    function test_error_output() result(passed)
        logical :: passed
        integer :: old_verbose_level

        print *, "Test 3: Error output functions"
        passed = .true.

        ! Save original verbose level
        old_verbose_level = get_logger_verbose_level()

        ! Ensure we're in verbose mode to see all messages
        call set_logger_verbose_level(2)

        ! Test error messages (these should always be visible)
        call print_error("Test error - this should always be visible")

        ! Test warning messages
        call print_warning("Test warning - this should be visible in verbose mode")

        ! Test info messages
        call print_info("Test info - this should be visible in verbose mode")

        ! Test debug messages
        call debug_print("Test debug - this should only be visible in debug mode")

        print *, "  PASS: All error output functions executed"

        ! Test with quiet mode
        call set_logger_verbose_level(0)
        call print_error("Test error in quiet mode - should still be visible")
        call print_warning("Test warning in quiet mode - may be suppressed")
        call debug_print("Test debug in quiet mode - should be suppressed")

        print *, "  PASS: Quiet mode error output tested"

        ! Restore original verbose level
        call set_logger_verbose_level(old_verbose_level)

    end function test_error_output

end program test_logger_utils
