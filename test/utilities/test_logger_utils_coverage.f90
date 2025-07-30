program test_logger_utils_coverage
    use iso_fortran_env, only: error_unit
    use logger_utils
    implicit none

    logical :: all_tests_passed

    print *, "=== Logger Utils Coverage Tests ==="
    print *

    all_tests_passed = .true.

    if (.not. test_all_verbose_levels()) all_tests_passed = .false.
    if (.not. test_edge_case_messages()) all_tests_passed = .false.
    if (.not. test_verbose_level_persistence()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All logger utils coverage tests passed!"
        stop 0
    else
        print *, "Some logger utils coverage tests failed!"
        stop 1
    end if

contains

    function test_all_verbose_levels() result(passed)
        logical :: passed
        integer :: level

        print *, "Test: All verbose levels"
        passed = .true.

        ! Test setting and getting verbose levels from -1 to 5
        do level = -1, 5
            call set_logger_verbose_level(level)
            if (get_logger_verbose_level() /= level) then
                print *, "  FAIL: Level mismatch at", level
                passed = .false.
            end if
        end do

        ! Test at each level
        print *, "  Testing verbose level -1 (silent):"
        call set_logger_verbose_level(-1)
        call debug_print("This debug should not appear")
        call print_info("This info should not appear")
        call print_warning("This warning SHOULD appear")
        call print_error("This error SHOULD appear")

        print *, "  Testing verbose level 0 (warnings/errors only):"
        call set_logger_verbose_level(0)
        call debug_print("This debug should not appear")
        call print_info("This info should not appear")
        call print_warning("This warning SHOULD appear")
        call print_error("This error SHOULD appear")

        print *, "  Testing verbose level 1 (info+):"
        call set_logger_verbose_level(1)
        call debug_print("This debug should not appear")
        call print_info("This info SHOULD appear")
        call print_warning("This warning SHOULD appear")
        call print_error("This error SHOULD appear")

        print *, "  Testing verbose level 2 (debug+):"
        call set_logger_verbose_level(2)
        call debug_print("This debug SHOULD appear")
        call print_info("This info SHOULD appear")
        call print_warning("This warning SHOULD appear")
        call print_error("This error SHOULD appear")

        print *, "  Testing verbose level 3+ (everything):"
        call set_logger_verbose_level(3)
        call debug_print("Debug at level 3")
        call print_info("Info at level 3")
        call print_warning("Warning at level 3")
        call print_error("Error at level 3")

        print *, "  PASS: All verbose levels tested"

    end function test_all_verbose_levels

    function test_edge_case_messages() result(passed)
        logical :: passed

        print *, "Test: Edge case messages"
        passed = .true.

        call set_logger_verbose_level(3)

        ! Test empty messages
        call debug_print("")
        call print_info("")
        call print_warning("")
        call print_error("")

        ! Test very long messages
        call debug_print(repeat("x", 200))
        call print_info(repeat("y", 200))
        call print_warning(repeat("z", 200))
        call print_error(repeat("!", 200))

        ! Test messages with special characters
        call debug_print("Tab"//char(9)//"test")
        call print_info("Newline"//char(10)//"test")
        call print_warning("Quote""test")
        call print_error("Backslash\test")

        ! Test unicode-like messages
        call debug_print("Test with spaces     and    tabs")
        call print_info("Test-with-hyphens-and_underscores")
        call print_warning("Test@with#special$chars%")
        call print_error("Test[with]brackets{and}braces")

        print *, "  PASS: Edge case messages handled"

    end function test_edge_case_messages

    function test_verbose_level_persistence() result(passed)
        logical :: passed
        integer :: i, level

        print *, "Test: Verbose level persistence across calls"
        passed = .true.

        ! Test rapid level changes
        do i = 1, 10
            level = mod(i, 4)
            call set_logger_verbose_level(level)
            if (get_logger_verbose_level() /= level) then
                print *, "  FAIL: Level not persistent at iteration", i
                passed = .false.
            end if
        end do

        ! Test that level persists after logging
        call set_logger_verbose_level(2)
        call debug_print("Test message")
        if (get_logger_verbose_level() /= 2) then
            print *, "  FAIL: Level changed after debug_print"
            passed = .false.
        end if

        call print_info("Another test")
        if (get_logger_verbose_level() /= 2) then
            print *, "  FAIL: Level changed after print_info"
            passed = .false.
        end if

        call print_warning("Warning test")
        if (get_logger_verbose_level() /= 2) then
            print *, "  FAIL: Level changed after print_warning"
            passed = .false.
        end if

        call print_error("Error test")
        if (get_logger_verbose_level() /= 2) then
            print *, "  FAIL: Level changed after print_error"
            passed = .false.
        end if

        print *, "  PASS: Verbose level persistence verified"

    end function test_verbose_level_persistence

end program test_logger_utils_coverage
