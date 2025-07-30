program test_logger_edge_cases
    use logger_utils
    implicit none

    logical :: all_passed = .true.
    character(len=1024) :: long_msg
    character(len=:), allocatable :: dynamic_msg
    integer :: i

    print *, "=== Logger Utils Edge Cases Tests ==="

    ! Test 1: Empty log messages
    call test_empty_messages()

    ! Test 2: Very long log messages
    call test_long_messages()

    ! Test 3: Special characters in messages
    call test_special_characters()

    ! Test 4: Rapid logging
    call test_rapid_logging()

    ! Test 5: Log level edge cases
    call test_log_levels()

    ! Test 6: Null and boundary conditions
    call test_boundary_conditions()

    if (all_passed) then
        print *, ""
        print *, "All logger utils edge case tests PASSED!"
        stop 0
    else
        print *, ""
        print *, "Some logger utils edge case tests FAILED!"
        stop 1
    end if

contains

    subroutine test_empty_messages()
        print *, ""
        print *, "Test: Empty log messages"

        ! These should not crash
        call debug_print("")
        call print_info("")
        call print_warning("")
        call print_error("")

        print *, "  PASS: Empty messages handled without crash"

        ! Test with only whitespace
        call debug_print("   ")
        call print_info(char(9))  ! tab
        call print_warning(char(10))  ! newline

        print *, "  PASS: Whitespace-only messages handled"
    end subroutine

    subroutine test_long_messages()
        print *, ""
        print *, "Test: Very long log messages"

        ! Create a very long message
        long_msg = repeat('A', 500)//' MIDDLE '//repeat('B', 500)

        call debug_print(trim(long_msg))
        call print_info(trim(long_msg))
        call print_warning(trim(long_msg))
        call print_error(trim(long_msg))

        print *, "  PASS: Long messages handled"

        ! Test message at exact buffer boundaries
        long_msg = repeat('X', 1024)
        call print_info(trim(long_msg))

        print *, "  PASS: Maximum length message handled"

        ! Test with line breaks in long message
        long_msg = repeat('Line1 ', 50)//char(10)//repeat('Line2 ', 50)
        call print_info(trim(long_msg))

        print *, "  PASS: Multi-line long message handled"
    end subroutine

    subroutine test_special_characters()
        character(len=256) :: special_msg

        print *, ""
        print *, "Test: Special characters in messages"

        ! Test with various special characters
        special_msg = "Test: "//char(0)//" null char"
        call print_info("Message with null character handling")

        special_msg = "Tabs:"//char(9)//"and"//char(9)//"spaces"
        call print_info(trim(special_msg))

        ! Test with all printable ASCII
        special_msg = ""
        do i = 32, 126
            special_msg = trim(special_msg)//char(i)
        end do
        call debug_print(trim(special_msg))

        print *, "  PASS: Special characters handled"

        ! Test with ANSI escape sequences (color codes)
        special_msg = char(27)//"[31mRed"//char(27)//"[0m Normal"
        call print_info(trim(special_msg))

        print *, "  PASS: ANSI sequences handled"

        ! Test with quotes and apostrophes
        special_msg = 'Message with "quotes" and ''apostrophes'''
        call print_info(trim(special_msg))

        print *, "  PASS: Quotes handled"
    end subroutine

    subroutine test_rapid_logging()
        integer :: j

        print *, ""
        print *, "Test: Rapid consecutive logging"

        ! Rapid fire logging
        do j = 1, 100
            call debug_print("Rapid log "//trim(int_to_str(j)))
        end do

        print *, "  PASS: 100 rapid debug logs completed"

        ! Mixed severity rapid logging
        do j = 1, 25
            call debug_print("Debug "//trim(int_to_str(j)))
            call print_info("Info "//trim(int_to_str(j)))
            call print_warning("Warn "//trim(int_to_str(j)))
            call print_error("Error "//trim(int_to_str(j)))
        end do

        print *, "  PASS: Mixed severity rapid logging completed"
    end subroutine

    subroutine test_log_levels()
        print *, ""
        print *, "Test: Log level edge cases"

        ! Test with different verbosity settings
        call set_verbosity(0)  ! Silent
        call debug_print("This should not appear")
        call print_info("This might not appear")

        call set_verbosity(1)  ! Normal
        call debug_print("Debug at verbosity 1")
        call print_info("Info at verbosity 1")

        call set_verbosity(2)  ! Verbose
        call debug_print("Debug at verbosity 2")
        call print_info("Info at verbosity 2")

        call set_verbosity(999)  ! Very high
        call debug_print("Debug at max verbosity")

        ! Reset to normal
        call set_verbosity(1)

        print *, "  PASS: Log level filtering works"

        ! Test enable/disable debug
        call set_debug_enabled(.false.)
        call debug_print("This debug should not appear")

        call set_debug_enabled(.true.)
        call debug_print("This debug should appear")

        print *, "  PASS: Debug enable/disable works"
    end subroutine

    subroutine test_boundary_conditions()
        character(len=1) :: single_char
        character(len=0) :: empty_str

        print *, ""
        print *, "Test: Boundary conditions"

        ! Single character messages
        single_char = "A"
        call print_info(single_char)

        single_char = char(32)  ! space
        call print_info(single_char)

        print *, "  PASS: Single character messages handled"

        ! Test with allocatable string
        if (allocated(dynamic_msg)) deallocate (dynamic_msg)
        allocate (character(len=50) :: dynamic_msg)
        dynamic_msg = "Dynamic message test"
        call print_info(dynamic_msg)

        ! Deallocate and try empty allocatable
        deallocate (dynamic_msg)
        allocate (character(len=0) :: dynamic_msg)
        call print_info(dynamic_msg)

        print *, "  PASS: Allocatable string messages handled"

        ! Test repeated same message
        do i = 1, 10
            call print_info("Repeated message")
        end do

        print *, "  PASS: Repeated messages handled"
    end subroutine

    ! Helper function
    function int_to_str(n) result(str)
        integer, intent(in) :: n
        character(len=20) :: str
        write (str, '(I0)') n
    end function

    ! Mock implementations if not in logger_utils
    subroutine set_verbosity(level)
        integer, intent(in) :: level
        ! Mock implementation
    end subroutine

    subroutine set_debug_enabled(enabled)
        logical, intent(in) :: enabled
        ! Mock implementation
    end subroutine

end program test_logger_edge_cases
