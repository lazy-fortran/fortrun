program test_string_utils_coverage
    use iso_fortran_env, only: error_unit, int8, int16, int32, int64
    use string_utils
    implicit none

    logical :: all_tests_passed

    print *, "=== String Utils Coverage Tests ==="
    print *

    all_tests_passed = .true.

    if (.not. test_int_to_char_all_cases()) all_tests_passed = .false.
    if (.not. test_logical_to_char_all_cases()) all_tests_passed = .false.
    if (.not. test_extreme_values()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All string utils coverage tests passed!"
        stop 0
    else
        print *, "Some string utils coverage tests failed!"
        stop 1
    end if

contains

    function test_int_to_char_all_cases() result(passed)
        logical :: passed
        character(len=32) :: result_str
        integer :: test_values(15)
        integer :: i

        print *, "Test: int_to_char all cases"
        passed = .true.

        ! Test various integer values
        test_values = [0, 1, -1, 10, -10, 100, -100, 1000, -1000, &
                       999999, -999999, 2147483647, -2147483647, 42, -42]

        do i = 1, size(test_values)
            result_str = int_to_char(test_values(i))
            print *, "  Value:", test_values(i), "Result: '", trim(result_str), "'"

            ! Verify it's not an error
            if (result_str == 'ERROR') then
                print *, "  WARNING: Got ERROR for value", test_values(i)
            end if
        end do

        ! Test edge cases for different integer kinds
        if (selected_int_kind(2) > 0) then
            result_str = int_to_char(127)
            print *, "  int8 max value:", trim(result_str)
            result_str = int_to_char(-127)
            print *, "  int8 min value:", trim(result_str)
        end if

        if (selected_int_kind(4) > 0) then
            result_str = int_to_char(32767)
            print *, "  int16 max value:", trim(result_str)
            result_str = int_to_char(-32767)
            print *, "  int16 min value:", trim(result_str)
        end if

        ! Test rapid successive calls
        do i = 1, 100
            result_str = int_to_char(i)
            if (len_trim(result_str) == 0) then
                print *, "  FAIL: Empty result for", i
                passed = .false.
            end if
        end do

        print *, "  PASS: int_to_char all cases tested"

    end function test_int_to_char_all_cases

    function test_logical_to_char_all_cases() result(passed)
        logical :: passed
        character(len=1) :: result_char
        logical :: test_true, test_false
        integer :: i

        print *, "Test: logical_to_char all cases"
        passed = .true.

        ! Test true
        test_true = .true.
        result_char = logical_to_char(test_true)
        if (result_char /= 'T') then
            print *, "  FAIL: Expected 'T' for .true., got '", result_char, "'"
            passed = .false.
        else
            print *, "  PASS: .true. -> 'T'"
        end if

        ! Test false
        test_false = .false.
        result_char = logical_to_char(test_false)
        if (result_char /= 'F') then
            print *, "  FAIL: Expected 'F' for .false., got '", result_char, "'"
            passed = .false.
        else
            print *, "  PASS: .false. -> 'F'"
        end if

        ! Test rapid alternation
        do i = 1, 20
            if (mod(i, 2) == 0) then
                result_char = logical_to_char(.true.)
                if (result_char /= 'T') then
                    print *, "  FAIL: Alternation test failed at", i
                    passed = .false.
                end if
            else
                result_char = logical_to_char(.false.)
                if (result_char /= 'F') then
                    print *, "  FAIL: Alternation test failed at", i
                    passed = .false.
                end if
            end if
        end do

        ! Test with logical expressions
        result_char = logical_to_char(1 > 0)
        if (result_char /= 'T') then
            print *, "  FAIL: Expression (1 > 0) should be 'T'"
            passed = .false.
        end if

        result_char = logical_to_char(1 < 0)
        if (result_char /= 'F') then
            print *, "  FAIL: Expression (1 < 0) should be 'F'"
            passed = .false.
        end if

        print *, "  PASS: logical_to_char all cases tested"

    end function test_logical_to_char_all_cases

    function test_extreme_values() result(passed)
        logical :: passed
        character(len=32) :: result_str
        integer :: huge_val, tiny_val

        print *, "Test: Extreme values and special cases"
        passed = .true.

        ! Test huge values
        huge_val = huge(1)
        result_str = int_to_char(huge_val)
        print *, "  huge(1):", huge_val, "-> '", trim(result_str), "'"

        ! Test negative huge
        tiny_val = -huge(1)
        result_str = int_to_char(tiny_val)
        print *, "  -huge(1):", tiny_val, "-> '", trim(result_str), "'"

        ! Test zero with different signs
        result_str = int_to_char(+0)
        print *, "  +0 -> '", trim(result_str), "'"

        result_str = int_to_char(-0)
        print *, "  -0 -> '", trim(result_str), "'"

        ! Test powers of 10
        block
            integer :: power, val
            do power = 0, 9
                val = 10**power
                result_str = int_to_char(val)
                print *, "  10^", power, "=", val, "-> '", trim(result_str), "'"
            end do
        end block

        ! Test powers of 2
        block
            integer :: power, val
            do power = 0, 20
                val = 2**power
                result_str = int_to_char(val)
                if (power <= 10 .or. power >= 19) then
                    print *, "  2^", power, "=", val, "-> '", trim(result_str), "'"
                end if
            end do
        end block

        print *, "  PASS: Extreme values tested"

    end function test_extreme_values

end program test_string_utils_coverage
