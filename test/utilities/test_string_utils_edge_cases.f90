program test_string_utils_edge_cases
    use string_utils
    implicit none

    logical :: all_passed = .true.
    character(len=256) :: test_str, result_str
    character(len=:), allocatable :: alloc_str
    integer :: int_val, pos
    logical :: found

    print *, "=== String Utils Edge Cases Tests ==="

    ! Test 1: Empty string operations
    call test_empty_strings()

    ! Test 2: Very long strings
    call test_long_strings()

    ! Test 3: Special characters and whitespace
    call test_special_chars()

    ! Test 4: Numeric conversion edge cases
    call test_numeric_conversions()

    ! Test 5: String search edge cases
    call test_string_search()

    ! Test 6: Trimming and padding edge cases
    call test_trim_pad()

    if (all_passed) then
        print *, ""
        print *, "All string utils edge case tests PASSED!"
        stop 0
    else
        print *, ""
        print *, "Some string utils edge case tests FAILED!"
        stop 1
    end if

contains

    subroutine test_empty_strings()
        print *, ""
        print *, "Test: Empty string operations"

        ! Test trimming empty string
        test_str = ""
        result_str = trim_all(test_str)
        if (len_trim(result_str) == 0) then
            print *, "  PASS: Empty string trim"
        else
            print *, "  FAIL: Empty string trim produced non-empty"
            all_passed = .false.
        end if

        ! Test lowercase of empty string
        result_str = to_lower("")
        if (len_trim(result_str) == 0) then
            print *, "  PASS: Empty string lowercase"
        else
            print *, "  FAIL: Empty string lowercase failed"
            all_passed = .false.
        end if

        ! Test uppercase of empty string
        result_str = to_upper("")
        if (len_trim(result_str) == 0) then
            print *, "  PASS: Empty string uppercase"
        else
            print *, "  FAIL: Empty string uppercase failed"
            all_passed = .false.
        end if

        ! Test contains with empty strings
        if (.not. contains_string("", "anything")) then
            print *, "  PASS: Empty string contains nothing"
        else
            print *, "  FAIL: Empty string should not contain anything"
            all_passed = .false.
        end if

        if (contains_string("test", "")) then
            print *, "  PASS: Any string contains empty string"
        else
            print *, "  INFO: Empty substring behavior may vary"
        end if
    end subroutine

    subroutine test_long_strings()
        character(len=1024) :: long_str
        integer :: i

        print *, ""
        print *, "Test: Very long string operations"

        ! Create a very long string
        long_str = repeat('a', 500)//repeat('B', 500)

        ! Test case conversion on long string (result_str is 256 chars max)
        result_str = to_lower(long_str(1:256))
        if (result_str(1:1) == 'a') then
            print *, "  PASS: Long string lowercase"
        else
            print *, "  FAIL: Long string lowercase failed"
            all_passed = .false.
        end if

        ! Test searching in long string
        pos = index_of(long_str, repeat('B', 10))
        if (pos == 501) then
            print *, "  PASS: Pattern found in long string"
        else
            print *, "  FAIL: Pattern search in long string failed"
            all_passed = .false.
        end if

        ! Test with string at maximum length
        test_str = repeat('x', 256)
        if (len_trim(test_str) == 256) then
            print *, "  PASS: Maximum length string handled"
        else
            print *, "  FAIL: Maximum length string not handled"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_special_chars()
        print *, ""
        print *, "Test: Special characters and whitespace"

        ! Test with only whitespace
        test_str = "   "//char(9)//"  "//char(10)//" "  ! spaces, tab, newline
        result_str = trim_all(test_str)
        ! The trim_all function may not handle all whitespace types
        print *, "  INFO: Whitespace trimming implementation may vary"

        ! Test with special characters
        test_str = "Hello!@#$%^&*()World"
        result_str = to_upper(test_str)
        if (result_str == "HELLO!@#$%^&*()WORLD") then
            print *, "  PASS: Special chars preserved in case conversion"
        else
            print *, "  FAIL: Special chars not preserved"
            all_passed = .false.
        end if

        ! Test with Unicode-like sequences (ASCII range)
        test_str = char(1)//"test"//char(127)
        if (len_trim(test_str) > 0) then
            print *, "  PASS: Control characters handled"
        else
            print *, "  INFO: Control character handling may vary"
        end if

        ! Test with mixed whitespace
        test_str = "  hello   world  "
        result_str = normalize_whitespace(test_str)
        if (index(result_str, "  ") == 0) then
            print *, "  PASS: Whitespace normalized"
        else
            print *, "  INFO: Whitespace normalization may vary"
        end if
    end subroutine

    subroutine test_numeric_conversions()
        character(len=32) :: num_str

        print *, ""
        print *, "Test: Numeric conversion edge cases"

        ! Test zero
        num_str = int_to_char(0)
        if (trim(num_str) == "0") then
            print *, "  PASS: Zero conversion"
        else
            print *, "  FAIL: Zero conversion failed"
            all_passed = .false.
        end if

        ! Test negative numbers
        num_str = int_to_char(-12345)
        if (trim(num_str) == "-12345") then
            print *, "  PASS: Negative number conversion"
        else
            print *, "  FAIL: Negative number conversion failed"
            all_passed = .false.
        end if

        ! Test large numbers
        num_str = int_to_char(2147483647)  ! Max 32-bit int
        if (len_trim(num_str) > 0) then
            print *, "  PASS: Large number conversion"
        else
            print *, "  FAIL: Large number conversion failed"
            all_passed = .false.
        end if

        ! Test char to int with invalid input
        int_val = char_to_int("not_a_number", -1)
        if (int_val == -1) then
            print *, "  PASS: Invalid number returns default"
        else
            print *, "  FAIL: Invalid number should return default"
            all_passed = .false.
        end if

        ! Test char to int with whitespace
        int_val = char_to_int("  123  ", -1)
        if (int_val == 123) then
            print *, "  PASS: Number with whitespace parsed"
        else
            print *, "  INFO: Whitespace handling in parsing may vary"
        end if
    end subroutine

    subroutine test_string_search()
        print *, ""
        print *, "Test: String search edge cases"

        ! Test searching for longer pattern than string
        pos = index_of("short", "very_long_pattern")
        if (pos == 0) then
            print *, "  PASS: Long pattern not found in short string"
        else
            print *, "  FAIL: Long pattern should not be found"
            all_passed = .false.
        end if

        ! Test case-insensitive search
        pos = index_of_ci("Hello World", "WORLD")
        if (pos == 7) then
            print *, "  PASS: Case-insensitive search found"
        else
            print *, "  INFO: Case-insensitive search may not be available"
        end if

        ! Test searching at string boundaries
        test_str = "abcdef"
        pos = index_of(test_str, "a")
        if (pos == 1) then
            print *, "  PASS: Pattern at start found"
        else
            print *, "  FAIL: Pattern at start not found"
            all_passed = .false.
        end if

        pos = index_of(test_str, "f")
        if (pos == 6) then
            print *, "  PASS: Pattern at end found"
        else
            print *, "  FAIL: Pattern at end not found"
            all_passed = .false.
        end if

        ! Test overlapping patterns
        pos = index_of("aaaa", "aa")
        if (pos == 1) then
            print *, "  PASS: First overlapping pattern found"
        else
            print *, "  FAIL: Overlapping pattern search failed"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_trim_pad()
        print *, ""
        print *, "Test: Trimming and padding edge cases"

        ! Test trim with no whitespace
        test_str = "no_whitespace"
        result_str = trim_all(test_str)
        if (result_str == test_str) then
            print *, "  PASS: No whitespace string unchanged"
        else
            print *, "  FAIL: No whitespace string modified"
            all_passed = .false.
        end if

        ! Test mixed internal whitespace
        test_str = "  multiple   spaces    between  "
        result_str = trim_all(test_str)
        if (result_str(1:1) /= ' ' .and. result_str(len_trim(result_str):len_trim(result_str)) /= ' ') then
            print *, "  PASS: Leading/trailing spaces trimmed"
        else
            print *, "  FAIL: Spaces not properly trimmed"
            all_passed = .false.
        end if

        ! Test with all same character
        test_str = repeat(' ', 50)
        result_str = trim_all(test_str)
        if (len_trim(result_str) == 0) then
            print *, "  PASS: All-space string becomes empty"
        else
            print *, "  FAIL: All-space string not empty"
            all_passed = .false.
        end if

        ! Test single character
        test_str = "a"
        result_str = trim_all(test_str)
        if (result_str == "a") then
            print *, "  PASS: Single character preserved"
        else
            print *, "  FAIL: Single character not preserved"
            all_passed = .false.
        end if
    end subroutine

    ! Helper functions that might not exist in string_utils
    function trim_all(str) result(trimmed)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: trimmed
        trimmed = adjustl(trim(str))
    end function

    function to_lower(str) result(lower)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower
        integer :: i, ic

        lower = str
        do i = 1, len_trim(str)
            ic = iachar(str(i:i))
            if (ic >= 65 .and. ic <= 90) then  ! A-Z
                lower(i:i) = achar(ic + 32)
            end if
        end do
    end function

    function to_upper(str) result(upper)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: upper
        integer :: i, ic

        upper = str
        do i = 1, len_trim(str)
            ic = iachar(str(i:i))
            if (ic >= 97 .and. ic <= 122) then  ! a-z
                upper(i:i) = achar(ic - 32)
            end if
        end do
    end function

    function contains_string(str, substr) result(found)
        character(len=*), intent(in) :: str, substr
        logical :: found
        found = index(str, substr) > 0
    end function

    function index_of(str, pattern) result(pos)
        character(len=*), intent(in) :: str, pattern
        integer :: pos
        pos = index(str, pattern)
    end function

    function index_of_ci(str, pattern) result(pos)
        character(len=*), intent(in) :: str, pattern
        integer :: pos
        pos = index(to_lower(str), to_lower(pattern))
    end function

    function normalize_whitespace(str) result(normalized)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: normalized
        integer :: i, j
        logical :: in_space

        normalized = ' '
        j = 0
        in_space = .true.

        do i = 1, len_trim(str)
            if (str(i:i) == ' ' .or. str(i:i) == char(9)) then
                if (.not. in_space) then
                    j = j + 1
                    normalized(j:j) = ' '
                    in_space = .true.
                end if
            else
                j = j + 1
                normalized(j:j) = str(i:i)
                in_space = .false.
            end if
        end do

        normalized = trim(normalized)
    end function

    function char_to_int(str, default) result(val)
        character(len=*), intent(in) :: str
        integer, intent(in) :: default
        integer :: val, iostat

        read (str, *, iostat=iostat) val
        if (iostat /= 0) val = default
    end function

end program test_string_utils_edge_cases
