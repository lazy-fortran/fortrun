program test_string_utils
    use iso_fortran_env, only: error_unit
    use string_utils
    use fpm_filesystem, only: join_path
    implicit none

    logical :: all_tests_passed

    print *, "=== String Utils Tests ==="
    print *

    all_tests_passed = .true.

    ! Test string utility functions
    if (.not. test_int_to_char()) all_tests_passed = .false.
    if (.not. test_logical_to_char()) all_tests_passed = .false.
    if (.not. test_join_path()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All string utils tests passed!"
        stop 0
    else
        print *, "Some string utils tests failed!"
        stop 1
    end if

contains

    function test_int_to_char() result(passed)
        logical :: passed
        character(len=:), allocatable :: result

        print *, "Test 1: Integer to character conversion"
        passed = .true.

        ! Test positive integer
        result = int_to_char(42)
        if (result /= "42") then
            print *, "  FAIL: Expected '42', got '", result, "'"
            passed = .false.
        else
            print *, "  PASS: Positive integer converted correctly"
        end if

        ! Test negative integer
        result = int_to_char(-123)
        if (result /= "-123") then
            print *, "  FAIL: Expected '-123', got '", result, "'"
            passed = .false.
        else
            print *, "  PASS: Negative integer converted correctly"
        end if

        ! Test zero
        result = int_to_char(0)
        if (result /= "0") then
            print *, "  FAIL: Expected '0', got '", result, "'"
            passed = .false.
        else
            print *, "  PASS: Zero converted correctly"
        end if

        ! Test large integer
        result = int_to_char(999999)
        if (result /= "999999") then
            print *, "  FAIL: Expected '999999', got '", result, "'"
            passed = .false.
        else
            print *, "  PASS: Large integer converted correctly"
        end if

    end function test_int_to_char

    function test_logical_to_char() result(passed)
        logical :: passed
        character(len=:), allocatable :: result

        print *, "Test 2: Logical to character conversion"
        passed = .true.

        ! Test true
        result = logical_to_char(.true.)
        if (result /= "T") then
            print *, "  FAIL: Expected 'T' for true, got '", result, "'"
            passed = .false.
        else
            print *, "  PASS: True converted correctly"
        end if

        ! Test false
        result = logical_to_char(.false.)
        if (result /= "F") then
            print *, "  FAIL: Expected 'F' for false, got '", result, "'"
            passed = .false.
        else
            print *, "  PASS: False converted correctly"
        end if

    end function test_logical_to_char

    function test_join_path() result(passed)
        logical :: passed
        character(len=:), allocatable :: result
        character(len=:), allocatable :: path1, path2

        print *, "Test 3: Join path function"
        passed = .true.

        ! Test basic path joining
        path1 = "home"
        path2 = "user"
        result = join_path(path1, path2)
        if (len_trim(result) == 0) then
            print *, "  FAIL: Empty result from join_path"
            passed = .false.
        else if (index(result, "home") == 0 .or. index(result, "user") == 0) then
            print *, "  FAIL: Path components missing in result: ", result
            passed = .false.
        else
            print *, "  PASS: Basic path joining: ", result
        end if

        ! Test empty components
        path1 = ""
        path2 = "user"
        result = join_path(path1, path2)
        if (result /= "user") then
            print *, "  FAIL: Empty first component should return second: ", result
            passed = .false.
        else
            print *, "  PASS: Empty first component handled correctly"
        end if

        path1 = "home"
        path2 = ""
        result = join_path(path1, path2)
        ! On Windows, join_path might add trailing separator, so check both possibilities
        if (result /= "home" .and. result /= "home\" .and. result /= "home/") then
            print *, "  FAIL: Empty second component should return first (or with separator): ", result
            passed = .false.
        else
            print *, "  PASS: Empty second component handled correctly: ", result
        end if

        ! Test both empty
        path1 = ""
        path2 = ""
        result = join_path(path1, path2)
        if (len_trim(result) /= 0) then
            print *, "  FAIL: Both empty should return empty: ", result
            passed = .false.
        else
            print *, "  PASS: Both empty handled correctly"
        end if

        ! Test with separator already present
        path1 = "home/"
        path2 = "user"
        result = join_path(path1, path2)
        if (len_trim(result) == 0) then
            print *, "  FAIL: Empty result with trailing separator"
            passed = .false.
        else if (index(result, "home") == 0 .or. index(result, "user") == 0) then
            print *, "  FAIL: Components missing with trailing separator: ", result
            passed = .false.
        else
            print *, "  PASS: Trailing separator handled: ", result
        end if

    end function test_join_path

end program test_string_utils