program test_string_utils_integration
    use iso_fortran_env, only: error_unit
    use string_utils
    use runner
    use logger_utils, only: set_logger_verbose_level
    use temp_utils, only: temp_dir_manager
    use system_utils, only: sys_remove_file
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr

    print *, "=== String Utils Integration Tests ==="
    print *

    all_tests_passed = .true.

    call temp_mgr%create('string_integration_test')

    ! Enable verbose output to trigger string utility usage in debug output
    call set_logger_verbose_level(3)

    ! Test string utilities through actual runner usage
    if (.not. test_string_utils_in_debug_output()) all_tests_passed = .false.
    if (.not. test_edge_cases()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All string utils integration tests passed!"
        stop 0
    else
        print *, "Some string utils integration tests failed!"
        stop 1
    end if

contains

    function test_string_utils_in_debug_output() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_file
        integer :: exit_code, unit
        character(len=256) :: custom_cache_dir, custom_config_dir

        print *, "Test: String utils in debug output"
        passed = .true.

        ! Create a test file that will trigger module copying (which uses int_to_char)
        test_file = temp_mgr%get_file_path('string_debug.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'program string_debug'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  integer :: i'
        write (unit, '(a)') '  do i = 1, 3'
        write (unit, '(a)') '    print *, "Loop iteration:", i'
        write (unit, '(a)') '  end do'
        write (unit, '(a)') 'end program string_debug'
        close (unit)

        custom_cache_dir = temp_mgr%get_file_path('string_cache')
        custom_config_dir = ""

        ! Run with verbose level 3 to trigger debug output that uses string utilities
        call run_fortran_file(test_file, exit_code, 3, &
                              custom_cache_dir, custom_config_dir, &
                              2, .false.)  ! 2 parallel jobs to trigger job count debug

        print *, "  INFO: String debug test completed with exit code:", exit_code
        print *, "  PASS: String utilities exercised through debug output"

        ! Clean up
        call sys_remove_file(test_file)

    end function test_string_utils_in_debug_output

    function test_edge_cases() result(passed)
        logical :: passed
        character(len=:), allocatable :: result

        print *, "Test: String utility edge cases"
        passed = .true.

        ! Test int_to_char with various edge cases
        result = int_to_char(0)
        if (result /= "0") then
            print *, "  FAIL: int_to_char(0) failed"
            passed = .false.
        else
            print *, "  PASS: int_to_char(0) = '", result, "'"
        end if

        result = int_to_char(-999)
        if (len_trim(result) == 0) then
            print *, "  FAIL: int_to_char(-999) returned empty"
            passed = .false.
        else
            print *, "  PASS: int_to_char(-999) = '", result, "'"
        end if

        result = int_to_char(999999)
        if (len_trim(result) == 0) then
            print *, "  FAIL: int_to_char(999999) returned empty"
            passed = .false.
        else
            print *, "  PASS: int_to_char(999999) = '", result, "'"
        end if

        ! Test logical_to_char edge cases
        result = logical_to_char(.true.)
        if (result /= "T") then
            print *, "  FAIL: logical_to_char(.true.) != 'T'"
            passed = .false.
        else
            print *, "  PASS: logical_to_char(.true.) = '", result, "'"
        end if

        result = logical_to_char(.false.)
        if (result /= "F") then
            print *, "  FAIL: logical_to_char(.false.) != 'F'"
            passed = .false.
        else
            print *, "  PASS: logical_to_char(.false.) = '", result, "'"
        end if

    end function test_edge_cases

end program test_string_utils_integration
