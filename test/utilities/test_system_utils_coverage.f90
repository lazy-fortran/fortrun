program test_system_utils_coverage
    use iso_fortran_env, only: error_unit
    use system_utils
    use temp_utils, only: temp_dir_manager
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr

    print *, "=== System Utils Coverage Tests ==="
    print *

    all_tests_passed = .true.

    call temp_mgr%create('system_utils_coverage')

    ! Test just the key functions to improve coverage
    if (.not. test_basic_functions()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All system utils coverage tests passed!"
        stop 0
    else
        print *, "Some system utils coverage tests failed!"
        stop 1
    end if

contains

    function test_basic_functions() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_dir, test_file
        character(len=:), allocatable :: redirect, temp_dir
        logical :: exists_result

        print *, "Test: Basic system functions"
        passed = .true.

        ! Test stderr redirect
        redirect = get_stderr_redirect()
        if (.not. allocated(redirect) .or. len_trim(redirect) == 0) then
            print *, "  FAIL: get_stderr_redirect failed"
            passed = .false.
        else
            print *, "  PASS: get_stderr_redirect works"
        end if

        ! Test temp directory
        temp_dir = sys_get_temp_dir()
        if (.not. allocated(temp_dir) .or. len_trim(temp_dir) == 0) then
            print *, "  FAIL: sys_get_temp_dir failed"
            passed = .false.
        else
            print *, "  PASS: sys_get_temp_dir works"
        end if

        ! Test directory creation and checks
        test_dir = temp_mgr%get_file_path('test_coverage_dir')
        call sys_create_dir(test_dir)
        if (.not. sys_dir_exists(test_dir)) then
            print *, "  FAIL: sys_create_dir/sys_dir_exists failed"
            passed = .false.
        else
            print *, "  PASS: sys_create_dir/sys_dir_exists works"
            call sys_remove_dir(test_dir)
        end if

        ! Test file existence check
        test_file = temp_mgr%get_file_path('nonexistent.txt')
        if (sys_file_exists(test_file)) then
            print *, "  FAIL: sys_file_exists should return false for nonexistent file"
            passed = .false.
        else
            print *, "  PASS: sys_file_exists correctly detects nonexistent file"
        end if

    end function test_basic_functions

end program test_system_utils_coverage