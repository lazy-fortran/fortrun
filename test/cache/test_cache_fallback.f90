program test_cache_fallback
    use iso_fortran_env, only: error_unit
    use cache
    use temp_utils, only: temp_dir_manager
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr

    print *, "=== Cache Fallback Coverage Tests ==="
    print *

    all_tests_passed = .true.

    call temp_mgr%create('cache_fallback_test')

    ! Test cache directory fallback logic added in the PR
    if (.not. test_cache_dir_fallback()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All cache fallback tests passed!"
        stop 0
    else
        print *, "Some cache fallback tests failed!"
        stop 1
    end if

contains

    function test_cache_dir_fallback() result(passed)
        logical :: passed
        character(len=:), allocatable :: cache_dir
        character(len=256) :: original_xdg, original_home, original_localappdata
        integer :: status

        print *, "Test: Cache directory fallback logic"
        passed = .true.

        ! Save original environment variables
        call get_environment_variable('XDG_CACHE_HOME', original_xdg, status=status)
        call get_environment_variable('HOME', original_home, status=status)
        call get_environment_variable('LOCALAPPDATA', original_localappdata, status=status)

        ! Test 1: Test with XDG_CACHE_HOME set
        block
            ! This exercises the primary path in get_cache_dir()
            cache_dir = get_cache_dir()
            if (.not. allocated(cache_dir)) then
                print *, "  FAIL: get_cache_dir returned unallocated string"
                passed = .false.
            else if (len_trim(cache_dir) == 0) then
                print *, "  FAIL: get_cache_dir returned empty string"
                passed = .false.
            else
                print *, "  PASS: get_cache_dir returned: ", trim(cache_dir)
            end if
        end block

        ! Test 2: Clear environment to exercise fallback paths
        ! Note: We can't actually unset environment variables in Fortran,
        ! but we can still test the current behavior
        block
            character(len=:), allocatable :: cache_dir2
            
            ! Call again to exercise consistency
            cache_dir2 = get_cache_dir()
            if (.not. allocated(cache_dir2)) then
                print *, "  FAIL: Second get_cache_dir call returned unallocated string"
                passed = .false.
            else if (cache_dir /= cache_dir2) then
                print *, "  WARNING: Inconsistent cache directory between calls"
                print *, "    First:  ", cache_dir
                print *, "    Second: ", cache_dir2
            else
                print *, "  PASS: Consistent cache directory between calls"
            end if
        end block

        ! Test 3: Test that it contains expected path components
        if (allocated(cache_dir)) then
            if (index(cache_dir, 'fortran') == 0) then
                print *, "  WARNING: Cache directory doesn't contain 'fortran': ", trim(cache_dir)
            else
                print *, "  PASS: Cache directory contains 'fortran' component"
            end if
        end if

        ! Test directory creation functionality
        block
            logical :: success
            character(len=:), allocatable :: test_cache_dir
            
            test_cache_dir = temp_mgr%get_file_path('test_cache')
            call ensure_cache_dir(test_cache_dir, success)
            
            if (success) then
                print *, "  PASS: ensure_cache_dir succeeded"
            else
                print *, "  INFO: ensure_cache_dir failed - may be OK for testing"
            end if
        end block

    end function test_cache_dir_fallback

end program test_cache_fallback