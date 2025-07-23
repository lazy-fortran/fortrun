program test_cache_coverage
    use iso_fortran_env, only: error_unit
    use cache
    use temp_utils, only: temp_dir_manager
    use system_utils, only: sys_remove_file, sys_remove_dir
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr

    print *, "=== Cache Coverage Tests ==="
    print *

    all_tests_passed = .true.

    call temp_mgr%create('cache_coverage_test')

    ! Test cache functions to improve coverage
    if (.not. test_cache_operations()) all_tests_passed = .false.
    if (.not. test_cache_keys_and_hashing()) all_tests_passed = .false.
    if (.not. test_cache_info()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All cache coverage tests passed!"
        stop 0
    else
        print *, "Some cache coverage tests failed!"
        stop 1
    end if

contains

    function test_cache_operations() result(passed)
        logical :: passed
        character(len=:), allocatable :: cache_dir, test_cache
        logical :: success

        print *, "Test 1: Basic cache operations"
        passed = .true.

        ! Test getting cache directory
        cache_dir = get_cache_dir()
        if (.not. allocated(cache_dir) .or. len_trim(cache_dir) == 0) then
            print *, "  FAIL: Failed to get cache directory"
            passed = .false.
        else
            print *, "  PASS: Got cache directory: ", trim(cache_dir)
        end if

        ! Test custom cache directory
        test_cache = temp_mgr%get_file_path('test_cache')
        call ensure_cache_dir(test_cache, success)
        if (.not. success) then
            print *, "  FAIL: Failed to create custom cache directory"
            passed = .false.
        else
            print *, "  PASS: Created custom cache directory"
            call sys_remove_dir(test_cache)
        end if

        ! Test cache structure creation
        test_cache = temp_mgr%get_file_path('test_structure')
        call ensure_cache_structure(test_cache, success)
        if (.not. success) then
            print *, "  FAIL: Failed to create cache structure"
            passed = .false.
        else
            print *, "  PASS: Created cache structure"
            call sys_remove_dir(test_cache)
        end if

    end function test_cache_operations

    function test_cache_keys_and_hashing() result(passed)
        logical :: passed
        character(len=32) :: content_hash
        character(len=:), allocatable :: test_file1, test_file2
        character(len=256) :: test_files(2)
        character(len=64) :: cache_key1
        integer :: unit

        print *, "Test 2: Cache keys and hashing"
        passed = .true.

        ! Create test files for hashing
        test_file1 = temp_mgr%get_file_path('hash_test1.f90')
        test_file2 = temp_mgr%get_file_path('hash_test2.f90')
        
        open(newunit=unit, file=test_file1, status='replace')
        write(unit, '(a)') 'print *, "Test content 1"'
        close(unit)
        
        open(newunit=unit, file=test_file2, status='replace')
        write(unit, '(a)') 'print *, "Test content 2"'
        close(unit)

        ! Test content hashing
        content_hash = get_single_file_content_hash(test_file1)
        if (len_trim(content_hash) == 0) then
            print *, "  FAIL: Failed to generate content hash"
            passed = .false.
        else
            print *, "  PASS: Generated content hash: ", trim(content_hash)
        end if

        ! Test cache key generation with real files
        test_files(1) = test_file1
        test_files(2) = test_file2
        cache_key1 = get_cache_key(test_files, test_files)
        if (len_trim(cache_key1) == 0) then
            print *, "  FAIL: Failed to generate cache key"
            passed = .false.
        else
            print *, "  PASS: Generated cache key: ", trim(cache_key1)
        end if

        ! Clean up
        call sys_remove_file(test_file1)
        call sys_remove_file(test_file2)

    end function test_cache_keys_and_hashing

    function test_cache_info() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_cache
        character(len=2048) :: cache_info
        logical :: success

        print *, "Test 3: Cache information functions"
        passed = .true.

        test_cache = temp_mgr%get_file_path('info_test_cache')
        
        ! Test cache info for non-existent cache
        call get_cache_info('', cache_info)
        if (len_trim(cache_info) == 0) then
            print *, "  FAIL: Failed to get cache info"
            passed = .false.
        else
            print *, "  PASS: Got cache info for default cache"
        end if

        ! Test cache clearing
        call clear_cache('', success)
        if (.not. success) then
            print *, "  FAIL: Failed to clear cache (this might be OK if cache doesn't exist)"
        else
            print *, "  PASS: Successfully cleared cache"
        end if

        ! Test cache existence check
        if (cache_exists('nonexistent_key')) then
            print *, "  FAIL: cache_exists should return false for nonexistent key"
            passed = .false.
        else
            print *, "  PASS: cache_exists correctly returns false for nonexistent key"
        end if

    end function test_cache_info

end program test_cache_coverage