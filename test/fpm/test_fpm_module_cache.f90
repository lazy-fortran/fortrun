program test_fpm_module_cache
    use iso_fortran_env, only: error_unit
    use fpm_module_cache
    use fpm_model, only: srcfile_t
    use fpm_compiler, only: compiler_t, id_gcc
    use fpm_strings, only: str
    use fpm_error, only: error_t
    use temp_utils, only: temp_dir_manager
    use system_utils, only: sys_remove_file
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr

    print *, "=== FPM Module Cache Tests ==="
    print *

    all_tests_passed = .true.

    call temp_mgr%create('fpm_cache_test')

    ! Test module cache functionality
    if (.not. test_module_cache_creation()) all_tests_passed = .false.
    if (.not. test_get_module_cache_dir()) all_tests_passed = .false.
    if (.not. test_cache_key_generation()) all_tests_passed = .false.
    if (.not. test_cache_operations()) all_tests_passed = .false.
    if (.not. test_deep_copy()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All FPM module cache tests passed!"
        stop 0
    else
        print *, "Some FPM module cache tests failed!"
        stop 1
    end if

contains

    function test_module_cache_creation() result(passed)
        logical :: passed
        type(compiler_t) :: compiler
        type(module_cache_t) :: cache

        print *, "Test 1: Module cache creation"
        passed = .true.

        ! Create compiler instance
        compiler%id = id_gcc

        ! Test cache creation with compiler
        cache = new_module_cache(compiler)
        if (.not. allocated(cache%cache_dir)) then
            print *, "  FAIL: Cache directory not set"
            passed = .false.
        end if

        if (.not. allocated(cache%compiler_id)) then
            print *, "  FAIL: Compiler ID not set"
            passed = .false.
        end if

        if (cache%enabled .neqv. .true.) then
            print *, "  FAIL: Cache should be enabled by default"
            passed = .false.
        end if

        if (passed) then
            print *, "  PASS: Module cache created successfully"
        end if

    end function test_module_cache_creation

    function test_get_module_cache_dir() result(passed)
        logical :: passed
        character(len=:), allocatable :: cache_dir

        print *, "Test 2: Get module cache directory"
        passed = .true.

        cache_dir = get_module_cache_dir()
        if (.not. allocated(cache_dir)) then
            print *, "  FAIL: Cache directory not allocated"
            passed = .false.
        else if (len_trim(cache_dir) == 0) then
            print *, "  FAIL: Empty cache directory"
            passed = .false.
        else if (index(cache_dir, 'modules') == 0) then
            print *, "  FAIL: Cache directory doesn't contain 'modules'"
            passed = .false.
        else
            print *, "  PASS: Valid cache directory: ", trim(cache_dir)
        end if

    end function test_get_module_cache_dir

    function test_cache_key_generation() result(passed)
        logical :: passed
        type(compiler_t) :: compiler
        type(module_cache_t) :: cache
        type(srcfile_t) :: srcfile
        character(len=64) :: key1, key2

        print *, "Test 3: Cache key generation"
        passed = .true.

        compiler%id = id_gcc
        cache = new_module_cache(compiler)

        ! Create test source file
        srcfile%file_name = 'test.f90'
        srcfile%digest = 12345_8

        key1 = cache%get_cache_key(srcfile)
        key2 = cache%get_cache_key(srcfile)

        if (len_trim(key1) == 0) then
            print *, "  FAIL: Empty cache key generated"
            passed = .false.
        else if (key1 /= key2) then
            print *, "  FAIL: Inconsistent cache keys for same file"
            passed = .false.
        else
            print *, "  PASS: Cache key generated: ", trim(key1)
        end if

        ! Test different source files generate different keys
        srcfile%digest = 54321_8
        key2 = cache%get_cache_key(srcfile)
        if (key1 == key2) then
            print *, "  FAIL: Same cache key for different files"
            passed = .false.
        else
            print *, "  PASS: Different files generate different cache keys"
        end if

    end function test_cache_key_generation

    function test_cache_operations() result(passed)
        logical :: passed
        type(compiler_t) :: compiler
        type(module_cache_t) :: cache
        type(srcfile_t) :: srcfile
        character(len=64) :: cache_key
        logical :: is_cached, found
        type(error_t), allocatable :: error

        print *, "Test 4: Cache operations"
        passed = .true.

        compiler%id = id_gcc
        cache = new_module_cache(compiler)

        ! Create test source file
        srcfile%file_name = 'test_module.f90'
        srcfile%digest = 98765_8
        allocate(srcfile%modules_provided(1))
        srcfile%modules_provided(1)%s = 'test_module'

        cache_key = cache%get_cache_key(srcfile)

        ! Test is_cached for non-existent entry
        is_cached = cache%is_cached(cache_key)
        if (is_cached) then
            print *, "  FAIL: Non-existent cache entry reported as cached"
            passed = .false.
        else
            print *, "  PASS: Non-existent cache correctly reported as not cached"
        end if

        ! Test get_module_dir
        block
            character(len=:), allocatable :: module_dir
            module_dir = cache%get_module_dir(cache_key)
            if (len_trim(module_dir) == 0) then
                print *, "  FAIL: Empty module directory path"
                passed = .false.
            else if (index(module_dir, cache_key) == 0) then
                print *, "  FAIL: Module directory doesn't contain cache key"
                passed = .false.
            else
                print *, "  PASS: Valid module directory path"
            end if
        end block

    end function test_cache_operations

    function test_deep_copy() result(passed)
        logical :: passed
        type(compiler_t) :: compiler
        type(module_cache_t) :: cache1, cache2

        print *, "Test 5: Deep copy operations"
        passed = .true.

        compiler%id = id_gcc
        cache1 = new_module_cache(compiler, 'v1.0')

        ! Test deep copy
        cache2 = cache1%deep_copy()

        if (cache2%enabled .neqv. cache1%enabled) then
            print *, "  FAIL: Enabled flag not copied"
            passed = .false.
        end if

        if (.not. allocated(cache2%cache_dir) .or. cache2%cache_dir /= cache1%cache_dir) then
            print *, "  FAIL: Cache directory not copied correctly"
            passed = .false.
        end if

        if (.not. allocated(cache2%compiler_id) .or. cache2%compiler_id /= cache1%compiler_id) then
            print *, "  FAIL: Compiler ID not copied correctly"
            passed = .false.
        end if

        if (.not. allocated(cache2%compiler_version) .or. cache2%compiler_version /= cache1%compiler_version) then
            print *, "  FAIL: Compiler version not copied correctly"
            passed = .false.
        end if

        if (passed) then
            print *, "  PASS: Deep copy successful"
        end if

        ! Test assignment
        cache1%enabled = .false.
        cache2 = cache1
        if (cache2%enabled .neqv. .false.) then
            print *, "  FAIL: Assignment failed to copy enabled flag"
            passed = .false.
        else
            print *, "  PASS: Assignment successful"
        end if

    end function test_deep_copy

end program test_fpm_module_cache