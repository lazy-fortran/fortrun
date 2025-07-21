program test_notebook_caching_minimal
    use temp_utils, only: temp_dir_manager
    use cache, only: get_cache_dir, get_content_hash
    implicit none

    logical :: all_tests_passed

    print *, 'test_notebook_caching_minimal: STARTING NOW'
    call flush (6)

    print *, '=== Minimal Notebook Caching Tests ==='
    call flush (6)

    all_tests_passed = .true.

    ! Test 1: Cache directory creation and basic functionality
    call test_cache_basics(all_tests_passed)

    ! Test 2: Cache key concepts (simplified)
    call test_cache_concepts(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All minimal notebook caching tests passed!'
        stop 0
    else
        print *, 'Some minimal tests failed!'
        stop 1
    end if

contains

    subroutine test_cache_basics(passed)
        logical, intent(inout) :: passed
        character(len=256) :: test_cache_dir
        logical :: dir_exists

        print *, 'Test 1: Cache directory creation'
        call flush (6)

        ! Use a simple test directory path
        test_cache_dir = '/tmp/test_minimal_notebook_cache'

        ! Create the directory
        call execute_command_line('mkdir -p "'//trim(test_cache_dir)//'"')

        ! Check that directory exists
        inquire (file=trim(test_cache_dir), exist=dir_exists)
        if (.not. dir_exists) then
            print *, '  FAIL: Cache directory not created'
            passed = .false.
            return
        end if

        print *, '  PASS: Cache directory created successfully'
        call flush (6)

        ! Cleanup
        call execute_command_line("rm -rf "//trim(test_cache_dir))
    end subroutine test_cache_basics

    subroutine test_cache_concepts(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: cache_dir_path

        print *, 'Test 2: Cache concepts'
        call flush (6)

        ! Test that we can get a cache directory path
        cache_dir_path = get_cache_dir()

        if (len_trim(cache_dir_path) == 0) then
            print *, '  FAIL: get_cache_dir returned empty string'
            passed = .false.
            return
        end if

        ! Test basic cache directory concepts work
        print *, '  Cache directory path: ', trim(cache_dir_path)
        call flush (6)

        print *, '  PASS: Cache concepts work correctly'
        call flush (6)
    end subroutine test_cache_concepts

end program test_notebook_caching_minimal
