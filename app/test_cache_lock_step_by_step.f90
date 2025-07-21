program test_cache_lock_step_by_step
    implicit none

    print '(a)', 'Step 0: Program started'
    flush (6)

    ! Step 1: Try cache_lock module
    print '(a)', 'Step 1: About to use cache_lock'
    flush (6)

    block
        use cache_lock
        print '(a)', 'Step 1: cache_lock loaded successfully'
        flush (6)
    end block

    ! Step 2: Try temp_utils imports one by one
    print '(a)', 'Step 2: About to use temp_utils create_temp_dir'
    flush (6)

    block
        use temp_utils, only: create_temp_dir
        print '(a)', 'Step 2: create_temp_dir loaded successfully'
        flush (6)
    end block

    print '(a)', 'Step 3: About to use temp_utils get_temp_file_path'
    flush (6)

    block
        use temp_utils, only: get_temp_file_path
        print '(a)', 'Step 3: get_temp_file_path loaded successfully'
        flush (6)
    end block

    print '(a)', 'Step 4: About to use temp_utils create_test_cache_dir'
    flush (6)

    block
        use temp_utils, only: create_test_cache_dir
        print '(a)', 'Step 4: create_test_cache_dir loaded successfully'
        flush (6)
    end block

    print '(a)', 'Step 5: About to use temp_utils path_join'
    flush (6)

    block
        use temp_utils, only: path_join
        print '(a)', 'Step 5: path_join loaded successfully'
        flush (6)
    end block

    print '(a)', 'Step 6: About to use temp_utils mkdir'
    flush (6)

    block
        use temp_utils, only: mkdir
        print '(a)', 'Step 6: mkdir loaded successfully'
        flush (6)
    end block

    ! Step 7: Try system_utils
    print '(a)', 'Step 7: About to use system_utils'
    flush (6)

    block
        use system_utils, only: sys_remove_dir
        print '(a)', 'Step 7: system_utils loaded successfully'
        flush (6)
    end block

    ! Step 8: Try fmp_environment
    print '(a)', 'Step 8: About to use fpm_environment'
    flush (6)

    block
        use fpm_environment, only: get_os_type, OS_WINDOWS
        print '(a)', 'Step 8: fpm_environment loaded successfully'
        flush (6)
    end block

    ! Step 9: Try variable declarations like the original test
    print '(a)', 'Step 9: About to declare variables'
    flush (6)

    block
        character(len=:), allocatable :: temp_cache_dir
        logical :: success, locked
        integer :: i, unit
        print '(a)', 'Step 9: Variables declared successfully'
        flush (6)
    end block

    print '(a)', 'Step 10: All steps completed successfully!'

end program test_cache_lock_step_by_step
