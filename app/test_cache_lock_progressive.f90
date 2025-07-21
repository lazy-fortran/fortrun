program test_cache_lock_progressive
    implicit none

    print '(a)', 'test_cache_lock_progressive: Step 1 - Basic start'
    flush (6)

    ! Step 2: Add cache_lock module
    block
        use cache_lock
        print '(a)', 'test_cache_lock_progressive: Step 2 - cache_lock module loaded'
        flush (6)
    end block

    ! Step 3: Add temp_utils
    block
        use temp_utils, only: create_test_cache_dir
        print '(a)', 'test_cache_lock_progressive: Step 3 - temp_utils loaded'
        flush (6)
    end block

    ! Step 4: Add more temp_utils imports
    block
        use temp_utils, only: create_temp_dir, get_temp_file_path, create_test_cache_dir, path_join, mkdir
        print '(a)', 'test_cache_lock_progressive: Step 4 - multiple temp_utils imports'
        flush (6)
    end block

    ! Step 5: Add system_utils
    block
        use system_utils, only: sys_remove_dir
        print '(a)', 'test_cache_lock_progressive: Step 5 - system_utils loaded'
        flush (6)
    end block

    ! Step 6: Add fpm_environment
    block
        use fpm_environment, only: get_os_type, OS_WINDOWS
        print '(a)', 'test_cache_lock_progressive: Step 6 - fmp_environment loaded'
        flush (6)
    end block

    print '(a)', 'test_cache_lock_progressive: All steps completed successfully!'

end program test_cache_lock_progressive
