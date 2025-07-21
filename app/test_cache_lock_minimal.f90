program test_cache_lock_minimal
    use cache_lock
    use temp_utils, only: create_test_cache_dir
    use system_utils, only: sys_remove_dir
    implicit none

    character(len=256) :: temp_cache_dir
    logical :: success

    print '(a)', 'test_cache_lock_minimal: Starting...'

    ! Create temporary directory for testing
    print '(a)', 'test_cache_lock_minimal: Creating temp directory...'
    temp_cache_dir = create_test_cache_dir('cache_lock_test')
    print '(a,a)', 'test_cache_lock_minimal: Created directory: ', trim(temp_cache_dir)

    print '(a)', 'test_cache_lock_minimal: Testing lock acquisition...'
    success = acquire_lock(trim(temp_cache_dir), 'test_project', .false.)
    print '(a,l)', 'test_cache_lock_minimal: acquire_lock returned: ', success

    if (success) then
        print '(a)', 'test_cache_lock_minimal: Lock acquired successfully!'
        call release_lock(trim(temp_cache_dir), 'test_project')
        print '(a)', 'test_cache_lock_minimal: Lock released'
    else
        print '(a)', 'test_cache_lock_minimal: Failed to acquire lock'
        stop 1
    end if

    ! Cleanup
    call sys_remove_dir(temp_cache_dir)
    print '(a)', 'test_cache_lock_minimal: Complete!'

end program test_cache_lock_minimal
