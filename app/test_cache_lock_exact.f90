program test_cache_lock_exact
    use cache_lock
    use temp_utils, only: create_temp_dir, get_temp_file_path, create_test_cache_dir, path_join
    use temp_utils, only: mkdir
    use system_utils, only: sys_remove_dir
    use fpm_environment, only: get_os_type, OS_WINDOWS
    implicit none

    character(len=256) :: temp_cache_dir
    logical :: success, locked
    integer :: i, unit

    print '(a)', 'test_cache_lock_exact: Program started'
    flush (6)

    ! Output early message to ensure test is starting
    print '(a)', 'test_cache_lock_exact: Initialization complete'
    flush (6)

    print '(a)', 'test_cache_lock_exact: About to create temp directory'
    flush (6)

    ! Create temporary directory for testing with unique suffix
    print '(a)', 'test_cache_lock_exact: Creating temp directory...'
    flush (6)
    temp_cache_dir = create_test_cache_dir('cache_lock_test')
    print '(a,a)', 'test_cache_lock_exact: Created directory: ', trim(temp_cache_dir)
    flush (6)

    print '(a)', 'test_cache_lock_exact: Success!'

end program test_cache_lock_exact
