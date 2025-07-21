program test_cache_dir
    use temp_utils, only: create_test_cache_dir
    implicit none
    character(len=256) :: cache_dir
    logical :: exists

    print '(a)', 'Testing create_test_cache_dir...'
    cache_dir = create_test_cache_dir('test_ci')
    print '(a,a)', 'Created: ', trim(cache_dir)

    inquire (file=trim(cache_dir), exist=exists)
    print '(a,l)', 'Exists: ', exists

    print '(a)', 'Cache dir test complete'

end program test_cache_dir
