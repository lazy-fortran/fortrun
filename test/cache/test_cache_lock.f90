program test_cache_lock
    use cache_lock
    use temp_utils, only: create_temp_dir, get_temp_file_path, create_test_cache_dir, path_join
    use temp_utils, only: mkdir
    use system_utils, only: sys_remove_dir, sys_find_files, sys_file_exists
    use fpm_environment, only: get_os_type, OS_WINDOWS
    implicit none

    character(len=:), allocatable :: temp_cache_dir
    logical :: success, locked
    integer :: i, unit

    ! Output early message to ensure test is starting
    print '(a)', 'test_cache_lock: Initialization complete'
    flush (6)

    print '(a)', 'test_cache_lock: About to call create_test_cache_dir'
    flush (6)

    ! Create temporary directory for testing with unique suffix
    print '(a)', 'test_cache_lock: Creating temp directory...'
    flush (6)

    ! Add debug print before calling create_test_cache_dir
    print '(a)', 'test_cache_lock: DEBUG - About to call create_test_cache_dir'
    flush (6)

    temp_cache_dir = create_test_cache_dir('cache_lock_test')

    ! Add debug print after successful call
    print '(a)', 'test_cache_lock: DEBUG - create_test_cache_dir call completed'
    flush (6)

    print '(a,a)', 'test_cache_lock: Created directory: ', trim(temp_cache_dir)
    flush (6)

    print '(a)', 'Testing cache lock functionality...'
    flush (6)  ! Ensure output is flushed

    ! Test 1: Basic lock acquisition and release
    print '(a)', ''
    print '(a)', 'Test 1: Basic lock acquisition and release'
    print '(a,a)', 'Using cache dir: ', trim(temp_cache_dir)

    ! Verify directory was created
    block
        logical :: dir_exists
        print '(a)', 'test_cache_lock: DEBUG - About to check if directory exists'
        flush (6)

        inquire (file=trim(temp_cache_dir), exist=dir_exists)

   print '(a,l)', 'test_cache_lock: DEBUG - Directory exists check result: ', dir_exists
        flush (6)

        if (.not. dir_exists) then
            print '(a)', '  ✗ CRITICAL: Cache directory was not created!'
            print '(a,a)', '  DEBUG: Expected directory: ', trim(temp_cache_dir)
            flush (6)
            stop 1
        else
            print '(a)', '  ✓ Cache directory exists'
        end if
    end block
    print '(a)', '  Attempting to acquire lock...'
    print '(a)', 'test_cache_lock: DEBUG - About to call acquire_lock'
    flush (6)

    success = acquire_lock(trim(temp_cache_dir), 'test_project', .false.)

    print '(a)', 'test_cache_lock: DEBUG - acquire_lock call completed'
    flush (6)
    print '(a,l)', '  acquire_lock returned: ', success
    if (success) then
        print '(a)', '  ✓ Lock acquired successfully'
        ! Check if lock file exists
        block
            logical :: file_exists
            character(len=256) :: lock_path
            lock_path = path_join(temp_cache_dir, 'test_project.lock')
            file_exists = sys_file_exists(lock_path)
            print '(a,a,l)', '  DEBUG: Lock file exists at ', trim(lock_path), ': ', file_exists
        end block
        ! List directory contents right after acquiring lock
        call list_lock_files(temp_cache_dir)
    else
        print '(a)', '  ✗ Failed to acquire lock'
        stop 1
    end if

    ! Check if locked
    locked = is_locked(trim(temp_cache_dir), 'test_project')
    if (locked) then
        print '(a)', '  ✓ Lock is detected as locked'
    else
        print '(a)', '  ✗ Lock not detected'
        stop 1
    end if

    ! Release lock
    call release_lock(trim(temp_cache_dir), 'test_project')
    locked = is_locked(trim(temp_cache_dir), 'test_project')
    if (.not. locked) then
        print '(a)', '  ✓ Lock released successfully'
    else
        print '(a)', '  ✗ Failed to release lock'
        stop 1
    end if

    ! Test 2: Lock conflict with no-wait
    print '(a)', ''
    print '(a)', 'Test 2: Lock conflict with no-wait'
    success = acquire_lock(trim(temp_cache_dir), 'test_project2', .false.)
    if (success) then
        print '(a)', '  ✓ First lock acquired'

        ! Try to acquire same lock with no-wait
        success = acquire_lock(trim(temp_cache_dir), 'test_project2', .false.)
        if (.not. success) then
            print '(a)', '  ✓ Second lock correctly failed with no-wait'
        else
            print '(a)', '  ✗ Second lock should have failed'
            ! Debug: check if lock file exists
            inquire (file=path_join(temp_cache_dir, 'test_project2.lock'), exist=locked)
            print '(a,l)', '  Debug: Lock file exists = ', locked
            call list_lock_files(temp_cache_dir)
            stop 1
        end if

        call release_lock(trim(temp_cache_dir), 'test_project2')
        print '(a)', '  ✓ Lock released'
    else
        print '(a)', '  ✗ Failed to acquire first lock'
        stop 1
    end if

    ! Test 3: Multiple different locks
    print '(a)', ''
    print '(a)', 'Test 3: Multiple different project locks'
    success = acquire_lock(trim(temp_cache_dir), 'project_a', .false.)
    if (.not. success) then
        print '(a)', '  ✗ Failed to acquire lock for project_a'
        stop 1
    end if

    success = acquire_lock(trim(temp_cache_dir), 'project_b', .false.)
    if (.not. success) then
        print '(a)', '  ✗ Failed to acquire lock for project_b'
        stop 1
    end if

    print '(a)', '  ✓ Multiple project locks acquired successfully'

    call release_lock(trim(temp_cache_dir), 'project_a')
    call release_lock(trim(temp_cache_dir), 'project_b')
    print '(a)', '  ✓ All locks released'

    ! Test 4: Stale lock cleanup
    print '(a)', ''
    print '(a)', 'Test 4: Stale lock cleanup'
    call create_stale_lock(trim(temp_cache_dir), 'stale_project')

    ! Cleanup should remove stale locks
    call cleanup_stale_locks(trim(temp_cache_dir))

    ! Now we should be able to acquire the lock
    success = acquire_lock(trim(temp_cache_dir), 'stale_project', .false.)
    if (success) then
        print '(a)', '  ✓ Stale lock was cleaned up and new lock acquired'
        call release_lock(trim(temp_cache_dir), 'stale_project')
    else
        print '(a)', '  ✗ Failed to acquire lock after stale cleanup'
        stop 1
    end if

    ! Cleanup
    call sys_remove_dir(temp_cache_dir)

    print '(a)', ''
    print '(a)', 'All cache lock tests passed!'

contains

    subroutine list_lock_files(cache_dir)
        character(len=*), intent(in) :: cache_dir
        character(len=512) :: lock_files(100)
        integer :: num_files, i
        
        ! Use system utilities to list lock files
        print '(a,a)', '  DEBUG: Looking for lock files in: ', trim(cache_dir)
        call sys_find_files(cache_dir, '*.lock', lock_files, num_files, .false., 1)
        print '(a,i0)', '  DEBUG: sys_find_files returned num_files=', num_files
        
        print '(a)', '  Lock files in directory:'
        if (num_files == 0) then
            print '(a)', '    (none)'
        else
            do i = 1, min(num_files, 10)
                print '(a,a)', '    ', trim(lock_files(i))
            end do
            if (num_files > 10) then
                print '(a,i0,a)', '    ... and ', num_files - 10, ' more'
            end if
        end if
    end subroutine list_lock_files

    subroutine get_temp_dir(dir)
        character(len=*), intent(out) :: dir
        integer :: unit, iostat, last_char

        ! Use temp_utils for cross-platform temp directory creation
        dir = create_test_cache_dir('cache_lock_test_thread')
    end subroutine get_temp_dir

    subroutine create_stale_lock(cache_dir, project_name)
        character(len=*), intent(in) :: cache_dir, project_name
        character(len=512) :: lock_file
        integer :: unit

        lock_file = path_join(cache_dir, trim(project_name)//'.lock')

        ! Create a lock file with old timestamp
        open (newunit=unit, file=lock_file, status='new')
        write (unit, '(a)') '99999'  ! Non-existent PID
        write (unit, '(a)') '20200101000000'  ! Old timestamp
        close (unit)

    end subroutine create_stale_lock

    function get_timestamp_str() result(timestamp)
        character(len=16) :: timestamp
        integer :: values(8)

        call date_and_time(values=values)
        write (timestamp, '(i0,5i2.2)') values(1), values(2), values(3), &
            values(5), values(6), values(7)
    end function get_timestamp_str

end program test_cache_lock
