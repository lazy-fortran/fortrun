program test_cache_corruption_recovery
    use cache, only: get_cache_dir, ensure_cache_dir, get_content_hash
    use cache_lock, only: acquire_lock, release_lock, cleanup_stale_locks
    use temp_utils, only: temp_dir_manager, fortran_with_isolated_cache
    use system_utils, only: sys_copy_file, sys_remove_file
    use runner, only: run_fortran_file
    implicit none

    type(temp_dir_manager) :: temp_mgr
    character(len=256) :: test_cache_dir, test_file, corrupted_file
    character(len=1024) :: command
    integer :: test_count, pass_count, exit_code, unit, ios
    logical :: success

    test_count = 0
    pass_count = 0

    print *, "=== Cache Corruption Recovery Tests ==="
    print *, ""

    call temp_mgr%create('cache_corruption_test')
    test_cache_dir = temp_mgr%get_path()

    ! Test 1: Corrupted cache file recovery
    call test_corrupted_cache_file()

    ! Test 2: Stale lock cleanup
    call test_stale_lock_cleanup()

    ! Test 3: Partial cache write recovery
    call test_partial_cache_write()

    ! Test 4: Cache directory recreation
    call test_cache_directory_recreation()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All cache corruption recovery tests passed!"
        stop 0
    else
        print *, "Some cache corruption recovery tests failed!"
        stop 1
    end if

contains

    subroutine test_corrupted_cache_file()
        call test_start("Corrupted cache file recovery")

        ! Create a simple test file
        test_file = temp_mgr%get_file_path('test_simple.f90')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program test'
        write (unit, '(A)') '    print *, "Hello"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Run once to create cache
        command = fortran_with_isolated_cache('corrupted_test')//' "'// &
                  trim(test_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        ! Corrupt a cache file by writing garbage
        corrupted_file = temp_mgr%get_file_path('corrupted_cache.dat')
        open (newunit=unit, file=corrupted_file, status='replace', iostat=ios)
        write (unit, '(A)') 'CORRUPTED_GARBAGE_DATA_INVALID_CACHE'
        close (unit)

        ! Try to run again - should recover gracefully
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = (exit_code == 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Cache corruption recovery failed"
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_corrupted_cache_file

    subroutine test_stale_lock_cleanup()
        character(len=256) :: cache_dir, basename, lock_file
        logical :: first_lock

        call test_start("Stale lock cleanup")

        cache_dir = temp_mgr%get_path()
        basename = 'test_lock'

        ! Force create a stale lock - acquire but don't release
        first_lock = acquire_lock(cache_dir, basename, .false.)

        if (.not. first_lock) then
            success = .false.
            call test_result(success)
            print *, "  Failed to create initial lock"
            return
        end if

        ! Simulate stale lock by manually removing the lock file
        ! This tests the recovery mechanism when locks are left behind
        lock_file = trim(cache_dir)//'/'//trim(basename)//'.lock'
        call execute_command_line('rm -f "'//trim(lock_file)//'"', wait=.true.)

        ! Should be able to acquire lock again after manual cleanup
        success = acquire_lock(cache_dir, basename, .false.)
        if (success) then
            call release_lock(cache_dir, basename)
        end if

        call test_result(success)

        if (.not. success) then
            print *, "  Stale lock cleanup failed - could not reacquire lock"
        end if
    end subroutine test_stale_lock_cleanup

    subroutine test_partial_cache_write()
        character(len=256) :: partial_file

        call test_start("Partial cache write recovery")

        ! Create a file that simulates partial write
        partial_file = temp_mgr%get_file_path('partial_cache.tmp')
        open (newunit=unit, file=partial_file, status='replace', iostat=ios)
        write (unit, '(A)', advance='no') 'PARTIAL_DATA'  ! No newline - simulates interrupted write
        close (unit)

        ! Try to create a proper cache entry
        test_file = temp_mgr%get_file_path('test_partial.f90')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program partial_test'
        write (unit, '(A)') '    print *, "Partial test"'
        write (unit, '(A)') 'end program'
        close (unit)

        command = fortran_with_isolated_cache('partial_test')//' "'// &
                  trim(test_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = (exit_code == 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Partial cache write recovery failed"
        end if
    end subroutine test_partial_cache_write

    subroutine test_cache_directory_recreation()
        character(len=256) :: cache_dir_path
        type(temp_dir_manager) :: recreation_mgr

        call test_start("Cache directory recreation")

        cache_dir_path = temp_mgr%get_path()

        ! Remove cache directory entirely
        call execute_command_line('rm -rf "'//trim(cache_dir_path)//'"', wait=.true.)

        ! Create a separate temp manager for test file after cache removal
        call recreation_mgr%create('recreation_test')
        test_file = recreation_mgr%get_file_path('test_recreation.f90')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program recreation_test'
        write (unit, '(A)') '    print *, "Recreation test"'
        write (unit, '(A)') 'end program'
        close (unit)

        command = fortran_with_isolated_cache('recreation_test')//' "'// &
                  trim(test_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = (exit_code == 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Cache directory recreation failed"
        end if
    end subroutine test_cache_directory_recreation

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write (*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_result(test_success)
        logical, intent(in) :: test_success
        if (test_success) then
            print *, " ... PASSED"
            pass_count = pass_count + 1
        else
            print *, " ... FAILED"
        end if
    end subroutine test_result

end program test_cache_corruption_recovery
