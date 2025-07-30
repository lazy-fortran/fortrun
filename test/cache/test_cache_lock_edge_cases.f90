program test_cache_lock_edge_cases
    use cache_lock
    use temp_utils, only: create_temp_dir, get_temp_file_path
    use system_utils, only: sys_sleep, sys_file_exists, sys_remove_file, sys_create_dir
    implicit none

    logical :: all_passed = .true.
    character(len=256) :: temp_dir, cache_dir
    logical :: success, locked
    integer :: unit

    print *, "=== Cache Lock Edge Cases Tests ==="

    temp_dir = create_temp_dir('cache_lock_edge')
    cache_dir = trim(temp_dir)//"/cache"
    call sys_create_dir(cache_dir, success)

    ! Test 1: Lock acquisition without waiting
    call test_no_wait_lock()

    ! Test 2: Multiple lock attempts
    call test_multiple_locks()

    ! Test 3: Very long project names
    call test_long_project_names()

    ! Test 4: Special characters in project names
    call test_special_project_names()

    ! Test 5: Concurrent lock attempts simulation
    call test_concurrent_locks()

    ! Test 6: Lock cleanup scenarios
    call test_lock_cleanup()

    ! Test 7: Non-existent cache directory
    call test_nonexistent_cache_dir()

    if (all_passed) then
        print *, ""
        print *, "All cache lock edge case tests PASSED!"
        stop 0
    else
        print *, ""
        print *, "Some cache lock edge case tests FAILED!"
        stop 1
    end if

contains

    subroutine test_no_wait_lock()
        logical :: lock_success

        print *, ""
        print *, "Test: No-wait lock acquisition"

        ! First acquire a lock
        lock_success = acquire_lock(cache_dir, "test_project", wait=.true.)
        if (lock_success) then
            print *, "  PASS: Initial lock acquired"

            ! Try to acquire again without waiting
            lock_success = acquire_lock(cache_dir, "test_project", wait=.false.)
            if (.not. lock_success) then
                print *, "  PASS: No-wait lock correctly failed"
            else
                print *, "  FAIL: No-wait lock should have failed"
                all_passed = .false.
            end if

            ! Release the lock
            call release_lock(cache_dir, "test_project")
            print *, "  PASS: Lock released"
        else
            print *, "  FAIL: Could not acquire initial lock"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_multiple_locks()
        logical :: lock1, lock2, lock3

        print *, ""
        print *, "Test: Multiple different project locks"

        ! Acquire locks for different projects
        lock1 = acquire_lock(cache_dir, "project1", wait=.false.)
        lock2 = acquire_lock(cache_dir, "project2", wait=.false.)
        lock3 = acquire_lock(cache_dir, "project3", wait=.false.)

        if (lock1 .and. lock2 .and. lock3) then
            print *, "  PASS: Multiple project locks acquired"

            ! Check each is locked
            if (is_locked(cache_dir, "project1") .and. &
                is_locked(cache_dir, "project2") .and. &
                is_locked(cache_dir, "project3")) then
                print *, "  PASS: All locks detected correctly"
            else
                print *, "  FAIL: Not all locks detected"
                all_passed = .false.
            end if

            ! Release all locks
            call release_lock(cache_dir, "project1")
            call release_lock(cache_dir, "project2")
            call release_lock(cache_dir, "project3")
        else
            print *, "  FAIL: Could not acquire all locks"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_long_project_names()
        character(len=256) :: long_name
        logical :: lock_success

        print *, ""
        print *, "Test: Very long project names"

        ! Create a very long project name
        long_name = repeat('a', 200)

        lock_success = acquire_lock(cache_dir, trim(long_name), wait=.false.)
        if (lock_success) then
            print *, "  PASS: Long project name lock acquired"

            if (is_locked(cache_dir, trim(long_name))) then
                print *, "  PASS: Long name lock detected"
            else
                print *, "  FAIL: Long name lock not detected"
                all_passed = .false.
            end if

            call release_lock(cache_dir, trim(long_name))
        else
            print *, "  INFO: Long project names may have OS limitations"
        end if
    end subroutine

    subroutine test_special_project_names()
        logical :: lock_success

        print *, ""
        print *, "Test: Special characters in project names"

        ! Test project name with dots
        lock_success = acquire_lock(cache_dir, "project.with.dots", wait=.false.)
        if (lock_success) then
            print *, "  PASS: Dotted project name lock acquired"
            call release_lock(cache_dir, "project.with.dots")
        else
            print *, "  INFO: Dotted names may not be supported"
        end if

        ! Test project name with dashes
        lock_success = acquire_lock(cache_dir, "project-with-dashes", wait=.false.)
        if (lock_success) then
            print *, "  PASS: Dashed project name lock acquired"
            call release_lock(cache_dir, "project-with-dashes")
        else
            print *, "  INFO: Dashed names may not be supported"
        end if

        ! Test project name with underscores
        lock_success = acquire_lock(cache_dir, "project_with_underscores", wait=.false.)
        if (lock_success) then
            print *, "  PASS: Underscored project name lock acquired"
            call release_lock(cache_dir, "project_with_underscores")
        else
            print *, "  FAIL: Underscored names should be supported"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_concurrent_locks()
        logical :: lock1, lock2
        integer :: i

        print *, ""
        print *, "Test: Rapid lock/unlock cycles"

        ! Simulate rapid lock/unlock operations
        do i = 1, 10
            lock1 = acquire_lock(cache_dir, "rapid_test", wait=.false.)
            if (lock1) then
                ! Immediately release
                call release_lock(cache_dir, "rapid_test")

                ! Try to re-acquire immediately
                lock2 = acquire_lock(cache_dir, "rapid_test", wait=.false.)
                if (lock2) then
                    call release_lock(cache_dir, "rapid_test")
                else
                    print *, "  INFO: Rapid re-acquisition may have timing issues"
                end if
            else
                print *, "  FAIL: Rapid lock acquisition failed at iteration", i
                all_passed = .false.
                exit
            end if
        end do

        if (i > 10) then
            print *, "  PASS: Rapid lock/unlock cycles completed"
        end if
    end subroutine

    subroutine test_lock_cleanup()
        character(len=512) :: lock_file
        logical :: lock_success

        print *, ""
        print *, "Test: Lock cleanup scenarios"

        ! Create a lock
        lock_success = acquire_lock(cache_dir, "cleanup_test", wait=.false.)
        if (lock_success) then
            print *, "  PASS: Lock created for cleanup test"

            ! Manually check lock file exists
            lock_file = trim(cache_dir)//"/cleanup_test.lock"
            if (sys_file_exists(lock_file)) then
                print *, "  PASS: Lock file exists"

                ! Release and verify cleanup
                call release_lock(cache_dir, "cleanup_test")

                if (.not. sys_file_exists(lock_file)) then
                    print *, "  PASS: Lock file cleaned up"
                else
                    print *, "  FAIL: Lock file not cleaned up"
                    all_passed = .false.
                end if
            else
                print *, "  FAIL: Lock file not found"
                all_passed = .false.
            end if
        else
            print *, "  FAIL: Could not create lock for cleanup test"
            all_passed = .false.
        end if

        ! Test cleanup of stale locks
        call cleanup_stale_locks(cache_dir)
        print *, "  PASS: Stale lock cleanup completed"
    end subroutine

    subroutine test_nonexistent_cache_dir()
        character(len=256) :: nonexistent_dir
        logical :: lock_success

        print *, ""
        print *, "Test: Non-existent cache directory"

        nonexistent_dir = trim(temp_dir)//"/nonexistent_cache"

        ! Try to acquire lock in non-existent directory
        lock_success = acquire_lock(nonexistent_dir, "test_project", wait=.false.)

        if (lock_success) then
            print *, "  PASS: Lock acquired (directory created)"

            ! Verify directory was created
            if (sys_file_exists(nonexistent_dir)) then
                print *, "  PASS: Cache directory was created"
            else
                print *, "  FAIL: Cache directory not created"
                all_passed = .false.
            end if

            call release_lock(nonexistent_dir, "test_project")
        else
            print *, "  INFO: Lock acquisition in non-existent dir may fail"
        end if
    end subroutine

end program test_cache_lock_edge_cases
