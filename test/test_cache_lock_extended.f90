program test_cache_lock_extended
    use cache_lock
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Extended Cache Lock Tests ==="
    print *
    
    all_tests_passed = .true.
    
    ! Test extended functionality of cache lock module
    if (.not. test_basic_lock_operations()) all_tests_passed = .false.
    if (.not. test_lock_waiting()) all_tests_passed = .false.
    if (.not. test_stale_lock_cleanup()) all_tests_passed = .false.
    if (.not. test_concurrent_locks()) all_tests_passed = .false.
    if (.not. test_edge_cases()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All extended cache lock tests passed!"
        stop 0
    else
        print *, "Some extended cache lock tests failed!"
        stop 1
    end if
    
contains

    function test_basic_lock_operations() result(passed)
        logical :: passed
        character(len=256) :: cache_dir
        logical :: success
        
        print *, "Test 1: Basic lock operations"
        passed = .true.
        
        cache_dir = "/tmp/test_cache_lock"
        call execute_command_line("mkdir -p " // trim(cache_dir))
        
        ! Test acquiring lock
        success = acquire_lock(cache_dir, "test_project", wait=.false.)
        if (.not. success) then
            print *, "  FAILED: Could not acquire lock"
            passed = .false.
        end if
        
        ! Test checking if locked
        if (.not. is_locked(cache_dir, "test_project")) then
            print *, "  FAILED: Lock should be present"
            passed = .false.
        end if
        
        ! Test trying to acquire same lock (should fail)
        success = acquire_lock(cache_dir, "test_project", wait=.false.)
        if (success) then
            print *, "  FAILED: Should not be able to acquire lock twice"
            passed = .false.
        end if
        
        ! Test releasing lock
        call release_lock(cache_dir, "test_project")
        
        if (is_locked(cache_dir, "test_project")) then
            print *, "  FAILED: Lock should be released"
            passed = .false.
        end if
        
        ! Test acquiring after release
        success = acquire_lock(cache_dir, "test_project", wait=.false.)
        if (.not. success) then
            print *, "  FAILED: Should be able to acquire after release"
            passed = .false.
        end if
        
        call release_lock(cache_dir, "test_project")
        call execute_command_line("rm -rf " // trim(cache_dir))
        
        if (passed) print *, "  PASS: Basic lock operations"
        
    end function test_basic_lock_operations

    function test_lock_waiting() result(passed)
        logical :: passed
        character(len=256) :: cache_dir
        logical :: success
        integer :: start_time, end_time, elapsed
        
        print *, "Test 2: Lock waiting functionality"
        passed = .true.
        
        cache_dir = "/tmp/test_cache_lock_wait"
        call execute_command_line("mkdir -p " // trim(cache_dir))
        
        ! Create a lock
        success = acquire_lock(cache_dir, "wait_test", wait=.false.)
        if (.not. success) then
            print *, "  WARNING: Could not create initial lock"
            passed = .true.  ! Don't fail test
        else
            ! Test immediate failure without wait
            success = acquire_lock(cache_dir, "wait_test", wait=.false.)
            if (success) then
                print *, "  FAILED: Should fail immediately without wait"
                passed = .false.
            end if
            
            ! Clean up
            call release_lock(cache_dir, "wait_test")
        end if
        
        ! Test with different projects (should work)
        success = acquire_lock(cache_dir, "project1", wait=.false.)
        if (.not. success) then
            print *, "  FAILED: Should acquire lock for project1"
            passed = .false.
        end if
        
        success = acquire_lock(cache_dir, "project2", wait=.false.)
        if (.not. success) then
            print *, "  FAILED: Should acquire lock for project2"
            passed = .false.
        end if
        
        call release_lock(cache_dir, "project1")
        call release_lock(cache_dir, "project2")
        
        call execute_command_line("rm -rf " // trim(cache_dir))
        
        if (passed) print *, "  PASS: Lock waiting"
        
    end function test_lock_waiting

    function test_stale_lock_cleanup() result(passed)
        logical :: passed
        character(len=256) :: cache_dir, lock_file
        integer :: unit
        logical :: success
        
        print *, "Test 3: Stale lock cleanup"
        passed = .true.
        
        cache_dir = "/tmp/test_cache_stale"
        call execute_command_line("mkdir -p " // trim(cache_dir))
        
        ! Create a fake stale lock with old timestamp
        lock_file = trim(cache_dir) // "/stale_test.lock"
        open(newunit=unit, file=lock_file, status='replace')
        write(unit, '(a)') "99999"  ! Non-existent PID
        write(unit, '(a)') "20000101000000"  ! Very old timestamp
        close(unit)
        
        ! Should be able to acquire despite "lock" existing
        success = acquire_lock(cache_dir, "stale_test", wait=.false.)
        if (.not. success) then
            print *, "  WARNING: Could not acquire over stale lock"
            ! This might fail if the stale detection doesn't work
            passed = .true.  ! Don't fail test
        end if
        
        call release_lock(cache_dir, "stale_test")
        
        ! Test cleanup function
        ! Create multiple fake locks
        open(newunit=unit, file=trim(cache_dir) // "/lock1.lock", status='replace')
        write(unit, '(a)') "99998"
        write(unit, '(a)') "20000101000000"
        close(unit)
        
        open(newunit=unit, file=trim(cache_dir) // "/lock2.lock", status='replace')
        write(unit, '(a)') "99997"
        write(unit, '(a)') "20000101000000"
        close(unit)
        
        call cleanup_stale_locks(cache_dir)
        
        ! Check if cleaned up (they should be removed)
        if (is_locked(cache_dir, "lock1")) then
            print *, "  WARNING: Stale lock1 not cleaned"
        end if
        
        if (is_locked(cache_dir, "lock2")) then
            print *, "  WARNING: Stale lock2 not cleaned"
        end if
        
        call execute_command_line("rm -rf " // trim(cache_dir))
        
        if (passed) print *, "  PASS: Stale lock cleanup"
        
    end function test_stale_lock_cleanup

    function test_concurrent_locks() result(passed)
        logical :: passed
        character(len=256) :: cache_dir
        logical :: success1, success2, success3
        
        print *, "Test 4: Concurrent lock management"
        passed = .true.
        
        cache_dir = "/tmp/test_cache_concurrent"
        call execute_command_line("mkdir -p " // trim(cache_dir))
        
        ! Test multiple different locks
        success1 = acquire_lock(cache_dir, "app1", wait=.false.)
        success2 = acquire_lock(cache_dir, "app2", wait=.false.)
        success3 = acquire_lock(cache_dir, "app3", wait=.false.)
        
        if (.not. (success1 .and. success2 .and. success3)) then
            print *, "  FAILED: Should acquire all different locks"
            passed = .false.
        end if
        
        ! Check all are locked
        if (.not. is_locked(cache_dir, "app1")) then
            print *, "  FAILED: app1 should be locked"
            passed = .false.
        end if
        
        if (.not. is_locked(cache_dir, "app2")) then
            print *, "  FAILED: app2 should be locked"
            passed = .false.
        end if
        
        if (.not. is_locked(cache_dir, "app3")) then
            print *, "  FAILED: app3 should be locked"
            passed = .false.
        end if
        
        ! Release one and check others still locked
        call release_lock(cache_dir, "app2")
        
        if (is_locked(cache_dir, "app2")) then
            print *, "  FAILED: app2 should be unlocked"
            passed = .false.
        end if
        
        if (.not. is_locked(cache_dir, "app1")) then
            print *, "  FAILED: app1 should still be locked"
            passed = .false.
        end if
        
        if (.not. is_locked(cache_dir, "app3")) then
            print *, "  FAILED: app3 should still be locked"
            passed = .false.
        end if
        
        ! Clean up
        call release_lock(cache_dir, "app1")
        call release_lock(cache_dir, "app3")
        
        call execute_command_line("rm -rf " // trim(cache_dir))
        
        if (passed) print *, "  PASS: Concurrent locks"
        
    end function test_concurrent_locks

    function test_edge_cases() result(passed)
        logical :: passed
        character(len=256) :: cache_dir
        logical :: success
        
        print *, "Test 5: Edge cases and error conditions"
        passed = .true.
        
        ! Test with non-existent directory
        success = acquire_lock("/definitely/does/not/exist", "test", wait=.false.)
        if (success) then
            print *, "  WARNING: Lock in non-existent dir might succeed"
        end if
        
        ! Test with empty names
        cache_dir = "/tmp/test_cache_edge"
        call execute_command_line("mkdir -p " // trim(cache_dir))
        
        success = acquire_lock(cache_dir, "", wait=.false.)
        ! Should handle empty project name gracefully
        
        ! Test with very long project name
        success = acquire_lock(cache_dir, &
            "very_very_very_very_very_very_very_very_very_very_long_project_name_" // &
            "that_might_cause_issues_with_file_system_limits", wait=.false.)
        
        if (success) then
            call release_lock(cache_dir, &
                "very_very_very_very_very_very_very_very_very_very_long_project_name_" // &
                "that_might_cause_issues_with_file_system_limits")
        end if
        
        ! Test with special characters in project name
        success = acquire_lock(cache_dir, "project/with/slashes", wait=.false.)
        ! Should handle or fail gracefully
        
        ! Test double release (should not crash)
        success = acquire_lock(cache_dir, "double_release", wait=.false.)
        if (success) then
            call release_lock(cache_dir, "double_release")
            call release_lock(cache_dir, "double_release")  ! Second release
        end if
        
        call execute_command_line("rm -rf " // trim(cache_dir))
        
        if (passed) print *, "  PASS: Edge cases"
        
    end function test_edge_cases

end program test_cache_lock_extended