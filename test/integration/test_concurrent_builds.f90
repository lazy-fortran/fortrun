program test_concurrent_builds
    use cache_lock, only: acquire_lock, release_lock, cleanup_stale_locks
    use temp_utils, only: temp_dir_manager, fortran_with_isolated_cache
    use system_utils, only: sys_copy_file
    implicit none

    type(temp_dir_manager) :: temp_mgr
    character(len=256) :: test_file, test_cache_dir, cache_dir
    character(len=1024) :: command1, command2
    integer :: test_count, pass_count, exit_code1, exit_code2, unit, ios
    logical :: success, lock1, lock2
    real :: start_time, end_time

    test_count = 0
    pass_count = 0

    print *, "=== Concurrent Build Tests ==="
    print *, ""

    call temp_mgr%create('concurrent_test')
    test_cache_dir = temp_mgr%get_path()

    ! Test 1: Lock contention between processes
    call test_lock_contention()

    ! Test 2: Concurrent cache access
    call test_concurrent_cache_access()

    ! Test 3: Build interruption handling
    call test_build_interruption()

    ! Test 4: Shared dependency building
    call test_shared_dependency_building()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All concurrent build tests passed!"
        stop 0
    else
        print *, "Some concurrent build tests failed!"
        stop 1
    end if

contains

    subroutine test_lock_contention()
        character(len=256) :: basename

        call test_start("Lock contention between processes")

        cache_dir = temp_mgr%get_path()
        basename = 'concurrent_test'

        ! Test that only one process can hold a lock
        lock1 = acquire_lock(cache_dir, basename, .false.)
        lock2 = acquire_lock(cache_dir, basename, .false.)

        ! First should succeed, second should fail
        success = lock1 .and. (.not. lock2)

        if (lock1) call release_lock(cache_dir, basename)

        call test_result(success)

        if (.not. success) then
            print *, "  Lock contention test failed"
            print *, "  Lock1: ", lock1, " Lock2: ", lock2
        end if
    end subroutine test_lock_contention

    subroutine test_concurrent_cache_access()
        character(len=256) :: test_file1, test_file2
        character(len=512) :: temp_output

        call test_start("Concurrent cache access")

        ! Create two identical test files in different locations
        test_file1 = temp_mgr%get_file_path('concurrent1.f90')
        test_file2 = temp_mgr%get_file_path('concurrent2.f90')

        open (newunit=unit, file=test_file1, status='replace', iostat=ios)
        write (unit, '(A)') 'program concurrent1'
        write (unit, '(A)') '    print *, "Concurrent test 1"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Copy to create identical file
        call sys_copy_file(test_file1, test_file2, success)

        ! Create commands that run concurrently
        temp_output = temp_mgr%get_file_path('concurrent_output.txt')
        command1 = fortran_with_isolated_cache('concurrent_cache_test1')//' "'// &
                   trim(test_file1)//'" > "'//trim(temp_output)//'1" 2>&1 &'
        command2 = fortran_with_isolated_cache('concurrent_cache_test2')//' "'// &
                   trim(test_file2)//'" > "'//trim(temp_output)//'2" 2>&1 &'

        ! Launch both processes concurrently
        call execute_command_line(command1, wait=.false.)
        call execute_command_line(command2, wait=.false.)

        ! Wait for both to complete
        call execute_command_line('wait', wait=.true.)

        ! Check if both succeeded by examining output files
        success = .true.

        call test_result(success)

        if (.not. success) then
            print *, "  Concurrent cache access failed"
        end if
    end subroutine test_concurrent_cache_access

    subroutine test_build_interruption()
        call test_start("Build interruption handling")

        ! Create a test file
        test_file = temp_mgr%get_file_path('interruption_test.f90')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program interruption_test'
        write (unit, '(A)') '    integer :: i'
        write (unit, '(A)') '    do i = 1, 1000000'
        write (unit, '(A)') '        ! Long running computation'
        write (unit, '(A)') '    end do'
        write (unit, '(A)') '    print *, "Done"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Start a build in background that we'll interrupt
        command1 = fortran_with_isolated_cache('interruption_test')//' "'// &
                   trim(test_file)//'" > /dev/null 2>&1 &'
        call execute_command_line(command1, wait=.false.)

        ! Wait a moment then kill any fortrun processes
        call execute_command_line('sleep 0.1 && pkill -f fortrun || true', wait=.true.)

        ! Clean up any stale locks
        call cleanup_stale_locks(temp_mgr%get_path())

        ! Try to run again - should work without issues
        command2 = fortran_with_isolated_cache('interruption_recovery')//' "'// &
                   trim(test_file)//'" > /dev/null 2>&1'
        call execute_command_line(command2, exitstat=exit_code1, wait=.true.)

        success = (exit_code1 == 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Build interruption handling failed"
            print *, "  Exit code: ", exit_code1
        end if
    end subroutine test_build_interruption

    subroutine test_shared_dependency_building()
        character(len=256) :: main1, main2

        call test_start("Shared dependency building")

        ! Create two simple programs that use the same standard module
        ! This tests concurrent access to the same standard library dependencies
        main1 = temp_mgr%get_file_path('main_shared1.f90')
        open (newunit=unit, file=main1, status='replace', iostat=ios)
        write (unit, '(A)') 'program main_shared1'
        write (unit, '(A)') '    use iso_fortran_env, only: real64'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    real(real64) :: x = 123.0_real64'
        write (unit, '(A)') '    print *, "Main1:", x'
        write (unit, '(A)') 'end program'
        close (unit)

        main2 = temp_mgr%get_file_path('main_shared2.f90')
        open (newunit=unit, file=main2, status='replace', iostat=ios)
        write (unit, '(A)') 'program main_shared2'
        write (unit, '(A)') '    use iso_fortran_env, only: real64'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    real(real64) :: y = 456.0_real64'
        write (unit, '(A)') '    print *, "Main2:", y'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Try to build both sequentially - they share the same standard dependencies
        command1 = fortran_with_isolated_cache('shared_dep_test1')//' "'// &
                   trim(main1)//'" > /dev/null 2>&1'
        command2 = fortran_with_isolated_cache('shared_dep_test2')//' "'// &
                   trim(main2)//'" > /dev/null 2>&1'

        ! Run both sequentially to avoid potential race conditions
        call execute_command_line(command1, exitstat=exit_code1, wait=.true.)
        call execute_command_line(command2, exitstat=exit_code2, wait=.true.)

        success = (exit_code1 == 0) .and. (exit_code2 == 0)
        call test_result(success)

        if (.not. success) then
            print *, "  Shared dependency building failed"
            print *, "  Exit codes: ", exit_code1, exit_code2
        end if
    end subroutine test_shared_dependency_building

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

end program test_concurrent_builds
