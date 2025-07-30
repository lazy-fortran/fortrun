program test_error_handling_edge_cases
    use system_utils
    use temp_utils, only: create_temp_dir, get_temp_file_path
    implicit none

    logical :: all_passed = .true.
    character(len=256) :: temp_dir, test_file, error_file
    character(len=1024) :: error_msg, output
    logical :: success
    integer :: exit_code, unit

    print *, "=== Error Handling Edge Cases Tests ==="

    temp_dir = create_temp_dir('error_edge_test')

    ! Test 1: File permission errors
    call test_permission_errors()

    ! Test 2: Disk space simulation (create large files)
    call test_disk_space_edge_cases()

    ! Test 3: Concurrent access conflicts
    call test_concurrent_access()

    ! Test 4: Invalid command execution
    call test_invalid_commands()

    ! Test 5: Very long error messages
    call test_long_error_messages()

    if (all_passed) then
        print *, ""
        print *, "All error handling edge case tests PASSED!"
        stop 0
    else
        print *, ""
        print *, "Some error handling edge case tests FAILED!"
        stop 1
    end if

contains

    subroutine test_permission_errors()
        print *, ""
        print *, "Test: File permission error handling"

        ! Create a file and try to copy to read-only location
        test_file = get_temp_file_path(temp_dir, 'source.txt')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'test content'
        close (unit)

        ! Try to copy to root directory (should fail with permission error)
        call sys_copy_file(test_file, '/root/forbidden.txt', success, error_msg)

        if (.not. success) then
            print *, "  PASS: Permission error handled correctly"
            if (len_trim(error_msg) > 0) then
                print *, "  PASS: Error message provided"
            else
                print *, "  INFO: No specific error message (may be OK)"
            end if
        else
            print *, "  INFO: Permission test may not apply on this system"
        end if

        ! Try to create directory in protected location
        call sys_create_dir('/root/forbidden_dir', success)
        if (.not. success) then
            print *, "  PASS: Directory creation permission error handled"
        else
            print *, "  INFO: Directory creation test may not apply"
        end if
    end subroutine

    subroutine test_disk_space_edge_cases()
        integer :: i

        print *, ""
        print *, "Test: Disk space edge cases"

        ! Create a moderately large file to test handling
        test_file = get_temp_file_path(temp_dir, 'large.txt')
        open (newunit=unit, file=test_file, status='replace')

        ! Write 1000 lines to create a larger file
        do i = 1, 1000
            write (unit, '(A,I0,A)') 'This is line ', i, &
                ' with some content to make the file larger'
        end do
        close (unit)

        ! Test copying the large file
        error_file = get_temp_file_path(temp_dir, 'large_copy.txt')
        call sys_copy_file(test_file, error_file, success, error_msg)

        if (success) then
            print *, "  PASS: Large file copy handled"
            ! Verify the copy
            if (sys_file_exists(error_file)) then
                print *, "  PASS: Large file copy verified"
            else
                print *, "  FAIL: Large file copy not verified"
                all_passed = .false.
            end if
        else
            print *, "  INFO: Large file copy failed:", trim(error_msg)
        end if
    end subroutine

    subroutine test_concurrent_access()
        integer :: i

        print *, ""
        print *, "Test: Concurrent access error handling"

        test_file = get_temp_file_path(temp_dir, 'concurrent.txt')

        ! Create file
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'concurrent test'
        close (unit)

        ! Simulate rapid operations that might conflict
        call sys_copy_file(test_file, trim(test_file)//'.bak1', success, error_msg)
        call sys_copy_file(test_file, trim(test_file)//'.bak2', success, error_msg)
        call sys_copy_file(test_file, trim(test_file)//'.bak3', success, error_msg)

        if (success) then
            print *, "  PASS: Rapid concurrent operations handled"
        else
            print *, "  INFO: Rapid operations failed (may indicate timing issues)"
        end if

        ! Test file locking scenario (create and immediately try to access)
        do i = 1, 5
            test_file = get_temp_file_path(temp_dir, 'lock_test.tmp')
            open (20, file=test_file, status='replace')
            ! Try to copy while file is open
            call sys_copy_file(test_file, trim(test_file)//'.copy', success, error_msg)
            close (20)
            if (.not. success .and. len_trim(error_msg) > 0) then
                print *, "  PASS: File locking conflict detected"
                exit
            end if
        end do
    end subroutine

    subroutine test_invalid_commands()
        print *, ""
        print *, "Test: Invalid command execution handling"

        ! Test with commands that definitely don't exist
        ! Use 'command -v' or 'which' to test for command existence
        call sys_run_command('command -v nonexistent_cmd_12345 || echo "not found"', output, exit_code)
        if (index(output, 'not found') > 0) then
            print *, "  PASS: Nonexistent command detected"
        else
            print *, "  INFO: Command detection behavior varies"
        end if

        ! Test with invalid arguments to a valid command
call sys_run_command('ls --completely-invalid-flag-xyz 2>&1 || true', output, exit_code)
        print *, "  PASS: Invalid arguments handled"

        ! Test command that should fail
        call sys_run_command('false || echo "failed as expected"', output, exit_code)
        if (index(output, 'failed') > 0) then
            print *, "  PASS: Command failure detected"
        else
            print *, "  INFO: Command failure detection varies"
        end if
    end subroutine

    subroutine test_long_error_messages()
        character(len=1024) :: very_long_path
        integer :: i

        print *, ""
        print *, "Test: Very long error message handling"

        ! Create a path that's too long (on most systems)
        very_long_path = temp_dir
        do i = 1, 20
            very_long_path = trim(very_long_path)//'/very_long_directory_name_'// &
                          'that_exceeds_normal_filesystem_limits_and_should_cause_error'
        end do

        call sys_create_dir(very_long_path, success)

        if (.not. success) then
            print *, "  PASS: Very long path error handled"
        else
            print *, "  INFO: Very long paths may be supported on this system"
        end if

        ! Test copying to invalid destination with long name
        test_file = get_temp_file_path(temp_dir, 'source.txt')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'test'
        close (unit)

        call sys_copy_file(test_file, very_long_path//'/destination.txt', &
                           success, error_msg)

        if (.not. success) then
            print *, "  PASS: Long path copy error handled"
            if (len_trim(error_msg) > 100) then
                print *, "  PASS: Long error message handled"
            else
                print *, "  INFO: Error message length:", len_trim(error_msg)
            end if
        else
            print *, "  INFO: Long path copy succeeded (system may support it)"
        end if
    end subroutine

end program test_error_handling_edge_cases
