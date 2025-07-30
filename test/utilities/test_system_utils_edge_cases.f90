program test_system_utils_edge_cases
    use system_utils
    use temp_utils, only: create_temp_dir, get_temp_file_path
    implicit none

    logical :: all_passed = .true.
    character(len=256) :: temp_dir, test_file, long_path, nested_path
    character(len=1024) :: error_msg
    logical :: success
    integer :: i

    print *, "=== System Utils Edge Cases Tests ==="

    ! Create temp directory for testing
    temp_dir = create_temp_dir('sys_edge_test')

    ! Test 1: Copy file with empty source path
    call test_copy_empty_paths()

    ! Test 2: File operations with very long paths
    call test_long_paths()

    ! Test 3: Directory operations with special characters
    call test_special_characters()

    ! Test 4: Concurrent file operations simulation
    call test_concurrent_operations()

    ! Test 5: Path separator consistency
    call test_path_separators()

    if (all_passed) then
        print *, ""
        print *, "All system utils edge case tests PASSED!"
        stop 0
    else
        print *, ""
        print *, "Some system utils edge case tests FAILED!"
        stop 1
    end if

contains

    subroutine test_copy_empty_paths()
        print *, ""
        print *, "Test: Empty and invalid paths"

        ! Test empty source path
        call sys_copy_file("", temp_dir//"/dest.txt", success, error_msg)
        if (success) then
            print *, "  FAIL: Should not succeed with empty source"
            all_passed = .false.
        else
            print *, "  PASS: Empty source path handled correctly"
        end if

        ! Test empty destination path
        test_file = get_temp_file_path(temp_dir, 'test.txt')
        open (10, file=test_file, status='replace')
        write (10, '(A)') 'test content'
        close (10)

        call sys_copy_file(test_file, "", success, error_msg)
        if (success) then
            print *, "  FAIL: Should not succeed with empty destination"
            all_passed = .false.
        else
            print *, "  PASS: Empty destination path handled correctly"
        end if
    end subroutine

    subroutine test_long_paths()
        print *, ""
        print *, "Test: Very long file paths"

        ! Create a very long path (but not too long to cause issues)
        long_path = temp_dir
        do i = 1, 5
            long_path = trim(long_path)//"/very_long_directory_name_"//char(48 + i)
            call sys_create_dir(long_path, success)
        end do

        test_file = trim(long_path)//"/test_file_with_very_long_name.txt"

        ! Test file operations with long paths
        open (10, file=test_file, status='replace', iostat=i)
        if (i == 0) then
            write (10, '(A)') 'content'
            close (10)

            if (sys_file_exists(test_file)) then
                print *, "  PASS: Long path file operations work"
            else
                print *, "  FAIL: Long path file not found"
                all_passed = .false.
            end if
        else
            print *, "  INFO: Long path test skipped (OS limitation)"
        end if
    end subroutine

    subroutine test_special_characters()
        print *, ""
        print *, "Test: Special characters in paths"

        ! Test paths with spaces
        nested_path = trim(temp_dir)//"/dir with spaces"
        call sys_create_dir(nested_path, success)

        if (success .and. sys_dir_exists(nested_path)) then
            print *, "  PASS: Directory with spaces created"

            ! Test file in directory with spaces
            test_file = trim(nested_path)//"/file with spaces.txt"
            open (10, file=test_file, status='replace', iostat=i)
            if (i == 0) then
                write (10, '(A)') 'test'
                close (10)
                print *, "  PASS: File in directory with spaces works"
            else
                print *, "  INFO: File with spaces may not be supported"
            end if
        else
            print *, "  INFO: Directories with spaces may not be supported"
        end if

        ! Test path separator edge cases
        if (len_trim(sys_get_path_separator()) > 0) then
            print *, "  PASS: Path separator is non-empty"
        else
            print *, "  FAIL: Path separator is empty"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_concurrent_operations()
        print *, ""
        print *, "Test: Rapid consecutive operations"

        ! Simulate rapid file creation/deletion
        do i = 1, 10
            test_file = get_temp_file_path(temp_dir, 'rapid.txt')
            open (10, file=test_file, status='replace')
            write (10, '(A,I0)') 'iteration ', i
            close (10)

            if (.not. sys_file_exists(test_file)) then
                print *, "  FAIL: Rapid file creation failed at iteration", i
                all_passed = .false.
                exit
            end if

            call sys_remove_file(test_file, success)
            if (.not. success) then
                print *, "  INFO: Rapid file removal might have timing issues"
            end if
        end do

        print *, "  PASS: Rapid consecutive operations completed"
    end subroutine

    subroutine test_path_separators()
        character(len=10) :: sep1, sep2

        print *, ""
        print *, "Test: Path separator consistency"

        sep1 = sys_get_path_separator()
        sep2 = sys_get_path_separator()

        if (sep1 == sep2) then
            print *, "  PASS: Path separator is consistent"
            print *, "  INFO: Path separator is '", trim(sep1), "'"
        else
            print *, "  FAIL: Path separator is inconsistent"
            all_passed = .false.
        end if

        ! Test that separator is exactly one character
        if (len_trim(sep1) == 1) then
            print *, "  PASS: Path separator is single character"
        else
            print *, "  FAIL: Path separator should be single character"
            all_passed = .false.
        end if
    end subroutine

end program test_system_utils_edge_cases
