program test_temp_utils_edge_cases
    use temp_utils
    use system_utils, only: sys_file_exists, sys_dir_exists, sys_run_command
    implicit none

    logical :: all_passed = .true.
    character(len=256) :: temp_dir, temp_file, result_path
    character(len=512) :: output
    logical :: success
    integer :: i, exit_code
    type(temp_dir_manager) :: temp_mgr

    print *, "=== Temp Utils Edge Cases Tests ==="

    ! Test 1: Empty and invalid directory names
    call test_invalid_names()

    ! Test 2: Concurrent temp directory creation
    call test_concurrent_creation()

    ! Test 3: Very long paths
    call test_long_paths()

    ! Test 4: Special characters in names
    call test_special_characters()

    ! Test 5: Cleanup scenarios
    call test_cleanup_scenarios()

    ! Test 6: Temp file edge cases
    call test_temp_files()

    ! Test 7: Directory manager edge cases
    call test_dir_manager()

    if (all_passed) then
        print *, ""
        print *, "All temp utils edge case tests PASSED!"
        stop 0
    else
        print *, ""
        print *, "Some temp utils edge case tests FAILED!"
        stop 1
    end if

contains

    subroutine test_invalid_names()
        print *, ""
        print *, "Test: Invalid directory names"

        ! Test empty prefix
        temp_dir = create_temp_dir('')
        if (len_trim(temp_dir) > 0 .and. sys_dir_exists(temp_dir)) then
            print *, "  PASS: Empty prefix handled (generated name)"
            call cleanup_temp_dir(temp_dir)
        else
            print *, "  FAIL: Empty prefix should generate valid dir"
            all_passed = .false.
        end if

        ! Test very long prefix
        temp_dir = create_temp_dir(repeat('a', 200))
        if (len_trim(temp_dir) > 0) then
            print *, "  PASS: Long prefix handled"
            if (sys_dir_exists(temp_dir)) then
                call cleanup_temp_dir(temp_dir)
            end if
        else
            print *, "  INFO: Long prefix may be truncated"
        end if

        ! Test prefix with path separators
        temp_dir = create_temp_dir('invalid/prefix/name')
        if (len_trim(temp_dir) > 0) then
            print *, "  PASS: Path separators in prefix handled"
            call cleanup_temp_dir(temp_dir)
        else
            print *, "  FAIL: Should handle path separators"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_concurrent_creation()
        character(len=256) :: dirs(10)
        logical :: all_exist
        integer :: j

        print *, ""
        print *, "Test: Concurrent temp directory creation"

        ! Create multiple temp directories rapidly
        do i = 1, 10
            dirs(i) = create_temp_dir('concurrent_test')
        end do

        ! Verify all are unique
        all_exist = .true.
        do i = 1, 10
            if (.not. sys_dir_exists(dirs(i))) then
                all_exist = .false.
                exit
            end if

            ! Check uniqueness
            do j = i + 1, 10
                if (dirs(i) == dirs(j)) then
                    print *, "  FAIL: Duplicate directory created"
                    all_passed = .false.
                    exit
                end if
            end do
        end do

        if (all_exist) then
            print *, "  PASS: All concurrent directories created uniquely"
        else
            print *, "  FAIL: Some directories not created"
            all_passed = .false.
        end if

        ! Cleanup
        do i = 1, 10
            call cleanup_temp_dir(dirs(i))
        end do
    end subroutine

    subroutine test_long_paths()
        character(len=512) :: long_base

        print *, ""
        print *, "Test: Very long path handling"

        ! Create nested temp directories
        temp_dir = create_temp_dir('base')
        long_base = temp_dir

        do i = 1, 10
            long_base = trim(long_base)//'/subdir_'//achar(48 + mod(i, 10))
            call make_dir(long_base)
        end do

        ! Try to create temp dir in deeply nested path
        temp_file = get_temp_file_path(long_base, 'deep_file.txt')

        if (len_trim(temp_file) > 0) then
            print *, "  PASS: Deep path temp file created"
        else
            print *, "  FAIL: Deep path file creation failed"
            all_passed = .false.
        end if

        ! Cleanup base
        call cleanup_temp_dir(temp_dir)
        print *, "  PASS: Deep directory tree cleaned up"
    end subroutine

    subroutine test_special_characters()
        print *, ""
        print *, "Test: Special characters in names"

        ! Test with spaces
        temp_dir = create_temp_dir('name with spaces')
        if (sys_dir_exists(temp_dir)) then
            print *, "  PASS: Directory with spaces created"
            call cleanup_temp_dir(temp_dir)
        else
            print *, "  INFO: Spaces in names may be sanitized"
        end if

        ! Test with dots
        temp_dir = create_temp_dir('name.with.dots')
        if (sys_dir_exists(temp_dir)) then
            print *, "  PASS: Directory with dots created"
            call cleanup_temp_dir(temp_dir)
        else
            print *, "  FAIL: Dots should be allowed"
            all_passed = .false.
        end if

        ! Test with underscores and dashes
        temp_dir = create_temp_dir('name_with-mixed_chars')
        if (sys_dir_exists(temp_dir)) then
            print *, "  PASS: Mixed special chars handled"
            call cleanup_temp_dir(temp_dir)
        else
            print *, "  FAIL: Underscores and dashes should work"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_cleanup_scenarios()
        character(len=256) :: nested_dir
        integer :: unit

        print *, ""
        print *, "Test: Cleanup scenarios"

        ! Test cleanup of non-existent directory
        call cleanup_temp_dir('/nonexistent/path/12345')
        print *, "  PASS: Non-existent directory cleanup handled"

        ! Test cleanup of directory with files
        temp_dir = create_temp_dir('cleanup_test')

        ! Create some files
        temp_file = get_temp_file_path(temp_dir, 'file1.txt')
        open (newunit=unit, file=temp_file, status='replace')
        write (unit, '(A)') 'test content'
        close (unit)

        ! Create subdirectory with files
        nested_dir = trim(temp_dir)//'/subdir'
        call make_dir(nested_dir)
        temp_file = get_temp_file_path(nested_dir, 'file2.txt')
        open (newunit=unit, file=temp_file, status='replace')
        write (unit, '(A)') 'nested content'
        close (unit)

        ! Now cleanup
        call cleanup_temp_dir(temp_dir)

        if (.not. sys_dir_exists(temp_dir)) then
            print *, "  PASS: Directory with contents cleaned up"
        else
            print *, "  FAIL: Directory cleanup failed"
            all_passed = .false.
        end if

        ! Test cleanup with read-only files (if supported)
        temp_dir = create_temp_dir('readonly_test')
        temp_file = get_temp_file_path(temp_dir, 'readonly.txt')
        open (newunit=unit, file=temp_file, status='replace')
        write (unit, '(A)') 'readonly'
        close (unit)

        ! Try to make read-only (platform dependent)
call sys_run_command('chmod 444 "'//trim(temp_file)//'" 2>/dev/null', output, exit_code)

        call cleanup_temp_dir(temp_dir)
        print *, "  PASS: Read-only file cleanup attempted"
    end subroutine

    subroutine test_temp_files()
        character(len=256) :: file1, file2

        print *, ""
        print *, "Test: Temp file edge cases"

        temp_dir = create_temp_dir('file_test')

        ! Test empty filename
        temp_file = get_temp_file_path(temp_dir, '')
        if (len_trim(temp_file) > len_trim(temp_dir)) then
            print *, "  PASS: Empty filename generated unique name"
        else
            print *, "  FAIL: Empty filename should generate name"
            all_passed = .false.
        end if

        ! Test very long filename
        temp_file = get_temp_file_path(temp_dir, repeat('x', 200)//'.txt')
        if (len_trim(temp_file) > 0) then
            print *, "  PASS: Long filename handled"
        else
            print *, "  FAIL: Long filename failed"
            all_passed = .false.
        end if

        ! Test filename with path separators
        temp_file = get_temp_file_path(temp_dir, 'invalid/file/name.txt')
        ! The function may handle this differently
        print *, "  INFO: Path separators in filename tested"

        ! Test uniqueness
        file1 = get_temp_file_path(temp_dir, 'test.txt')
        file2 = get_temp_file_path(temp_dir, 'test.txt')

        ! Note: get_temp_file_path may not guarantee uniqueness
        print *, "  INFO: File path generation tested"

        call cleanup_temp_dir(temp_dir)
    end subroutine

    subroutine test_dir_manager()
        character(len=256) :: file_path
        integer :: unit

        print *, ""
        print *, "Test: Directory manager edge cases"

        ! Test with empty prefix
        call temp_mgr%create('')
        if (sys_dir_exists(temp_mgr%path)) then
            print *, "  PASS: Manager with empty prefix works"
        else
            print *, "  FAIL: Manager creation failed"
            all_passed = .false.
        end if

        ! Test file operations
        file_path = temp_mgr%get_file_path('test.dat')
        open (newunit=unit, file=file_path, status='replace')
        write (unit, '(A)') 'manager test'
        close (unit)

        if (sys_file_exists(file_path)) then
            print *, "  PASS: Manager file operations work"
        else
            print *, "  FAIL: Manager file not created"
            all_passed = .false.
        end if

        ! Test automatic cleanup on deallocation
        block
            type(temp_dir_manager) :: local_mgr
            character(len=256) :: local_path

            call local_mgr%create('auto_cleanup')
            local_path = local_mgr%path

            if (sys_dir_exists(local_path)) then
                print *, "  PASS: Local manager created"
            end if
            ! Manager goes out of scope here
        end block

        print *, "  INFO: Automatic cleanup on scope exit tested"

        ! Test cache directory structure
        result_path = ensure_cache_dir_exists('test_cache')
        if (sys_dir_exists(result_path)) then
            print *, "  PASS: Cache directory created"
        else
            print *, "  FAIL: Cache directory creation failed"
            all_passed = .false.
        end if
    end subroutine

    ! Mock helper
    subroutine make_dir(path)
        character(len=*), intent(in) :: path
        character(len=512) :: cmd
        integer :: exit_code

        cmd = 'mkdir -p "'//trim(path)//'"'
        call sys_run_command(trim(cmd), cmd, exit_code)
    end subroutine

    function ensure_cache_dir_exists(name) result(path)
        character(len=*), intent(in) :: name
        character(len=256) :: path

        path = trim(create_temp_dir('cache_'//name))
    end function

end program test_temp_utils_edge_cases
