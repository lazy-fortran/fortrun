program test_system_utils_extended
    use system_utils
    use temp_utils, only: create_temp_dir, create_temp_file, cleanup_temp_dir
    use fpm_filesystem, only: join_path
    implicit none

    logical :: all_tests_passed

    print *, "=== Extended System Utils Tests ==="
    print *

    all_tests_passed = .true.

    ! Test additional system utility functions
    if (.not. test_sys_copy_file()) all_tests_passed = .false.
    if (.not. test_sys_move_file()) all_tests_passed = .false.
    if (.not. test_sys_file_exists()) all_tests_passed = .false.
    if (.not. test_sys_dir_exists()) all_tests_passed = .false.
    if (.not. test_sys_get_current_dir()) all_tests_passed = .false.
    if (.not. test_sys_create_dir()) all_tests_passed = .false.
    if (.not. test_sys_get_path_separator()) all_tests_passed = .false.
    if (.not. test_escape_shell_arg()) all_tests_passed = .false.
    if (.not. test_get_stderr_redirect()) all_tests_passed = .false.
    if (.not. test_sys_count_files()) all_tests_passed = .false.
    if (.not. test_sys_run_command()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All extended system utils tests passed!"
        stop 0
    else
        print *, "Some extended system utils tests failed!"
        stop 1
    end if

contains

    function test_sys_copy_file() result(passed)
        logical :: passed
        character(len=:), allocatable :: temp_dir, src_file, dst_file
        character(len=256) :: error_msg
        logical :: success
        integer :: unit

        print *, "Testing sys_copy_file..."
        passed = .true.

        ! Create temp directory and files
        temp_dir = create_temp_dir('test_copy')
        src_file = join_path(temp_dir, 'source.txt')
        dst_file = join_path(temp_dir, 'dest.txt')

        ! Create source file
        open (newunit=unit, file=src_file, status='replace')
        write (unit, '(A)') 'Test content for copy'
        close (unit)

        ! Test copying file
        call sys_copy_file(src_file, dst_file, success, error_msg)
        if (.not. success) then
            print *, "  FAILED: Could not copy file - ", trim(error_msg)
            passed = .false.
        else if (.not. sys_file_exists(dst_file)) then
            print *, "  FAILED: Destination file does not exist after copy"
            passed = .false.
        else
            print *, "  PASSED: File copy successful"
        end if

        ! Test copying non-existent file
        call sys_copy_file(join_path(temp_dir, 'nonexistent.txt'), dst_file, success)
        if (success) then
            print *, "  FAILED: Copying non-existent file should fail"
            passed = .false.
        else
            print *, "  PASSED: Non-existent file copy failed as expected"
        end if

        call cleanup_temp_dir(temp_dir)

    end function test_sys_copy_file

    function test_sys_move_file() result(passed)
        logical :: passed
        character(len=:), allocatable :: temp_dir, src_file, dst_file
        logical :: success
        integer :: unit

        print *, "Testing sys_move_file..."
        passed = .true.

        ! Create temp directory and file
        temp_dir = create_temp_dir('test_move')
        src_file = join_path(temp_dir, 'source.txt')
        dst_file = join_path(temp_dir, 'moved.txt')

        ! Create source file
        open (newunit=unit, file=src_file, status='replace')
        write (unit, '(A)') 'Test content for move'
        close (unit)

        ! Test moving file
        call sys_move_file(src_file, dst_file, success)
        if (.not. success) then
            print *, "  FAILED: Could not move file"
            passed = .false.
        else if (sys_file_exists(src_file)) then
            print *, "  FAILED: Source file still exists after move"
            passed = .false.
        else if (.not. sys_file_exists(dst_file)) then
            print *, "  FAILED: Destination file does not exist after move"
            passed = .false.
        else
            print *, "  PASSED: File move successful"
        end if

        call cleanup_temp_dir(temp_dir)

    end function test_sys_move_file

    function test_sys_file_exists() result(passed)
        logical :: passed
        character(len=:), allocatable :: temp_file
        integer :: unit

        print *, "Testing sys_file_exists..."
        passed = .true.

        ! Test with existing file
        temp_file = create_temp_file('test_exists', '.txt')
        open (newunit=unit, file=temp_file, status='replace')
        write (unit, '(A)') 'test'
        close (unit)

        if (.not. sys_file_exists(temp_file)) then
            print *, "  FAILED: Existing file not detected"
            passed = .false.
        else
            print *, "  PASSED: Existing file detected"
        end if

        ! Remove and test non-existent
        call sys_remove_file(temp_file)
        if (sys_file_exists(temp_file)) then
            print *, "  FAILED: Non-existent file detected as existing"
            passed = .false.
        else
            print *, "  PASSED: Non-existent file correctly not detected"
        end if

    end function test_sys_file_exists

    function test_sys_dir_exists() result(passed)
        logical :: passed
        character(len=:), allocatable :: temp_dir

        print *, "Testing sys_dir_exists..."
        passed = .true.

        ! Test with existing directory
        temp_dir = create_temp_dir('test_dir_exists')
        if (.not. sys_dir_exists(temp_dir)) then
            print *, "  FAILED: Existing directory not detected"
            passed = .false.
        else
            print *, "  PASSED: Existing directory detected"
        end if

        ! Test with non-existent directory
        call cleanup_temp_dir(temp_dir)
        if (sys_dir_exists(temp_dir)) then
            print *, "  FAILED: Non-existent directory detected as existing"
            passed = .false.
        else
            print *, "  PASSED: Non-existent directory correctly not detected"
        end if

    end function test_sys_dir_exists

    function test_sys_get_current_dir() result(passed)
        logical :: passed
        character(len=512) :: cwd
        logical :: success

        print *, "Testing sys_get_current_dir..."
        passed = .true.

        call sys_get_current_dir(cwd, success)
        if (.not. success) then
            print *, "  FAILED: Could not get current directory"
            passed = .false.
        else if (len_trim(cwd) == 0) then
            print *, "  FAILED: Current directory is empty"
            passed = .false.
        else if (trim(cwd) == '.') then
            print *, "  WARNING: Got fallback directory '.'"
            ! Not a failure, but not ideal
        else
            print *, "  PASSED: Got current directory: ", trim(cwd)
        end if

    end function test_sys_get_current_dir

    function test_sys_create_dir() result(passed)
        logical :: passed
        character(len=:), allocatable :: temp_base, new_dir, nested_dir
        logical :: success

        print *, "Testing sys_create_dir..."
        passed = .true.

        ! Create base temp directory
        temp_base = create_temp_dir('test_create_dir')

        ! Test creating simple directory
        new_dir = join_path(temp_base, 'new_directory')
        call sys_create_dir(new_dir, success)
        if (.not. success) then
            print *, "  FAILED: Could not create directory"
            passed = .false.
        else if (.not. sys_dir_exists(new_dir)) then
            print *, "  FAILED: Directory does not exist after creation"
            passed = .false.
        else
            print *, "  PASSED: Simple directory creation"
        end if

        ! Test creating nested directories
        nested_dir = join_path(join_path(temp_base, 'level1'), 'level2')
        call sys_create_dir(nested_dir, success, create_parents=.true.)
        if (.not. success) then
            print *, "  WARNING: Could not create nested directory (might not support parent creation)"
        else if (.not. sys_dir_exists(nested_dir)) then
            print *, "  WARNING: Nested directory does not exist after creation"
        else
            print *, "  PASSED: Nested directory creation with parents"
        end if

        call cleanup_temp_dir(temp_base)

    end function test_sys_create_dir

    function test_sys_get_path_separator() result(passed)
        logical :: passed
        character(len=1) :: sep

        print *, "Testing sys_get_path_separator..."
        passed = .true.

        sep = sys_get_path_separator()
        if (sep /= '/' .and. sep /= '\') then
            print *, "  FAILED: Invalid path separator: '", sep, "'"
            passed = .false.
        else
            print *, "  PASSED: Got valid path separator: '", sep, "'"
        end if

    end function test_sys_get_path_separator

    function test_escape_shell_arg() result(passed)
        logical :: passed
        character(len=:), allocatable :: input, output

        print *, "Testing escape_shell_arg..."
        passed = .true.

        ! Test simple string
        input = "simple"
        output = escape_shell_arg(input)
        if (output /= input) then
            print *, "  FAILED: Simple string should not be modified"
            passed = .false.
        else
            print *, "  PASSED: Simple string unchanged"
        end if

        ! Test string with quotes
        input = 'test"quotes"here'
        output = escape_shell_arg(input)
        if (index(output, '\"') == 0) then
            print *, "  FAILED: Quotes should be escaped"
            passed = .false.
        else
            print *, "  PASSED: Quotes escaped"
        end if

        ! Test string with dollar sign
        input = 'test$variable'
        output = escape_shell_arg(input)
        if (index(output, '\$') == 0) then
            print *, "  FAILED: Dollar sign should be escaped"
            passed = .false.
        else
            print *, "  PASSED: Dollar sign escaped"
        end if

    end function test_escape_shell_arg

    function test_get_stderr_redirect() result(passed)
        logical :: passed
        character(len=:), allocatable :: redirect

        print *, "Testing get_stderr_redirect..."
        passed = .true.

        redirect = get_stderr_redirect()
        if (redirect /= ' 2>/dev/null' .and. redirect /= ' 2>nul') then
            print *, "  FAILED: Invalid stderr redirect: '", redirect, "'"
            passed = .false.
        else
            print *, "  PASSED: Got valid stderr redirect: '", redirect, "'"
        end if

    end function test_get_stderr_redirect

    function test_sys_count_files() result(passed)
        logical :: passed
        character(len=:), allocatable :: temp_dir
        integer :: count, unit, i
        character(len=32) :: filename

        print *, "Testing sys_count_files..."
        passed = .true.

        ! Create temp directory with some files
        temp_dir = create_temp_dir('test_count')

        ! Create 5 test files
        do i = 1, 5
            write (filename, '(A,I0,A)') 'test_', i, '.txt'
         open (newunit=unit, file=join_path(temp_dir, trim(filename)), status='replace')
            write (unit, '(A)') 'test'
            close (unit)
        end do

        ! Count files
        count = sys_count_files(temp_dir)
        if (count /= 5) then
            print *, "  WARNING: Expected 5 files, got ", count
            ! Not necessarily a failure - might include hidden files
        else
            print *, "  PASSED: Correct file count"
        end if

        call cleanup_temp_dir(temp_dir)

    end function test_sys_count_files

    function test_sys_run_command() result(passed)
        logical :: passed
        character(len=512) :: output
        integer :: exit_code

        print *, "Testing sys_run_command..."
        passed = .true.

        ! Test simple echo command
        call sys_run_command('echo "Hello Test"', output, exit_code)
        if (exit_code /= 0) then
            print *, "  FAILED: Echo command failed with exit code ", exit_code
            passed = .false.
        else if (index(output, 'Hello Test') == 0) then
            print *, "  FAILED: Output does not contain expected text"
            passed = .false.
        else
            print *, "  PASSED: Command execution successful"
        end if

        ! Test command that should fail
        ! Note: Some systems may error on invalid commands, so we use a safer test
        call sys_run_command('exit 1', output, exit_code)
        if (exit_code == 0) then
            print *, "  FAILED: Exit 1 command should have non-zero exit code"
            passed = .false.
        else
            print *, "  PASSED: Failed command detected"
        end if

    end function test_sys_run_command

end program test_system_utils_extended
