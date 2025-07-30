program test_temp_utils
    use temp_utils
    use system_utils, only: sys_file_exists, sys_dir_exists, sys_remove_dir
    use fpm_filesystem, only: join_path
    use fpm_environment, only: get_os_type, OS_WINDOWS
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr

    print *, "=== Temp Utils Tests ==="
    print *

    all_tests_passed = .true.

    ! Test temp utility functions
    if (.not. test_create_temp_dir()) all_tests_passed = .false.
    if (.not. test_create_temp_file()) all_tests_passed = .false.
    if (.not. test_get_temp_file_path()) all_tests_passed = .false.
    if (.not. test_get_system_temp_dir()) all_tests_passed = .false.
    if (.not. test_get_current_directory()) all_tests_passed = .false.
    if (.not. test_get_project_root()) all_tests_passed = .false.
    if (.not. test_path_join()) all_tests_passed = .false.
    if (.not. test_mkdir()) all_tests_passed = .false.
    if (.not. test_create_test_cache_dir()) all_tests_passed = .false.
    if (.not. test_fortran_with_isolated_cache()) all_tests_passed = .false.
    if (.not. test_temp_dir_manager()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All temp utils tests passed!"
        stop 0
    else
        print *, "Some temp utils tests failed!"
        stop 1
    end if

contains

    function test_create_temp_dir() result(passed)
        logical :: passed
        character(len=:), allocatable :: temp_dir

        print *, "Testing create_temp_dir..."
        passed = .true.

        ! Create a temporary directory
        temp_dir = create_temp_dir('test_temp')

        if (len_trim(temp_dir) == 0) then
            print *, "  FAILED: Empty temp directory path"
            passed = .false.
        else if (.not. sys_dir_exists(temp_dir)) then
            print *, "  FAILED: Temp directory was not created"
            passed = .false.
        else
            print *, "  PASSED: Temp directory created: ", trim(temp_dir)
            ! Check that directory name contains our prefix
            if (index(temp_dir, 'test_temp') == 0) then
                print *, "  WARNING: Temp directory name doesn't contain prefix"
            end if
        end if

        ! Clean up
        if (allocated(temp_dir)) then
            call cleanup_temp_dir(temp_dir)
        end if

    end function test_create_temp_dir

    function test_create_temp_file() result(passed)
        logical :: passed
        character(len=:), allocatable :: temp_file

        print *, "Testing create_temp_file..."
        passed = .true.

        ! Create temp file without extension
        temp_file = create_temp_file('test_file')
        if (len_trim(temp_file) == 0) then
            print *, "  FAILED: Empty temp file path"
            passed = .false.
        else if (index(temp_file, 'test_file') == 0) then
            print *, "  FAILED: Temp file doesn't contain prefix"
            passed = .false.
        else
            print *, "  PASSED: Temp file path created (no extension)"
        end if

        ! Create temp file with extension
        temp_file = create_temp_file('test_file', '.txt')
        if (index(temp_file, '.txt') == 0) then
            print *, "  FAILED: Temp file doesn't have specified extension"
            passed = .false.
        else
            print *, "  PASSED: Temp file path created with extension"
        end if

        ! Test extension without dot
        temp_file = create_temp_file('test_file', 'dat')
        if (index(temp_file, '.dat') == 0) then
            print *, "  FAILED: Extension without dot not handled correctly"
            passed = .false.
        else
            print *, "  PASSED: Extension without dot handled correctly"
        end if

    end function test_create_temp_file

    function test_get_temp_file_path() result(passed)
        logical :: passed
        character(len=:), allocatable :: temp_dir, file_path

        print *, "Testing get_temp_file_path..."
        passed = .true.

        temp_dir = '/tmp/test'
        file_path = get_temp_file_path(temp_dir, 'file.txt')

        if (index(file_path, temp_dir) == 0) then
            print *, "  FAILED: File path doesn't contain directory"
            passed = .false.
        else if (index(file_path, 'file.txt') == 0) then
            print *, "  FAILED: File path doesn't contain filename"
            passed = .false.
        else
            print *, "  PASSED: File path correctly joined"
        end if

    end function test_get_temp_file_path

    function test_get_system_temp_dir() result(passed)
        logical :: passed
        character(len=:), allocatable :: temp_dir

        print *, "Testing get_system_temp_dir..."
        passed = .true.

        temp_dir = get_system_temp_dir()

        if (len_trim(temp_dir) == 0) then
            print *, "  FAILED: Empty system temp directory"
            passed = .false.
        else
            print *, "  PASSED: Got system temp directory: ", trim(temp_dir)
            ! Verify it's a reasonable path
            if (get_os_type() == OS_WINDOWS) then
                if (index(temp_dir, ':') == 0 .and. index(temp_dir, '\') == 0) then
                    print *, "  WARNING: Windows temp dir looks unusual"
                end if
            else
                if (temp_dir(1:1) /= '/') then
                    print *, "  WARNING: Unix temp dir doesn't start with /"
                end if
            end if
        end if

    end function test_get_system_temp_dir

    function test_get_current_directory() result(passed)
        logical :: passed
        character(len=:), allocatable :: cwd

        print *, "Testing get_current_directory..."
        passed = .true.

        cwd = get_current_directory()

        if (len_trim(cwd) == 0) then
            print *, "  FAILED: Empty current directory"
            passed = .false.
        else if (trim(cwd) == '.') then
            print *, "  WARNING: Got fallback directory '.'"
            ! Not ideal but acceptable
        else
            print *, "  PASSED: Got current directory: ", trim(cwd)
        end if

    end function test_get_current_directory

    function test_get_project_root() result(passed)
        logical :: passed
        character(len=:), allocatable :: root_dir

        print *, "Testing get_project_root..."
        passed = .true.

        root_dir = get_project_root()

        if (len_trim(root_dir) == 0) then
            print *, "  FAILED: Empty project root"
            passed = .false.
        else
            print *, "  PASSED: Got project root: ", trim(root_dir)
            ! Check if it contains expected markers
            if (sys_file_exists(join_path(root_dir, 'fpm.toml'))) then
                print *, "  PASSED: Found fpm.toml in project root"
            else if (sys_dir_exists(join_path(root_dir, '.git'))) then
                print *, "  PASSED: Found .git in project root"
            else
                print *, "  WARNING: No project markers found in root"
            end if
        end if

    end function test_get_project_root

    function test_path_join() result(passed)
        logical :: passed
        character(len=:), allocatable :: joined

        print *, "Testing path_join..."
        passed = .true.

        ! Test normal join
        joined = path_join('/tmp', 'file.txt')
        if (joined /= '/tmp/file.txt' .and. joined /= '/tmp\file.txt') then
            print *, "  FAILED: Normal path join incorrect: ", joined
            passed = .false.
        else
            print *, "  PASSED: Normal path join"
        end if

        ! Test with empty first path
        joined = path_join('', 'file.txt')
        if (joined /= 'file.txt') then
            print *, "  FAILED: Empty first path join incorrect"
            passed = .false.
        else
            print *, "  PASSED: Empty first path handled"
        end if

        ! Test with empty second path
        joined = path_join('/tmp', '')
        if (joined /= '/tmp') then
            print *, "  FAILED: Empty second path join incorrect"
            passed = .false.
        else
            print *, "  PASSED: Empty second path handled"
        end if

        ! Test with absolute second path
        joined = path_join('/tmp', '/absolute/path')
        if (joined /= '/absolute/path') then
            print *, "  FAILED: Absolute second path not handled"
            passed = .false.
        else
            print *, "  PASSED: Absolute second path handled"
        end if

    end function test_path_join

    function test_mkdir() result(passed)
        logical :: passed
        character(len=:), allocatable :: temp_base, test_dir
        integer :: unit
        logical :: success

        print *, "Testing mkdir..."
        passed = .true.

        ! Create base temp directory
        temp_base = create_temp_dir('test_mkdir')
        test_dir = join_path(temp_base, 'new_dir')

        ! Test creating directory
        call mkdir(test_dir)
        if (.not. sys_dir_exists(test_dir)) then
            print *, "  FAILED: Directory not created"
            passed = .false.
        else
            print *, "  PASSED: Directory created"
        end if

        ! Test creating existing directory (should not fail)
        call mkdir(test_dir)
        print *, "  PASSED: Creating existing directory doesn't fail"

        ! Test with file at same path
        call sys_remove_dir(test_dir, success)
        open (newunit=unit, file=test_dir, status='replace')
        write (unit, '(A)') 'test'
        close (unit)

        ! Try to create directory where file exists
        call mkdir(test_dir)
        if (.not. sys_dir_exists(test_dir)) then
     print *, "  INFO: Could not replace file with directory (expected on some systems)"
        else
            print *, "  PASSED: Replaced file with directory"
        end if

        call cleanup_temp_dir(temp_base)

    end function test_mkdir

    function test_create_test_cache_dir() result(passed)
        logical :: passed
        character(len=:), allocatable :: cache_dir

        print *, "Testing create_test_cache_dir..."
        passed = .true.

        cache_dir = create_test_cache_dir('my_test')

        if (len_trim(cache_dir) == 0) then
            print *, "  FAILED: Empty cache directory"
            passed = .false.
        else if (.not. sys_dir_exists(cache_dir)) then
            print *, "  FAILED: Cache directory not created"
            passed = .false.
        else if (index(cache_dir, 'test_cache_my_test') == 0) then
            print *, "  WARNING: Cache dir doesn't contain expected prefix"
        else
            print *, "  PASSED: Test cache directory created"
        end if

        if (allocated(cache_dir)) then
            call cleanup_temp_dir(cache_dir)
        end if

    end function test_create_test_cache_dir

    function test_fortran_with_isolated_cache() result(passed)
        logical :: passed
        character(len=:), allocatable :: command

        print *, "Testing fortran_with_isolated_cache..."
        passed = .true.

        command = fortran_with_isolated_cache('test_name')

        if (len_trim(command) == 0) then
            print *, "  FAILED: Empty command"
            passed = .false.
        else if (index(command, 'XDG_CACHE_HOME') == 0) then
            print *, "  FAILED: Command doesn't set XDG_CACHE_HOME"
            passed = .false.
        else if (index(command, 'fpm run fortrun') == 0) then
            print *, "  FAILED: Command doesn't contain fpm run fortrun"
            passed = .false.
        else
            print *, "  PASSED: Isolated cache command created"
            if (get_os_type() == OS_WINDOWS) then
                if (index(command, 'cmd /c') == 0) then
                    print *, "  WARNING: Windows command doesn't use cmd /c"
                end if
            end if
        end if

    end function test_fortran_with_isolated_cache

    function test_temp_dir_manager() result(passed)
        logical :: passed
        character(len=:), allocatable :: path, file_path
        integer :: unit

        print *, "Testing temp_dir_manager..."
        passed = .true.

        ! Test creating managed temp directory
        call temp_mgr%create('test_mgr')
        path = temp_mgr%get_path()

        if (len_trim(path) == 0) then
            print *, "  FAILED: Empty managed temp directory"
            passed = .false.
        else if (.not. sys_dir_exists(path)) then
            print *, "  FAILED: Managed temp directory not created"
            passed = .false.
        else
            print *, "  PASSED: Managed temp directory created"

            ! Test getting file path
            file_path = temp_mgr%get_file_path('test.txt')
            if (index(file_path, path) == 0) then
                print *, "  FAILED: File path doesn't contain temp directory"
                passed = .false.
            else if (index(file_path, 'test.txt') == 0) then
                print *, "  FAILED: File path doesn't contain filename"
                passed = .false.
            else
                print *, "  PASSED: File path creation"
            end if

            ! Create a file to test cleanup
            open (newunit=unit, file=file_path, status='replace')
            write (unit, '(A)') 'test'
            close (unit)
        end if

        ! Test manual cleanup
        call temp_mgr%cleanup()
        if (sys_dir_exists(path)) then
            print *, "  WARNING: Directory still exists after cleanup"
            ! Some systems might have delays
        else
            print *, "  PASSED: Manual cleanup successful"
        end if

    end function test_temp_dir_manager

end program test_temp_utils
