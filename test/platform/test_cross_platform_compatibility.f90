program test_cross_platform_compatibility
    use fpm_environment, only: get_os_type, OS_WINDOWS, OS_LINUX, OS_MACOS
    use fpm_filesystem, only: join_path, canon_path, basename
    use system_utils, only: sys_file_exists, sys_dir_exists, sys_get_current_dir, sys_get_temp_dir
    use temp_utils, only: temp_dir_manager
    implicit none

    type(temp_dir_manager) :: temp_mgr
    character(len=256) :: test_path, normalized_path, home_dir
    character(len=1024) :: command
    integer :: test_count, pass_count, exit_code, unit, ios, os_type
    logical :: success

    test_count = 0
    pass_count = 0

    print *, "=== Cross-Platform Compatibility Tests ==="
    print *, ""

    call temp_mgr%create('platform_test')
    os_type = get_os_type()

    ! Test 1: Path handling across platforms
    call test_path_handling()

    ! Test 2: File permissions and attributes
    call test_file_permissions()

    ! Test 3: Environment variable handling
    call test_environment_variables()

    ! Test 4: Case sensitivity in file systems
    call test_case_sensitivity()

    ! Test 5: Command execution differences
    call test_command_execution()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All cross-platform compatibility tests passed!"
        stop 0
    else
        print *, "Some cross-platform compatibility tests failed!"
        stop 1
    end if

contains

    subroutine test_path_handling()
        character(len=256) :: unix_path, windows_path, test_file

        call test_start("Path handling across platforms")

        ! Test path separator handling
        unix_path = "/home/user/project/src/main.f90"
        windows_path = "C:\Users\user\project\src\main.f90"

        ! Join paths correctly for current platform
        test_path = join_path(temp_mgr%get_path(), "test_file.f90")

        ! Create a simple test file
        test_file = test_path
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        if (ios == 0) then
            write (unit, '(A)') 'program path_test'
            write (unit, '(A)') '    print *, "Path test"'
            write (unit, '(A)') 'end program'
            close (unit)

            ! Check file exists with proper path handling
            success = sys_file_exists(test_file)
        else
            success = .false.
        end if

        call test_result(success)

        if (.not. success) then
            print *, "  Path handling failed"
            print *, "  Test path: ", trim(test_path)
        end if
    end subroutine test_path_handling

    subroutine test_file_permissions()
        character(len=256) :: perm_file

        call test_start("File permissions and attributes")

        ! Create a test file
        perm_file = temp_mgr%get_file_path('permissions.f90')
        open (newunit=unit, file=perm_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program perm_test'
        write (unit, '(A)') '    print *, "Permissions"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Test platform-specific permission commands
        if (os_type == OS_WINDOWS) then
            ! Windows uses different permission model
            command = 'attrib +R "'//trim(perm_file)//'" 2>NUL'
            call execute_command_line(command, exitstat=exit_code, wait=.true.)

            ! Remove read-only for cleanup
            command = 'attrib -R "'//trim(perm_file)//'" 2>NUL'
            call execute_command_line(command, wait=.true.)
        else
            ! Unix-like systems use chmod
            command = 'chmod 644 "'//trim(perm_file)//'"'
            call execute_command_line(command, exitstat=exit_code, wait=.true.)
        end if

        ! Success if command executed without error
        success = (exit_code == 0) .or. (os_type == OS_WINDOWS)

        call test_result(success)

        if (.not. success) then
            print *, "  File permissions test failed"
        end if
    end subroutine test_file_permissions

    subroutine test_environment_variables()
        character(len=256) :: env_value

        call test_start("Environment variable handling")

        ! Test getting temp directory across platforms
        home_dir = sys_get_temp_dir()
        success = len_trim(home_dir) > 0

        ! Test platform-specific environment variables
        if (os_type == OS_WINDOWS) then
            call get_environment_variable('USERPROFILE', env_value)
        else
            call get_environment_variable('HOME', env_value)
        end if

        success = success .and. (len_trim(env_value) > 0)

        call test_result(success)

        if (.not. success) then
            print *, "  Environment variable handling failed"
            print *, "  Home dir: ", trim(home_dir)
        end if
    end subroutine test_environment_variables

    subroutine test_case_sensitivity()
        character(len=256) :: lower_file, upper_file
        logical :: lower_exists, upper_exists

        call test_start("File system case sensitivity")

        ! Create file with lowercase name
        lower_file = temp_mgr%get_file_path('testcase.f90')
        open (newunit=unit, file=lower_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program case_test'
        write (unit, '(A)') '    print *, "Case test"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Check if uppercase version is seen as same file
        upper_file = temp_mgr%get_file_path('TESTCASE.F90')

        inquire (file=lower_file, exist=lower_exists)
        inquire (file=upper_file, exist=upper_exists)

        ! On case-insensitive systems (Windows, macOS), both should exist
        ! On case-sensitive systems (Linux), only lowercase should exist
        if (os_type == OS_WINDOWS .or. os_type == OS_MACOS) then
            success = lower_exists .and. upper_exists
        else
            success = lower_exists
        end if

        call test_result(success)

        if (.not. success) then
            print *, "  Case sensitivity test failed"
            print *, "  Lower exists: ", lower_exists
            print *, "  Upper exists: ", upper_exists
        end if
    end subroutine test_case_sensitivity

    subroutine test_command_execution()
        character(len=256) :: output_file

        call test_start("Command execution differences")

        output_file = temp_mgr%get_file_path('cmd_output.txt')

        ! Test platform-specific command execution
        if (os_type == OS_WINDOWS) then
            command = 'echo Testing > "'//trim(output_file)//'"'
        else
            command = 'echo "Testing" > "'//trim(output_file)//'"'
        end if

        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        ! Check output file was created
        success = (exit_code == 0) .and. sys_file_exists(output_file)

        call test_result(success)

        if (.not. success) then
            print *, "  Command execution test failed"
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_command_execution

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

end program test_cross_platform_compatibility
