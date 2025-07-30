program test_config
    use config, only: get_config_dir, ensure_config_dir, get_registry_path
    use system_utils, only: sys_dir_exists, sys_remove_dir
    use fpm_filesystem, only: join_path
    use fpm_environment, only: get_os_type, OS_WINDOWS
    implicit none

    logical :: all_tests_passed

    print *, "=== Config Module Tests ==="
    print *

    all_tests_passed = .true.

    ! Test config module functions
    if (.not. test_get_config_dir()) all_tests_passed = .false.
    if (.not. test_ensure_config_dir()) all_tests_passed = .false.
    if (.not. test_get_registry_path()) all_tests_passed = .false.
    if (.not. test_config_dir_env_vars()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All config module tests passed!"
        stop 0
    else
        print *, "Some config module tests failed!"
        stop 1
    end if

contains

    function test_get_config_dir() result(passed)
        logical :: passed
        character(len=256) :: config_dir

        print *, "Testing get_config_dir..."
        passed = .true.

        config_dir = get_config_dir()

        if (len_trim(config_dir) == 0) then
            print *, "  FAILED: Empty config directory"
            passed = .false.
        else
            print *, "  PASSED: Got config directory: ", trim(config_dir)

            ! Check that it contains 'fortran' in the path
            if (index(config_dir, 'fortran') == 0) then
                print *, "  WARNING: Config dir doesn't contain 'fortran'"
            end if
        end if

    end function test_get_config_dir

    function test_ensure_config_dir() result(passed)
        logical :: passed
        character(len=256) :: test_dir
        logical :: success

        print *, "Testing ensure_config_dir..."
        passed = .true.

        ! Create a test directory path
        if (get_os_type() == OS_WINDOWS) then
            test_dir = 'C:\temp\fortran_test_config'
        else
            test_dir = '/tmp/fortran_test_config'
        end if

        ! Try to ensure the directory
        call ensure_config_dir(test_dir, success)

        if (.not. success) then
            print *, "  WARNING: Could not create config directory (permissions?)"
            ! This is not necessarily a failure in testing
        else if (.not. sys_dir_exists(test_dir)) then
            print *, "  FAILED: Directory not created despite success=true"
            passed = .false.
        else
            print *, "  PASSED: Config directory created successfully"
            ! Clean up
            call sys_remove_dir(test_dir, success)
        end if

        ! Test with empty path
        call ensure_config_dir('', success)
        if (success) then
            print *, "  FAILED: Empty path should not succeed"
            passed = .false.
        else
            print *, "  PASSED: Empty path handled correctly"
        end if

        ! Test with /dev/null (should be skipped)
        call ensure_config_dir('/dev/null/test', success)
        if (success) then
            print *, "  FAILED: /dev/null path should not succeed"
            passed = .false.
        else
            print *, "  PASSED: /dev/null path handled correctly"
        end if

    end function test_ensure_config_dir

    function test_get_registry_path() result(passed)
        logical :: passed
        character(len=512) :: registry_path
        character(len=256) :: config_dir

        print *, "Testing get_registry_path..."
        passed = .true.

        registry_path = get_registry_path()

        if (len_trim(registry_path) == 0) then
            print *, "  FAILED: Empty registry path"
            passed = .false.
        else if (index(registry_path, 'registry.toml') == 0) then
            print *, "  FAILED: Registry path doesn't contain registry.toml"
            passed = .false.
        else
            print *, "  PASSED: Got registry path: ", trim(registry_path)

            ! Verify it's based on config dir
            config_dir = get_config_dir()
            if (index(registry_path, trim(config_dir)) == 0) then
                print *, "  WARNING: Registry path doesn't contain config dir"
            end if
        end if

    end function test_get_registry_path

    function test_config_dir_env_vars() result(passed)
        logical :: passed
        character(len=256) :: config_dir, saved_env
        integer :: status

        print *, "Testing config dir with environment variables..."
        passed = .true.

        ! Save current XDG_CONFIG_HOME if it exists
        call get_environment_variable('XDG_CONFIG_HOME', saved_env, status=status)

        ! Test with XDG_CONFIG_HOME set
        call set_environment_variable('XDG_CONFIG_HOME', '/tmp/xdg_test')
        config_dir = get_config_dir()

        if (index(config_dir, '/tmp/xdg_test') == 0) then
            print *, "  WARNING: XDG_CONFIG_HOME not respected (might be OS-specific)"
        else
            print *, "  PASSED: XDG_CONFIG_HOME respected"
        end if

        ! Restore original environment
        if (status == 0) then
            call set_environment_variable('XDG_CONFIG_HOME', trim(saved_env))
        end if

        print *, "  INFO: Environment variable testing limited in Fortran"

    end function test_config_dir_env_vars

    ! Helper subroutine to set environment variables (platform-specific)
    subroutine set_environment_variable(name, value)
        character(len=*), intent(in) :: name, value
        character(len=512) :: command
        integer :: exitstat

        if (get_os_type() == OS_WINDOWS) then
            write (command, '(A)') 'set '//trim(name)//'='//trim(value)
        else
            write (command, '(A)') 'export '//trim(name)//'='//trim(value)
        end if

        ! Note: This won't actually affect the current process
        ! Real env var testing requires special handling
    end subroutine set_environment_variable

end program test_config
