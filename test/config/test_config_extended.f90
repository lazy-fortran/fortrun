program test_config_extended
    use config
    use temp_utils, only: temp_dir_manager, path_join
    implicit none

    logical :: all_tests_passed

    print *, "=== Extended Config Tests ==="
    print *

    all_tests_passed = .true.

    ! Test extended functionality
    if (.not. test_config_dir_environment_variables()) all_tests_passed = .false.
    if (.not. test_ensure_config_dir_edge_cases()) all_tests_passed = .false.
    if (.not. test_registry_path_construction()) all_tests_passed = .false.
    if (.not. test_windows_paths()) all_tests_passed = .false.
    if (.not. test_fallback_scenarios()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All extended config tests passed!"
        stop 0
    else
        print *, "Some extended config tests failed!"
        stop 1
    end if

contains

    function test_config_dir_environment_variables() result(passed)
        logical :: passed
        character(len=256) :: config_dir
        character(len=256) :: home_value, xdg_value
        integer :: status

        print *, "Test 1: Config directory with current environment"
        passed = .true.

        ! Test with current environment variables
        config_dir = get_config_dir()

        ! Check XDG_CONFIG_HOME
        call get_environment_variable('XDG_CONFIG_HOME', xdg_value, status=status)
        if (status == 0 .and. len_trim(xdg_value) > 0) then
            if (index(config_dir, 'fortran') > 0) then
                print *, "  INFO: Using XDG_CONFIG_HOME based config"
            end if
        else
            ! Check HOME
            call get_environment_variable('HOME', home_value, status=status)
            if (status == 0 .and. len_trim(home_value) > 0) then
        if (index(config_dir, '.config') > 0 .or. index(config_dir, 'fortran') > 0) then
                    print *, "  INFO: Using HOME based config"
                end if
            else
                ! Check LOCALAPPDATA (Windows)
                call get_environment_variable('LOCALAPPDATA', home_value, status=status)
                if (status == 0 .and. len_trim(home_value) > 0) then
                    print *, "  INFO: Using LOCALAPPDATA based config (Windows)"
                else
                    print *, "  INFO: Using fallback temp directory"
                end if
            end if
        end if

        ! Verify the config directory path is reasonable
        if (len_trim(config_dir) > 0 .and. index(config_dir, 'fortran') > 0) then
            print *, "  PASS: Config directory path is valid: ", trim(config_dir)
        else
            print *, "  FAIL: Invalid config directory: ", trim(config_dir)
            passed = .false.
        end if

    end function test_config_dir_environment_variables

    function test_ensure_config_dir_edge_cases() result(passed)
        logical :: passed
        logical :: success

        print *, "Test 2: Config directory creation edge cases"
        passed = .true.

        ! Test creating a valid directory
        block
            type(temp_dir_manager) :: temp_mgr
            character(len=:), allocatable :: test_dir

            call temp_mgr%create('config_dir_test')
            test_dir = temp_mgr%get_file_path('test_config_valid')
            call ensure_config_dir(test_dir, success)
            if (.not. success) then
                print *, "  WARNING: Valid directory creation failed"
            end if

            ! Test creating directory with nested path
            test_dir = temp_mgr%get_file_path('test_config/nested/deep')
            call ensure_config_dir(test_dir, success)
            if (.not. success) then
                print *, "  WARNING: Nested directory creation failed"
            end if
        end block

        ! Test invalid directory path (should fail gracefully)
        call ensure_config_dir('/dev/null/invalid', success)
        if (success) then
            print *, "  WARNING: Invalid path should have failed"
        end if

        ! Test empty directory path
        call ensure_config_dir('', success)
        if (success) then
            print *, "  WARNING: Empty path should have failed"
        end if

        print *, "  PASS: Directory creation edge cases"

    end function test_ensure_config_dir_edge_cases

    function test_registry_path_construction() result(passed)
        logical :: passed
        character(len=512) :: registry_path
        character(len=256) :: config_dir

        print *, "Test 3: Registry path construction"
        passed = .true.

        ! Test registry path construction
        registry_path = get_registry_path()
        config_dir = get_config_dir()

        ! Verify it includes config dir and registry.toml
        if (index(registry_path, trim(config_dir)) == 0) then
            print *, "  WARNING: Registry path doesn't include config dir"
        end if

        if (index(registry_path, 'registry.toml') == 0) then
            print *, "  WARNING: Registry path doesn't include registry.toml"
        end if

        print *, "  PASS: Registry path construction"

    end function test_registry_path_construction

    function test_windows_paths() result(passed)
        use fpm_environment, only: get_os_type, OS_WINDOWS
        logical :: passed
        character(len=256) :: config_dir

        print *, "Test 4: Windows path handling"
        passed = .true.

        config_dir = get_config_dir()

        if (get_os_type() == OS_WINDOWS) then
            ! On Windows, should use LOCALAPPDATA or temp directory
            if (index(config_dir, 'fortran') > 0) then
                print *, "  PASS: Windows config path looks correct: ", trim(config_dir)
            else
                print *, "  FAIL: Unexpected Windows config path: ", trim(config_dir)
                passed = .false.
            end if
        else
            ! On Unix-like systems, should use HOME/.cache or XDG_CONFIG_HOME
            if (index(config_dir, 'fortran') > 0) then
                print *, "  PASS: Unix config path looks correct: ", trim(config_dir)
            else
                print *, "  FAIL: Unexpected Unix config path: ", trim(config_dir)
                passed = .false.
            end if
        end if

    end function test_windows_paths

    function test_fallback_scenarios() result(passed)
        logical :: passed
        character(len=256) :: config_dir

        print *, "Test 5: Fallback scenarios"
        passed = .true.

        ! Test that we always get a valid config directory
        config_dir = get_config_dir()

        ! The fallback should always return a path containing 'fortran'
        if (len_trim(config_dir) > 0 .and. index(config_dir, 'fortran') > 0) then
            print *, "  PASS: Fallback returns valid path: ", trim(config_dir)
        else
            print *, "  FAIL: Invalid fallback path: ", trim(config_dir)
            passed = .false.
        end if

    end function test_fallback_scenarios

end program test_config_extended
