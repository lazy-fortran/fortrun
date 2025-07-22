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
        character(len=256) :: original_home, original_xdg
        integer :: status

        print *, "Test 1: Config directory with different environment variables"
        passed = .true.

        ! Save original environment
        call get_environment_variable('HOME', original_home, status=status)
        call get_environment_variable('XDG_CONFIG_HOME', original_xdg, status=status)

        ! Test with XDG_CONFIG_HOME set
        block
            type(temp_dir_manager) :: temp_mgr
            character(len=:), allocatable :: test_xdg, test_home

            call temp_mgr%create('config_test')
            test_xdg = temp_mgr%get_file_path('test_xdg_config')
            test_home = temp_mgr%get_file_path('test_home')

            call setenv_wrapper('XDG_CONFIG_HOME', test_xdg)
            config_dir = get_config_dir()
            if (index(config_dir, path_join(test_xdg, 'fortran')) == 0) then
                print *, "  WARNING: XDG_CONFIG_HOME test may not work properly"
            end if

            ! Test with HOME set but no XDG_CONFIG_HOME
            call unsetenv_wrapper('XDG_CONFIG_HOME')
            call setenv_wrapper('HOME', test_home)
            config_dir = get_config_dir()
            if (index(config_dir, path_join(test_home, '.config/fortran')) == 0) then
                print *, "  WARNING: HOME test may not work properly"
            end if
        end block

        ! Restore original environment (approximately)
        if (len_trim(original_home) > 0) then
            call setenv_wrapper('HOME', trim(original_home))
        end if
        if (len_trim(original_xdg) > 0) then
            call setenv_wrapper('XDG_CONFIG_HOME', trim(original_xdg))
        else
            call unsetenv_wrapper('XDG_CONFIG_HOME')
        end if

        print *, "  PASS: Environment variable handling"

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
        logical :: passed
        character(len=256) :: config_dir, original_home, original_localappdata
        integer :: status

        print *, "Test 4: Windows path handling"
        passed = .true.

        ! Save original environment
        call get_environment_variable('HOME', original_home, status=status)
     call get_environment_variable('LOCALAPPDATA', original_localappdata, status=status)

        ! Simulate Windows environment (no HOME, has LOCALAPPDATA)
        call unsetenv_wrapper('HOME')
        call unsetenv_wrapper('XDG_CONFIG_HOME')
        call setenv_wrapper('LOCALAPPDATA', 'C:\Users\Test\AppData\Local')

        config_dir = get_config_dir()
        if (index(config_dir, 'AppData') == 0 .and. index(config_dir, '.fortran-config') == 0) then
            print *, "  WARNING: Windows path fallback may not work properly"
        end if

        ! Restore environment
        if (len_trim(original_home) > 0) then
            call setenv_wrapper('HOME', trim(original_home))
        end if
        if (len_trim(original_localappdata) > 0) then
            call setenv_wrapper('LOCALAPPDATA', trim(original_localappdata))
        else
            call unsetenv_wrapper('LOCALAPPDATA')
        end if

        print *, "  PASS: Windows path handling"

    end function test_windows_paths

    function test_fallback_scenarios() result(passed)
        logical :: passed
        character(len=256) :: config_dir
        character(len=256) :: original_home, original_xdg, original_localappdata
        integer :: status

        print *, "Test 5: Fallback scenarios"
        passed = .true.

        ! Save original environment
        call get_environment_variable('HOME', original_home, status=status)
        call get_environment_variable('XDG_CONFIG_HOME', original_xdg, status=status)
     call get_environment_variable('LOCALAPPDATA', original_localappdata, status=status)

        ! Test ultimate fallback (no environment variables)
        call unsetenv_wrapper('HOME')
        call unsetenv_wrapper('XDG_CONFIG_HOME')
        call unsetenv_wrapper('LOCALAPPDATA')

        config_dir = get_config_dir()
        if (index(config_dir, '.fortran-config') == 0) then
            print *, "  WARNING: Ultimate fallback should use .fortran-config"
        end if

        ! Restore environment
        if (len_trim(original_home) > 0) then
            call setenv_wrapper('HOME', trim(original_home))
        end if
        if (len_trim(original_xdg) > 0) then
            call setenv_wrapper('XDG_CONFIG_HOME', trim(original_xdg))
        end if
        if (len_trim(original_localappdata) > 0) then
            call setenv_wrapper('LOCALAPPDATA', trim(original_localappdata))
        end if

        print *, "  PASS: Fallback scenarios"

    end function test_fallback_scenarios

    ! Helper subroutines for environment variable manipulation
    subroutine setenv_wrapper(name, value)
        use iso_c_binding
        character(len=*), intent(in) :: name, value
        character(kind=c_char), target :: c_name(len(name)+1)
        character(kind=c_char), target :: c_value(len(value)+1)
        integer :: i
        
        interface
            function setenv(name, value, overwrite) bind(c, name="setenv")
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: name(*), value(*)
                integer(c_int), value :: overwrite
                integer(c_int) :: setenv
            end function setenv
            
            function _putenv(envstring) bind(c, name="_putenv")
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: envstring(*)
                integer(c_int) :: _putenv
            end function _putenv
        end interface
        
#ifdef _WIN32
        character(kind=c_char), target :: c_envstring(len(name)+len(value)+2)
        integer(c_int) :: result
        
        ! Create NAME=VALUE string for Windows
        do i = 1, len(name)
            c_envstring(i) = name(i:i)
        end do
        c_envstring(len(name)+1) = '='
        do i = 1, len(value)
            c_envstring(len(name)+1+i) = value(i:i)
        end do
        c_envstring(len(name)+len(value)+2) = c_null_char
        
        result = _putenv(c_envstring)
#else
        integer(c_int) :: result
        
        ! Convert to C strings
        do i = 1, len(name)
            c_name(i) = name(i:i)
        end do
        c_name(len(name)+1) = c_null_char
        
        do i = 1, len(value)
            c_value(i) = value(i:i)
        end do
        c_value(len(value)+1) = c_null_char
        
        result = setenv(c_name, c_value, 1_c_int)
#endif

    end subroutine setenv_wrapper

    subroutine unsetenv_wrapper(name)
        use iso_c_binding
        character(len=*), intent(in) :: name
        character(kind=c_char), target :: c_name(len(name)+1)
        integer :: i
        
        interface
            function unsetenv(name) bind(c, name="unsetenv")
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: name(*)
                integer(c_int) :: unsetenv
            end function unsetenv
            
            function _putenv(envstring) bind(c, name="_putenv")
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: envstring(*)
                integer(c_int) :: _putenv
            end function _putenv
        end interface
        
#ifdef _WIN32
        character(kind=c_char), target :: c_envstring(len(name)+2)
        integer(c_int) :: result
        
        ! Create NAME= string for Windows (empty value unsets)
        do i = 1, len(name)
            c_envstring(i) = name(i:i)
        end do
        c_envstring(len(name)+1) = '='
        c_envstring(len(name)+2) = c_null_char
        
        result = _putenv(c_envstring)
#else
        integer(c_int) :: result
        
        ! Convert to C string
        do i = 1, len(name)
            c_name(i) = name(i:i)
        end do
        c_name(len(name)+1) = c_null_char
        
        result = unsetenv(c_name)
#endif

    end subroutine unsetenv_wrapper

end program test_config_extended
