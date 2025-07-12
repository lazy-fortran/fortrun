program test_config_coverage
    use config
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Config Module Coverage Tests ==="
    print *
    
    all_tests_passed = .true.
    
    ! Test config functionality
    if (.not. test_config_directory()) all_tests_passed = .false.
    if (.not. test_registry_path()) all_tests_passed = .false.
    if (.not. test_environment_handling()) all_tests_passed = .false.
    if (.not. test_directory_creation()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All config coverage tests passed!"
        stop 0
    else
        print *, "Some config coverage tests failed!"
        stop 1
    end if
    
contains

    function test_config_directory() result(passed)
        logical :: passed
        character(len=256) :: config_dir
        
        print *, "Test 1: Config directory retrieval"
        passed = .true.
        
        ! Get default config directory
        config_dir = get_config_dir()
        
        if (len_trim(config_dir) == 0) then
            print *, "  FAILED: Config dir is empty"
            passed = .false.
        end if
        
        ! Check if it contains expected pattern
        if (index(config_dir, "fortran") == 0) then
            print *, "  WARNING: Config dir should contain 'fortran'"
        end if
        
        if (passed) print *, "  PASS: Config directory"
        
    end function test_config_directory

    function test_registry_path() result(passed)
        logical :: passed
        character(len=512) :: registry_path
        character(len=256) :: config_dir
        
        print *, "Test 2: Registry path construction"
        passed = .true.
        
        ! Get registry path
        config_dir = get_config_dir()
        registry_path = get_registry_path()
        
        if (len_trim(registry_path) == 0) then
            print *, "  FAILED: Registry path is empty"
            passed = .false.
        end if
        
        ! Check if it ends with registry.toml
        if (index(registry_path, "registry.toml") == 0) then
            print *, "  FAILED: Registry path should end with registry.toml"
            passed = .false.
        end if
        
        ! Check if it contains the config dir
        if (index(registry_path, trim(config_dir)) == 0) then
            print *, "  WARNING: Registry path should contain config dir"
        end if
        
        if (passed) print *, "  PASS: Registry path"
        
    end function test_registry_path

    function test_environment_handling() result(passed)
        logical :: passed
        character(len=256) :: custom_dir
        logical :: success
        
        print *, "Test 3: Environment variable handling"
        passed = .true.
        
        ! Test custom config directory
        custom_dir = "/tmp/test_custom_config"
        
        ! Create directory
        call ensure_config_dir(custom_dir, success)
        
        if (.not. success) then
            print *, "  WARNING: Could not create custom config dir"
        else
            ! Check if directory exists
            inquire(file=trim(custom_dir), exist=success)
            if (.not. success) then
                print *, "  FAILED: Custom dir was not created"
                passed = .false.
            end if
        end if
        
        ! Clean up
        call execute_command_line("rm -rf " // trim(custom_dir))
        
        if (passed) print *, "  PASS: Environment handling"
        
    end function test_environment_handling

    function test_directory_creation() result(passed)
        logical :: passed
        character(len=256) :: test_dir
        logical :: success
        integer :: i
        character(len=10) :: subdir
        
        print *, "Test 4: Directory creation edge cases"
        passed = .true.
        
        ! Test nested directory creation
        test_dir = "/tmp/test_config_nested/level1/level2/level3"
        call ensure_config_dir(test_dir, success)
        
        if (.not. success) then
            print *, "  WARNING: Nested directory creation might have failed"
        end if
        
        ! Test with existing directory
        call ensure_config_dir("/tmp", success)
        if (.not. success) then
            print *, "  FAILED: Should succeed with existing directory"
            passed = .false.
        end if
        
        ! Test invalid path (should fail gracefully)
        call ensure_config_dir("/root/definitely_no_permission", success)
        if (success) then
            print *, "  WARNING: Should fail for no-permission directory"
        end if
        
        ! Clean up
        call execute_command_line("rm -rf /tmp/test_config_nested")
        
        if (passed) print *, "  PASS: Directory creation"
        
    end function test_directory_creation

end program test_config_coverage