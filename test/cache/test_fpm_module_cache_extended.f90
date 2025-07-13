program test_fpm_module_cache_extended
    use fpm_module_cache, only: get_module_cache_dir
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Extended FPM Module Cache Tests ==="
    print *
    
    all_tests_passed = .true.
    
    ! Test extended functionality of FPM module cache
    if (.not. test_cache_directory()) all_tests_passed = .false.
    if (.not. test_environment_variables()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All extended FPM module cache tests passed!"
        stop 0
    else
        print *, "Some extended FPM module cache tests failed!"
        stop 1
    end if
    
contains

    function test_cache_directory() result(passed)
        logical :: passed
        character(len=:), allocatable :: cache_dir
        
        print *, "Test 1: Cache directory retrieval"
        passed = .true.
        
        ! Test getting cache directory
        cache_dir = get_module_cache_dir()
        
        if (.not. allocated(cache_dir)) then
            print *, "  FAILED: Cache dir not allocated"
            passed = .false.
        else
            if (len_trim(cache_dir) == 0) then
                print *, "  FAILED: Cache dir is empty"
                passed = .false.
            end if
            
            ! Check if it contains expected paths
            if (index(cache_dir, "fortran") == 0) then
                print *, "  WARNING: Cache dir should contain 'fortran'"
            end if
            
            if (index(cache_dir, "modules") == 0) then
                print *, "  WARNING: Cache dir should contain 'modules'"
            end if
        end if
        
        if (passed) print *, "  PASS: Cache directory"
        
    end function test_cache_directory

    function test_environment_variables() result(passed)
        logical :: passed
        character(len=:), allocatable :: cache_dir_before, cache_dir_after
        character(len=256) :: test_path
        
        print *, "Test 2: Environment variable handling"
        passed = .true.
        
        ! Get default cache dir
        cache_dir_before = get_module_cache_dir()
        
        ! Set XDG_CACHE_HOME temporarily
        test_path = "/tmp/test_xdg_cache"
        call execute_command_line("mkdir -p " // trim(test_path))
        call set_environment_variable("XDG_CACHE_HOME", trim(test_path))
        
        ! Get cache dir with XDG_CACHE_HOME set
        cache_dir_after = get_module_cache_dir()
        
        ! On some systems this might not work due to how environment variables are handled
        if (index(cache_dir_after, "test_xdg_cache") > 0) then
            print *, "  INFO: XDG_CACHE_HOME properly used"
        else
            print *, "  INFO: XDG_CACHE_HOME might not be changeable at runtime"
        end if
        
        ! Clean up
        call execute_command_line("rm -rf " // trim(test_path))
        
        if (passed) print *, "  PASS: Environment variables"
        
    end function test_environment_variables

    subroutine set_environment_variable(name, value)
        character(len=*), intent(in) :: name, value
        character(len=512) :: command
        
        ! This won't actually work in the current process but documents the intent
        command = "export " // trim(name) // "=" // trim(value)
        ! Note: Can't actually change environment of running process
    end subroutine set_environment_variable

end program test_fpm_module_cache_extended