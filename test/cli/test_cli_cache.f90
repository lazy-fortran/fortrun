program test_cli_cache
    use cli
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Run tests
    if (.not. test_clear_cache_option()) all_passed = .false.
    if (.not. test_cache_info_option()) all_passed = .false.
    if (.not. test_clear_cache_with_file()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All CLI cache tests passed"
        stop 0
    else
        print '(a)', "Some CLI cache tests failed"
        stop 1
    end if

contains

    logical function test_clear_cache_option()
        character(len=256) :: filename, custom_cache_dir, custom_config_dir
        character(len=256) :: notebook_output, custom_flags
        logical :: show_help, no_wait, notebook_mode, preprocess_only
        logical :: clear_cache_flag, cache_info_flag
        integer :: verbose_level, parallel_jobs
        character(len=256), dimension(2) :: args
        
        test_clear_cache_option = .true.
        print '(a)', "Testing --clear-cache option..."
        
        ! Simulate command line: fortran --clear-cache
        args(1) = "fortran"
        args(2) = "--clear-cache"
        call set_test_args(args, 2)
        
        block
            logical :: debug_tokens, debug_ast, debug_codegen
            call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                                custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                                notebook_output, preprocess_only, custom_flags, &
                                clear_cache_flag, cache_info_flag, debug_tokens, debug_ast, debug_codegen)
        end block
        
        if (.not. clear_cache_flag) then
            print '(a)', "FAIL: --clear-cache flag not recognized"
            test_clear_cache_option = .false.
        else if (cache_info_flag) then
            print '(a)', "FAIL: cache_info_flag should not be set"
            test_clear_cache_option = .false.
        else if (show_help) then
            print '(a)', "FAIL: show_help should not be set"
            test_clear_cache_option = .false.
        else
            print '(a)', "PASS: --clear-cache option"
        end if
        
    end function test_clear_cache_option

    logical function test_cache_info_option()
        character(len=256) :: filename, custom_cache_dir, custom_config_dir
        character(len=256) :: notebook_output, custom_flags
        logical :: show_help, no_wait, notebook_mode, preprocess_only
        logical :: clear_cache_flag, cache_info_flag
        integer :: verbose_level, parallel_jobs
        character(len=256), dimension(2) :: args
        
        test_cache_info_option = .true.
        print '(a)', "Testing --cache-info option..."
        
        ! Simulate command line: fortran --cache-info
        args(1) = "fortran"
        args(2) = "--cache-info"
        call set_test_args(args, 2)
        
        block
            logical :: debug_tokens, debug_ast, debug_codegen
            call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                                custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                                notebook_output, preprocess_only, custom_flags, &
                                clear_cache_flag, cache_info_flag, debug_tokens, debug_ast, debug_codegen)
        end block
        
        if (.not. cache_info_flag) then
            print '(a)', "FAIL: --cache-info flag not recognized"
            test_cache_info_option = .false.
        else if (clear_cache_flag) then
            print '(a)', "FAIL: clear_cache_flag should not be set"
            test_cache_info_option = .false.
        else if (show_help) then
            print '(a)', "FAIL: show_help should not be set"
            test_cache_info_option = .false.
        else
            print '(a)', "PASS: --cache-info option"
        end if
        
    end function test_cache_info_option

    logical function test_clear_cache_with_file()
        character(len=256) :: filename, custom_cache_dir, custom_config_dir
        character(len=256) :: notebook_output, custom_flags
        logical :: show_help, no_wait, notebook_mode, preprocess_only
        logical :: clear_cache_flag, cache_info_flag
        integer :: verbose_level, parallel_jobs
        character(len=256), dimension(3) :: args
        
        test_clear_cache_with_file = .true.
        print '(a)', "Testing --clear-cache with file..."
        
        ! Simulate command line: fortran --clear-cache test.f90
        args(1) = "fortran"
        args(2) = "--clear-cache"
        args(3) = "test.f90"
        call set_test_args(args, 3)
        
        block
            logical :: debug_tokens, debug_ast, debug_codegen
            call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                                custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                                notebook_output, preprocess_only, custom_flags, &
                                clear_cache_flag, cache_info_flag, debug_tokens, debug_ast, debug_codegen)
        end block
        
        if (.not. clear_cache_flag) then
            print '(a)', "FAIL: --clear-cache flag not recognized"
            test_clear_cache_with_file = .false.
        else if (filename /= "test.f90") then
            print '(a)', "FAIL: filename not parsed correctly"
            test_clear_cache_with_file = .false.
        else
            print '(a)', "PASS: --clear-cache with file"
        end if
        
    end function test_clear_cache_with_file

    ! Helper subroutine to set test command line arguments
    subroutine set_test_args(args, nargs)
        character(len=*), intent(in) :: args(:)
        integer, intent(in) :: nargs
        
        ! This is a simplified version - in a real test environment,
        ! we would need to mock the command_argument_count and 
        ! get_command_argument functions
        
        ! For now, just print what we would test
        print '(a,i0,a)', "(Would test with ", nargs, " arguments)"
        
    end subroutine set_test_args

end program test_cli_cache