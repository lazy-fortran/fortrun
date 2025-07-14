program test_cli_debug
    use cli, only: parse_arguments
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== CLI Debug Arguments Tests ==='
    print *
    
    ! Run tests
    if (.not. test_debug_tokens()) all_passed = .false.
    if (.not. test_debug_ast()) all_passed = .false.
    if (.not. test_debug_codegen()) all_passed = .false.
    if (.not. test_multiple_debug_flags()) all_passed = .false.
    
    ! Report results
    print *
    if (all_passed) then
        print *, 'All CLI debug tests passed!'
        stop 0
    else
        print *, 'Some CLI debug tests failed!'
        stop 1
    end if
    
contains

    logical function test_debug_tokens()
        character(len=256) :: filename, custom_cache_dir, custom_config_dir
        character(len=256) :: notebook_output, custom_flags
        logical :: show_help, no_wait, notebook_mode, preprocess_only
        logical :: clear_cache, cache_info
        logical :: debug_tokens, debug_ast, debug_semantic, debug_codegen
        integer :: verbose_level, parallel_jobs
        character(len=256), dimension(3) :: args
        
        test_debug_tokens = .true.
        print *, 'Testing --debug-tokens flag...'
        
        ! Simulate command line: fortran --debug-tokens test.f
        args(1) = "fortran"
        args(2) = "--debug-tokens"
        args(3) = "test.f"
        call set_test_args(args, 3)
        
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                            custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                            notebook_output, preprocess_only, custom_flags, &
                            clear_cache, cache_info, debug_tokens, debug_ast, debug_semantic, debug_codegen)
        
        ! This will fail until we implement the CLI parsing
        if (.not. debug_tokens) then
            print *, 'FAIL: --debug-tokens flag not recognized'
            test_debug_tokens = .false.
        else if (debug_ast .or. debug_semantic .or. debug_codegen) then
            print *, 'FAIL: Other debug flags should not be set'
            test_debug_tokens = .false.
        else
            print *, 'PASS: --debug-tokens flag works'
        end if
        
    end function test_debug_tokens

    logical function test_debug_ast()
        character(len=256) :: filename, custom_cache_dir, custom_config_dir
        character(len=256) :: notebook_output, custom_flags
        logical :: show_help, no_wait, notebook_mode, preprocess_only
        logical :: clear_cache, cache_info
        logical :: debug_tokens, debug_ast, debug_semantic, debug_codegen
        integer :: verbose_level, parallel_jobs
        character(len=256), dimension(3) :: args
        
        test_debug_ast = .true.
        print *, 'Testing --debug-ast flag...'
        
        ! Simulate command line: fortran --debug-ast test.f
        args(1) = "fortran"
        args(2) = "--debug-ast"
        args(3) = "test.f"
        call set_test_args(args, 3)
        
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                            custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                            notebook_output, preprocess_only, custom_flags, &
                            clear_cache, cache_info, debug_tokens, debug_ast, debug_semantic, debug_codegen)
        
        if (.not. debug_ast) then
            print *, 'FAIL: --debug-ast flag not recognized'
            test_debug_ast = .false.
        else if (debug_tokens .or. debug_codegen) then
            print *, 'FAIL: Other debug flags should not be set'
            test_debug_ast = .false.
        else
            print *, 'PASS: --debug-ast flag works'
        end if
        
    end function test_debug_ast

    logical function test_debug_codegen()
        character(len=256) :: filename, custom_cache_dir, custom_config_dir
        character(len=256) :: notebook_output, custom_flags
        logical :: show_help, no_wait, notebook_mode, preprocess_only
        logical :: clear_cache, cache_info
        logical :: debug_tokens, debug_ast, debug_semantic, debug_codegen
        integer :: verbose_level, parallel_jobs
        character(len=256), dimension(3) :: args
        
        test_debug_codegen = .true.
        print *, 'Testing --debug-codegen flag...'
        
        ! Simulate command line: fortran --debug-codegen test.f
        args(1) = "fortran"
        args(2) = "--debug-codegen"
        args(3) = "test.f"
        call set_test_args(args, 3)
        
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                            custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                            notebook_output, preprocess_only, custom_flags, &
                            clear_cache, cache_info, debug_tokens, debug_ast, debug_semantic, debug_codegen)
        
        if (.not. debug_codegen) then
            print *, 'FAIL: --debug-codegen flag not recognized'
            test_debug_codegen = .false.
        else if (debug_tokens .or. debug_ast) then
            print *, 'FAIL: Other debug flags should not be set'
            test_debug_codegen = .false.
        else
            print *, 'PASS: --debug-codegen flag works'
        end if
        
    end function test_debug_codegen

    logical function test_multiple_debug_flags()
        character(len=256) :: filename, custom_cache_dir, custom_config_dir
        character(len=256) :: notebook_output, custom_flags
        logical :: show_help, no_wait, notebook_mode, preprocess_only
        logical :: clear_cache, cache_info
        logical :: debug_tokens, debug_ast, debug_semantic, debug_codegen
        integer :: verbose_level, parallel_jobs
        character(len=256), dimension(5) :: args
        
        test_multiple_debug_flags = .true.
        print *, 'Testing multiple debug flags...'
        
        ! Simulate command line: fortran --debug-tokens --debug-ast --debug-codegen test.f
        args(1) = "fortran"
        args(2) = "--debug-tokens"
        args(3) = "--debug-ast"
        args(4) = "--debug-codegen"
        args(5) = "test.f"
        call set_test_args(args, 5)
        
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                            custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                            notebook_output, preprocess_only, custom_flags, &
                            clear_cache, cache_info, debug_tokens, debug_ast, debug_semantic, debug_codegen)
        
        if (.not. (debug_tokens .and. debug_ast .and. debug_semantic .and. debug_codegen)) then
            print *, 'FAIL: Not all debug flags recognized'
            test_multiple_debug_flags = .false.
        else
            print *, 'PASS: Multiple debug flags work'
        end if
        
    end function test_multiple_debug_flags

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

end program test_cli_debug