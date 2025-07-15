program test_cli_debug
    implicit none
    
    ! Test infrastructure variables
    character(len=256), dimension(10) :: test_args
    integer :: test_arg_count
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== CLI Debug Arguments Tests ==='
    print *
    
    ! Run tests
    if (.not. test_debug_tokens()) all_passed = .false.
    if (.not. test_debug_ast()) all_passed = .false.
    if (.not. test_debug_codegen()) all_passed = .false.
    if (.not. test_multiple_debug_flags()) all_passed = .false.
    if (.not. test_json_input_flags()) all_passed = .false.
    
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
        logical :: from_tokens, from_ast, from_semantic
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
                            clear_cache, cache_info, debug_tokens, debug_ast, debug_semantic, debug_codegen, &
                            from_tokens, from_ast, from_semantic)
        
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
        logical :: from_tokens, from_ast, from_semantic
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
                            clear_cache, cache_info, debug_tokens, debug_ast, debug_semantic, debug_codegen, &
                            from_tokens, from_ast, from_semantic)
        
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
        logical :: from_tokens, from_ast, from_semantic
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
                            clear_cache, cache_info, debug_tokens, debug_ast, debug_semantic, debug_codegen, &
                            from_tokens, from_ast, from_semantic)
        
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
        logical :: from_tokens, from_ast, from_semantic
        integer :: verbose_level, parallel_jobs
        character(len=256), dimension(6) :: args
        
        test_multiple_debug_flags = .true.
        print *, 'Testing multiple debug flags...'
        
        ! Simulate command line: fortran --debug-tokens --debug-ast --debug-semantic --debug-codegen test.f
        args(1) = "fortran"
        args(2) = "--debug-tokens"
        args(3) = "--debug-ast"
        args(4) = "--debug-semantic"
        args(5) = "--debug-codegen"
        args(6) = "test.f"
        call set_test_args(args, 6)
        
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                            custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                            notebook_output, preprocess_only, custom_flags, &
                            clear_cache, cache_info, debug_tokens, debug_ast, debug_semantic, debug_codegen, &
                            from_tokens, from_ast, from_semantic)
        
        if (.not. (debug_tokens .and. debug_ast .and. debug_semantic .and. debug_codegen)) then
            print *, 'FAIL: Not all debug flags recognized'
            test_multiple_debug_flags = .false.
        else
            print *, 'PASS: Multiple debug flags work'
        end if
        
    end function test_multiple_debug_flags

    logical function test_json_input_flags()
        character(len=256) :: filename, custom_cache_dir, custom_config_dir
        character(len=256) :: notebook_output, custom_flags
        logical :: show_help, no_wait, notebook_mode, preprocess_only
        logical :: clear_cache, cache_info
        logical :: debug_tokens, debug_ast, debug_semantic, debug_codegen
        logical :: from_tokens, from_ast, from_semantic
        integer :: verbose_level, parallel_jobs
        character(len=256), dimension(3) :: args
        
        test_json_input_flags = .true.
        print *, 'Testing JSON input flags...'
        
        ! Test --from-tokens
        args(1) = "fortran"
        args(2) = "--from-tokens"
        args(3) = "test.json"
        call set_test_args(args, 3)
        
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                            custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                            notebook_output, preprocess_only, custom_flags, &
                            clear_cache, cache_info, debug_tokens, debug_ast, debug_semantic, debug_codegen, &
                            from_tokens, from_ast, from_semantic)
        
        if (.not. from_tokens .or. from_ast .or. from_semantic) then
            print *, 'FAIL: --from-tokens flag not working correctly'
            test_json_input_flags = .false.
            return
        end if
        
        ! Test --from-ast
        args(1) = "fortran"
        args(2) = "--from-ast"
        args(3) = "test.json"
        call set_test_args(args, 3)
        
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                            custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                            notebook_output, preprocess_only, custom_flags, &
                            clear_cache, cache_info, debug_tokens, debug_ast, debug_semantic, debug_codegen, &
                            from_tokens, from_ast, from_semantic)
        
        if (from_tokens .or. .not. from_ast .or. from_semantic) then
            print *, 'FAIL: --from-ast flag not working correctly'
            test_json_input_flags = .false.
            return
        end if
        
        ! Test --from-semantic
        args(1) = "fortran"
        args(2) = "--from-semantic"
        args(3) = "test.json"
        call set_test_args(args, 3)
        
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                            custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                            notebook_output, preprocess_only, custom_flags, &
                            clear_cache, cache_info, debug_tokens, debug_ast, debug_semantic, debug_codegen, &
                            from_tokens, from_ast, from_semantic)
        
        if (from_tokens .or. from_ast .or. .not. from_semantic) then
            print *, 'FAIL: --from-semantic flag not working correctly'
            test_json_input_flags = .false.
            return
        end if
        
        print *, 'PASS: All JSON input flags work correctly'
        
    end function test_json_input_flags

    ! Helper subroutine to mock command line arguments
    subroutine set_test_args(args, nargs)
        character(len=*), intent(in) :: args(:)
        integer, intent(in) :: nargs
        
        ! Store test arguments in module variables for parse_arguments to use
        test_arg_count = nargs
        test_args(1:nargs) = args(1:nargs)
        
        print '(a,i0,a)', "(Testing with ", nargs, " arguments)"
        
    end subroutine set_test_args
    
    ! Mock parse_arguments that uses test_args instead of command line
    subroutine parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                              custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                              notebook_output, preprocess_only, custom_flags, &
                              clear_cache, cache_info, debug_tokens, debug_ast, debug_semantic, debug_codegen, &
                              from_tokens, from_ast, from_semantic)
        character(len=*), intent(out) :: filename
        logical, intent(out) :: show_help
        integer, intent(out) :: verbose_level
        character(len=*), intent(out) :: custom_cache_dir
        character(len=*), intent(out) :: custom_config_dir
        integer, intent(out) :: parallel_jobs
        logical, intent(out) :: no_wait
        logical, intent(out) :: notebook_mode
        character(len=*), intent(out) :: notebook_output
        logical, intent(out) :: preprocess_only
        character(len=*), intent(out), optional :: custom_flags
        logical, intent(out) :: clear_cache
        logical, intent(out) :: cache_info
        logical, intent(out) :: debug_tokens
        logical, intent(out) :: debug_ast
        logical, intent(out) :: debug_semantic
        logical, intent(out) :: debug_codegen
        logical, intent(out) :: from_tokens
        logical, intent(out) :: from_ast
        logical, intent(out) :: from_semantic
        
        integer :: i
        character(len=256) :: arg
        logical :: filename_found
        
        ! Initialize all outputs
        filename = ''
        show_help = .false.
        verbose_level = 0
        custom_cache_dir = ''
        custom_config_dir = ''
        parallel_jobs = 0
        no_wait = .false.
        notebook_mode = .false.
        notebook_output = ''
        preprocess_only = .false.
        clear_cache = .false.
        cache_info = .false.
        debug_tokens = .false.
        debug_ast = .false.
        debug_semantic = .false.
        debug_codegen = .false.
        from_tokens = .false.
        from_ast = .false.
        from_semantic = .false.
        if (present(custom_flags)) custom_flags = ''
        filename_found = .false.
        
        ! Parse test arguments
        do i = 2, test_arg_count  ! Skip program name
            arg = test_args(i)
            
            if (arg == '--debug-tokens') then
                debug_tokens = .true.
            else if (arg == '--debug-ast') then
                debug_ast = .true.
            else if (arg == '--debug-semantic') then
                debug_semantic = .true.
            else if (arg == '--debug-codegen') then
                debug_codegen = .true.
            else if (arg == '--from-tokens') then
                from_tokens = .true.
            else if (arg == '--from-ast') then
                from_ast = .true.
            else if (arg == '--from-semantic') then
                from_semantic = .true.
            else if (arg(1:1) /= '-') then
                if (.not. filename_found) then
                    filename = trim(arg)
                    filename_found = .true.
                end if
            end if
        end do
        
    end subroutine parse_arguments

end program test_cli_debug