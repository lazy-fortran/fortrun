program main
    use cli, only: parse_arguments
    use runner, only: run_fortran_file, is_lazy_fortran_file
    use cache, only: clear_cache, get_cache_info
    use logger, only: set_verbose_level
    ! use notebook_types
    ! use notebook_parser
    ! use notebook_executor
    ! use notebook_renderer
    use temp_utils, only: create_temp_dir, get_temp_file_path, create_temp_file
    use test_cli, only: handle_test_command
    implicit none

  character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output, custom_flags
  logical :: show_help, no_wait, notebook_mode, standardize_only, clear_cache_flag, cache_info_flag
    logical :: debug_tokens = .false., debug_ast = .false., debug_semantic = .false.
    logical :: debug_standardize = .false., debug_codegen = .false.
    logical :: from_tokens, from_ast, from_semantic, show_version
    integer :: exit_code, verbose_level, parallel_jobs
    ! type(notebook_t) :: notebook
    ! type(execution_result_t) :: results
    character(len=256) :: first_arg

    ! Initialize variables properly
    filename = ''
    custom_cache_dir = ''
    custom_config_dir = ''
    notebook_output = ''
    custom_flags = ''

    ! Check for test command first
    if (command_argument_count() > 0) then
        call get_command_argument(1, first_arg)
        if (trim(first_arg) == '--test') then
            call handle_test_subcommand()
            stop 0
        end if
    end if

  call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, custom_config_dir, &
                      parallel_jobs, no_wait, notebook_mode, notebook_output, standardize_only, custom_flags, &
           clear_cache_flag, cache_info_flag, debug_tokens, debug_ast, debug_semantic, &
                         debug_standardize, debug_codegen, &
                         from_tokens, from_ast, from_semantic, show_version)

    ! Initialize logging based on verbose level
    call set_verbose_level(verbose_level)

    if (show_help) then
        call print_help()
        stop 0
    end if

    if (show_version) then
        print '(a)', 'fortrun version 2025.07.git'
        stop 0
    end if

    if (clear_cache_flag) then
        call handle_clear_cache(custom_cache_dir, filename, verbose_level)
        stop 0
    end if

    if (cache_info_flag) then
        call handle_cache_info(custom_cache_dir)
        stop 0
    end if

    if (standardize_only) then
        call handle_standardize_only(filename)
        stop 0
    end if

    ! MLIR emission and compilation now handled by fortc package

    if (from_tokens .or. from_ast .or. from_semantic) then
        print '(a)', 'ERROR: JSON input functionality moved to fortfront package'
        print '(a)', 'Use fortfront directly for JSON input processing'
        stop 1
    end if

    ! Compilation mode now handled by fortc package

    if (notebook_mode) then
        print '(a)', 'ERROR: Notebook mode moved to fnb package'
        print '(a)', 'Install fnb: https://github.com/lazy-fortran/fnb'
        stop 1
    else
        ! Normal execution mode

        call run_fortran_file(filename, exit_code, verbose_level, custom_cache_dir, &
                              custom_config_dir, parallel_jobs, no_wait, custom_flags)

        if (exit_code /= 0) then
            stop 1
        end if
    end if

contains

    subroutine print_help()
        print '(a)', 'Usage: fortrun [options] <file>'
        print '(a)', ''
        print '(a)', 'Run a Fortran program file with automatic dependency resolution.'
        print '(a)', ''
        print '(a)', 'Arguments:'
        print '(a)', '  <file>        Path to Fortran source file'
        print '(a)', '                .f90/.F90 = Standard Fortran (no preprocessing)'
        print '(a)', '                .lf/.LF   = Lowercase Fortran (frontend with type inference)'
        print '(a)', ''
        print '(a)', 'Options:'
        print '(a)', '  -h, --help        Show this help message'
        print '(a)', '  -v, --verbose 1   Show FPM output'
        print '(a)', '  -vv, --verbose 2  Show detailed FPM output'
        print '(a)', '  -j, --jobs N      (Reserved for future use)'
        print '(a)', '  --cache-dir DIR   Use custom cache directory'
        print '(a)', '  --config-dir DIR  Use custom config directory'
        print '(a)', '  --flag FLAGS      Pass custom flags to FPM compiler'
print '(a)', '                    (.f90: user flags only, .lf: opinionated + user flags)'
        print '(a)', '  --no-wait         Fail immediately if cache is locked'
     print '(a)', '  --standardize     Output standardized (.lf files to F90) to STDOUT'
        print '(a)', '  --emit-f90        Synonym for --standardize'
        print '(a)', '  --emit-hlfir      Emit HLFIR (High-Level Fortran IR) to STDOUT'
  print '(a)', '  --emit-fir        Emit FIR (Fortran IR, lowered from HLFIR) to STDOUT'
        print '(a)', '  --emit-llvm       Emit LLVM IR (lowered from FIR) to STDOUT'
        print '(a)', '  --compile         Compile to object code via full MLIR pipeline'
        print '(a)', '  --enable-ad       Enable automatic differentiation with Enzyme'
        print '(a)', '  -o, --output FILE Output file for --compile/--emit-* modes'
        print '(a)', ''
        print '(a)', 'Cache Management:'
        print '(a)', '  --clear-cache     Clear all cached files'
        print '(a)', '  --cache-info      Show cache statistics'
        print '(a)', ''
        print '(a)', 'Notebook Mode:'
        print '(a)', '  --notebook        Run as notebook with cells and output capture'
        print '(a)', '  -o, --output FILE Output markdown file (default: <input>.md)'
        print '(a)', ''
        print '(a)', 'JSON Pipeline Input:'
        print '(a)', '  --from-tokens     Read tokens from JSON file (skip lexing)'
      print '(a)', '  --from-ast        Read AST from JSON file (skip lexing + parsing)'
   print '(a)', '  --from-semantic   Read semantic AST from JSON file (skip to codegen)'
        print '(a)', ''
        print '(a)', 'Debug Output (JSON):'
        print '(a)', '  --debug-tokens    Output tokens as JSON'
        print '(a)', '  --debug-ast       Output AST as JSON'
        print '(a)', '  --debug-semantic  Output semantic analysis as JSON'
        print '(a)', '  --debug-codegen   Output generated code as JSON'
        print '(a)', ''
        print '(a)', 'Environment:'
   print '(a)', '  OMP_NUM_THREADS   Number of parallel build threads (FPM uses OpenMP)'
    end subroutine print_help

    subroutine handle_standardize_only(input_file)
        character(len=*), intent(in) :: input_file
        character(len=256) :: temp_output, error_msg
        character(len=1024) :: line
        integer :: unit, ios
        logical :: is_lowercase_fortran

        ! Check if input is a .lf file (lowercase fortran)
        is_lowercase_fortran = is_lazy_fortran_file(input_file)

        ! Create temporary output file
        temp_output = create_temp_file('fortran_main_output', '.f90')

        ! Process based on file type
        if (is_lowercase_fortran) then
            ! Transform using fortfront CLI
            block
                use system_utils, only: escape_shell_arg
                character(len=1024) :: command
                integer :: exit_status

                command = 'fortfront < "'//trim(escape_shell_arg(input_file))// &
                          '" > "'//trim(escape_shell_arg(temp_output))//'"'
                call execute_command_line(command, exitstat=exit_status, wait=.true.)

                if (exit_status /= 0) then
                    error_msg = 'fortfront transformation failed'
                else
                    error_msg = ''
                end if
            end block
        else
            ! For standard Fortran files, just copy them as-is
            block
                use system_utils, only: escape_shell_arg
                call execute_command_line('cp "'//trim(escape_shell_arg(input_file))// &
                          '" "'//trim(escape_shell_arg(temp_output))//'"', exitstat=ios)
            end block
            if (ios /= 0) then
                error_msg = 'Failed to copy file'
            else
                error_msg = ''
            end if
        end if

        if (len_trim(error_msg) > 0) then
            write (*, '(a,a)') 'Error: ', trim(error_msg)
            stop 1
        end if

        ! Output the preprocessed content to STDOUT
        open (newunit=unit, file=temp_output, status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read (unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                write (*, '(A)') trim(line)
            end do
            close (unit)

            ! Clean up temporary file
            open (newunit=unit, file=temp_output, status='old', iostat=ios)
            if (ios == 0) then
                close (unit, status='delete')
            end if
        else
            write (*, '(a)') 'Error: Failed to read preprocessed output'
            stop 1
        end if

    end subroutine handle_standardize_only

    ! Compilation functionality moved to fortc package

    ! MLIR emission functionality moved to fortc package

    ! JSON input functionality moved to fortfront package

    subroutine handle_clear_cache(custom_cache_dir, filename, verbose_level)
        character(len=*), intent(in) :: custom_cache_dir
        character(len=*), intent(in) :: filename
        integer, intent(in) :: verbose_level
        logical :: success
        integer :: exit_code

        if (verbose_level > 0) then
            print '(a)', 'Clearing cache...'
        end if

        call clear_cache(custom_cache_dir, success)

        if (success) then
            if (verbose_level > 0) then
                print '(a)', 'Cache cleared successfully'
            end if

            ! If a filename was provided, run it after clearing cache
            if (len_trim(filename) > 0) then
                if (verbose_level > 0) then
                    print '(a)', 'Running file with cleared cache...'
                end if
           call run_fortran_file(filename, exit_code, verbose_level, custom_cache_dir, &
                                      '', 0, .false., '')
                if (exit_code /= 0) then
                    stop 1
                end if
            end if
        else
            print '(a)', 'Error: Failed to clear cache'
            stop 1
        end if
    end subroutine handle_clear_cache

    subroutine handle_cache_info(custom_cache_dir)
        character(len=*), intent(in) :: custom_cache_dir
        character(len=1024) :: info

        call get_cache_info(custom_cache_dir, info)
        print '(a)', trim(info)
    end subroutine handle_cache_info

    subroutine handle_test_subcommand()
        integer :: nargs, i, test_exit_code
        character(len=256), allocatable :: test_args(:)

        ! Get all arguments except the first one (--test)
        nargs = command_argument_count() - 1

        if (nargs > 0) then
            allocate (test_args(nargs))
            do i = 1, nargs
                call get_command_argument(i + 1, test_args(i))
            end do
            call handle_test_command(test_args, test_exit_code)
            deallocate (test_args)
        else
            ! No additional arguments, run all tests
            allocate (test_args(0))
            call handle_test_command(test_args, test_exit_code)
            deallocate (test_args)
        end if

        if (test_exit_code /= 0) then
            stop 1
        end if
    end subroutine handle_test_subcommand

end program main
