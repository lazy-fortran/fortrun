program main
  use cli, only: parse_arguments
  use runner, only: run_fortran_file
  use frontend_integration, only: compile_with_frontend, compile_with_frontend_debug, is_simple_fortran_file
  use cache, only: clear_cache, get_cache_info
  use debug_state, only: set_debug_flags
  use notebook_parser
  use notebook_executor
  use notebook_renderer
  implicit none
  
  character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output, custom_flags
  logical :: show_help, no_wait, notebook_mode, preprocess_only, clear_cache_flag, cache_info_flag
  logical :: debug_tokens, debug_ast, debug_codegen
  integer :: exit_code, verbose_level, parallel_jobs
  type(notebook_t) :: notebook
  type(execution_result_t) :: results
  
  call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, custom_config_dir, &
                      parallel_jobs, no_wait, notebook_mode, notebook_output, preprocess_only, custom_flags, &
                      clear_cache_flag, cache_info_flag, debug_tokens, debug_ast, debug_codegen)
  
  if (show_help) then
    call print_help()
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
  
  if (preprocess_only) then
    call handle_preprocess_only(filename)
    stop 0
  end if
  
  if (notebook_mode) then
    ! Run in notebook mode
    if (verbose_level > 0) then
      print '(a)', 'Running in notebook mode...'
    end if
    
    call parse_notebook_file(filename, notebook)
    call execute_notebook(notebook, results, custom_cache_dir, verbose_level)
    call save_notebook_markdown(notebook, results, notebook_output)
    
    if (verbose_level > 0) then
      print '(a,a)', 'Notebook output saved to: ', trim(notebook_output)
    end if
    
    call free_notebook(notebook)
    call free_execution_results(results)
  else
    ! Normal execution mode
    ! Set global debug flags for the runner to use
    call set_debug_flags(debug_tokens, debug_ast, debug_codegen)
    
    call run_fortran_file(filename, exit_code, verbose_level, custom_cache_dir, &
                          custom_config_dir, parallel_jobs, no_wait, custom_flags)
    
    if (exit_code /= 0) then
      stop 1
    end if
  end if
  
contains

  subroutine print_help()
    print '(a)', 'Usage: fortran [options] <file>'
    print '(a)', ''
    print '(a)', 'Run a Fortran program file with automatic dependency resolution.'
    print '(a)', ''
    print '(a)', 'Arguments:'
    print '(a)', '  <file>        Path to Fortran source file'
    print '(a)', '                .f90/.F90 = Standard Fortran (no preprocessing)'
    print '(a)', '                .f/.F     = Simple Fortran (frontend with type inference)'
    print '(a)', ''
    print '(a)', 'Options:'
    print '(a)', '  -h, --help        Show this help message'
    print '(a)', '  -v, --verbose 1   Show FPM output'
    print '(a)', '  -vv, --verbose 2  Show detailed FPM output'
    print '(a)', '  -j, --jobs N      (Reserved for future use)'
    print '(a)', '  --cache-dir DIR   Use custom cache directory'
    print '(a)', '  --config-dir DIR  Use custom config directory'
    print '(a)', '  --flag FLAGS      Pass custom flags to FPM compiler'
    print '(a)', '                    (.f90: user flags only, .f: opinionated + user flags)'
    print '(a)', '  --no-wait         Fail immediately if cache is locked'
    print '(a)', '  --preprocess      Preprocess .f file and output .f90 to STDOUT'
    print '(a)', ''
    print '(a)', 'Cache Management:'
    print '(a)', '  --clear-cache     Clear all cached files'
    print '(a)', '  --cache-info      Show cache statistics'
    print '(a)', ''
    print '(a)', 'Notebook Mode:'
    print '(a)', '  --notebook        Run as notebook with cells and output capture'
    print '(a)', '  -o, --output FILE Output markdown file (default: <input>.md)'
    print '(a)', ''
    print '(a)', 'Environment:'
    print '(a)', '  OMP_NUM_THREADS   Number of parallel build threads (FPM uses OpenMP)'
  end subroutine print_help
  
  subroutine handle_preprocess_only(input_file)
    character(len=*), intent(in) :: input_file
    character(len=256) :: temp_output, error_msg
    character(len=1024) :: line
    integer :: unit, ios
    
    ! Check if input is a .f file
    if (.not. is_simple_fortran_file(input_file)) then
      write(*, '(a)') 'Error: --preprocess can only be used with .f files'
      stop 1
    end if
    
    ! Create temporary output file
    temp_output = trim(input_file) // '.tmp.f90'
    
    ! Compile with frontend to Fortran IR
    if (debug_tokens .or. debug_ast .or. debug_codegen) then
      call compile_with_frontend_debug(input_file, temp_output, error_msg, debug_tokens, debug_ast, debug_codegen)
    else
      call compile_with_frontend(input_file, temp_output, error_msg)
    end if
    
    if (len_trim(error_msg) > 0) then
      write(*, '(a,a)') 'Error: ', trim(error_msg)
      stop 1
    end if
    
    ! Output the preprocessed content to STDOUT
    open(newunit=unit, file=temp_output, status='old', action='read', iostat=ios)
    if (ios == 0) then
      do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        write(*, '(A)') trim(line)
      end do
      close(unit)
      
      ! Clean up temporary file
      open(newunit=unit, file=temp_output, status='old', iostat=ios)
      if (ios == 0) then
        close(unit, status='delete')
      end if
    else
      write(*, '(a)') 'Error: Failed to read preprocessed output'
      stop 1
    end if
    
  end subroutine handle_preprocess_only
  
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
  
  
end program main
