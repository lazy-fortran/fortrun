program main
  use cli, only: parse_arguments
  use runner, only: run_fortran_file
  use notebook_parser
  use notebook_executor
  use notebook_renderer
  implicit none
  
  character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output
  logical :: show_help, no_wait, notebook_mode
  integer :: exit_code, verbose_level, parallel_jobs
  type(notebook_t) :: notebook
  type(execution_result_t) :: results
  
  call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, custom_config_dir, &
                      parallel_jobs, no_wait, notebook_mode, notebook_output)
  
  if (show_help) then
    call print_help()
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
    call run_fortran_file(filename, exit_code, verbose_level, custom_cache_dir, custom_config_dir, parallel_jobs, no_wait)
    
    if (exit_code /= 0) then
      stop 1
    end if
  end if
  
contains

  subroutine print_help()
    print '(a)', 'Usage: fortran [options] <file.f90>'
    print '(a)', ''
    print '(a)', 'Run a Fortran program file with automatic dependency resolution.'
    print '(a)', ''
    print '(a)', 'Arguments:'
    print '(a)', '  <file.f90>    Path to the Fortran source file to run (.f90 or .f)'
    print '(a)', ''
    print '(a)', 'Options:'
    print '(a)', '  -h, --help        Show this help message'
    print '(a)', '  -v, --verbose 1   Show FPM output'
    print '(a)', '  -vv, --verbose 2  Show detailed FPM output'
    print '(a)', '  -j, --jobs N      (Reserved for future use)'
    print '(a)', '  --cache-dir DIR   Use custom cache directory'
    print '(a)', '  --config-dir DIR  Use custom config directory'
    print '(a)', '  --no-wait         Fail immediately if cache is locked'
    print '(a)', ''
    print '(a)', 'Notebook Mode:'
    print '(a)', '  --notebook        Run as notebook with cells and output capture'
    print '(a)', '  -o, --output FILE Output markdown file (default: <input>.md)'
    print '(a)', ''
    print '(a)', 'Environment:'
    print '(a)', '  OMP_NUM_THREADS   Number of parallel build threads (FPM uses OpenMP)'
  end subroutine print_help
  
end program main
