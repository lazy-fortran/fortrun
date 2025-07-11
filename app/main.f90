program main
  use cli, only: parse_arguments
  use runner, only: run_fortran_file
  implicit none
  
  character(len=256) :: filename, custom_cache_dir, custom_config_dir
  logical :: show_help
  integer :: exit_code, verbose_level, parallel_jobs
  
  call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, custom_config_dir, parallel_jobs)
  
  if (show_help) then
    call print_help()
    stop 0
  end if
  
  call run_fortran_file(filename, exit_code, verbose_level, custom_cache_dir, custom_config_dir, parallel_jobs)
  
  if (exit_code /= 0) then
    stop 1
  end if
  
contains

  subroutine print_help()
    print '(a)', 'Usage: fortran [options] <file.f90>'
    print '(a)', ''
    print '(a)', 'Run a Fortran program file with automatic dependency resolution.'
    print '(a)', ''
    print '(a)', 'Arguments:'
    print '(a)', '  <file.f90>    Path to the Fortran source file to run'
    print '(a)', ''
    print '(a)', 'Options:'
    print '(a)', '  -h, --help        Show this help message'
    print '(a)', '  -v, --verbose 1   Show FPM output'
    print '(a)', '  -vv, --verbose 2  Show detailed FPM output'
    print '(a)', '  -j, --jobs N      Number of parallel build jobs'
    print '(a)', '  --cache-dir DIR   Use custom cache directory'
    print '(a)', '  --config-dir DIR  Use custom config directory'
  end subroutine print_help
  
end program main
