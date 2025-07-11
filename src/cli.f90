module cli
  implicit none
  private
  public :: parse_arguments
  
contains

  subroutine parse_arguments(filename, show_help, verbose_level, custom_cache_dir, custom_config_dir, parallel_jobs)
    character(len=*), intent(out) :: filename
    logical, intent(out) :: show_help
    integer, intent(out) :: verbose_level
    character(len=*), intent(out) :: custom_cache_dir
    character(len=*), intent(out) :: custom_config_dir
    integer, intent(out) :: parallel_jobs
    
    integer :: nargs, i, iostat
    character(len=256) :: arg
    logical :: filename_found, expecting_cache_dir, expecting_config_dir, expecting_jobs
    
    filename = ''
    show_help = .false.
    verbose_level = 0
    custom_cache_dir = ''
    custom_config_dir = ''
    parallel_jobs = 0  ! 0 means use FPM default
    filename_found = .false.
    expecting_cache_dir = .false.
    expecting_config_dir = .false.
    expecting_jobs = .false.
    
    nargs = command_argument_count()
    
    if (nargs == 0) then
      show_help = .true.
      return
    end if
    
    i = 1
    do while (i <= nargs)
      call get_command_argument(i, arg)
      
      if (expecting_cache_dir) then
        custom_cache_dir = trim(arg)
        expecting_cache_dir = .false.
      else if (expecting_config_dir) then
        custom_config_dir = trim(arg)
        expecting_config_dir = .false.
      else if (expecting_jobs) then
        read(arg, *, iostat=iostat) parallel_jobs
        if (iostat /= 0 .or. parallel_jobs < 1) then
          print '(a)', 'Error: Invalid number of jobs. Must be a positive integer.'
          stop 1
        end if
        expecting_jobs = .false.
      else if (arg == '--help' .or. arg == '-h') then
        show_help = .true.
        return
      else if (arg == '-v') then
        verbose_level = 1
      else if (arg == '-vv') then
        verbose_level = 2
      else if (arg == '--verbose') then
        ! Check if next argument is a number (1 or 2)
        if (i < nargs) then
          call get_command_argument(i+1, arg)
          if (arg == '1') then
            verbose_level = 1
            i = i + 1  ! Skip the next argument since we consumed it
          else if (arg == '2') then
            verbose_level = 2
            i = i + 1  ! Skip the next argument since we consumed it
          else
            ! Next arg is not a number, default to level 1
            verbose_level = 1
          end if
        else
          ! No more arguments, default to level 1
          verbose_level = 1
        end if
      else if (arg == '--cache-dir') then
        expecting_cache_dir = .true.
      else if (arg == '--config-dir') then
        expecting_config_dir = .true.
      else if (arg == '-j' .or. arg == '--jobs') then
        expecting_jobs = .true.
      else if (arg(1:1) /= '-') then
        ! Not a flag, must be filename
        if (.not. filename_found) then
          filename = trim(arg)
          filename_found = .true.
        end if
      end if
      
      i = i + 1
    end do
    
    if (expecting_cache_dir) then
      print '(a)', 'Error: --cache-dir requires an argument'
      show_help = .true.
    end if
    
    if (expecting_config_dir) then
      print '(a)', 'Error: --config-dir requires an argument'
      show_help = .true.
    end if
    
    if (expecting_jobs) then
      print '(a)', 'Error: -j/--jobs requires a number'
      show_help = .true.
    end if
    
    
    if (.not. filename_found) then
      show_help = .true.
    end if
    
  end subroutine parse_arguments
  
end module cli