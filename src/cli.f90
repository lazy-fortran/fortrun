module cli
  implicit none
  private
  public :: parse_arguments
  
contains

  subroutine parse_arguments(filename, show_help, verbose_level, custom_cache_dir)
    character(len=*), intent(out) :: filename
    logical, intent(out) :: show_help
    integer, intent(out) :: verbose_level
    character(len=*), intent(out) :: custom_cache_dir
    
    integer :: nargs, i
    character(len=256) :: arg
    logical :: filename_found, expecting_cache_dir
    
    filename = ''
    show_help = .false.
    verbose_level = 0
    custom_cache_dir = ''
    filename_found = .false.
    expecting_cache_dir = .false.
    
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
      else if (arg == '--help' .or. arg == '-h') then
        show_help = .true.
        return
      else if (arg == '--verbose' .or. arg == '-v') then
        verbose_level = 1
      else if (arg == '-vv') then
        verbose_level = 2
      else if (arg == '--cache-dir') then
        expecting_cache_dir = .true.
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
    
    if (.not. filename_found) then
      show_help = .true.
    end if
    
  end subroutine parse_arguments
  
end module cli