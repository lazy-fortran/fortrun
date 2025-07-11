module cli
  implicit none
  private
  public :: parse_arguments
  
contains

  subroutine parse_arguments(filename, show_help)
    character(len=*), intent(out) :: filename
    logical, intent(out) :: show_help
    
    integer :: nargs
    character(len=256) :: arg
    
    filename = ''
    show_help = .false.
    
    nargs = command_argument_count()
    
    if (nargs == 0) then
      show_help = .true.
      return
    end if
    
    call get_command_argument(1, arg)
    
    if (arg == '--help' .or. arg == '-h') then
      show_help = .true.
      return
    end if
    
    filename = trim(arg)
    
  end subroutine parse_arguments
  
end module cli