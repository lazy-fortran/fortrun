module cache
  use, intrinsic :: iso_c_binding
  implicit none
  private
  public :: get_cache_dir, ensure_cache_dir
  
contains

  function get_cache_dir() result(cache_dir)
    character(len=256) :: cache_dir
    character(len=256) :: home_dir
    integer :: status
    
    ! Try to get XDG_CACHE_HOME first (Linux standard)
    call get_environment_variable('XDG_CACHE_HOME', cache_dir, status=status)
    
    if (status == 0 .and. len_trim(cache_dir) > 0) then
      cache_dir = trim(cache_dir) // '/fortran'
    else
      ! Fallback to HOME directory
      call get_environment_variable('HOME', home_dir, status=status)
      
      if (status == 0) then
        ! Linux/macOS: ~/.cache/fortran
        cache_dir = trim(home_dir) // '/.cache/fortran'
      else
        ! Windows fallback: try LOCALAPPDATA
        call get_environment_variable('LOCALAPPDATA', cache_dir, status=status)
        if (status == 0) then
          cache_dir = trim(cache_dir) // '/fortran/cache'
        else
          ! Last resort - use current directory
          cache_dir = './.fortran-cache'
        end if
      end if
    end if
    
  end function get_cache_dir
  
  subroutine ensure_cache_dir(cache_dir, success)
    character(len=*), intent(in) :: cache_dir
    logical, intent(out) :: success
    character(len=512) :: command
    integer :: exitstat, cmdstat
    
    ! Create directory with parents (-p flag)
    command = 'mkdir -p "' // trim(cache_dir) // '"'
    
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    
    success = (cmdstat == 0 .and. exitstat == 0)
    
  end subroutine ensure_cache_dir
  
end module cache