module cache
  use, intrinsic :: iso_c_binding
  implicit none
  private
  public :: get_cache_dir, ensure_cache_dir, ensure_cache_structure, get_cache_subdir
  
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
  
  subroutine ensure_cache_structure(cache_dir, success)
    character(len=*), intent(in) :: cache_dir
    logical, intent(out) :: success
    character(len=512) :: command
    integer :: exitstat, cmdstat
    logical :: builds_ok, modules_ok, executables_ok, metadata_ok
    
    ! Create main cache directory first
    call ensure_cache_dir(cache_dir, success)
    if (.not. success) return
    
    ! Create subdirectories
    command = 'mkdir -p "' // trim(cache_dir) // '/builds"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    builds_ok = (cmdstat == 0 .and. exitstat == 0)
    
    command = 'mkdir -p "' // trim(cache_dir) // '/modules"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    modules_ok = (cmdstat == 0 .and. exitstat == 0)
    
    command = 'mkdir -p "' // trim(cache_dir) // '/executables"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    executables_ok = (cmdstat == 0 .and. exitstat == 0)
    
    command = 'mkdir -p "' // trim(cache_dir) // '/metadata"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    metadata_ok = (cmdstat == 0 .and. exitstat == 0)
    
    success = builds_ok .and. modules_ok .and. executables_ok .and. metadata_ok
    
  end subroutine ensure_cache_structure
  
  function get_cache_subdir(cache_dir, subdir_name) result(subdir_path)
    character(len=*), intent(in) :: cache_dir, subdir_name
    character(len=512) :: subdir_path
    
    subdir_path = trim(cache_dir) // '/' // trim(subdir_name)
    
  end function get_cache_subdir
  
end module cache