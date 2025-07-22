module config
    use fpm_filesystem, only: join_path
    use fpm_environment, only: get_os_type, OS_WINDOWS
    use system_utils, only: escape_shell_arg
    implicit none
    private
    public :: get_config_dir, ensure_config_dir, get_registry_path

contains

    function get_config_dir() result(config_dir)
        character(len=256) :: config_dir
        character(len=256) :: home_dir
        integer :: status

        ! Try to get XDG_CONFIG_HOME first (Linux standard)
        call get_environment_variable('XDG_CONFIG_HOME', config_dir, status=status)

        if (status == 0 .and. len_trim(config_dir) > 0) then
            config_dir = join_path(trim(config_dir), 'fortran')
        else
            ! Fallback to HOME directory
            call get_environment_variable('HOME', home_dir, status=status)

            if (status == 0) then
                ! Linux/macOS: ~/.config/fortran
                config_dir = join_path(trim(home_dir), '.config', 'fortran')
            else
                ! Windows fallback: try LOCALAPPDATA
                call get_environment_variable('LOCALAPPDATA', config_dir, status=status)
                if (status == 0) then
                    config_dir = join_path(trim(config_dir), 'fortran', 'config')
                else
                    ! Last resort - use current directory
                    config_dir = './.fortran-config'
                end if
            end if
        end if

    end function get_config_dir

    subroutine ensure_config_dir(config_dir, success)
        character(len=*), intent(in) :: config_dir
        logical, intent(out) :: success
        character(len=512) :: command
        integer :: exitstat, cmdstat

        ! Initialize success to false
        success = .false.

        ! Skip invalid or empty paths
        if (len_trim(config_dir) == 0) return
        if (index(config_dir, '/dev/null') > 0) return

        ! Try to create directory using safe command approach
        ! This avoids FPM's mkdir which calls fpm_stop on failure
        if (get_os_type() == OS_WINDOWS) then
            ! Windows: Need to create parent directories too
            ! First create parent, then target directory
            block
                character(len=512) :: parent_dir
                integer :: last_sep

                ! Find last path separator
                last_sep = max(index(config_dir, '\', back=.true.), &
                               index(config_dir, '/', back=.true.))

                if (last_sep > 0) then
                    parent_dir = config_dir(1:last_sep - 1)
                    ! Create parent directory first
                    command = 'cmd /C if not exist "'//trim(escape_shell_arg(parent_dir))// &
                              '" mkdir "'//trim(escape_shell_arg(parent_dir))//'"'
                  call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
                end if

                ! Now create target directory
                command = 'cmd /C if not exist "'//trim(escape_shell_arg(config_dir))// &
                          '" mkdir "'//trim(escape_shell_arg(config_dir))//'"'
            end block
        else
            command = 'mkdir -p "'//trim(escape_shell_arg(config_dir))//'" 2>/dev/null'
        end if

        call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)

        ! Check if directory exists
        inquire (file=join_path(trim(config_dir), '.'), exist=success)

    end subroutine ensure_config_dir

    function get_registry_path() result(registry_path)
        character(len=512) :: registry_path
        character(len=256) :: config_dir

        config_dir = get_config_dir()
        registry_path = join_path(trim(config_dir), 'registry.toml')

    end function get_registry_path

end module config
