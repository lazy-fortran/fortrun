module config
    use temp_utils, only: mkdir_p
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
            config_dir = trim(config_dir)//'/fortran'
        else
            ! Fallback to HOME directory
            call get_environment_variable('HOME', home_dir, status=status)

            if (status == 0) then
                ! Linux/macOS: ~/.config/fortran
                config_dir = trim(home_dir)//'/.config/fortran'
            else
                ! Windows fallback: try LOCALAPPDATA
                call get_environment_variable('LOCALAPPDATA', config_dir, status=status)
                if (status == 0) then
                    config_dir = trim(config_dir)//'/fortran/config'
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

        ! Create directory with parents (-p flag)
        call mkdir_p(trim(config_dir))

        ! Check if directory exists
        inquire (file=trim(config_dir)//'/.', exist=success)

    end subroutine ensure_config_dir

    function get_registry_path() result(registry_path)
        character(len=512) :: registry_path
        character(len=256) :: config_dir

        config_dir = get_config_dir()
        registry_path = trim(config_dir)//'/registry.toml'

    end function get_registry_path

end module config
