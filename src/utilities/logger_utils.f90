module logger_utils
    !> Logger utilities with verbose level support
    !> Provides standardized logging at different verbosity levels
    implicit none
    private

    public :: debug_print, print_info, print_warning, print_error
    public :: set_logger_verbose_level, get_logger_verbose_level

    ! Module variable to store current verbose level
    integer, save :: current_verbose_level = 0

contains

    !> Set the global verbose level for the logger
    subroutine set_logger_verbose_level(level)
        integer, intent(in) :: level
        current_verbose_level = level
    end subroutine set_logger_verbose_level

    !> Get the current verbose level
    function get_logger_verbose_level() result(level)
        integer :: level
        level = current_verbose_level
    end function get_logger_verbose_level

    !> Print debug message (only if verbose level >= 2)
    subroutine debug_print(message)
        character(len=*), intent(in) :: message
        if (current_verbose_level >= 2) then
            print '(a,a)', ' DEBUG: ', trim(message)
        end if
    end subroutine debug_print

    !> Print info message (only if verbose level >= 1)
    subroutine print_info(message)
        character(len=*), intent(in) :: message
        if (current_verbose_level >= 1) then
            print '(a,a)', ' INFO: ', trim(message)
        end if
    end subroutine print_info

    !> Print warning message (always shown, verbose level >= 0)
    subroutine print_warning(message)
        character(len=*), intent(in) :: message
        print '(a,a)', ' WARNING: ', trim(message)
    end subroutine print_warning

    !> Print error message (always shown, verbose level >= 0)
    subroutine print_error(message)
        character(len=*), intent(in) :: message
        print '(a,a)', ' ERROR: ', trim(message)
    end subroutine print_error

end module logger_utils