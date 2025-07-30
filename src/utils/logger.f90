module logger
    ! Centralized logging and debug output system
    ! Integrates with existing CLI verbose levels: 0=quiet, 1=info, 2=debug, 3=verbose

    implicit none
    private

    ! Standard log levels
    integer, parameter :: LEVEL_ERROR = 0    ! Critical errors
    integer, parameter :: LEVEL_WARN = 1     ! Warnings
    integer, parameter :: LEVEL_INFO = 2     ! General information
    integer, parameter :: LEVEL_DEBUG = 3    ! Debug information

    ! Verbose level constants
    integer, parameter :: VERBOSE_QUIET = 0      ! Errors only
    integer, parameter :: VERBOSE_INFO = 1       ! -v
    integer, parameter :: VERBOSE_DEBUG = 2      ! -vv
    integer, parameter :: VERBOSE_ULTRA = 3      ! -vvv

    ! Public interface
    public :: set_verbose_level, get_current_log_level, should_log, &
              log_debug, log_info, log_warn, log_error, log_verbose, &
              is_debug_category_enabled

    ! Current verbose level (maps to CLI verbose_level)
    integer :: current_verbose_level = 0

    ! Debug categories for fine-grained control
    logical :: debug_parsing = .false.
    logical :: debug_lexing = .false.
    logical :: debug_semantic = .false.
    logical :: debug_codegen = .false.
    logical :: debug_frontend = .false.

contains

    ! Set verbose level (integrates with CLI verbose_level)
    ! CLI mapping: 0=quiet(errors only), 1=info(-v), 2=debug(-vv), 3=verbose(-vvv)
    subroutine set_verbose_level(level)
        integer, intent(in) :: level
        current_verbose_level = level

        ! Enable debug categories based on verbose level
        if (level >= VERBOSE_ULTRA) then  ! -vvv: ultra verbose debug
            debug_parsing = .true.
            debug_lexing = .true.
            debug_semantic = .true.
            debug_codegen = .true.
            debug_frontend = .true.
        else if (level >= VERBOSE_DEBUG) then  ! -vv: debug level
            debug_parsing = .true.
            debug_frontend = .true.
            debug_semantic = .false.
            debug_lexing = .false.
            debug_codegen = .false.
        else
            debug_parsing = .false.
            debug_lexing = .false.
            debug_semantic = .false.
            debug_codegen = .false.
            debug_frontend = .false.
        end if
    end subroutine set_verbose_level

    ! Get current log level (for testing)
    integer function get_current_log_level()
        get_current_log_level = current_verbose_level
    end function get_current_log_level

    ! Check if a message should be logged at given level
    logical function should_log(level)
        integer, intent(in) :: level
        should_log = (current_verbose_level >= level)
    end function should_log

    ! Check if debug category is enabled
    logical function is_debug_category_enabled(category)
        character(len=*), intent(in) :: category

        select case (trim(category))
        case ("parsing")
            is_debug_category_enabled = debug_parsing
        case ("lexing")
            is_debug_category_enabled = debug_lexing
        case ("semantic")
            is_debug_category_enabled = debug_semantic
        case ("codegen")
            is_debug_category_enabled = debug_codegen
        case ("frontend")
            is_debug_category_enabled = debug_frontend
        case default
            is_debug_category_enabled = .false.
        end select
    end function is_debug_category_enabled

    ! Log error message (always shown - level 0)
    subroutine log_error(message)
        character(len=*), intent(in) :: message
        if (current_verbose_level >= LEVEL_ERROR) then
            write (*, '(A, A)') 'ERROR: ', message
        end if
    end subroutine log_error

    ! Log warning message (level 1)
    subroutine log_warn(message)
        character(len=*), intent(in) :: message
        if (current_verbose_level >= LEVEL_WARN) then
            write (*, '(A, A)') 'WARN: ', message
        end if
    end subroutine log_warn

    ! Log info message (-v, level 2)
    subroutine log_info(message)
        character(len=*), intent(in) :: message
        if (current_verbose_level >= LEVEL_INFO) then
            write (*, '(A)') message
        end if
    end subroutine log_info

    ! Log debug message (-vv, level 3)
    subroutine log_debug(category, message)
        character(len=*), intent(in) :: category, message
        if (current_verbose_level >= LEVEL_DEBUG) then
            write (*, '(A, A, A, A)') 'DEBUG[', category, ']: ', message
        end if
    end subroutine log_debug

    ! Log verbose debug message (-vvv, level 3+)
    subroutine log_verbose(category, message)
        character(len=*), intent(in) :: category, message
        if (current_verbose_level >= VERBOSE_ULTRA) then
            write (*, '(A, A, A, A)') 'VERBOSE[', category, ']: ', message
        end if
    end subroutine log_verbose

end module logger
