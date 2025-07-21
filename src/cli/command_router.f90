module command_router
    use test_cli, only: handle_test_command, print_test_help
    implicit none
    private

    public :: route_command

contains

    subroutine route_command(exit_code)
        integer, intent(out) :: exit_code

        integer :: nargs, i
        character(len=256) :: arg
        character(len=256), allocatable :: test_args(:)
        integer :: test_argc
        logical :: test_mode

        exit_code = 0
        test_mode = .false.

        ! Check command line arguments for --test
        nargs = command_argument_count()

        if (nargs > 0) then
            call get_command_argument(1, arg)
            if (trim(arg) == '--test') then
                test_mode = .true.

                ! Collect remaining arguments for test command
                test_argc = nargs - 1
                if (test_argc > 0) then
                    allocate (test_args(test_argc))
                    do i = 1, test_argc
                        call get_command_argument(i + 1, test_args(i))
                    end do
                    call handle_test_command(test_args, exit_code)
                    deallocate (test_args)
                else
                    ! No additional arguments, just run all tests
                    allocate (test_args(0))
                    call handle_test_command(test_args, exit_code)
                    deallocate (test_args)
                end if
                return
            end if
        end if

        ! Not test mode, delegate to original main logic
        call run_original_main(exit_code)
    end subroutine route_command

    subroutine run_original_main(exit_code)
        integer, intent(out) :: exit_code

        ! This will contain the original main program logic
        ! For now, we'll call the existing main logic
        ! This is a placeholder - we'd need to refactor the existing main.f90

        write (*, '(A)') "Original fortran functionality would run here"
        write (*, '(A)') "Use 'fortran --test' to run tests"
        exit_code = 0
    end subroutine run_original_main

end module command_router
