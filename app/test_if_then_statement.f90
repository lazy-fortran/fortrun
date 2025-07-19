program test_if_then_statement
    use frontend, only: compile_source, compilation_options_t, BACKEND_FORTRAN
    implicit none

    character(len=:), allocatable :: source
    character(len=256) :: output_file, error_msg
    type(compilation_options_t) :: options
    integer :: unit

    ! If block followed by statement
    source = &
        "x = 1"//new_line('a')// &
        "if (x > 0) then"//new_line('a')// &
        "    print *, 'positive'"//new_line('a')// &
        "end if"//new_line('a')// &
        "print *, 'after if block'"

    ! Write to file
    open (newunit=unit, file="/tmp/if_then.f", status='replace')
    write (unit, '(A)') source
    close (unit)

    output_file = "/tmp/if_then_out.f90"
    options%backend = BACKEND_FORTRAN
    options%output_file = output_file

    print *, "Testing if-then followed by statement:"
    call compile_source("/tmp/if_then.f", options, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "ERROR:", trim(error_msg)
    else
        print *, "SUCCESS!"
        call execute_command_line("cat "//trim(output_file))
    end if

end program test_if_then_statement
