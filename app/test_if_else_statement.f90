program test_if_else_statement
    use frontend, only: compile_source, compilation_options_t, BACKEND_FORTRAN
    implicit none

    character(len=:), allocatable :: source
    character(len=256) :: output_file, error_msg
    type(compilation_options_t) :: options
    integer :: unit

    ! If-else block followed by statement (like in control_flow_simple.f)
    source = &
        "x = 1"//new_line('a')// &
        "if (x < 0) then"//new_line('a')// &
        "    print *, 'negative'"//new_line('a')// &
        "else if (x > 0) then"//new_line('a')// &
        "    print *, 'positive'"//new_line('a')// &
        "else"//new_line('a')// &
        "    print *, 'zero'"//new_line('a')// &
        "end if"//new_line('a')// &
        "print *, 'after if-else block'"

    ! Write to file
    open (newunit=unit, file="/tmp/if_else.f", status='replace')
    write (unit, '(A)') source
    close (unit)

    output_file = "/tmp/if_else_out.f90"
    options%backend = BACKEND_FORTRAN
    options%output_file = output_file

    print *, "Testing if-else-endif followed by statement:"
    call compile_source("/tmp/if_else.f", options, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "ERROR:", trim(error_msg)
    else
        print *, "SUCCESS!"
        call execute_command_line("cat "//trim(output_file))
    end if

end program test_if_else_statement
