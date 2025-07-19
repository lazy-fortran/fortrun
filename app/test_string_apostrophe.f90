program test_string_apostrophe
    use frontend, only: compile_source, compilation_options_t, BACKEND_FORTRAN
    implicit none

    character(len=:), allocatable :: source
    character(len=256) :: output_file, error_msg
    type(compilation_options_t) :: options
    integer :: unit

    ! Test string with apostrophe
    source = &
        "print *, 'It''s freezing!'"//new_line('a')// &
        "print *, ""It's cold"""

    ! Write to file
    open (newunit=unit, file="/tmp/string_test.f", status='replace')
    write (unit, '(A)') source
    close (unit)

    output_file = "/tmp/string_test_out.f90"
    options%backend = BACKEND_FORTRAN
    options%output_file = output_file

    print *, "Testing string with apostrophe:"
    call compile_source("/tmp/string_test.f", options, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "ERROR:", trim(error_msg)
    else
        print *, "SUCCESS!"
        call execute_command_line("cat "//trim(output_file))
    end if

end program test_string_apostrophe
