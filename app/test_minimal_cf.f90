program test_minimal_cf
    use frontend, only: compile_source, compilation_options_t, BACKEND_FORTRAN
    implicit none

    character(len=:), allocatable :: source
    character(len=256) :: output_file, error_msg
    type(compilation_options_t) :: options
    integer :: unit

    ! Create a minimal version of control_flow_simple.f
    source = &
        "! control_flow_simple.f - Simplified control flow examples"//new_line('a')// &
        "! Initialize test variables"//new_line('a')// &
        "temperature = 25.5"//new_line('a')// &
        "grade = 85"//new_line('a')// &
   "! WORKAROUND: Basic type inference can't handle logical literals"//new_line('a')// &
        "is_passed = grade < 0  ! Will be false"//new_line('a')// &
        ""//new_line('a')// &
        "print *, 'Temperature:', temperature, '°C'"//new_line('a')// &
        "print *, 'Grade:', grade"//new_line('a')// &
        ""//new_line('a')// &
        "! Temperature classification"//new_line('a')// &
        "if (temperature < 0.0) then"//new_line('a')// &
        "    print *, 'It''s freezing!'"//new_line('a')// &
        "else if (temperature < 20.0) then"//new_line('a')// &
        "    print *, 'It''s cold'"//new_line('a')// &
        "else if (temperature < 30.0) then"//new_line('a')// &
        "    print *, 'It''s mild'"//new_line('a')// &
        "else"//new_line('a')// &
        "    print *, 'It''s hot!'"//new_line('a')// &
        "end if"//new_line('a')// &
        ""//new_line('a')// &
        "! Grade evaluation"//new_line('a')// &
        "if (grade >= 60) then"//new_line('a')// &
        "    is_passed = .true."//new_line('a')// &
        "    print *, 'Student passed with grade:', grade"//new_line('a')// &
        "else"//new_line('a')// &
        "    is_passed = .false."//new_line('a')// &
        "    print *, 'Student failed with grade:', grade"//new_line('a')// &
        "end if"//new_line('a')// &
        ""//new_line('a')// &
        "print *, 'Final status:', is_passed"

    ! Write to file
    open (newunit=unit, file="/tmp/minimal_cf.f", status='replace')
    write (unit, '(A)') source
    close (unit)

    output_file = "/tmp/minimal_cf_out.f90"
    options%backend = BACKEND_FORTRAN
    options%output_file = output_file

    print *, "Testing minimal control flow:"
    call compile_source("/tmp/minimal_cf.f", options, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "ERROR:", trim(error_msg)
    else
        print *, "SUCCESS!"
        call execute_command_line("cat "//trim(output_file)//" | nl -v 1")
    end if

end program test_minimal_cf
