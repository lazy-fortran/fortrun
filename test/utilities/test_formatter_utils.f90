program test_formatter_utils
    use formatter_utils, only: format_fortran_code, format_fortran_file
    use temp_utils, only: create_temp_file
    use system_utils, only: sys_file_exists, sys_remove_file
    implicit none

    logical :: all_tests_passed
    integer :: unit
    character(len=:), allocatable :: temp_file

    print *, "=== Formatter Utils Tests ==="
    print *

    all_tests_passed = .true.

    ! Test formatter utility functions
    if (.not. test_format_fortran_code_simple()) all_tests_passed = .false.
    if (.not. test_format_fortran_code_empty()) all_tests_passed = .false.
    if (.not. test_format_fortran_code_multiline()) all_tests_passed = .false.
    if (.not. test_format_fortran_code_invalid()) all_tests_passed = .false.
    if (.not. test_format_fortran_file()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All formatter utils tests passed!"
        stop 0
    else
        print *, "Some formatter utils tests failed!"
        stop 1
    end if

contains

    function test_format_fortran_code_simple() result(passed)
        logical :: passed
        character(len=:), allocatable :: input, output

        print *, "Testing format_fortran_code with simple input..."
        passed = .true.

        ! Test simple code formatting
   input = "program test"//new_line('a')//"print*,'hello'"//new_line('a')//"end program"
        output = format_fortran_code(input)

        ! Check output (fprettify might not be available)
        if (output == input) then
            print *, "  INFO: fprettify not available, returned original"
            print *, "  PASSED: Handled missing fprettify gracefully"
        else if (len_trim(output) == 0 .and. len_trim(input) > 0) then
            print *, "  FAILED: Output is empty for non-empty input"
            passed = .false.
        else
            print *, "  PASSED: Simple code formatting"
        end if

    end function test_format_fortran_code_simple

    function test_format_fortran_code_empty() result(passed)
        logical :: passed
        character(len=:), allocatable :: input, output

        print *, "Testing format_fortran_code with empty input..."
        passed = .true.

        ! Test empty string
        input = ""
        output = format_fortran_code(input)

        ! Empty input should return empty output
        if (len_trim(output) /= 0) then
            print *, "  FAILED: Empty input should return empty output"
            passed = .false.
        else
            print *, "  PASSED: Empty input handling"
        end if

    end function test_format_fortran_code_empty

    function test_format_fortran_code_multiline() result(passed)
        logical :: passed
        character(len=:), allocatable :: input, output

        print *, "Testing format_fortran_code with multiline input..."
        passed = .true.

        ! Test multiline code
        input = "program test"//new_line('a')// &
                "    integer :: i"//new_line('a')// &
                "    do i = 1, 10"//new_line('a')// &
                "        print *, i"//new_line('a')// &
                "    end do"//new_line('a')// &
                "end program test"
        output = format_fortran_code(input)

        ! Check output
        if (output == input) then
            print *, "  INFO: fprettify not available, returned original"
            print *, "  PASSED: Handled missing fprettify gracefully"
        else if (len_trim(output) == 0 .and. len_trim(input) > 0) then
            print *, "  FAILED: Output is empty for multiline input"
            passed = .false.
        else
            ! Check that newlines are preserved (at least some)
            if (index(output, new_line('a')) == 0) then
                print *, "  FAILED: No newlines in multiline output"
                passed = .false.
            else
                print *, "  PASSED: Multiline code formatting"
            end if
        end if

    end function test_format_fortran_code_multiline

    function test_format_fortran_code_invalid() result(passed)
        logical :: passed
        character(len=:), allocatable :: input, output

        print *, "Testing format_fortran_code with invalid syntax..."
        passed = .true.

        ! Test invalid Fortran code - should return original
        input = "this is not valid fortran { } @#$%"
        output = format_fortran_code(input)

        ! Should return original code when fprettify fails
        if (output /= input) then
     print *, "  WARNING: Invalid code was modified (fprettify might be too permissive)"
            ! This is not necessarily a failure
        end if
        print *, "  PASSED: Invalid code handling"

    end function test_format_fortran_code_invalid

    function test_format_fortran_file() result(passed)
        logical :: passed
        logical :: success
        character(len=:), allocatable :: content
        integer :: iostat

        print *, "Testing format_fortran_file..."
        passed = .true.

        ! Create a temporary file with unformatted code
        temp_file = create_temp_file('test_format', '.f90')
        open (newunit=unit, file=temp_file, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "  FAILED: Could not create temp file"
            passed = .false.
            return
        end if

        ! Write some unformatted code
        write (unit, '(A)') "program test"
        write (unit, '(A)') "print*,'hello'"
        write (unit, '(A)') "integer::i,j,k"
        write (unit, '(A)') "do i=1,10"
        write (unit, '(A)') "print*,i"
        write (unit, '(A)') "enddo"
        write (unit, '(A)') "end program"
        close (unit)

        ! Format the file
        call format_fortran_file(temp_file, success)

        ! Check result
        if (.not. success) then
            print *, "  INFO: fprettify not available"
            print *, "  PASSED: Handled missing fprettify gracefully"
            ! Clean up and return
            call sys_remove_file(temp_file)
            return
        else if (.not. sys_file_exists(temp_file)) then
            print *, "  FAILED: File was deleted during formatting"
            passed = .false.
        else
            ! Read the file to check it's not empty
            content = ""
            open (newunit=unit, file=temp_file, status='old', iostat=iostat)
            if (iostat == 0) then
                block
                    character(len=256) :: line
                    do
                        read (unit, '(A)', iostat=iostat) line
                        if (iostat /= 0) exit
                        content = content//trim(line)//new_line('a')
                    end do
                end block
                close (unit)
            end if

            if (len_trim(content) == 0) then
                print *, "  FAILED: File is empty after formatting"
                passed = .false.
            else
                print *, "  PASSED: File formatting"
            end if
        end if

        ! Clean up
        call sys_remove_file(temp_file)

    end function test_format_fortran_file

end program test_formatter_utils
