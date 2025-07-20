module formatter_utils
    use temp_utils, only: create_temp_dir, cleanup_temp_dir, get_temp_file_path
    implicit none
    private
    public :: format_fortran_code, format_fortran_file

contains

    ! Format Fortran code string using fprettify
    function format_fortran_code(code) result(formatted_code)
        character(len=*), intent(in) :: code
        character(len=:), allocatable :: formatted_code
        character(len=:), allocatable :: temp_dir, input_file, output_file
        integer :: unit, iostat
        character(len=512) :: line
        character(len=4096) :: command
        logical :: first_line

        ! Create temporary directory
        temp_dir = create_temp_dir('formatter')

        ! Write code to temporary file
        input_file = get_temp_file_path(temp_dir, 'input.f90')
        open (newunit=unit, file=input_file, status='replace', iostat=iostat)
        if (iostat /= 0) then
            formatted_code = code  ! Return original on error
            call cleanup_temp_dir(temp_dir)
            return
        end if
        write (unit, '(A)') code
        close (unit)

        ! Format using fprettify
        output_file = get_temp_file_path(temp_dir, 'output.f90')
        write(command, '(A)') 'fprettify "'//trim(input_file)//'" > "'//trim(output_file)//'" 2>/dev/null'
        call execute_command_line(trim(command), exitstat=iostat)

        if (iostat /= 0) then
            ! If fprettify fails, return original code
            formatted_code = code
            call cleanup_temp_dir(temp_dir)
            return
        end if

        ! Read formatted code
        formatted_code = ''
        first_line = .true.
        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                if (.not. first_line) formatted_code = formatted_code//new_line('a')
                formatted_code = formatted_code//trim(line)
                first_line = .false.
            end do
            close (unit)
        else
            formatted_code = code  ! Return original on error
        end if

        ! Clean up
        call cleanup_temp_dir(temp_dir)

    end function format_fortran_code

    ! Format a Fortran file in place using fprettify
    subroutine format_fortran_file(filename, success)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: success
        character(len=4096) :: command
        integer :: iostat

        ! Format file in place
        write (command, '(A)') 'fprettify "'//trim(filename)//'" 2>/dev/null'
        call execute_command_line(trim(command), exitstat=iostat)

        success = (iostat == 0)

    end subroutine format_fortran_file

end module formatter_utils
