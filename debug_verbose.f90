program debug_verbose
    use temp_utils, only: create_temp_dir, get_temp_file_path
    use system_utils, only: sys_remove_dir, sys_remove_file, escape_shell_arg
    implicit none

    integer :: exit_code
    character(len=8192) :: output
    character(len=256) :: test_program

    ! Create a simple test program
    test_program = 'debug_test_hello.f'
    call create_test_program(test_program)

    ! Test: Default (quiet) mode - should not show FPM output
    print *, 'Debug: Testing quiet mode'
    call run_fortran(test_program, '', output, exit_code)
    
    print *, 'Exit code:', exit_code
    print *, 'Output length:', len_trim(output)
    print *, 'Output content: [', trim(output), ']'
    
    ! Check specific strings
    print *, 'Contains "Project compiled":', index(output, 'Project compiled') > 0
    print *, 'Contains ".f90.o":', index(output, '.f90.o') > 0
    print *, 'Contains "build/":', index(output, 'build/') > 0
    
    ! Clean up
    call sys_remove_file(test_program)

contains

    subroutine create_test_program(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') '! Simple test program for verbose test'
        write (unit, '(a)') 'print *, "Test output"'
        close (unit)
    end subroutine create_test_program

    subroutine run_fortran(filename, flags, output, exit_code)
        character(len=*), intent(in) :: filename, flags
        character(len=*), intent(out) :: output
        integer, intent(out) :: exit_code

        character(len=512) :: command
        character(len=256) :: temp_cache
        character(len=:), allocatable :: temp_dir, temp_output_file
        integer :: unit, iostat
        character(len=2048) :: line

        ! Use temporary cache to ensure fresh builds
        temp_cache = './debug_verbose_cache'
        call sys_remove_dir(temp_cache)

        ! Create temp directory and file path
        temp_dir = create_temp_dir('fortran_test')
        temp_output_file = get_temp_file_path(temp_dir, 'test_output.tmp')

        ! Build command with custom cache
        command = 'fpm run fortran -- --cache-dir "'//trim(escape_shell_arg(temp_cache))// &
                  '" '//trim(flags)//' "'// &
                  trim(escape_shell_arg(filename))//'" > "'//trim(escape_shell_arg(temp_output_file))//'" 2>&1'

        print *, 'Debug: Command = ', trim(command)

        ! Run command
        call execute_command_line(trim(command), exitstat=exit_code)

        ! Read output
        output = ''
        open (newunit=unit, file=temp_output_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                output = trim(output)//' '//trim(line)
            end do
            close (unit)
        end if

        ! Clean up
        call execute_command_line('rm -f "'//trim(escape_shell_arg(temp_output_file))//'"')
        call sys_remove_dir(temp_cache)

    end subroutine run_fortran

end program debug_verbose