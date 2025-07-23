program test_verbose
    use, intrinsic :: iso_fortran_env, only: error_unit
    use temp_utils, only: create_temp_dir, get_temp_file_path
    use system_utils, only: sys_remove_dir, sys_remove_file, escape_shell_arg
    implicit none

    integer :: exit_code
    character(len=8192) :: output
    character(len=256) :: test_program
    logical :: verbose_found, build_found
    character(len=256) :: github_env
    integer :: env_status
    logical :: is_ci
    
    ! Check if running on CI (check multiple CI environment variables)
    is_ci = .false.
    
    ! Check GITHUB_ACTIONS
    call get_environment_variable('GITHUB_ACTIONS', github_env, status=env_status)
    if (env_status == 0 .and. len_trim(github_env) > 0) is_ci = .true.
    
    ! Check generic CI variable
    if (.not. is_ci) then
        call get_environment_variable('CI', github_env, status=env_status)
        if (env_status == 0 .and. len_trim(github_env) > 0) is_ci = .true.
    end if
    
    ! Check CONTINUOUS_INTEGRATION
    if (.not. is_ci) then
        call get_environment_variable('CONTINUOUS_INTEGRATION', github_env, status=env_status)
        if (env_status == 0 .and. len_trim(github_env) > 0) is_ci = .true.
    end if
    
    if (is_ci) then
        print *, 'SKIP: Verbose tests on CI (fortran CLI shows debug output)'
        stop 0
    end if

    ! Create a simple test program
    test_program = 'test_verbose_hello.f'
    call create_test_program(test_program)

    ! Test 1: Default (quiet) mode - should not show FPM output
    print *, 'Test 1: Default quiet mode'
    call run_fortran(test_program, '', output, exit_code)
    if (exit_code /= 0) then
        write (error_unit, *) 'FAIL: Non-zero exit code in quiet mode'
        stop 1
    end if

    ! Check that user build output is suppressed (FPM CLI tool build is OK)
    ! Look for fortran CLI specific verbose output instead of generic FPM output
    verbose_found = index(output, 'Cache miss') > 0 .or. &
                   index(output, 'Cache hit') > 0 .or. &
                   index(output, '<INFO>') > 0
    if (verbose_found) then
        write (error_unit, *) 'FAIL: FPM output visible in quiet mode'
        stop 1
    end if
    print *, 'PASS: Quiet mode suppresses FPM output'

    ! Test 2: Verbose mode (-v) - should show FPM output
    print *, ''
    print *, 'Test 2: Verbose mode (-v)'
    call run_fortran(test_program, '-v', output, exit_code)
    if (exit_code /= 0) then
        write (error_unit, *) 'FAIL: Non-zero exit code in verbose mode'
        stop 1
    end if

    ! Look for fortran CLI verbose output (cache info or compilation details)
    verbose_found = index(output, 'Cache miss') > 0 .or. &
                   index(output, 'Cache hit') > 0 .or. &
                   index(output, '<INFO>') > 0 .or. &
                   index(output, 'Found') > 0
    if (.not. verbose_found) then
        write (error_unit, *) 'FAIL: FPM output not visible in verbose mode'
        stop 1
    end if
    print *, 'PASS: Verbose mode shows FPM output'

    ! Test 3: Very verbose mode (-vv) - should show detailed output
    print *, ''
    print *, 'Test 3: Very verbose mode (-vv)'
    call run_fortran(test_program, '-vv', output, exit_code)
    if (exit_code /= 0) then
        write (error_unit, *) 'FAIL: Non-zero exit code in very verbose mode'
        stop 1
    end if

    verbose_found = index(output, '<INFO>') > 0
    if (.not. verbose_found) then
        write (error_unit, *) 'FAIL: Detailed output not visible in very verbose mode'
        stop 1
    end if
    print *, 'PASS: Very verbose mode shows detailed output'

    ! Clean up
    call sys_remove_file(test_program)

    print *, ''
    print *, 'All verbose tests passed!'
    stop 0

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
        temp_cache = './test_verbose_cache'
        call sys_remove_dir(temp_cache)

        ! Create temp directory and file path
        temp_dir = create_temp_dir('fortran_test')
        temp_output_file = get_temp_file_path(temp_dir, 'test_output.tmp')

        ! Build command with custom cache
        command = 'fpm run fortran -- --cache-dir "'//trim(escape_shell_arg(temp_cache))// &
                  '" '//trim(flags)//' "'// &
                  trim(escape_shell_arg(filename))//'" > "'//trim(escape_shell_arg(temp_output_file))//'" 2>&1'

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

end program test_verbose
