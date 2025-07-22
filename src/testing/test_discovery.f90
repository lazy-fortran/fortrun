module test_discovery
    use iso_fortran_env, only: error_unit
    use temp_utils, only: get_system_temp_dir, get_temp_file_path
    use fpm_environment, only: get_os_type, OS_WINDOWS
    use system_utils, only: sys_remove_file, escape_shell_arg
    implicit none
    private

    public :: discover_fpm_tests, test_case_t

    integer, parameter :: MAX_TESTS = 200
    integer, parameter :: MAX_PATH_LEN = 512

    type :: test_case_t
        character(len=MAX_PATH_LEN) :: name = ""
        character(len=MAX_PATH_LEN) :: executable = ""
    end type test_case_t

contains

    subroutine discover_fpm_tests(tests, num_tests, filter, quiet, success)
        type(test_case_t), intent(out) :: tests(:)
        integer, intent(out) :: num_tests
        character(len=*), intent(in) :: filter
        logical, intent(in) :: quiet
        logical, intent(out) :: success

        character(len=1024) :: command, line
        character(len=512) :: test_list_file
        integer :: unit, ios, idx

        success = .false.
        num_tests = 0

        ! Build tests first
        if (.not. quiet) write (*, '(A)') "Building tests..."
        if (get_os_type() == OS_WINDOWS) then
            command = "fpm build --tests > nul 2>&1"
        else
            command = "fpm build --tests > /dev/null 2>&1"
        end if
        call execute_command_line(trim(command), exitstat=ios)
        if (ios /= 0) then
            if (.not. quiet) write (error_unit, '(A)') "ERROR: Failed to build tests"
            return
        end if

        ! Discover test executables using fpm test --runner echo
        if (.not. quiet) write (*, '(A)') "Discovering tests..."

     test_list_file = get_temp_file_path(get_system_temp_dir(), "fortran_test_list.txt")

        open (newunit=unit, file=test_list_file, status="replace")
        if (get_os_type() == OS_WINDOWS) then
            command = "fpm test --runner echo 2>nul | findstr /B build/ | sort > "// &
                      trim(escape_shell_arg(test_list_file))
        else
            command = "fpm test --runner echo 2>/dev/null | grep -E '^build/' | sort | uniq > "// &
                      trim(escape_shell_arg(test_list_file))
        end if
        call execute_command_line(trim(command))
        close (unit)

        ! Read test list
        open (newunit=unit, file=test_list_file, status="old", iostat=ios)
        if (ios /= 0) then
            if (.not. quiet) write (error_unit, '(A)') "ERROR: Failed to read test list"
            return
        end if

        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            line = trim(line)
            if (len_trim(line) == 0) cycle

            ! Extract test name from path
            idx = index(line, '/', back=.true.)
            if (idx > 0) then
                tests(num_tests + 1)%name = line(idx + 1:)
            else
                tests(num_tests + 1)%name = trim(line)
            end if
            tests(num_tests + 1)%executable = trim(line)

            ! Apply filter if specified
            if (len_trim(filter) > 0) then
                if (index(tests(num_tests + 1)%name, trim(filter)) == 0) cycle
            end if

            num_tests = num_tests + 1
            if (num_tests >= size(tests)) exit
        end do

        close (unit)
        call sys_remove_file(test_list_file)

        success = .true.
    end subroutine discover_fpm_tests

end module test_discovery
