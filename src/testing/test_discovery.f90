module test_discovery
    use iso_fortran_env, only: error_unit
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
        integer :: unit, ios, idx

        success = .false.
        num_tests = 0

        ! Build tests first
        if (.not. quiet) write (*, '(A)') "Building tests..."
        command = "fpm build --tests > /dev/null 2>&1"
        call execute_command_line(trim(command), exitstat=ios)
        if (ios /= 0) then
            if (.not. quiet) write (error_unit, '(A)') "ERROR: Failed to build tests"
            return
        end if

        ! Discover test executables using fpm test --runner echo
        if (.not. quiet) write (*, '(A)') "Discovering tests..."

        open (newunit=unit, file="/tmp/fortran_test_list.txt", status="replace")
        command = "fpm test --runner echo 2>/dev/null | grep -E '^build/' | sort | uniq > /tmp/fortran_test_list.txt"
        call execute_command_line(trim(command))
        close (unit)

        ! Read test list
        open (newunit=unit, file="/tmp/fortran_test_list.txt", status="old", iostat=ios)
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
        call execute_command_line("rm -f /tmp/fortran_test_list.txt")

        success = .true.
    end subroutine discover_fpm_tests

end module test_discovery
