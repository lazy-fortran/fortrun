module test_execution
    use iso_fortran_env, only: error_unit
    use omp_lib
    implicit none
    private

    public :: test_result_t, run_single_test, TEST_PENDING, TEST_RUNNING, TEST_PASSED, TEST_FAILED

    integer, parameter :: MAX_PATH_LEN = 512
    integer, parameter :: MAX_OUTPUT_LEN = 50000

    ! Test status codes
    integer, parameter :: TEST_PENDING = 0
    integer, parameter :: TEST_RUNNING = 1
    integer, parameter :: TEST_PASSED = 2
    integer, parameter :: TEST_FAILED = 3

    type :: test_result_t
        character(len=MAX_PATH_LEN) :: name = ""
        character(len=MAX_PATH_LEN) :: executable = ""
        integer :: status = TEST_PENDING
        character(len=MAX_OUTPUT_LEN) :: output = ""
        integer :: exit_code = 0
        real :: duration = 0.0
    end type test_result_t

contains

    subroutine run_single_test(test_executable, result)
        character(len=*), intent(in) :: test_executable
        type(test_result_t), intent(out) :: result

        character(len=1024) :: command
        character(len=MAX_OUTPUT_LEN) :: output_buffer
        integer :: unit, ios, idx
        real :: test_start, test_end

        ! Initialize result - extract name from executable path
        idx = index(test_executable, '/', back=.true.)
        if (idx > 0) then
            result%name = test_executable(idx + 1:)
        else
            result%name = trim(test_executable)
        end if
        result%executable = trim(test_executable)
        result%status = TEST_RUNNING
        result%output = ""
        result%exit_code = 0
        result%duration = 0.0

        test_start = omp_get_wtime()

        ! Create command to run test executable directly
        command = "timeout 60 "//trim(test_executable)//" 2>&1"

        ! Run test and capture output
        open (newunit=unit, file="/tmp/fortran_test_output.txt", status="replace")
        call execute_command_line(trim(command) // " > /tmp/fortran_test_output.txt", exitstat=result%exit_code)
        close (unit)

        ! Read output
        output_buffer = ""
      open (newunit=unit, file="/tmp/fortran_test_output.txt", status="old", iostat=ios)
        if (ios == 0) then
            read (unit, '(A)', iostat=ios) output_buffer
            close (unit)
        end if
        call execute_command_line("rm -f /tmp/fortran_test_output.txt")

        test_end = omp_get_wtime()
        result%duration = test_end - test_start
        result%output = trim(output_buffer)

        ! Handle timeout (exit code 124)
        if (result%exit_code == 124) then
            result%output = "Test timed out after 60 seconds"
            result%exit_code = 1
        end if

        ! Set final status
        if (result%exit_code == 0) then
            result%status = TEST_PASSED
        else
            result%status = TEST_FAILED
        end if
    end subroutine run_single_test

end module test_execution
