module test_execution
    use iso_fortran_env, only: error_unit
    use omp_lib
    use temp_utils, only: get_system_temp_dir, get_temp_file_path
    use fpm_environment, only: get_os_type, OS_WINDOWS
    use system_utils, only: sys_remove_file
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
        character(len=:), allocatable :: temp_file
        integer :: thread_id

        ! Initialize result - extract name from executable path
        ! First, try extracting the first word (handles commands with arguments)
        idx = index(test_executable, ' ')
        if (idx > 0) then
            ! Command has arguments, extract first word
            result%name = test_executable(1:idx-1)
            ! Now check if the first word is actually a path
            idx = max(index(result%name, '/', back=.true.), &
                      index(result%name, '\', back=.true.))
            if (idx > 0) then
                ! First word is a path, extract just the filename
                result%name = result%name(idx + 1:)
            end if
        else
            ! No arguments, check if it's a path
            idx = max(index(test_executable, '/', back=.true.), &
                      index(test_executable, '\', back=.true.))
            if (idx > 0) then
                result%name = test_executable(idx + 1:)
            else
                result%name = trim(test_executable)
            end if
        end if
        result%executable = trim(test_executable)
        result%status = TEST_RUNNING
        result%output = ""
        result%exit_code = 0
        result%duration = 0.0

        test_start = omp_get_wtime()

        ! Create unique temp file for this thread
        thread_id = omp_get_thread_num()
        block
            character(len=32) :: temp_name
            write (temp_name, '(A,I0,A)') 'fortran_test_', thread_id, '.txt'
            temp_file = get_temp_file_path(get_system_temp_dir(), temp_name)
        end block

        ! Create command to run test executable directly
        if (get_os_type() == OS_WINDOWS) then
            command = 'cmd /c ""'//trim(test_executable)//'" > "'//trim(temp_file)//'" 2>&1"'
        else
            command = "timeout 60 "//trim(test_executable)//" > "//trim(temp_file)//" 2>&1"
        end if

        ! Run test and capture output
        call execute_command_line(trim(command), exitstat=result%exit_code)

        ! Read output
        output_buffer = ""
        open (newunit=unit, file=trim(temp_file), status="old", iostat=ios)
        if (ios == 0) then
            read (unit, '(A)', iostat=ios) output_buffer
            close (unit)
        end if
        call sys_remove_file(temp_file)

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
