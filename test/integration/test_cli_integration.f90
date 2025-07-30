program test_cli_integration
    use runner, only: is_lazy_fortran_file
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    print *, "=== Fortrun CLI Integration Tests ==="
    print *, ""

    ! Test 1: File extension detection
    call test_extension_detection()

    ! Test 2: CLI subprocess execution (basic)
    call test_cli_subprocess()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All CLI integration tests passed!"
        stop 0
    else
        print *, "Some CLI integration tests failed!"
        stop 1
    end if

contains

    subroutine test_extension_detection()
        logical :: result

        call test_start("File extension detection")

        ! Test .lf files are detected as lazy fortran
        result = is_lazy_fortran_file("test.lf") .and. &
                 is_lazy_fortran_file("example.LF") .and. &
                 .not. is_lazy_fortran_file("standard.f90") .and. &
                 .not. is_lazy_fortran_file("standard.F90")

        call test_result(result)
        if (.not. result) then
            print *, "  Extension detection failed"
        end if
    end subroutine test_extension_detection

    subroutine test_cli_subprocess()
        character(len=1024) :: command
        integer :: exitstat, cmdstat
        logical :: success

        call test_start("CLI subprocess execution")

        ! Test that fortfront CLI can be executed
        command = 'echo "print *, ''test''" | fortfront > /dev/null 2>&1'

     call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat, wait=.true.)

        success = (cmdstat == 0 .and. exitstat == 0)

        call test_result(success)
        if (.not. success) then
            print *, "  CLI subprocess execution failed"
            print *, "  Command: ", trim(command)
            print *, "  cmdstat: ", cmdstat, " exitstat: ", exitstat
        end if
    end subroutine test_cli_subprocess

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write (*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_result(success)
        logical, intent(in) :: success
        if (success) then
            print *, " ... PASSED"
            pass_count = pass_count + 1
        else
            print *, " ... FAILED"
        end if
    end subroutine test_result

end program test_cli_integration
