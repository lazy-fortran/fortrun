program test_testing_execution
    use test_execution, only: test_result_t, run_single_test, TEST_PASSED, TEST_FAILED
    implicit none

    type(test_result_t) :: result
    character(len=512) :: test_executable

    write (*, '(A)') "Testing test execution..."

    ! Test with a simple command that should pass
    test_executable = "/bin/true"
    call run_single_test(test_executable, result)

    if (result%status /= TEST_PASSED) then
        write (*, '(A)') "FAIL: /bin/true should pass"
        stop 1
    end if

    if (result%exit_code /= 0) then
        write (*, '(A,I0)') "FAIL: Expected exit code 0, got ", result%exit_code
        stop 1
    end if

    write (*, '(A,F0.3,A)') "PASS: /bin/true passed in ", result%duration, "s"

    ! Test with a command that should fail
    test_executable = "/bin/false"
    call run_single_test(test_executable, result)

    if (result%status /= TEST_FAILED) then
        write (*, '(A)') "FAIL: /bin/false should fail"
        stop 1
    end if

    if (result%exit_code == 0) then
        write (*, '(A)') "FAIL: Expected non-zero exit code for /bin/false"
        stop 1
    end if

 write (*, '(A,F0.3,A)') "PASS: /bin/false failed as expected in ", result%duration, "s"

    ! Test name extraction
    if (trim(result%name) /= "false") then
       write (*, '(A,A,A)') "FAIL: Expected name 'false', got '", trim(result%name), "'"
        stop 1
    end if

    write (*, '(A)') "PASS: Name extraction works correctly"

    write (*, '(A)') "PASS: All execution tests passed"
end program test_testing_execution
