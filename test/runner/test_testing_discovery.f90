program test_testing_discovery
    use test_discovery, only: discover_fpm_tests, test_case_t
    implicit none

    integer, parameter :: MAX_TESTS = 10
    type(test_case_t) :: tests(MAX_TESTS)
    integer :: num_tests
    logical :: success
    integer :: i

    ! Test basic discovery
    write (*, '(A)') "Testing FPM test discovery..."

    call discover_fpm_tests(tests, num_tests, "", .false., success)

    if (.not. success) then
        write (*, '(A)') "FAIL: Test discovery failed"
        stop 1
    end if

    write (*, '(A,I0,A)') "Found ", num_tests, " tests:"
    do i = 1, num_tests
     write (*, '(A,A,A,A)') "  ", trim(tests(i)%name), " -> ", trim(tests(i)%executable)
    end do

    ! Test filtering
    write (*, '(A)') "Testing frontend filter..."
    call discover_fpm_tests(tests, num_tests, "frontend", .false., success)

    if (.not. success) then
        write (*, '(A)') "FAIL: Filtered test discovery failed"
        stop 1
    end if

    write (*, '(A,I0,A)') "Found ", num_tests, " frontend tests"

    ! Verify all found tests contain "frontend"
    do i = 1, num_tests
        if (index(tests(i)%name, "frontend") == 0) then
 write (*, '(A,A,A)') "FAIL: Test ", trim(tests(i)%name), " does not contain 'frontend'"
            stop 1
        end if
    end do

    write (*, '(A)') "PASS: All tests passed"
end program test_testing_discovery
