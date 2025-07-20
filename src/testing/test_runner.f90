module test_runner
    use iso_fortran_env, only: output_unit, error_unit
    use omp_lib
    use test_discovery, only: test_case_t, discover_fpm_tests
    use test_execution, only: test_result_t, run_single_test, &
                              TEST_PENDING, TEST_RUNNING, TEST_PASSED, TEST_FAILED
    implicit none
    private

    public :: test_options_t, run_parallel_tests

    type :: test_options_t
        logical :: verbose = .false.
        logical :: quiet = .false.
        character(len=256) :: filter = ""
        integer :: max_threads = 0  ! 0 = use all available
    end type test_options_t

contains

    subroutine run_parallel_tests(options, total_passed, total_failed, total_time)
        type(test_options_t), intent(in) :: options
        integer, intent(out) :: total_passed, total_failed
        real, intent(out) :: total_time

        integer, parameter :: MAX_TESTS = 200
        type(test_case_t), allocatable :: tests(:)
        type(test_result_t), allocatable :: results(:)
        integer :: num_tests, i
        logical :: success

        integer :: next_test_index = 1
        integer :: completed_tests = 0
        real :: start_time, end_time

        ! OpenMP locks
        integer(omp_lock_kind) :: queue_lock
        integer(omp_lock_kind) :: output_lock

        total_passed = 0
        total_failed = 0
        total_time = 0.0

        ! Allocate arrays
        allocate (tests(MAX_TESTS))
        allocate (results(MAX_TESTS))

        ! Discover tests
       call discover_fpm_tests(tests, num_tests, options%filter, options%quiet, success)
        if (.not. success .or. num_tests == 0) then
            if (num_tests == 0) write (error_unit, '(A)') "No tests found"
            return
        end if

        ! Set number of threads if specified
        if (options%max_threads > 0) then
            call omp_set_num_threads(options%max_threads)
        end if

        if (.not. options%quiet) then
            write (output_unit, '(A,I0,A,I0,A)') "Running ", num_tests, " tests on ", &
                omp_get_max_threads(), " threads"
        end if

        ! Initialize OpenMP locks
        call omp_init_lock(queue_lock)
        call omp_init_lock(output_lock)

        ! Initialize results
        do i = 1, num_tests
            results(i)%name = tests(i)%name
            results(i)%executable = tests(i)%executable
            results(i)%status = TEST_PENDING
        end do

        ! Start timing
        start_time = omp_get_wtime()

        ! Run tests in parallel
        !$omp parallel shared(tests, results, num_tests, next_test_index, completed_tests, &
        !$omp                 queue_lock, output_lock, options)
    call run_tests_worker(tests, results, num_tests, next_test_index, completed_tests, &
                              queue_lock, output_lock, options)
        !$omp end parallel

        end_time = omp_get_wtime()
        total_time = end_time - start_time

        ! Clean up locks
        call omp_destroy_lock(queue_lock)
        call omp_destroy_lock(output_lock)

        ! Display results
        call display_results(results, num_tests, options, total_passed, total_failed, total_time)

    contains

        subroutine run_tests_worker(tests, results, num_tests, next_test_index, completed_tests, &
                                    queue_lock, output_lock, options)
            type(test_case_t), intent(in) :: tests(:)
            type(test_result_t), intent(inout) :: results(:)
            integer, intent(in) :: num_tests
            integer, intent(inout) :: next_test_index, completed_tests
            integer(omp_lock_kind), intent(inout) :: queue_lock, output_lock
            type(test_options_t), intent(in) :: options

            integer :: test_idx, thread_id, i
            character(len=512) :: test_name

            thread_id = omp_get_thread_num()

            do
                ! Get next test from queue (thread-safe)
                call omp_set_lock(queue_lock)
                if (next_test_index > num_tests) then
                    call omp_unset_lock(queue_lock)
                    exit
                end if
                test_idx = next_test_index
                next_test_index = next_test_index + 1
                results(test_idx)%status = TEST_RUNNING
                call omp_unset_lock(queue_lock)

                test_name = tests(test_idx)%name

                ! Run the test
                call run_single_test(tests(test_idx)%executable, results(test_idx))

                ! Update completion counter and show progress (thread-safe)
                call omp_set_lock(output_lock)
                completed_tests = completed_tests + 1
                if (.not. options%quiet) then
                    write (output_unit, '(A)', advance='no') char(13)
                    write (output_unit, '(A,I0,A,I0,A)', advance='no') &
                        'Completed: ', completed_tests, '/', num_tests, ' tests'
                    call flush (output_unit)
                end if
                call omp_unset_lock(output_lock)
            end do
        end subroutine run_tests_worker

    end subroutine run_parallel_tests

    subroutine display_results(results, num_tests, options, total_passed, total_failed, total_time)
        type(test_result_t), intent(in) :: results(:)
        integer, intent(in) :: num_tests
        type(test_options_t), intent(in) :: options
        integer, intent(out) :: total_passed, total_failed
        real, intent(in) :: total_time

        integer :: i

        total_passed = 0
        total_failed = 0

        ! Count results
        do i = 1, num_tests
            if (results(i)%status == TEST_PASSED) then
                total_passed = total_passed + 1
            else if (results(i)%status == TEST_FAILED) then
                total_failed = total_failed + 1
            end if
        end do

        ! Clear progress line
        if (.not. options%quiet) then
            write (output_unit, '(A)') ""
        end if

        ! Show failed tests (always)
        if (total_failed > 0) then
            do i = 1, num_tests
                if (results(i)%status == TEST_FAILED) then
                    write (output_unit, '(A,A)') " ✗ FAIL: ", trim(results(i)%name)
                end if
            end do
        end if

        ! Show passed tests only in verbose mode
        if (options%verbose .and. total_passed > 0) then
            if (total_failed > 0) write (output_unit, '(A)') ""
            do i = 1, num_tests
                if (results(i)%status == TEST_PASSED) then
                    write (output_unit, '(A,A)') " ✓ PASS: ", trim(results(i)%name)
                end if
            end do
        end if

        ! Show failed test details
        if (total_failed > 0) then
            write (output_unit, '(A)') ""
            write (output_unit, '(A)') "=== Failed Test Details ==="
            do i = 1, num_tests
                if (results(i)%status == TEST_FAILED) then
            write (output_unit, '(A,A,A)') "=== Failed: ", trim(results(i)%name), " ==="
                    write (output_unit, '(A)') trim(results(i)%output)
                    write (output_unit, '(A)') ""
                end if
            end do
        end if

        ! Show verbose output
        if (options%verbose .and. total_passed > 0) then
            write (output_unit, '(A)') ""
            write (output_unit, '(A)') "=== Detailed Output ==="
            do i = 1, num_tests
        if (results(i)%status == TEST_PASSED .and. len_trim(results(i)%output) > 0) then
                    write (output_unit, '(A,A,A)') "=== ", trim(results(i)%name), " ==="
                    write (output_unit, '(A)') trim(results(i)%output)
                    write (output_unit, '(A)') ""
                end if
            end do
        end if

        ! Summary
        write (output_unit, '(A)') ""
        write (output_unit, '(A)') "=== Summary ==="
        write (output_unit, '(A,I0)') "Tests run: ", num_tests
        write (output_unit, '(A,I0)') "Passed: ", total_passed
        write (output_unit, '(A,I0)') "Failed: ", total_failed
        write (output_unit, '(A,F0.1,A)') "Time: ", total_time, "s"

        ! Final status
        write (output_unit, '(A)') ""
        if (total_failed == 0) then
            write (output_unit, '(A)') "All tests passed!"
        else
            write (output_unit, '(A,I0,A)') "FAILED: ", total_failed, " test(s) failed!"
        end if
    end subroutine display_results

end module test_runner
