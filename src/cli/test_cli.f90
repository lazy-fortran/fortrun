module test_cli
    use test_runner, only: test_options_t, run_parallel_tests
    implicit none
    private

    public :: handle_test_command, print_test_help

contains

    subroutine handle_test_command(args, exit_code)
        character(len=*), intent(in) :: args(:)
        integer, intent(out) :: exit_code

        type(test_options_t) :: options
        integer :: total_passed, total_failed
        real :: total_time
        integer :: i

        ! Initialize default options
        options%verbose = .false.
        options%quiet = .false.
        options%filter = ""
        options%max_threads = 0  ! Use all available

        exit_code = 0

        ! Parse test-specific arguments
        i = 1
        do while (i <= size(args))
            select case (trim(args(i)))
            case ('-v', '--verbose')
                options%verbose = .true.

            case ('-q', '--quiet')
                options%quiet = .true.

            case ('--filter')
                if (i < size(args)) then
                    i = i + 1
                    options%filter = trim(args(i))
                else
                    write (*, '(A)') "ERROR: --filter requires a pattern argument"
                    exit_code = 1
                    return
                end if

            case ('-j', '--jobs')
                if (i < size(args)) then
                    i = i + 1
                    read (args(i), *, iostat=exit_code) options%max_threads
                    if (exit_code /= 0) then
                        write (*, '(A)') "ERROR: --jobs requires a numeric argument"
                        exit_code = 1
                        return
                    end if
                    exit_code = 0
                else
                    write (*, '(A)') "ERROR: --jobs requires a numeric argument"
                    exit_code = 1
                    return
                end if

            case ('-h', '--help')
                call print_test_help()
                return

            case default
                ! Assume it's a filter pattern
                options%filter = trim(args(i))
            end select

            i = i + 1
        end do

        ! Run the tests
        call run_parallel_tests(options, total_passed, total_failed, total_time)

        ! Set exit code based on results
        if (total_failed > 0) then
            exit_code = 1
        end if
    end subroutine handle_test_command

    subroutine print_test_help()
        write (*, '(A)') "Usage: fortran --test [OPTIONS] [FILTER]"
        write (*, '(A)') ""
  write (*, '(A)') "Run project tests with parallel execution and smart load balancing."
        write (*, '(A)') ""
        write (*, '(A)') "Options:"
        write (*, '(A)') "  -v, --verbose     Show detailed output for all tests"
       write (*, '(A)') "  -q, --quiet       Minimal output (only failures and summary)"
        write (*, '(A)') "  --filter PATTERN  Run only tests matching the pattern"
        write (*, '(A)') "  -j, --jobs N      Use N threads (default: auto-detect)"
        write (*, '(A)') "  -h, --help        Show this help message"
        write (*, '(A)') ""
        write (*, '(A)') "Examples:"
        write (*, '(A)') "  fortran --test                    # Run all tests"
  write (*, '(A)') "  fortran --test frontend           # Run tests matching 'frontend'"
     write (*, '(A)') "  fortran --test --filter cache     # Run tests matching 'cache'"
 write (*, '(A)') "  fortran --test -v --jobs 4        # Verbose output using 4 threads"
write (*, '(A)') "  fortran --test -q                 # Quiet mode (only show failures)"
        write (*, '(A)') ""
        write (*, '(A)') "Features:"
        write (*, '(A)') "  • Parallel execution with dynamic work queue"
        write (*, '(A)') "  • Real-time progress reporting"
        write (*, '(A)') "  • Automatic test discovery using FPM API"
        write (*, '(A)') "  • Detailed failure diagnostics"
        write (*, '(A)') "  • Smart load balancing across CPU cores"
    end subroutine print_test_help

end module test_cli
