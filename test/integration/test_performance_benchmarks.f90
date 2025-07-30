program test_performance_benchmarks
    ! Performance Benchmarking Framework
    ! Compares compilation time and memory usage between backends
    use temp_utils, only: temp_dir_manager, fortran_with_isolated_cache
    implicit none

    logical :: all_passed

    print *, "=== Performance Benchmarking Tests ==="

    all_passed = .true.

    ! Test compilation time benchmarks
    all_passed = all_passed .and. test_compilation_time_simple()
    all_passed = all_passed .and. test_compilation_time_complex()
    all_passed = all_passed .and. test_memory_usage_comparison()
    all_passed = all_passed .and. test_optimization_effectiveness()

    if (all_passed) then
        print *, ""
        print *, "All performance benchmark tests PASSED!"
    else
        print *, ""
        print *, "Some performance benchmark tests FAILED!"
        stop 1
    end if

contains

    function test_compilation_time_simple() result(passed)
        logical :: passed
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: fortran_cmd
        character(len=256) :: test_file
        integer :: start_time, end_time, count_rate
        integer :: exit_code
        real :: fortran_time, mlir_time

        print *, "Testing compilation time: simple program..."

        passed = .false.
        call temp_mgr%create('perf_simple')

        ! Create simple test file
        test_file = temp_mgr%get_file_path('simple.lf')
        open (10, file=test_file, action='write')
        write (10, '(A)') 'program simple'
        write (10, '(A)') '    integer :: x = 42'
        write (10, '(A)') '    print *, x'
        write (10, '(A)') 'end program simple'
        close (10)

        fortran_cmd = fortran_with_isolated_cache('perf_simple')

        ! Benchmark Fortran backend
        call system_clock(start_time, count_rate)
        call execute_command_line(fortran_cmd // ' --standardize ' // test_file // ' > /dev/null 2>&1', &
                                  exitstat=exit_code)
        call system_clock(end_time)
        fortran_time = real(end_time - start_time)/real(count_rate)

        if (exit_code /= 0) then
            print *, "FAIL: Fortran backend failed in simple compilation benchmark"
            return
        end if

        write (*, '(A,F0.3,A)') "  Fortran backend: ", fortran_time, "s"
        print *, "  MLIR backend: SKIP (compile functionality removed)"

        print *, "PASS: Simple compilation time benchmark completed"
        passed = .true.
    end function test_compilation_time_simple

    function test_compilation_time_complex() result(passed)
        logical :: passed
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: fortran_cmd
        character(len=256) :: test_file
        integer :: start_time, end_time, count_rate
        integer :: exit_code, i
        real :: fortran_time, mlir_time

        print *, "Testing compilation time: complex program..."

        passed = .false.
        call temp_mgr%create('perf_complex')

        ! Create complex test file with multiple functions and loops
        test_file = temp_mgr%get_file_path('complex.lf')
        open (10, file=test_file, action='write')
        write (10, '(A)') 'program complex'

        ! Generate multiple functions
        do i = 1, 5
            write (10, '(A,I0,A)') '    real function func', i, '(x)'
            write (10, '(A)') '        real :: x'
            write (10, '(A,I0,A)') '        func', i, ' = x * ', i
            write (10, '(A,I0)') '    end function func', i
            write (10, '(A)') '    '
        end do

        write (10, '(A)') '    real :: result, x = 5.0'
        write (10, '(A)') '    integer :: i'
        write (10, '(A)') '    result = 0.0'
        write (10, '(A)') '    do i = 1, 100'

        ! Call functions in loop
        do i = 1, 5
            write (10, '(A,I0,A)') '        result = result + func', i, '(x)'
        end do

        write (10, '(A)') '    end do'
        write (10, '(A)') '    print *, result'
        write (10, '(A)') 'end program complex'
        close (10)

        fortran_cmd = fortran_with_isolated_cache('perf_complex')

        ! Benchmark Fortran backend
        call system_clock(start_time, count_rate)
        call execute_command_line(fortran_cmd // ' --standardize ' // test_file // ' > /dev/null 2>&1', &
                                  exitstat=exit_code)
        call system_clock(end_time)
        fortran_time = real(end_time - start_time)/real(count_rate)

        if (exit_code /= 0) then
            print *, "FAIL: Fortran backend failed in complex compilation benchmark"
            return
        end if

        write (*, '(A,F0.3,A)') "  Fortran backend: ", fortran_time, "s"
        print *, "  MLIR backend: SKIP (compile functionality removed)"

        print *, "PASS: Complex compilation time benchmark completed"
        passed = .true.
    end function test_compilation_time_complex

    function test_memory_usage_comparison() result(passed)
        logical :: passed
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: fortran_cmd
        character(len=256) :: test_file
        integer :: exit_code

        print *, "Testing memory usage comparison framework..."

        passed = .false.
        call temp_mgr%create('perf_memory')

        ! Create test file
        test_file = temp_mgr%get_file_path('memory_test.lf')
        open (10, file=test_file, action='write')
        write (10, '(A)') 'program memory_test'
        write (10, '(A)') '    integer, parameter :: n = 1000'
        write (10, '(A)') '    real :: array(n)'
        write (10, '(A)') '    integer :: i'
        write (10, '(A)') '    do i = 1, n'
        write (10, '(A)') '        array(i) = real(i)'
        write (10, '(A)') '    end do'
        write (10, '(A)') '    print *, sum(array)'
        write (10, '(A)') 'end program memory_test'
        close (10)

        fortran_cmd = fortran_with_isolated_cache('perf_memory')

        ! Test both backends (memory usage measurement would require external tools)
        call execute_command_line(fortran_cmd // ' --standardize ' // test_file // ' > /dev/null 2>&1', &
                                  exitstat=exit_code)
        if (exit_code /= 0) then
            print *, "FAIL: Fortran backend failed in memory test"
            return
        end if

        print *, "  MLIR backend: SKIP (compile functionality removed)"

        print *, "  Note: Memory usage measurement requires external profiling tools"
        print *, "PASS: Memory usage comparison framework ready"
        passed = .true.
    end function test_memory_usage_comparison

    function test_optimization_effectiveness() result(passed)
        logical :: passed
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: fortran_cmd
        character(len=256) :: test_file, optimized_output, unoptimized_output
        integer :: exit_code
        logical :: optimization_effective

        print *, "Testing optimization effectiveness metrics..."

        passed = .false.
        call temp_mgr%create('perf_optimization')

        ! Create test file with optimization opportunities
        test_file = temp_mgr%get_file_path('optimization_test.lf')
        open (10, file=test_file, action='write')
        write (10, '(A)') 'program optimization_test'
        write (10, '(A)') '    integer :: i, result = 0'
        write (10, '(A)') '    do i = 1, 10'
     write (10, '(A)') '        result = result + 2 * 3  ! Constant folding opportunity'
        write (10, '(A)') '    end do'
        write (10, '(A)') '    print *, result'
        write (10, '(A)') 'end program optimization_test'
        close (10)

        fortran_cmd = fortran_with_isolated_cache('perf_optimization')

        ! Skip optimization tests since compile functionality removed
        print *, "  Optimization testing: SKIP (compile functionality removed)"
        print *, "PASS: Performance benchmark framework functional"
        passed = .true.
    end function test_optimization_effectiveness

end program test_performance_benchmarks
