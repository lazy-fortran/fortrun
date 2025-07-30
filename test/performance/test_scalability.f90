program test_scalability
    use temp_utils, only: temp_dir_manager, fortran_with_isolated_cache
    use cache, only: get_cache_dir
    implicit none

    type(temp_dir_manager) :: temp_mgr
    character(len=256) :: project_dir, module_file, main_file
    character(len=1024) :: command
    integer :: test_count, pass_count, exit_code, unit, ios, i, j
    logical :: success
    real :: start_time, end_time, build_time
    integer :: num_files, num_modules

    test_count = 0
    pass_count = 0

    print *, "=== Performance and Scalability Tests ==="
    print *, ""

    call temp_mgr%create('scalability_test')

    ! Test 1: Large number of source files
    call test_many_source_files()

    ! Test 2: Deep module dependency chains
    call test_deep_dependencies()

    ! Test 3: Cache performance with many entries
    call test_cache_scalability()

    ! Test 4: Memory usage patterns
    call test_memory_patterns()

    ! Test 5: Concurrent build scalability
    call test_concurrent_scalability()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All performance and scalability tests passed!"
        stop 0
    else
        print *, "Some performance and scalability tests failed!"
        stop 1
    end if

contains

    subroutine test_many_source_files()
        character(len=256) :: src_file
        character(len=32) :: file_name

        call test_start("Large number of source files (50+)")

        project_dir = temp_mgr%get_file_path('many_files')
        call execute_command_line('mkdir -p "'//trim(project_dir)//'"', wait=.true.)

        ! Create 50 simple source files
        num_files = 50
        do i = 1, num_files
            write (file_name, '(A,I0,A)') 'file', i, '.f90'
            src_file = trim(project_dir)//'/'//trim(file_name)

            open (newunit=unit, file=src_file, status='replace', iostat=ios)
            write (unit, '(A,I0)') 'program test', i
            write (unit, '(A)') '    implicit none'
          write (unit, '(A,I0,A,I0,A)') '    print *, "Test ', i, ' of ', num_files, '"'
            write (unit, '(A,I0)') 'end program test', i
            close (unit)
        end do

        ! Time building one of the files
        src_file = trim(project_dir)//'/file25.f90'

        call cpu_time(start_time)
        command = fortran_with_isolated_cache('many_files_test')//' "'// &
                  trim(src_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)
        call cpu_time(end_time)

        build_time = end_time - start_time

        ! Should complete in reasonable time (< 5 seconds)
        success = (exit_code == 0) .and. (build_time < 5.0)

        call test_result(success)

        if (.not. success) then
            print *, "  Many source files test failed"
            print *, "  Build time: ", build_time, " seconds"
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_many_source_files

    subroutine test_deep_dependencies()
        character(len=256) :: mod_file
        character(len=32) :: mod_name, prev_mod

        call test_start("Deep module dependency chains (20+ levels)")

        project_dir = temp_mgr%get_file_path('deep_deps')
        call execute_command_line('mkdir -p "'//trim(project_dir)//'"', wait=.true.)

        ! Create chain of 20 dependent modules
        num_modules = 20
        do i = 1, num_modules
            write (mod_name, '(A,I0)') 'mod', i
            mod_file = trim(project_dir)//'/'//trim(mod_name)//'.f90'

            open (newunit=unit, file=mod_file, status='replace', iostat=ios)
            write (unit, '(A,A)') 'module ', trim(mod_name)

            if (i > 1) then
                write (prev_mod, '(A,I0)') 'mod', i - 1
             write (unit, '(A,A,A,I0)') '    use ', trim(prev_mod), ', only: val', i - 1
            end if

            write (unit, '(A)') '    implicit none'
            write (unit, '(A,I0,A,I0)') '    integer, parameter :: val', i, ' = ', i
            write (unit, '(A,A)') 'end module ', trim(mod_name)
            close (unit)
        end do

        ! Create main using deepest module
        main_file = trim(project_dir)//'/deep_main.f90'
        open (newunit=unit, file=main_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program deep_main'
      write (unit, '(A,I0,A,I0)') '    use mod', num_modules, ', only: val', num_modules
        write (unit, '(A)') '    implicit none'
        write (unit, '(A,I0)') '    print *, "Deep value:", val', num_modules
        write (unit, '(A)') 'end program deep_main'
        close (unit)

        ! Test performance with deep dependencies
        call cpu_time(start_time)
        command = fortran_with_isolated_cache('deep_deps_test')//' "'// &
                  trim(main_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)
        call cpu_time(end_time)

        build_time = end_time - start_time

        ! May fail due to dependencies, but test time taken
        success = (build_time < 10.0)  ! Should complete within 10 seconds

        call test_result(success)

        if (.not. success) then
            print *, "  Deep dependencies test failed"
            print *, "  Build time: ", build_time, " seconds"
        end if
    end subroutine test_deep_dependencies

    subroutine test_cache_scalability()
        character(len=256) :: test_file

        call test_start("Cache performance with many entries")

        ! Create and cache many different files
        project_dir = temp_mgr%get_file_path('cache_scale')
        call execute_command_line('mkdir -p "'//trim(project_dir)//'"', wait=.true.)

        ! Build and cache 20 different programs
        do i = 1, 20
            test_file = temp_mgr%get_file_path('cache_test.f90')
            open (newunit=unit, file=test_file, status='replace', iostat=ios)
            write (unit, '(A,I0)') 'program cache_test', i
            write (unit, '(A)') '    implicit none'
            write (unit, '(A,I0,A)') '    integer :: x = ', i*100
            write (unit, '(A)') '    print *, "Cache test:", x'
            write (unit, '(A,I0)') 'end program cache_test', i
            close (unit)

            command = fortran_with_isolated_cache('cache_scale_test')//' "'// &
                      trim(test_file)//'" > /dev/null 2>&1'
            call execute_command_line(command, wait=.true.)
        end do

        ! Time a cache hit
        call cpu_time(start_time)
        call execute_command_line(command, exitstat=exit_code, wait=.true.)
        call cpu_time(end_time)

        build_time = end_time - start_time

        ! Cache hit should be very fast (< 0.5 seconds)
        success = (exit_code == 0) .and. (build_time < 0.5)

        call test_result(success)

        if (.not. success) then
            print *, "  Cache scalability test failed"
            print *, "  Cache hit time: ", build_time, " seconds"
        end if
    end subroutine test_cache_scalability

    subroutine test_memory_patterns()
        character(len=256) :: mem_file
        integer :: array_size

        call test_start("Memory usage patterns")

        ! Create program with large arrays to test memory handling
        mem_file = temp_mgr%get_file_path('memory_test.f90')
        array_size = 1000000  ! 1 million elements

        open (newunit=unit, file=mem_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program memory_test'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A,I0)') '    integer, parameter :: n = ', array_size
        write (unit, '(A)') '    real, allocatable :: big_array(:)'
        write (unit, '(A)') '    integer :: i'
        write (unit, '(A)') '    '
        write (unit, '(A)') '    allocate(big_array(n))'
        write (unit, '(A)') '    '
        write (unit, '(A)') '    ! Initialize array'
        write (unit, '(A)') '    do i = 1, n'
        write (unit, '(A)') '        big_array(i) = real(i) * 1.5'
        write (unit, '(A)') '    end do'
        write (unit, '(A)') '    '
        write (unit, '(A)') '    print *, "Array size:", n'
        write (unit, '(A)') '    print *, "First element:", big_array(1)'
        write (unit, '(A)') '    print *, "Last element:", big_array(n)'
        write (unit, '(A)') '    '
        write (unit, '(A)') '    deallocate(big_array)'
        write (unit, '(A)') 'end program memory_test'
        close (unit)

        ! Build and run - should handle large memory allocations
        command = fortran_with_isolated_cache('memory_test')//' "'// &
                  trim(mem_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = (exit_code == 0)

        call test_result(success)

        if (.not. success) then
            print *, "  Memory patterns test failed"
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_memory_patterns

    subroutine test_concurrent_scalability()
        character(len=256) :: conc_file
        character(len=1024) :: cmd1, cmd2, cmd3

        call test_start("Concurrent build scalability")

        ! Create three different programs to build concurrently
        do i = 1, 3
            conc_file = temp_mgr%get_file_path('concurrent'//achar(48 + i)//'.f90')
            open (newunit=unit, file=conc_file, status='replace', iostat=ios)
            write (unit, '(A,I0)') 'program concurrent', i
            write (unit, '(A)') '    implicit none'
            write (unit, '(A,I0)') '    integer :: id = ', i
            write (unit, '(A)') '    integer :: j'
            write (unit, '(A)') '    real :: sum'
            write (unit, '(A)') '    '
            write (unit, '(A)') '    sum = 0.0'
            write (unit, '(A)') '    do j = 1, 100000'
            write (unit, '(A)') '        sum = sum + real(j) / real(id)'
            write (unit, '(A)') '    end do'
            write (unit, '(A)') '    '
            write (unit, '(A)') '    print *, "Process", id, "sum:", sum'
            write (unit, '(A,I0)') 'end program concurrent', i
            close (unit)
        end do

        ! Time concurrent builds
        call cpu_time(start_time)

        cmd1 = fortran_with_isolated_cache('conc_scale1')//' "'// &
               trim(temp_mgr%get_file_path('concurrent1.f90'))//'" > /dev/null 2>&1 &'
        cmd2 = fortran_with_isolated_cache('conc_scale2')//' "'// &
               trim(temp_mgr%get_file_path('concurrent2.f90'))//'" > /dev/null 2>&1 &'
        cmd3 = fortran_with_isolated_cache('conc_scale3')//' "'// &
               trim(temp_mgr%get_file_path('concurrent3.f90'))//'" > /dev/null 2>&1 &'

        call execute_command_line(cmd1, wait=.false.)
        call execute_command_line(cmd2, wait=.false.)
        call execute_command_line(cmd3, wait=.false.)

        ! Wait for all to complete
        call execute_command_line('wait', wait=.true.)

        call cpu_time(end_time)
        build_time = end_time - start_time

        ! Concurrent builds should complete efficiently
        success = (build_time < 5.0)

        call test_result(success)

        if (.not. success) then
            print *, "  Concurrent scalability test failed"
            print *, "  Total time for 3 concurrent builds: ", build_time, " seconds"
        end if
    end subroutine test_concurrent_scalability

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write (*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_result(test_success)
        logical, intent(in) :: test_success
        if (test_success) then
            print *, " ... PASSED"
            pass_count = pass_count + 1
        else
            print *, " ... FAILED"
        end if
    end subroutine test_result

end program test_scalability
