program test_full_workflow
    use temp_utils, only: temp_dir_manager, fortran_with_isolated_cache
    use system_utils, only: sys_copy_file
    use runner, only: is_lazy_fortran_file
    implicit none

    type(temp_dir_manager) :: temp_mgr
    character(len=256) :: main_file
    character(len=1024) :: command, output_file
    integer :: test_count, pass_count, exit_code, unit, ios
    logical :: success, file_exists

    test_count = 0
    pass_count = 0

    print *, "=== Full Workflow Integration Tests ==="
    print *, ""

    call temp_mgr%create('full_workflow_test')

    ! Test 1: Mixed file type project
    call test_mixed_file_project()

    ! Test 2: Module dependencies with caching
    call test_module_dependencies_workflow()

    ! Test 3: Multiple runs with modifications
    call test_incremental_builds()

    ! Test 4: Complex CLI combinations
    call test_cli_combinations()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All full workflow integration tests passed!"
        stop 0
    else
        print *, "Some full workflow integration tests failed!"
        stop 1
    end if

contains

    subroutine test_mixed_file_project()
        character(len=256) :: combined_file

        call test_start("Mixed file type project workflow")

        ! Create a single file with both module and main program
        combined_file = temp_mgr%get_file_path('mixed_project.f90')
        open (newunit=unit, file=combined_file, status='replace', iostat=ios)
        ! Write module first
        write (unit, '(A)') 'module math_utils'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    public :: add_numbers'
        write (unit, '(A)') 'contains'
        write (unit, '(A)') '    function add_numbers(a, b) result(sum)'
        write (unit, '(A)') '        integer, intent(in) :: a, b'
        write (unit, '(A)') '        integer :: sum'
        write (unit, '(A)') '        sum = a + b'
        write (unit, '(A)') '    end function add_numbers'
        write (unit, '(A)') 'end module math_utils'
        write (unit, '(A)') ''
        ! Write main program
        write (unit, '(A)') 'program mixed_project_main'
        write (unit, '(A)') '    use math_utils, only: add_numbers'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    integer :: result'
        write (unit, '(A)') '    result = add_numbers(5, 7)'
        write (unit, '(A)') '    print *, "Result:", result'
        write (unit, '(A)') 'end program mixed_project_main'
        close (unit)

        ! Run the combined program
        command = fortran_with_isolated_cache('mixed_workflow')//' "'// &
                  trim(combined_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = (exit_code == 0)

        call test_result(success)

        if (.not. success) then
            print *, "  Mixed file type project workflow failed"
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_mixed_file_project

    subroutine test_module_dependencies_workflow()
        character(len=256) :: combined_deps

        call test_start("Module dependencies workflow with caching")

        ! Create a single file with all modules and main program
        combined_deps = temp_mgr%get_file_path('deps_combined.f90')
        open (newunit=unit, file=combined_deps, status='replace', iostat=ios)
        ! Module A
        write (unit, '(A)') 'module constants'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    real, parameter :: PI = 3.14159'
        write (unit, '(A)') '    real, parameter :: E = 2.71828'
        write (unit, '(A)') 'end module constants'
        write (unit, '(A)') ''
        ! Module B that uses A
        write (unit, '(A)') 'module calculations'
        write (unit, '(A)') '    use constants, only: PI'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') 'contains'
        write (unit, '(A)') '    function circle_area(radius) result(area)'
        write (unit, '(A)') '        real, intent(in) :: radius'
        write (unit, '(A)') '        real :: area'
        write (unit, '(A)') '        area = PI * radius * radius'
        write (unit, '(A)') '    end function circle_area'
        write (unit, '(A)') 'end module calculations'
        write (unit, '(A)') ''
        ! Main that uses both
        write (unit, '(A)') 'program main_deps'
        write (unit, '(A)') '    use constants, only: E'
        write (unit, '(A)') '    use calculations, only: circle_area'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    real :: area'
        write (unit, '(A)') '    area = circle_area(2.0)'
        write (unit, '(A)') '    print *, "Circle area:", area'
        write (unit, '(A)') '    print *, "Euler number:", E'
        write (unit, '(A)') 'end program main_deps'
        close (unit)

        ! First run - builds everything
        command = fortran_with_isolated_cache('deps_workflow')//' "'// &
                  trim(combined_deps)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        if (exit_code /= 0) then
            success = .false.
        else
            ! Second run - should use cache
            call execute_command_line(command, exitstat=exit_code, wait=.true.)
            success = (exit_code == 0)
        end if

        call test_result(success)

        if (.not. success) then
            print *, "  Module dependencies workflow failed"
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_module_dependencies_workflow

    subroutine test_incremental_builds()
        character(len=256) :: source_file, modified_file

        call test_start("Incremental builds with modifications")

        ! Create initial source file
        source_file = temp_mgr%get_file_path('incremental.f90')
        open (newunit=unit, file=source_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program incremental_test'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    integer :: version = 1'
        write (unit, '(A)') '    print *, "Version:", version'
        write (unit, '(A)') 'end program incremental_test'
        close (unit)

        ! First build
        command = fortran_with_isolated_cache('incremental_test')//' "'// &
                  trim(source_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        if (exit_code /= 0) then
            success = .false.
            call test_result(success)
            return
        end if

        ! Modify the file
        open (newunit=unit, file=source_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program incremental_test'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    integer :: version = 2'  ! Changed
        write (unit, '(A)') '    print *, "Version:", version'
        write (unit, '(A)') '    print *, "Updated!"'       ! Added
        write (unit, '(A)') 'end program incremental_test'
        close (unit)

        ! Rebuild - should detect change and rebuild
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = (exit_code == 0)

        call test_result(success)

        if (.not. success) then
            print *, "  Incremental build failed"
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_incremental_builds

    subroutine test_cli_combinations()
        character(len=256) :: test_prog

        call test_start("Complex CLI combinations")

        ! Create a test program
        test_prog = temp_mgr%get_file_path('cli_test.f90')
        open (newunit=unit, file=test_prog, status='replace', iostat=ios)
        write (unit, '(A)') 'program cli_test'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    print *, "Testing CLI"'
        write (unit, '(A)') 'end program cli_test'
        close (unit)

        ! Test verbose output with caching
        output_file = temp_mgr%get_file_path('verbose_output.txt')
        command = fortran_with_isolated_cache('cli_combo_test')//' -v "'// &
                  trim(test_prog)//'" > "'//trim(output_file)//'" 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        if (exit_code /= 0) then
            success = .false.
        else
            ! Check that verbose output was generated
            inquire (file=output_file, exist=file_exists)
            success = file_exists
        end if

        call test_result(success)

        if (.not. success) then
            print *, "  CLI combinations test failed"
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_cli_combinations

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

end program test_full_workflow
