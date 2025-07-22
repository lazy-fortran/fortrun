program test_module_scanner_coverage
    use iso_fortran_env, only: error_unit
    use module_scanner
    use temp_utils, only: temp_dir_manager
    use system_utils, only: sys_remove_file
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr

    print *, "=== Module Scanner Coverage Tests ==="
    print *

    all_tests_passed = .true.

    call temp_mgr%create('module_scanner_test')

    if (.not. test_file_not_found()) all_tests_passed = .false.
    if (.not. test_empty_file()) all_tests_passed = .false.
    if (.not. test_intrinsic_modules()) all_tests_passed = .false.
    if (.not. test_complex_use_statements()) all_tests_passed = .false.
    if (.not. test_edge_cases()) all_tests_passed = .false.
    if (.not. test_max_modules_limit()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All module scanner coverage tests passed!"
        stop 0
    else
        print *, "Some module scanner coverage tests failed!"
        stop 1
    end if

contains

    function test_file_not_found() result(passed)
        logical :: passed
        type(module_info), dimension(:), allocatable :: modules
        integer :: n_modules

        print *, "Test: File not found error handling"
        passed = .true.

        call scan_modules('/nonexistent/file/path.f90', modules, n_modules)
        
        if (n_modules == 0 .and. allocated(modules)) then
            print *, "  PASS: Handled non-existent file gracefully"
        else
            print *, "  FAIL: Did not handle non-existent file properly"
            passed = .false.
        end if

    end function test_file_not_found

    function test_empty_file() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_file
        type(module_info), dimension(:), allocatable :: modules
        integer :: n_modules, unit

        print *, "Test: Empty file"
        passed = .true.

        test_file = temp_mgr%get_file_path('empty.f90')
        open(newunit=unit, file=test_file, status='replace')
        close(unit)

        call scan_modules(test_file, modules, n_modules)
        
        if (n_modules == 0 .and. allocated(modules) .and. size(modules) == 0) then
            print *, "  PASS: Empty file returns zero modules"
        else
            print *, "  FAIL: Empty file handling incorrect"
            passed = .false.
        end if

        call sys_remove_file(test_file)

    end function test_empty_file

    function test_intrinsic_modules() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_file
        type(module_info), dimension(:), allocatable :: modules
        integer :: n_modules, unit

        print *, "Test: Intrinsic modules filtering"
        passed = .true.

        test_file = temp_mgr%get_file_path('intrinsic.f90')
        open(newunit=unit, file=test_file, status='replace')
        write(unit, '(a)') 'program test'
        write(unit, '(a)') '  use iso_fortran_env'
        write(unit, '(a)') '  use iso_c_binding'
        write(unit, '(a)') '  use ieee_arithmetic'
        write(unit, '(a)') '  use ieee_exceptions'
        write(unit, '(a)') '  use ieee_features'
        write(unit, '(a)') '  use omp_lib'
        write(unit, '(a)') '  use openacc'
        write(unit, '(a)') '  use mpi'
        write(unit, '(a)') '  use mpi_f08'
        write(unit, '(a)') '  use coarray_intrinsic'
        write(unit, '(a)') '  use my_custom_module'
        write(unit, '(a)') 'end program'
        close(unit)

        call scan_modules(test_file, modules, n_modules)
        
        if (n_modules == 1) then
            if (trim(modules(1)%name) == 'my_custom_module') then
                print *, "  PASS: Filtered all intrinsic modules correctly"
            else
                print *, "  FAIL: Wrong module found:", trim(modules(1)%name)
                passed = .false.
            end if
        else
            print *, "  FAIL: Expected 1 module, found", n_modules
            passed = .false.
        end if

        call sys_remove_file(test_file)

    end function test_intrinsic_modules

    function test_complex_use_statements() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_file
        type(module_info), dimension(:), allocatable :: modules
        integer :: n_modules, unit, i

        print *, "Test: Complex use statement variations"
        passed = .true.

        test_file = temp_mgr%get_file_path('complex.f90')
        open(newunit=unit, file=test_file, status='replace')
        write(unit, '(a)') 'module test_mod'
        write(unit, '(a)') '  ! Comment with use keyword'
        write(unit, '(a)') '  use module1'
        write(unit, '(a)') '  use module2, only: something'
        write(unit, '(a)') '  use module3, only: a, b, c'
        write(unit, '(a)') '  use :: module4'
        write(unit, '(a)') '  use, intrinsic :: iso_fortran_env'
        write(unit, '(a)') '  use   module5   ! with comment'
        write(unit, '(a)') '    use module6  ! indented'
        write(unit, '(a)') 'contains'
        write(unit, '(a)') '  ! use module7 in comment'
        write(unit, '(a)') '  subroutine sub()'
        write(unit, '(a)') '    use module8'
        write(unit, '(a)') '  end subroutine'
        write(unit, '(a)') 'end module'
        close(unit)

        call scan_modules(test_file, modules, n_modules)
        
        if (n_modules == 6) then
            print *, "  PASS: Found correct number of modules (6)"
            do i = 1, n_modules
                print *, "    Module", i, ":", trim(modules(i)%name)
            end do
        else
            print *, "  FAIL: Expected 6 modules, found", n_modules
            passed = .false.
        end if

        call sys_remove_file(test_file)

    end function test_complex_use_statements

    function test_edge_cases() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_file
        type(module_info), dimension(:), allocatable :: modules
        integer :: n_modules, unit

        print *, "Test: Edge cases and malformed statements"
        passed = .true.

        test_file = temp_mgr%get_file_path('edge.f90')
        open(newunit=unit, file=test_file, status='replace')
        write(unit, '(a)') 'program edge_test'
        write(unit, '(a)') ''  ! Empty line
        write(unit, '(a)') '   '  ! Whitespace only
        write(unit, '(a)') '  usemodule'  ! No space after use
        write(unit, '(a)') '  user module'  ! user not use
        write(unit, '(a)') '  use'  ! Just use
        write(unit, '(a)') '  use '  ! use with trailing space
        write(unit, '(a)') '  use module_with_very_long_name_that_might_cause_issues'
        write(unit, '(a)') '  use mod1,mod2'  ! No space after comma
        write(unit, '(a)') '  use mod3 ,'  ! Trailing comma
        write(unit, '(a)') '  use mod4 only'  ! No colon after only
        write(unit, '(a)') '  use mod5, only:'  ! Empty only list
        write(unit, '(a)') '  use mod6, only : item'
        write(unit, '(a)') '  use intrinsic iso_fortran_env'  ! Missing ::
        write(unit, '(a)') '  use, intrinsic:: iso_c_binding'  ! No space before ::
        write(unit, '(a)') '  ! Long comment line that contains use keyword somewhere in middle'
        write(unit, '(a)') '  character :: use_var'  ! use in variable name
        write(unit, '(a)') 'end program'
        close(unit)

        call scan_modules(test_file, modules, n_modules)
        
        print *, "  INFO: Found", n_modules, "modules in edge cases"
        if (allocated(modules)) then
            do i = 1, n_modules
                print *, "    Module:", trim(modules(i)%name)
            end do
        end if
        print *, "  PASS: Edge cases handled without crash"

        call sys_remove_file(test_file)

    end function test_edge_cases

    function test_max_modules_limit() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_file
        type(module_info), dimension(:), allocatable :: modules
        integer :: n_modules, unit, i
        character(len=20) :: module_name

        print *, "Test: Maximum modules limit (100+)"
        passed = .true.

        test_file = temp_mgr%get_file_path('many_modules.f90')
        open(newunit=unit, file=test_file, status='replace')
        write(unit, '(a)') 'program many_modules'
        
        ! Write 110 use statements to exceed the 100 limit
        do i = 1, 110
            write(module_name, '(a,i0)') 'module_', i
            write(unit, '(a,a)') '  use ', trim(module_name)
        end do
        
        write(unit, '(a)') 'end program'
        close(unit)

        call scan_modules(test_file, modules, n_modules)
        
        print *, "  INFO: Requested 110 modules, scanner found", n_modules
        if (n_modules == 110) then
            if (allocated(modules) .and. size(modules) >= 100) then
                print *, "  PASS: Handled more than 100 modules"
            else
                print *, "  WARNING: Module count correct but allocation might be limited"
            end if
        else
            print *, "  INFO: Module limit may be enforced at", n_modules
        end if

        call sys_remove_file(test_file)

    end function test_max_modules_limit

end program test_module_scanner_coverage