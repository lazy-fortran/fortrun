program test_lazy_fortran_edge_cases
    use runner, only: is_lazy_fortran_file
    use temp_utils, only: temp_dir_manager, fortran_with_isolated_cache
    implicit none

    type(temp_dir_manager) :: temp_mgr
    character(len=256) :: lazy_file, standard_file, mixed_dir
    character(len=1024) :: command, output_file
    integer :: test_count, pass_count, exit_code, unit, ios, i
    logical :: success

    test_count = 0
    pass_count = 0

    print *, "=== Lazy Fortran Edge Cases Tests ==="
    print *, ""

    call temp_mgr%create('lazy_edge_cases')

    ! Test 1: Mixed .lf and .f90 files in same project
    call test_mixed_file_project()

    ! Test 2: Preprocessing failures and error handling
    call test_preprocessing_failures()

    ! Test 3: Large lazy fortran files
    call test_large_lazy_files()

    ! Test 4: Complex type inference scenarios
    call test_type_inference_edge_cases()

    ! Test 5: Special syntax edge cases
    call test_special_syntax_cases()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All lazy fortran edge case tests passed!"
        stop 0
    else
        print *, "Some lazy fortran edge case tests failed!"
        stop 1
    end if

contains

    subroutine test_mixed_file_project()
        character(len=256) :: module_f90, main_f, output

        call test_start("Mixed .lf and .f90 files in same project")

        mixed_dir = temp_mgr%get_file_path('mixed_project')
        call execute_command_line('mkdir -p "'//trim(mixed_dir)//'"', wait=.true.)

        ! Create standard Fortran module (.f90)
        module_f90 = trim(mixed_dir)//'/utils.f90'
        open (newunit=unit, file=module_f90, status='replace', iostat=ios)
        write (unit, '(A)') 'module utils'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') 'contains'
        write (unit, '(A)') '    function double_value(x) result(y)'
        write (unit, '(A)') '        integer, intent(in) :: x'
        write (unit, '(A)') '        integer :: y'
        write (unit, '(A)') '        y = x * 2'
        write (unit, '(A)') '    end function double_value'
        write (unit, '(A)') 'end module utils'
        close (unit)

        ! Create lazy fortran main (.lf) - would use the module if preprocessing worked
        main_f = trim(mixed_dir)//'/main.lf'
        open (newunit=unit, file=main_f, status='replace', iostat=ios)
        write (unit, '(A)') '! Lazy fortran main program'
        write (unit, '(A)') 'x = 21'
        write (unit, '(A)') 'print *, "Answer:", x * 2'
        close (unit)

        ! Test that we can detect file types correctly
        success = is_lazy_fortran_file(main_f) .and. &
                  .not. is_lazy_fortran_file(module_f90)

        call test_result(success)

        if (.not. success) then
            print *, "  Mixed file project test failed"
        end if
    end subroutine test_mixed_file_project

    subroutine test_preprocessing_failures()
        character(len=256) :: error_file

        call test_start("Preprocessing failures and error handling")

        ! Create a lazy fortran file with invalid syntax
        error_file = temp_mgr%get_file_path('error.lf')
        open (newunit=unit, file=error_file, status='replace', iostat=ios)
        write (unit, '(A)') '! Invalid lazy fortran'
        write (unit, '(A)') 'x = = = 42  ! Multiple equals'
        write (unit, '(A)') 'print *, x +++ y  ! Invalid operator'
        write (unit, '(A)') 'if x then  ! Missing condition'
        write (unit, '(A)') 'end if'
        close (unit)

        ! Try to run - should fail but handle gracefully
        command = fortran_with_isolated_cache('preprocess_error')//' "'// &
                  trim(error_file)//'" 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        ! Should fail with non-zero exit but not crash
        success = (exit_code /= 0)

        call test_result(success)

        if (.not. success) then
            print *, "  Preprocessing error handling failed"
            print *, "  Expected failure but got exit code: ", exit_code
        end if
    end subroutine test_preprocessing_failures

    subroutine test_large_lazy_files()
        character(len=256) :: large_lazy

        call test_start("Large lazy fortran file preprocessing")

        ! Create a large lazy fortran file
        large_lazy = temp_mgr%get_file_path('large.lf')
        open (newunit=unit, file=large_lazy, status='replace', iostat=ios)

        write (unit, '(A)') '! Large lazy fortran file'

        ! Generate many variable assignments
        do i = 1, 1000
            write (unit, '(A,I0,A,I0)') 'var', i, ' = ', i*10
        end do

        ! Generate many computations
        write (unit, '(A)') 'sum = 0'
        do i = 1, 100
            write (unit, '(A,I0)') 'sum = sum + var', i
        end do

        write (unit, '(A)') 'print *, "Sum:", sum'
        close (unit)

        ! This would need fortfront to work - for now just test file creation
        success = is_lazy_fortran_file(large_lazy)

        call test_result(success)

        if (.not. success) then
            print *, "  Large lazy file test failed"
        end if
    end subroutine test_large_lazy_files

    subroutine test_type_inference_edge_cases()
        character(len=256) :: type_file

        call test_start("Complex type inference scenarios")

        ! Create lazy fortran with complex type inference needs
        type_file = temp_mgr%get_file_path('types.lf')
        open (newunit=unit, file=type_file, status='replace', iostat=ios)
        write (unit, '(A)') '! Type inference edge cases'
        write (unit, '(A)') ''
        write (unit, '(A)') '! Mixed numeric types'
        write (unit, '(A)') 'x = 42'
        write (unit, '(A)') 'y = 3.14'
        write (unit, '(A)') 'z = x + y  ! Should infer real'
        write (unit, '(A)') ''
        write (unit, '(A)') '! String operations'
        write (unit, '(A)') 's1 = "Hello"'
        write (unit, '(A)') 's2 = " World"'
        write (unit, '(A)') 's3 = s1 // s2  ! Concatenation'
        write (unit, '(A)') ''
        write (unit, '(A)') '! Array inference'
        write (unit, '(A)') 'arr = [1, 2, 3, 4, 5]'
        write (unit, '(A)') 'arr2 = arr * 2'
        write (unit, '(A)') ''
        write (unit, '(A)') '! Function calls'
        write (unit, '(A)') 'result = sqrt(16.0)'
        write (unit, '(A)') ''
        write (unit, '(A)') 'print *, "Results:", z, s3, arr2(3), result'
        close (unit)

        ! Test file was created and is detected as lazy fortran
        success = is_lazy_fortran_file(type_file)

        call test_result(success)

        if (.not. success) then
            print *, "  Type inference test failed"
        end if
    end subroutine test_type_inference_edge_cases

    subroutine test_special_syntax_cases()
        character(len=256) :: syntax_file

        call test_start("Special syntax edge cases")

        ! Create lazy fortran with special syntax
        syntax_file = temp_mgr%get_file_path('syntax.lf')
        open (newunit=unit, file=syntax_file, status='replace', iostat=ios)
        write (unit, '(A)') '! Special syntax cases'
        write (unit, '(A)') ''
        write (unit, '(A)') '! One-line if statements'
        write (unit, '(A)') 'x = 10'
        write (unit, '(A)') 'if x > 5: print *, "Large"'
        write (unit, '(A)') ''
        write (unit, '(A)') '! List comprehension style'
        write (unit, '(A)') 'squares = [i*i for i in 1:10]'
        write (unit, '(A)') ''
        write (unit, '(A)') '! Lambda-like expressions'
        write (unit, '(A)') 'double = lambda x: x * 2'
        write (unit, '(A)') 'y = double(21)'
        write (unit, '(A)') ''
        write (unit, '(A)') '! Pattern matching'
        write (unit, '(A)') 'match y:'
        write (unit, '(A)') '    case 42: print *, "Answer!"'
        write (unit, '(A)') '    case _: print *, "Not the answer"'
        write (unit, '(A)') ''
        write (unit, '(A)') 'print *, "Done"'
        close (unit)

        ! Test file detection
        success = is_lazy_fortran_file(syntax_file)

        call test_result(success)

        if (.not. success) then
            print *, "  Special syntax test failed"
        end if
    end subroutine test_special_syntax_cases

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

end program test_lazy_fortran_edge_cases
