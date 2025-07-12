! Test preprocessor integration with function type inference
program test_preprocessor_function_integration
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use preprocessor, only: preprocess_file
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    call test_function_parameter_inference()
    call test_function_return_inference() 
    call test_nested_function_inference()
    
    call print_results()
    
contains

    subroutine test_function_parameter_inference()
        character(len=*), parameter :: test_name = "Function parameter type inference"
        character(len=256) :: input_file, output_file, error_msg
        logical :: success
        
        ! Create test input file with function needing type inference
        input_file = 'test_func_param.f'
        output_file = 'test_func_param.f90'
        
        call create_test_file(input_file, &
            'x = 5.0' // new_line('a') // &
            'y = square(x)' // new_line('a') // &
            '' // new_line('a') // &
            'function square(val)' // new_line('a') // &
            '    square = val * val' // new_line('a') // &
            'end function')
        
        call preprocess_file(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) == 0) then
            success = check_output_contains(output_file, 'real(8), intent(in) :: val')
            success = success .and. check_output_contains(output_file, 'real(8) :: square')
        else
            success = .false.
        end if
        
        call assert_true(success, test_name)
        call cleanup_files(input_file, output_file)
    end subroutine
    
    subroutine test_function_return_inference()
        character(len=*), parameter :: test_name = "Function return type inference"
        character(len=256) :: input_file, output_file, error_msg
        logical :: success
        
        input_file = 'test_func_return.f'
        output_file = 'test_func_return.f90'
        
        call create_test_file(input_file, &
            'result = is_positive(3.14)' // new_line('a') // &
            '' // new_line('a') // &
            'function is_positive(x)' // new_line('a') // &
            '    is_positive = x > 0' // new_line('a') // &
            'end function')
        
        call preprocess_file(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) == 0) then
            success = check_output_contains(output_file, 'logical :: is_positive')
            success = success .and. check_output_contains(output_file, 'real(8), intent(in) :: x')
        else
            success = .false.
        end if
        
        call assert_true(success, test_name)
        call cleanup_files(input_file, output_file)
    end subroutine
    
    subroutine test_nested_function_inference()
        character(len=*), parameter :: test_name = "Nested function call inference"
        character(len=256) :: input_file, output_file, error_msg
        logical :: success
        
        input_file = 'test_nested_func.f'
        output_file = 'test_nested_func.f90'
        
        call create_test_file(input_file, &
            'result = double_square(5)' // new_line('a') // &
            '' // new_line('a') // &
            'function square(x)' // new_line('a') // &
            '    square = x * x' // new_line('a') // &
            'end function' // new_line('a') // &
            '' // new_line('a') // &
            'function double_square(y)' // new_line('a') // &
            '    double_square = 2 * square(y)' // new_line('a') // &
            'end function')
        
        call preprocess_file(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) == 0) then
            success = check_output_contains(output_file, 'integer :: square')
            success = success .and. check_output_contains(output_file, 'integer :: double_square') 
            success = success .and. check_output_contains(output_file, 'integer, intent(in) :: x')
            success = success .and. check_output_contains(output_file, 'integer, intent(in) :: y')
        else
            success = .false.
        end if
        
        call assert_true(success, test_name)
        call cleanup_files(input_file, output_file)
    end subroutine

    subroutine create_test_file(filename, content)
        character(len=*), intent(in) :: filename, content
        integer :: unit, ios
        
        open(newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios == 0) then
            write(unit, '(A)') content
            close(unit)
        end if
    end subroutine
    
    function check_output_contains(filename, expected_text) result(found)
        character(len=*), intent(in) :: filename, expected_text
        logical :: found
        character(len=1024) :: line
        integer :: unit, ios
        
        found = .false.
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                if (index(line, trim(expected_text)) > 0) then
                    found = .true.
                    exit
                end if
            end do
            close(unit)
        end if
    end function
    
    subroutine cleanup_files(file1, file2)
        character(len=*), intent(in) :: file1, file2
        integer :: unit, ios
        
        open(newunit=unit, file=file1, status='old', iostat=ios)
        if (ios == 0) then
            close(unit, status='delete')
        end if
        
        open(newunit=unit, file=file2, status='old', iostat=ios) 
        if (ios == 0) then
            close(unit, status='delete')
        end if
    end subroutine

    subroutine assert_true(condition, test_name)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        if (condition) then
            pass_count = pass_count + 1
            write(*, '(A, A)') 'PASS: ', test_name
        else
            write(error_unit, '(A, A)') 'FAIL: ', test_name
        end if
    end subroutine

    subroutine print_results()
        write(*, '(A, I0, A, I0, A)') 'Tests: ', pass_count, '/', test_count, ' passed'
        if (pass_count /= test_count) then
            write(error_unit, '(A, I0, A)') 'FAILED: ', test_count - pass_count, ' tests failed'
            stop 1
        end if
    end subroutine

end program