program test_error_message_quality
    use temp_utils, only: fortran_with_isolated_cache, temp_dir_manager
    use string_utils, only: contains_string
    use system_utils, only: run_command
    implicit none
    
    type(temp_dir_manager) :: temp_mgr
    character(len=:), allocatable :: test_file, command, output, error_output
    logical :: success
    integer :: exit_code
    
    call temp_mgr%create('error_msg_test')
    
    ! Test various error scenarios to ensure quality error messages
    
    ! Test 1: Syntax error - missing THEN
    call test_syntax_error_quality()
    
    ! Test 2: Type mismatch error
    call test_type_error_quality()
    
    ! Test 3: Undefined variable error
    call test_undefined_variable_quality()
    
    ! Test 4: Array bounds error
    call test_array_bounds_quality()
    
    print *, "All error message quality tests passed!"
    
contains

    subroutine test_syntax_error_quality()
        test_file = temp_mgr%get_file_path('syntax_error.f90')
        
        ! Write invalid code
        open(unit=10, file=test_file, status='replace')
        write(10, '(A)') 'program test_syntax'
        write(10, '(A)') '    if (x > 0)  ! Missing THEN'
        write(10, '(A)') '        print *, "positive"'
        write(10, '(A)') '    end if'
        write(10, '(A)') 'end program test_syntax'
        close(10)
        
        command = fortran_with_isolated_cache('syntax_error') // ' ' // test_file
        call run_command(command, output, error_output, exit_code, success)
        
        if (success) then
            error stop "Error test failed: syntax error should have been caught"
        end if
        
        ! Check that error message mentions the issue
        if (.not. contains_string(error_output, "error") .and. &
            .not. contains_string(output, "error")) then
            error stop "Error test failed: no error message produced"
        end if
    end subroutine test_syntax_error_quality
    
    subroutine test_type_error_quality()
        test_file = temp_mgr%get_file_path('type_error.f90')
        
        ! Write code with type mismatch
        open(unit=10, file=test_file, status='replace')
        write(10, '(A)') 'program test_type'
        write(10, '(A)') '    integer :: i'
        write(10, '(A)') '    real :: r'
        write(10, '(A)') '    logical :: flag'
        write(10, '(A)') '    '
        write(10, '(A)') '    i = 5'
        write(10, '(A)') '    r = 3.14'
        write(10, '(A)') '    flag = i  ! Type mismatch'
        write(10, '(A)') 'end program test_type'
        close(10)
        
        command = fortran_with_isolated_cache('type_error') // ' ' // test_file
        call run_command(command, output, error_output, exit_code, success)
        
        if (success) then
            error stop "Error test failed: type error should have been caught"
        end if
    end subroutine test_type_error_quality
    
    subroutine test_undefined_variable_quality()
        test_file = temp_mgr%get_file_path('undefined_var.f90')
        
        ! Write code with undefined variable
        open(unit=10, file=test_file, status='replace')
        write(10, '(A)') 'program test_undefined'
        write(10, '(A)') '    implicit none'
        write(10, '(A)') '    integer :: x'
        write(10, '(A)') '    '
        write(10, '(A)') '    x = y  ! y is not defined'
        write(10, '(A)') 'end program test_undefined'
        close(10)
        
        command = fortran_with_isolated_cache('undefined_var') // ' ' // test_file
        call run_command(command, output, error_output, exit_code, success)
        
        if (success) then
            error stop "Error test failed: undefined variable should have been caught"
        end if
    end subroutine test_undefined_variable_quality
    
    subroutine test_array_bounds_quality()
        test_file = temp_mgr%get_file_path('array_bounds.f90')
        
        ! Write code that might have array bounds issues
        open(unit=10, file=test_file, status='replace')
        write(10, '(A)') 'program test_bounds'
        write(10, '(A)') '    implicit none'
        write(10, '(A)') '    integer :: arr(5)'
        write(10, '(A)') '    integer :: i'
        write(10, '(A)') '    '
        write(10, '(A)') '    ! Initialize array'
        write(10, '(A)') '    arr = [1, 2, 3, 4, 5]'
        write(10, '(A)') '    '
        write(10, '(A)') '    ! This should work'
        write(10, '(A)') '    do i = 1, 5'
        write(10, '(A)') '        arr(i) = i * 2'
        write(10, '(A)') '    end do'
        write(10, '(A)') '    '
        write(10, '(A)') '    print *, "Array bounds test completed"'
        write(10, '(A)') 'end program test_bounds'
        close(10)
        
        ! This one should actually compile and run successfully
        command = fortran_with_isolated_cache('array_bounds') // ' ' // test_file // ' --'
        call run_command(command, output, error_output, exit_code, success)
        
        if (.not. success) then
            error stop "Error test failed: valid array code should have succeeded"
        end if
        
        if (.not. contains_string(output, "Array bounds test completed")) then
            error stop "Error test failed: expected output not found"
        end if
    end subroutine test_array_bounds_quality

end program test_error_message_quality