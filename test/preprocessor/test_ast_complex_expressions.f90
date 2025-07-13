program test_ast_complex_expressions
    use preprocessor
    implicit none
    
    logical :: all_passed
    character(len=:), allocatable :: temp_dir
    
    all_passed = .true.
    
    ! Use a simple temporary directory
    temp_dir = "/tmp/fortran_test"
    call execute_command_line("mkdir -p " // temp_dir)
    
    ! Test cases that require proper AST parsing
    if (.not. test_nested_expressions()) all_passed = .false.
    if (.not. test_function_calls_in_expressions()) all_passed = .false.
    if (.not. test_mixed_type_expressions()) all_passed = .false.
    
    if (all_passed) then
        print '(a)', "All complex expression AST tests passed"
        stop 0
    else
        print '(a)', "Some complex expression AST tests failed"
        stop 1
    end if

contains

    logical function test_nested_expressions()
        ! Test that deeply nested expressions are parsed correctly
        ! This should fail with simple token pattern matching
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        integer :: unit, ios
        character(len=1024) :: line
        logical :: found_expression
        
        test_nested_expressions = .true.
        print '(a)', "Testing nested expression AST parsing..."
        
        input_file = temp_dir // "/test_nested.f"
        output_file = temp_dir // "/test_nested.f90"
        
        ! Create test input with nested expression
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "result = (a + b) * (c - d) / (e + f)"
        close(unit)
        
        ! Preprocess using AST-based function
        call preprocess_file_ast_based(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Preprocessing error: " // trim(error_msg)
            test_nested_expressions = .false.
            return
        end if
        
        ! Check that the complex expression is preserved
        found_expression = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, "(a + b) * (c - d) / (e + f)") > 0) found_expression = .true.
        end do
        close(unit)
        
        if (.not. found_expression) then
            print '(a)', "FAIL: Complex nested expression not preserved"
            test_nested_expressions = .false.
        else
            print '(a)', "PASS: Nested expressions handled correctly"
        end if
    end function test_nested_expressions

    logical function test_function_calls_in_expressions()
        ! Test expressions with function calls - requires proper AST parsing
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        integer :: unit, ios
        character(len=1024) :: line
        logical :: found_function_call
        
        test_function_calls_in_expressions = .true.
        print '(a)', "Testing function calls in expressions..."
        
        input_file = temp_dir // "/test_func_call.f"
        output_file = temp_dir // "/test_func_call.f90"
        
        ! Create test input with function call in expression
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "y = sin(x) + cos(2.0 * x)"
        close(unit)
        
        ! Preprocess using AST-based function
        call preprocess_file_ast_based(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Preprocessing error: " // trim(error_msg)
            test_function_calls_in_expressions = .false.
            return
        end if
        
        ! Check that the function calls are preserved
        found_function_call = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, "sin(x)") > 0 .and. index(line, "cos(2.0 * x)") > 0) then
                found_function_call = .true.
            end if
        end do
        close(unit)
        
        if (.not. found_function_call) then
            print '(a)', "FAIL: Function calls in expressions not preserved"
            test_function_calls_in_expressions = .false.
        else
            print '(a)', "PASS: Function calls handled correctly"
        end if
    end function test_function_calls_in_expressions

    logical function test_mixed_type_expressions()
        ! Test expressions that mix different types - requires sophisticated type inference
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        integer :: unit, ios
        character(len=1024) :: line
        logical :: found_mixed_types
        
        test_mixed_type_expressions = .true.
        print '(a)', "Testing mixed type expressions..."
        
        input_file = temp_dir // "/test_mixed_types.f"
        output_file = temp_dir // "/test_mixed_types.f90"
        
        ! Create test input with mixed types
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "i = 5"
        write(unit, '(a)') "x = 3.14"
        write(unit, '(a)') "result = i * x + 1.0"
        close(unit)
        
        ! Preprocess using AST-based function
        call preprocess_file_ast_based(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Preprocessing error: " // trim(error_msg)
            test_mixed_type_expressions = .false.
            return
        end if
        
        ! Check that types are inferred correctly
        ! This is a basic check - real AST should do proper type propagation
        found_mixed_types = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, "result = i * x + 1.0") > 0) found_mixed_types = .true.
        end do
        close(unit)
        
        if (.not. found_mixed_types) then
            print '(a)', "FAIL: Mixed type expression not preserved"
            test_mixed_type_expressions = .false.
        else
            print '(a)', "PASS: Mixed types handled correctly"
        end if
    end function test_mixed_type_expressions

end program test_ast_complex_expressions