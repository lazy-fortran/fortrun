program test_ast_assignments
    use preprocessor
    implicit none
    
    logical :: all_passed
    character(len=:), allocatable :: temp_dir
    character(len=256) :: error_msg
    
    all_passed = .true.
    
    ! Use a simple temporary directory
    temp_dir = "/tmp/fortran_test"
    call execute_command_line("mkdir -p " // temp_dir)
    
    ! Test proper AST-based assignment processing
    if (.not. test_simple_assignment_ast()) all_passed = .false.
    if (.not. test_expression_assignment_ast()) all_passed = .false.
    if (.not. test_multiple_assignments_ast()) all_passed = .false.
    
    if (all_passed) then
        print '(a)', "All AST assignment tests passed"
        stop 0
    else
        print '(a)', "Some AST assignment tests failed"
        stop 1
    end if

contains

    logical function test_simple_assignment_ast()
        ! Test that simple assignments are parsed into assignment_node AST
        ! and generate proper Fortran code with type inference
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        integer :: unit, ios
        character(len=1024) :: line
        logical :: found_implicit_none, found_real_declaration
        
        test_simple_assignment_ast = .true.
        print '(a)', "Testing simple assignment AST processing..."
        
        input_file = temp_dir // "/test_simple_assign.f"
        output_file = temp_dir // "/test_simple_assign.f90"
        
        ! Create test input with simple assignment
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "x = 42.0"
        close(unit)
        
        ! Preprocess using AST
        call preprocess_file(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Preprocessing error: " // trim(error_msg)
            test_simple_assignment_ast = .false.
            return
        end if
        
        ! Verify output has proper program structure and type inference
        found_implicit_none = .false.
        found_real_declaration = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, "implicit none") > 0) found_implicit_none = .true.
            if (index(line, "real(8)") > 0 .and. index(line, "x") > 0) found_real_declaration = .true.
        end do
        close(unit)
        
        if (.not. found_implicit_none) then
            print '(a)', "FAIL: Missing implicit none in output"
            test_simple_assignment_ast = .false.
        end if
        
        if (.not. found_real_declaration) then
            print '(a)', "FAIL: Missing proper type declaration for x"
            test_simple_assignment_ast = .false.
        end if
        
        if (test_simple_assignment_ast) then
            print '(a)', "PASS: Simple assignment processed via AST"
        end if
    end function test_simple_assignment_ast

    logical function test_expression_assignment_ast()
        ! Test that expression assignments are parsed into assignment_node 
        ! with proper binary_op_node values
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        integer :: unit, ios
        character(len=1024) :: line
        logical :: found_assignment
        
        test_expression_assignment_ast = .true.
        print '(a)', "Testing expression assignment AST processing..."
        
        input_file = temp_dir // "/test_expr_assign.f"
        output_file = temp_dir // "/test_expr_assign.f90"
        
        ! Create test input with expression assignment
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "result = a + b * 2.0"
        close(unit)
        
        ! Preprocess using AST
        call preprocess_file(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Preprocessing error: " // trim(error_msg)
            test_expression_assignment_ast = .false.
            return
        end if
        
        ! Verify the expression was preserved correctly
        found_assignment = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, "result = a + b * 2.0") > 0) found_assignment = .true.
        end do
        close(unit)
        
        if (.not. found_assignment) then
            print '(a)', "FAIL: Expression assignment not preserved correctly"
            test_expression_assignment_ast = .false.
        else
            print '(a)', "PASS: Expression assignment processed via AST"
        end if
    end function test_expression_assignment_ast

    logical function test_multiple_assignments_ast()
        ! Test that multiple assignments are each parsed as separate assignment_nodes
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        integer :: unit, ios
        character(len=1024) :: line
        logical :: found_x_assignment, found_y_assignment
        
        test_multiple_assignments_ast = .true.
        print '(a)', "Testing multiple assignments AST processing..."
        
        input_file = temp_dir // "/test_multi_assign.f"
        output_file = temp_dir // "/test_multi_assign.f90"
        
        ! Create test input with multiple assignments
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "x = 10"
        write(unit, '(a)') "y = x + 5"
        close(unit)
        
        ! Preprocess using AST
        call preprocess_file(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Preprocessing error: " // trim(error_msg)
            test_multiple_assignments_ast = .false.
            return
        end if
        
        ! Verify both assignments are present
        found_x_assignment = .false.
        found_y_assignment = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, "x = 10") > 0) found_x_assignment = .true.
            if (index(line, "y = x + 5") > 0) found_y_assignment = .true.
        end do
        close(unit)
        
        if (.not. found_x_assignment) then
            print '(a)', "FAIL: First assignment not found"
            test_multiple_assignments_ast = .false.
        end if
        
        if (.not. found_y_assignment) then
            print '(a)', "FAIL: Second assignment not found"
            test_multiple_assignments_ast = .false.
        end if
        
        if (test_multiple_assignments_ast) then
            print '(a)', "PASS: Multiple assignments processed via AST"
        end if
    end function test_multiple_assignments_ast

end program test_ast_assignments