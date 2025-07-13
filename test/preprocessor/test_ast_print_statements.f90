program test_ast_print_statements
    use preprocessor
    implicit none
    
    logical :: all_passed
    character(len=:), allocatable :: temp_dir
    
    all_passed = .true.
    
    ! Use a simple temporary directory
    temp_dir = "/tmp/fortran_test"
    call execute_command_line("mkdir -p " // temp_dir)
    
    ! Test print statement AST processing
    if (.not. test_simple_print_ast()) all_passed = .false.
    if (.not. test_print_with_variables()) all_passed = .false.
    if (.not. test_print_with_expressions()) all_passed = .false.
    
    if (all_passed) then
        print '(a)', "All print statement AST tests passed"
        stop 0
    else
        print '(a)', "Some print statement AST tests failed"
        stop 1
    end if

contains

    logical function test_simple_print_ast()
        ! Test that simple print statements are parsed into print_statement_node AST
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        integer :: unit, ios
        character(len=1024) :: line
        logical :: found_print
        
        test_simple_print_ast = .true.
        print '(a)', "Testing simple print statement AST processing..."
        
        input_file = temp_dir // "/test_print.f"
        output_file = temp_dir // "/test_print.f90"
        
        ! Create test input with print statement
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'print *, "Hello World"'
        close(unit)
        
        ! Preprocess using AST-based function
        call preprocess_file_ast_based(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Print statement preprocessing error: " // trim(error_msg)
            test_simple_print_ast = .false.
            return
        end if
        
        ! Verify print statement is preserved correctly
        found_print = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, 'print *, "Hello World"') > 0) found_print = .true.
        end do
        close(unit)
        
        if (.not. found_print) then
            print '(a)', "FAIL: Print statement not found in output"
            test_simple_print_ast = .false.
        else
            print '(a)', "PASS: Print statement processed via AST"
        end if
        
        ! Show output for debugging
        print '(a)', "Generated output:"
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print '(a)', trim(line)
        end do
        close(unit)
        
    end function test_simple_print_ast

    logical function test_print_with_variables()
        ! Test print statements with variables
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        integer :: unit, ios
        character(len=1024) :: line
        logical :: found_print, found_assignment
        
        test_print_with_variables = .true.
        print '(a)', "Testing print with variables..."
        
        input_file = temp_dir // "/test_print_var.f"
        output_file = temp_dir // "/test_print_var.f90"
        
        ! Create test input with assignment and print
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "x = 42"
        write(unit, '(a)') "print *, x"
        close(unit)
        
        ! Preprocess using AST-based function
        call preprocess_file_ast_based(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Print with variables preprocessing error: " // trim(error_msg)
            test_print_with_variables = .false.
            return
        end if
        
        ! Verify both statements are present
        found_print = .false.
        found_assignment = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, "print *, x") > 0) found_print = .true.
            if (index(line, "x = 42") > 0) found_assignment = .true.
        end do
        close(unit)
        
        if (.not. found_print) then
            print '(a)', "FAIL: Print statement with variable not found"
            test_print_with_variables = .false.
        end if
        
        if (.not. found_assignment) then
            print '(a)', "FAIL: Assignment before print not found"
            test_print_with_variables = .false.
        end if
        
        if (test_print_with_variables) then
            print '(a)', "PASS: Print with variables handled correctly"
        end if
    end function test_print_with_variables

    logical function test_print_with_expressions()
        ! Test print statements with expressions
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        integer :: unit, ios
        character(len=1024) :: line
        logical :: found_print_expr
        
        test_print_with_expressions = .true.
        print '(a)', "Testing print with expressions..."
        
        input_file = temp_dir // "/test_print_expr.f"
        output_file = temp_dir // "/test_print_expr.f90"
        
        ! Create test input with print expression
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "print *, a + b"
        close(unit)
        
        ! Preprocess using AST-based function
        call preprocess_file_ast_based(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Print with expression preprocessing error: " // trim(error_msg)
            test_print_with_expressions = .false.
            return
        end if
        
        ! Verify print expression is preserved
        found_print_expr = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, "print *, a + b") > 0) found_print_expr = .true.
        end do
        close(unit)
        
        if (.not. found_print_expr) then
            print '(a)', "FAIL: Print expression not preserved correctly"
            test_print_with_expressions = .false.
        else
            print '(a)', "PASS: Print with expressions handled correctly"
        end if
    end function test_print_with_expressions

end program test_ast_print_statements