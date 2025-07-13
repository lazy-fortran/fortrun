program test_ast_based_processing
    use preprocessor
    implicit none
    
    logical :: all_passed
    character(len=:), allocatable :: temp_dir
    
    all_passed = .true.
    
    ! Use a simple temporary directory
    temp_dir = "/tmp/fortran_test"
    call execute_command_line("mkdir -p " // temp_dir)
    
    ! Test new AST-based preprocessing function
    if (.not. test_ast_assignment_processing()) all_passed = .false.
    
    if (all_passed) then
        print '(a)', "All AST-based processing tests passed"
        stop 0
    else
        print '(a)', "Some AST-based processing tests failed"
        stop 1
    end if

contains

    logical function test_ast_assignment_processing()
        ! Test that the new AST-based function processes assignments correctly
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        integer :: unit, ios
        character(len=1024) :: line
        logical :: found_assignment
        
        test_ast_assignment_processing = .true.
        print '(a)', "Testing AST-based assignment processing..."
        
        input_file = temp_dir // "/test_ast_assign.f"
        output_file = temp_dir // "/test_ast_assign.f90"
        
        ! Create test input with assignment
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "x = 42.0"
        close(unit)
        
        ! Preprocess using new AST-based function
        call preprocess_file_ast_based(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print '(a)', "FAIL: AST preprocessing error: " // trim(error_msg)
            test_ast_assignment_processing = .false.
            return
        end if
        
        ! Verify output contains the assignment
        found_assignment = .false.
        
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, "x = 42.0") > 0) found_assignment = .true.
        end do
        close(unit)
        
        if (.not. found_assignment) then
            print '(a)', "FAIL: Assignment not found in AST output"
            test_ast_assignment_processing = .false.
        else
            print '(a)', "PASS: AST-based assignment processing works"
        end if
        
        ! Show the output for debugging
        print '(a)', "Generated output:"
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            print '(a)', trim(line)
        end do
        close(unit)
        
    end function test_ast_assignment_processing

end program test_ast_based_processing