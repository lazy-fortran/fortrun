program test_preprocessor_ast
    use preprocessor_ast
    implicit none
    
    logical :: all_passed
    character(len=:), allocatable :: temp_dir
    character(len=256) :: error_msg
    
    all_passed = .true.
    
    ! Create temporary directory for test files
    temp_dir = "/tmp/fortran_ast_test_" // timestamp()
    call execute_command_line("mkdir -p " // temp_dir)
    
    ! Run tests
    if (.not. test_simple_assignment()) all_passed = .false.
    if (.not. test_expression()) all_passed = .false.
    
    ! Cleanup
    call execute_command_line("rm -rf " // temp_dir)
    
    ! Report results
    if (all_passed) then
        print '(a)', "All AST preprocessor tests passed"
        stop 0
    else
        print '(a)', "Some AST preprocessor tests failed"
        stop 1
    end if

contains

    logical function test_simple_assignment()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        integer :: unit
        character(len=1024) :: line
        logical :: found_program, found_implicit, found_assignment
        
        test_simple_assignment = .true.
        print '(a)', "Testing simple assignment preprocessing..."
        
        ! Create test input file
        input_file = temp_dir // "/test_assign.f"
        output_file = temp_dir // "/test_assign.f90"
        
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "x = 42"
        close(unit)
        
        ! Preprocess
        call preprocess_file_ast(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Preprocessing error: " // trim(error_msg)
            test_simple_assignment = .false.
            return
        end if
        
        ! Check output
        found_program = .false.
        found_implicit = .false.
        found_assignment = .false.
        
        open(newunit=unit, file=output_file, status='old', action='read')
        do
            read(unit, '(a)', end=100) line
            if (index(line, "program") > 0) found_program = .true.
            if (index(line, "implicit none") > 0) found_implicit = .true.
            if (index(line, "x = 42") > 0) found_assignment = .true.
        end do
100     close(unit)
        
        if (.not. found_program) then
            print '(a)', "FAIL: Missing program statement"
            test_simple_assignment = .false.
        else if (.not. found_implicit) then
            print '(a)', "FAIL: Missing implicit none"
            test_simple_assignment = .false.
        else if (.not. found_assignment) then
            print '(a)', "FAIL: Missing assignment statement"
            test_simple_assignment = .false.
        else
            print '(a)', "PASS: Simple assignment preprocessing"
        end if
        
    end function test_simple_assignment

    logical function test_expression()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        integer :: unit
        
        test_expression = .true.
        print '(a)', "Testing expression preprocessing..."
        
        ! Create test input file
        input_file = temp_dir // "/test_expr.f"
        output_file = temp_dir // "/test_expr.f90"
        
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "y = a + b * c"
        close(unit)
        
        ! Preprocess
        call preprocess_file_ast(input_file, output_file, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Preprocessing error: " // trim(error_msg)
            test_expression = .false.
            return
        end if
        
        ! For now, just check that output file was created
        block
            integer :: ios
            open(newunit=unit, file=output_file, status='old', iostat=ios)
            if (ios /= 0) then
                print '(a)', "FAIL: Output file not created"
                test_expression = .false.
            else
                close(unit)
                print '(a)', "PASS: Expression preprocessing"
            end if
        end block
        
    end function test_expression

    function timestamp() result(ts)
        character(len=15) :: ts
        integer :: values(8)
        
        call date_and_time(values=values)
        write(ts, '(i0,"_",5i2.2)') values(1), values(2), values(3), &
                                    values(5), values(6), values(7)
    end function timestamp

end program test_preprocessor_ast