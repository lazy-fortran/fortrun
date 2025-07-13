program test_preprocessor_ast_functions
    use preprocessor_ast
    implicit none
    
    logical :: all_passed
    integer :: test_count
    character(len=256) :: test_dir
    
    all_passed = .true.
    test_count = 0
    
    ! Create test directory
    test_dir = "test_preprocessor_ast_functions_tmp"
    call execute_command_line("mkdir -p " // trim(test_dir))
    
    ! Run tests
    if (.not. test_simple_function()) all_passed = .false.
    if (.not. test_function_with_intent()) all_passed = .false.
    if (.not. test_subroutine()) all_passed = .false.
    if (.not. test_function_and_main()) all_passed = .false.
    
    ! Clean up test directory
    call execute_command_line("rm -rf " // trim(test_dir))
    
    ! Report results
    if (all_passed) then
        print '(a)', "All AST preprocessor function tests passed"
        stop 0
    else
        print '(a)', "Some AST preprocessor function tests failed"
        stop 1
    end if
    
contains

    logical function test_simple_function()
        character(len=:), allocatable :: input, output
        character(len=256) :: input_file, output_file, error_msg
        character(len=1024) :: line
        integer :: unit, ios
        
        test_simple_function = .true.
        test_count = test_count + 1
        print '(a)', "Testing simple function preprocessing..."
        
        ! Create input file
        write(input_file, '(a,i0,a)') trim(test_dir) // "/test", test_count, "_input.f"
        write(output_file, '(a,i0,a)') trim(test_dir) // "/test", test_count, "_output.f90"
        
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "real function square(x)"
        write(unit, '(a)') "  real :: x"
        write(unit, '(a)') "  square = x * x"
        write(unit, '(a)') "end function"
        close(unit)
        
        ! Preprocess the file
        call preprocess_file_ast(input_file, output_file, error_msg)
        
        if (error_msg /= "") then
            print '(a)', "FAIL: Preprocessing error: " // trim(error_msg)
            test_simple_function = .false.
            return
        end if
        
        ! Read output
        output = ""
        open(newunit=unit, file=output_file, status='old', iostat=ios)
        if (ios /= 0) then
            print '(a)', "FAIL: Could not open output file"
            test_simple_function = .false.
            return
        end if
        
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            output = output // trim(line) // new_line('a')
        end do
        close(unit)
        
        ! Check that output contains the function in contains section
        if (index(output, "contains") == 0) then
            print '(a)', "FAIL: Missing contains section"
            print '(a)', "Output: ", trim(output)
            test_simple_function = .false.
        else if (index(output, "real function square") == 0) then
            print '(a)', "FAIL: Function definition not found"
            print '(a)', "Output: ", trim(output)
            test_simple_function = .false.
        else if (index(output, "intent(in)") == 0) then
            print '(a)', "FAIL: intent(in) not added to parameter"
            print '(a)', "Output: ", trim(output)
            test_simple_function = .false.
        else
            print '(a)', "PASS: Simple function"
        end if
        
    end function test_simple_function
    
    logical function test_function_with_intent()
        character(len=:), allocatable :: output
        character(len=256) :: input_file, output_file, error_msg
        character(len=1024) :: line
        integer :: unit, ios
        
        test_function_with_intent = .true.
        test_count = test_count + 1
        print '(a)', "Testing function with explicit intent..."
        
        ! Create input file
        write(input_file, '(a,i0,a)') trim(test_dir) // "/test", test_count, "_input.f"
        write(output_file, '(a,i0,a)') trim(test_dir) // "/test", test_count, "_output.f90"
        
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "subroutine calculate(a, b)"
        write(unit, '(a)') "  real :: a"
        write(unit, '(a)') "  real, intent(out) :: b"
        write(unit, '(a)') "  b = a * 2.0"
        write(unit, '(a)') "end subroutine"
        close(unit)
        
        ! Preprocess the file
        call preprocess_file_ast(input_file, output_file, error_msg)
        
        if (error_msg /= "") then
            print '(a)', "FAIL: Preprocessing error: " // trim(error_msg)
            test_function_with_intent = .false.
            return
        end if
        
        ! Read output
        output = ""
        open(newunit=unit, file=output_file, status='old', iostat=ios)
        if (ios /= 0) then
            print '(a)', "FAIL: Could not open output file"
            test_function_with_intent = .false.
            return
        end if
        
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            output = output // trim(line) // new_line('a')
        end do
        close(unit)
        
        ! Check that intent(in) is added to 'a' but 'b' keeps intent(out)
        if (index(output, "real, intent(in) :: a") == 0) then
            print '(a)', "FAIL: intent(in) not added to parameter 'a'"
            print '(a)', "Output: ", trim(output)
            test_function_with_intent = .false.
        else if (index(output, "real, intent(out) :: b") == 0) then
            print '(a)', "FAIL: intent(out) should be preserved for 'b'"
            print '(a)', "Output: ", trim(output)
            test_function_with_intent = .false.
        else
            print '(a)', "PASS: Function with explicit intent"
        end if
        
    end function test_function_with_intent
    
    logical function test_subroutine()
        character(len=:), allocatable :: output
        character(len=256) :: input_file, output_file, error_msg
        character(len=1024) :: line
        integer :: unit, ios
        
        test_subroutine = .true.
        test_count = test_count + 1
        print '(a)', "Testing subroutine preprocessing..."
        
        ! Create input file
        write(input_file, '(a,i0,a)') trim(test_dir) // "/test", test_count, "_input.f"
        write(output_file, '(a,i0,a)') trim(test_dir) // "/test", test_count, "_output.f90"
        
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "subroutine print_value(x)"
        write(unit, '(a)') "  real :: x"
        write(unit, '(a)') "  print *, x"
        write(unit, '(a)') "end subroutine"
        close(unit)
        
        ! Preprocess the file
        call preprocess_file_ast(input_file, output_file, error_msg)
        
        if (error_msg /= "") then
            print '(a)', "FAIL: Preprocessing error: " // trim(error_msg)
            test_subroutine = .false.
            return
        end if
        
        ! Read output
        output = ""
        open(newunit=unit, file=output_file, status='old', iostat=ios)
        if (ios /= 0) then
            print '(a)', "FAIL: Could not open output file"
            test_subroutine = .false.
            return
        end if
        
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            output = output // trim(line) // new_line('a')
        end do
        close(unit)
        
        ! Check subroutine in contains section
        if (index(output, "contains") == 0) then
            print '(a)', "FAIL: Missing contains section"
            test_subroutine = .false.
        else if (index(output, "subroutine print_value") == 0) then
            print '(a)', "FAIL: Subroutine definition not found"
            test_subroutine = .false.
        else if (index(output, "real, intent(in) :: x") == 0) then
            print '(a)', "FAIL: intent(in) not added to subroutine parameter"
            test_subroutine = .false.
        else
            print '(a)', "PASS: Subroutine"
        end if
        
    end function test_subroutine
    
    logical function test_function_and_main()
        character(len=:), allocatable :: output
        character(len=256) :: input_file, output_file, error_msg
        character(len=1024) :: line
        integer :: unit, ios
        
        test_function_and_main = .true.
        test_count = test_count + 1
        print '(a)', "Testing function with main code..."
        
        ! Create input file
        write(input_file, '(a,i0,a)') trim(test_dir) // "/test", test_count, "_input.f"
        write(output_file, '(a,i0,a)') trim(test_dir) // "/test", test_count, "_output.f90"
        
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') "result = square(5.0)"
        write(unit, '(a)') "print *, result"
        write(unit, '(a)') ""
        write(unit, '(a)') "real function square(x)"
        write(unit, '(a)') "  real :: x"
        write(unit, '(a)') "  square = x * x"
        write(unit, '(a)') "end function"
        close(unit)
        
        ! Preprocess the file
        call preprocess_file_ast(input_file, output_file, error_msg)
        
        if (error_msg /= "") then
            print '(a)', "FAIL: Preprocessing error: " // trim(error_msg)
            test_function_and_main = .false.
            return
        end if
        
        ! Read output
        output = ""
        open(newunit=unit, file=output_file, status='old', iostat=ios)
        if (ios /= 0) then
            print '(a)', "FAIL: Could not open output file"
            test_function_and_main = .false.
            return
        end if
        
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            output = output // trim(line) // new_line('a')
        end do
        close(unit)
        
        ! Check that main code is before contains and function is after
        if (index(output, "real(8) :: result") == 0) then
            print '(a)', "FAIL: Variable declaration not generated"
            test_function_and_main = .false.
        else if (index(output, "contains") == 0) then
            print '(a)', "FAIL: Missing contains section"
            test_function_and_main = .false.
        else
            ! Verify order: declaration, assignment, contains, function
            if (index(output, "real(8) :: result") > index(output, "result = square")) then
                print '(a)', "FAIL: Declaration should come before assignment"
                test_function_and_main = .false.
            else if (index(output, "result = square") > index(output, "contains")) then
                print '(a)', "FAIL: Assignment should come before contains"
                test_function_and_main = .false.
            else if (index(output, "contains") > index(output, "real function square")) then
                print '(a)', "FAIL: Function should come after contains"
                test_function_and_main = .false.
            else
                print '(a)', "PASS: Function with main code"
            end if
        end if
        
    end function test_function_and_main

end program test_preprocessor_ast_functions