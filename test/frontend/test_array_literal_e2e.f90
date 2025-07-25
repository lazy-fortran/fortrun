program test_array_literal_e2e
    use frontend, only: compile_source, compilation_options_t, BACKEND_FORTRAN
    use temp_utils, only: create_temp_file
    use system_utils, only: run_command
    implicit none
    
    logical :: all_passed
    character(len=256) :: cmd_output
    integer :: exit_code
    
    all_passed = .true.
    
    print *, '=== Array Literal End-to-End Tests ==='
    print *
    
    if (.not. test_basic_array_compilation()) all_passed = .false.
    if (.not. test_empty_array_allocatable()) all_passed = .false.
    if (.not. test_mixed_array_types()) all_passed = .false.
    if (.not. test_character_arrays()) all_passed = .false.
    if (.not. test_expression_arrays()) all_passed = .false.
    if (.not. test_large_arrays()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, 'All array literal E2E tests passed!'
        stop 0
    else
        print *, 'Some array literal E2E tests failed!'
        stop 1
    end if
    
contains

    logical function test_basic_array_compilation()
        character(len=:), allocatable :: input_file, output_file, exe_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        
        test_basic_array_compilation = .true.
        print *, 'Testing basic array compilation...'
        
        ! Create test input
        input_file = create_temp_file('test_basic_arr', '.f')
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'arr = [10, 20, 30]'
        write(unit, '(a)') 'print *, "Sum:", sum(arr)'
        write(unit, '(a)') 'print *, "Size:", size(arr)'
        close(unit)
        
        ! Compile with frontend
        output_file = create_temp_file('test_basic_out', '.f90')
        options%backend = BACKEND_FORTRAN
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_basic_array_compilation = .false.
            return
        end if
        
        ! Compile and run the generated code
        exe_file = create_temp_file('test_basic_exe', '')
        call run_command('gfortran -o '//exe_file//' '//output_file//' 2>&1', cmd_output, exit_code)
        
        if (exit_code /= 0) then
            print *, '  FAIL: Fortran compilation failed:', trim(cmd_output)
            test_basic_array_compilation = .false.
            return
        end if
        
        ! Run the executable
        call run_command(exe_file//' 2>&1', cmd_output, exit_code)
        
        if (exit_code == 0 .and. index(cmd_output, 'Sum:') > 0 .and. index(cmd_output, '60') > 0) then
            print *, '  PASS: Basic array works correctly'
        else
            print *, '  FAIL: Output incorrect:', trim(cmd_output)
            test_basic_array_compilation = .false.
        end if
        
    end function test_basic_array_compilation
    
    logical function test_empty_array_allocatable()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        character(len=256) :: line
        logical :: found_allocatable
        
        test_empty_array_allocatable = .true.
        print *, 'Testing empty array generates allocatable...'
        
        ! Create test with empty array
        input_file = create_temp_file('test_empty', '.f')
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'empty_arr = []'
        write(unit, '(a)') 'allocate(empty_arr(3))'
        write(unit, '(a)') 'empty_arr = [1, 2, 3]'
        write(unit, '(a)') 'print *, "Allocated:", empty_arr'
        close(unit)
        
        ! Compile
        output_file = create_temp_file('test_empty_out', '.f90')
        options%backend = BACKEND_FORTRAN
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_empty_array_allocatable = .false.
            return
        end if
        
        ! Check for allocatable in output
        found_allocatable = .false.
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=unit) line
            if (unit /= 0) exit
            if (index(line, 'allocatable') > 0 .and. index(line, 'empty_arr') > 0) then
                found_allocatable = .true.
                print *, '  Found declaration:', trim(line)
                exit
            end if
        end do
        close(unit)
        
        if (found_allocatable) then
            print *, '  PASS: Empty array marked as allocatable'
        else
            print *, '  FAIL: Allocatable not found for empty array'
            test_empty_array_allocatable = .false.
        end if
        
    end function test_empty_array_allocatable
    
    logical function test_mixed_array_types()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        
        test_mixed_array_types = .true.
        print *, 'Testing mixed type arrays...'
        
        ! Create test with different types
        input_file = create_temp_file('test_mixed', '.f')
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'int_arr = [1, 2, 3, 4, 5]'
        write(unit, '(a)') 'real_arr = [1.5, 2.5, 3.5]'
        write(unit, '(a)') 'char_arr = ["abc", "def", "ghi"]'
        write(unit, '(a)') 'bool_arr = [.true., .false., .true., .true.]'
        write(unit, '(a)') 'print *, "Int sum:", sum(int_arr)'
        write(unit, '(a)') 'print *, "Real sum:", sum(real_arr)'
        write(unit, '(a)') 'print *, "Char array:", char_arr'
        write(unit, '(a)') 'print *, "Bool count true:", count(bool_arr)'
        close(unit)
        
        ! Compile
        output_file = create_temp_file('test_mixed_out', '.f90')
        options%backend = BACKEND_FORTRAN
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) == 0) then
            print *, '  PASS: Mixed type arrays compiled successfully'
        else
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_mixed_array_types = .false.
        end if
        
    end function test_mixed_array_types
    
    logical function test_character_arrays()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        character(len=256) :: line
        logical :: found_char_array
        
        test_character_arrays = .true.
        print *, 'Testing character array handling...'
        
        ! Create test
        input_file = create_temp_file('test_char', '.f')
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'words = ["hello", "world", "test "]'
        write(unit, '(a)') 'print *, words'
        close(unit)
        
        ! Compile
        output_file = create_temp_file('test_char_out', '.f90')
        options%backend = BACKEND_FORTRAN
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        ! Check character array declaration
        found_char_array = .false.
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=unit) line
            if (unit /= 0) exit
            if (index(line, 'character') > 0 .and. index(line, 'words(3)') > 0) then
                found_char_array = .true.
                exit
            end if
        end do
        close(unit)
        
        if (found_char_array) then
            print *, '  PASS: Character array with correct size'
        else
            print *, '  FAIL: Character array declaration not found'
            test_character_arrays = .false.
        end if
        
    end function test_character_arrays
    
    logical function test_expression_arrays()
        character(len=:), allocatable :: input_file, output_file, exe_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        
        test_expression_arrays = .true.
        print *, 'Testing arrays with expressions...'
        
        ! Create test
        input_file = create_temp_file('test_expr', '.f')
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'x = 10'
        write(unit, '(a)') 'y = 5'
        write(unit, '(a)') 'results = [x+y, x-y, x*y, x/y]'
        write(unit, '(a)') 'print *, "Results:", results'
        close(unit)
        
        ! Compile
        output_file = create_temp_file('test_expr_out', '.f90')
        options%backend = BACKEND_FORTRAN
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) /= 0) then
            print *, '  FAIL: Frontend compilation error:', trim(error_msg)
            test_expression_arrays = .false.
            return
        end if
        
        ! Try to compile the generated code
        exe_file = create_temp_file('test_expr_exe', '')
        call run_command('gfortran -o '//exe_file//' '//output_file//' 2>&1', cmd_output, exit_code)
        
        if (exit_code == 0) then
            print *, '  PASS: Expression arrays compile successfully'
        else
            print *, '  FAIL: Fortran compilation failed'
            test_expression_arrays = .false.
        end if
        
    end function test_expression_arrays
    
    logical function test_large_arrays()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        character(len=256) :: line
        logical :: found_large_array
        
        test_large_arrays = .true.
        print *, 'Testing large array size detection...'
        
        ! Create test with large array
        input_file = create_temp_file('test_large', '.f')
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'big = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]'
        write(unit, '(a)') 'print *, "Size:", size(big)'
        close(unit)
        
        ! Compile
        output_file = create_temp_file('test_large_out', '.f90')
        options%backend = BACKEND_FORTRAN
        options%output_file = output_file
        
        call compile_source(input_file, options, error_msg)
        
        ! Check for correct array size
        found_large_array = .false.
        open(newunit=unit, file=output_file, status='old')
        do
            read(unit, '(a)', iostat=unit) line
            if (unit /= 0) exit
            if (index(line, 'big(20)') > 0) then
                found_large_array = .true.
                exit
            end if
        end do
        close(unit)
        
        if (found_large_array) then
            print *, '  PASS: Large array size correctly detected'
        else
            print *, '  FAIL: Large array size not detected'
            test_large_arrays = .false.
        end if
        
    end function test_large_arrays

end program test_array_literal_e2e