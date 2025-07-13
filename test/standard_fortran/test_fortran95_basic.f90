program test_fortran95_basic
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    write(*, '(a)') '=== Standard Fortran 95 Basic Compatibility Tests ==='
    write(*, '(a)') ''
    
    call test_program_structure(test_count, pass_count)
    call test_intrinsic_types(test_count, pass_count)
    call test_arrays_basic(test_count, pass_count)
    call test_control_structures(test_count, pass_count)
    call test_expressions(test_count, pass_count)
    
    write(*, '(a)') ''
    write(*, '(a,i0,a,i0,a)') 'Fortran 95 basic tests: ', pass_count, '/', test_count, ' passed'
    
    if (pass_count /= test_count) then
        error stop 'Some Fortran 95 compatibility tests failed!'
    end if
    
contains

    subroutine test_program_structure(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        
        character(len=256) :: temp_input, temp_output, expected_output
        logical :: files_identical
        
        write(*, '(a)') 'Test 1: Program structure preservation'
        test_count = test_count + 1
        
        ! Create a standard Fortran 95 program
        temp_input = '/tmp/test_program_structure.f90'
        temp_output = '/tmp/test_program_structure_output.f90'
        
        call create_test_file(temp_input, [ &
            'program hello', &
            '    implicit none', &
            '    integer :: n', &
            '    n = 42', &
            '    write(*,*) "Hello, n =", n', &
            'end program hello' &
        ], 6)
        
        ! Process through frontend
        if (compile_standard_fortran(temp_input, temp_output)) then
            ! Compare input and output - should be identical
            call compare_files(temp_input, temp_output, files_identical)
            if (files_identical) then
                write(*, '(a)') '  ✓ PASS: Program structure preserved'
                pass_count = pass_count + 1
            else
                write(*, '(a)') '  ✗ FAIL: Program structure modified'
                call show_file_diff(temp_input, temp_output)
            end if
        else
            write(*, '(a)') '  ✗ FAIL: Compilation failed'
        end if
        
    end subroutine test_program_structure

    subroutine test_intrinsic_types(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        
        character(len=256) :: temp_input, temp_output
        logical :: files_identical
        
        write(*, '(a)') 'Test 2: Intrinsic type declarations'
        test_count = test_count + 1
        
        temp_input = '/tmp/test_intrinsic_types.f90'
        temp_output = '/tmp/test_intrinsic_types_output.f90'
        
        call create_test_file(temp_input, [ &
            'program types_test', &
            '    implicit none', &
            '    integer :: i', &
            '    real :: r', &
            '    double precision :: d', &
            '    complex :: c', &
            '    logical :: l', &
            '    character(len=10) :: str', &
            '    ', &
            '    i = 42', &
            '    r = 3.14', &
            '    d = 2.71828d0', &
            '    c = (1.0, 2.0)', &
            '    l = .true.', &
            '    str = "hello"', &
            'end program types_test' &
        ], 16)
        
        if (compile_standard_fortran(temp_input, temp_output)) then
            call compare_files(temp_input, temp_output, files_identical)
            if (files_identical) then
                write(*, '(a)') '  ✓ PASS: Intrinsic types preserved'
                pass_count = pass_count + 1
            else
                write(*, '(a)') '  ✗ FAIL: Intrinsic types modified'
            end if
        else
            write(*, '(a)') '  ✗ FAIL: Compilation failed'
        end if
        
    end subroutine test_intrinsic_types
    
    subroutine test_arrays_basic(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        
        character(len=256) :: temp_input, temp_output
        logical :: files_identical
        
        write(*, '(a)') 'Test 3: Basic array declarations'
        test_count = test_count + 1
        
        temp_input = '/tmp/test_arrays_basic.f90'
        temp_output = '/tmp/test_arrays_basic_output.f90'
        
        call create_test_file(temp_input, [ &
            'program arrays_test', &
            '    implicit none', &
            '    integer, parameter :: n = 10', &
            '    integer :: static_array(10)', &
            '    real :: matrix(3,3)', &
            '    integer, allocatable :: dynamic_array(:)', &
            '    integer :: i', &
            '    ', &
            '    allocate(dynamic_array(n))', &
            '    do i = 1, n', &
            '        static_array(i) = i', &
            '        dynamic_array(i) = i * 2', &
            '    end do', &
            '    deallocate(dynamic_array)', &
            'end program arrays_test' &
        ], 14)
        
        if (compile_standard_fortran(temp_input, temp_output)) then
            call compare_files(temp_input, temp_output, files_identical)
            if (files_identical) then
                write(*, '(a)') '  ✓ PASS: Array declarations preserved'
                pass_count = pass_count + 1
            else
                write(*, '(a)') '  ✗ FAIL: Array declarations modified'
            end if
        else
            write(*, '(a)') '  ✗ FAIL: Compilation failed'
        end if
        
    end subroutine test_arrays_basic
    
    subroutine test_control_structures(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        
        character(len=256) :: temp_input, temp_output
        logical :: files_identical
        
        write(*, '(a)') 'Test 4: Control structures'
        test_count = test_count + 1
        
        temp_input = '/tmp/test_control_structures.f90'
        temp_output = '/tmp/test_control_structures_output.f90'
        
        call create_test_file(temp_input, [ &
            'program control_test', &
            '    implicit none', &
            '    integer :: i, n', &
            '    ', &
            '    n = 5', &
            '    if (n > 0) then', &
            '        write(*,*) "Positive"', &
            '    else', &
            '        write(*,*) "Non-positive"', &
            '    end if', &
            '    ', &
            '    do i = 1, n', &
            '        if (mod(i, 2) == 0) then', &
            '            write(*,*) "Even:", i', &
            '        end if', &
            '    end do', &
            'end program control_test' &
        ], 16)
        
        if (compile_standard_fortran(temp_input, temp_output)) then
            call compare_files(temp_input, temp_output, files_identical)
            if (files_identical) then
                write(*, '(a)') '  ✓ PASS: Control structures preserved'
                pass_count = pass_count + 1
            else
                write(*, '(a)') '  ✗ FAIL: Control structures modified'
            end if
        else
            write(*, '(a)') '  ✗ FAIL: Compilation failed'
        end if
        
    end subroutine test_control_structures
    
    subroutine test_expressions(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        
        character(len=256) :: temp_input, temp_output
        logical :: files_identical
        
        write(*, '(a)') 'Test 5: Expressions and operators'
        test_count = test_count + 1
        
        temp_input = '/tmp/test_expressions.f90'
        temp_output = '/tmp/test_expressions_output.f90'
        
        call create_test_file(temp_input, [ &
            'program expressions_test', &
            '    implicit none', &
            '    integer :: a, b, c', &
            '    real :: x, y, z', &
            '    logical :: flag1, flag2', &
            '    ', &
            '    a = 10', &
            '    b = 3', &
            '    c = a + b * 2 - 1', &
            '    x = 3.14', &
            '    y = 2.71', &
            '    z = x**2 + y/2.0', &
            '    flag1 = a > b', &
            '    flag2 = flag1 .and. (c /= 0)', &
            'end program expressions_test' &
        ], 14)
        
        if (compile_standard_fortran(temp_input, temp_output)) then
            call compare_files(temp_input, temp_output, files_identical)
            if (files_identical) then
                write(*, '(a)') '  ✓ PASS: Expressions preserved'
                pass_count = pass_count + 1
            else
                write(*, '(a)') '  ✗ FAIL: Expressions modified'
            end if
        else
            write(*, '(a)') '  ✗ FAIL: Compilation failed'
        end if
        
    end subroutine test_expressions

    ! Helper function to compile standard Fortran through frontend
    logical function compile_standard_fortran(input_file, output_file)
        character(len=*), intent(in) :: input_file, output_file
        integer :: exit_code
        
        ! Use system command to test frontend (simplified for now)
        call execute_command_line('fpm run fortran -- ' // trim(input_file) // &
                                  ' --preprocess > ' // trim(output_file), &
                                  exitstat=exit_code)
        
        compile_standard_fortran = (exit_code == 0)
    end function compile_standard_fortran
    
    ! Helper to create test files
    subroutine create_test_file(filename, lines, num_lines)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: lines(:)
        integer, intent(in) :: num_lines
        integer :: unit, i
        
        open(newunit=unit, file=filename, status='replace', action='write')
        do i = 1, num_lines
            write(unit, '(a)') trim(lines(i))
        end do
        close(unit)
    end subroutine create_test_file
    
    ! Helper to compare two files
    subroutine compare_files(file1, file2, identical)
        character(len=*), intent(in) :: file1, file2
        logical, intent(out) :: identical
        integer :: unit1, unit2, ios1, ios2
        character(len=1024) :: line1, line2
        
        identical = .true.
        
        open(newunit=unit1, file=file1, status='old', action='read', iostat=ios1)
        open(newunit=unit2, file=file2, status='old', action='read', iostat=ios2)
        
        if (ios1 /= 0 .or. ios2 /= 0) then
            identical = .false.
            return
        end if
        
        do
            read(unit1, '(a)', iostat=ios1) line1
            read(unit2, '(a)', iostat=ios2) line2
            
            if (ios1 /= ios2) then
                identical = .false.
                exit
            end if
            
            if (ios1 /= 0) exit  ! End of both files
            
            if (trim(line1) /= trim(line2)) then
                identical = .false.
                exit
            end if
        end do
        
        close(unit1)
        close(unit2)
    end subroutine compare_files
    
    ! Helper to show file differences
    subroutine show_file_diff(file1, file2)
        character(len=*), intent(in) :: file1, file2
        write(*, '(a)') '    Expected (input):'
        call execute_command_line('head -5 ' // trim(file1) // ' | sed "s/^/      /"')
        write(*, '(a)') '    Actual (output):'
        call execute_command_line('head -5 ' // trim(file2) // ' | sed "s/^/      /"')
    end subroutine show_file_diff

end program test_fortran95_basic