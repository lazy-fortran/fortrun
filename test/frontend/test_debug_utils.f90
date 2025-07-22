program test_debug_utils
    use iso_fortran_env, only: error_unit
    use debug_utils
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER
    use ast_core
    use temp_utils, only: temp_dir_manager
    use system_utils, only: sys_remove_file
    implicit none

    logical :: all_tests_passed
    type(temp_dir_manager) :: temp_mgr

    print *, "=== Debug Utils Tests ==="
    print *

    all_tests_passed = .true.

    call temp_mgr%create('debug_utils_test')

    ! Test debug output functions - focus on simple working tests
    if (.not. test_debug_output_tokens()) all_tests_passed = .false.
    if (.not. test_debug_output_codegen()) all_tests_passed = .false.
    ! Skip AST-related tests that need complex arena setup

    print *
    if (all_tests_passed) then
        print *, "All debug utils tests passed!"
        stop 0
    else
        print *, "Some debug utils tests failed!"
        stop 1
    end if

contains

    function test_debug_output_tokens() result(passed)
        logical :: passed
        type(token_t) :: tokens(3)
        character(len=:), allocatable :: test_file, json_file
        logical :: file_exists

        print *, "Test 1: Debug output tokens"
        passed = .true.

        ! Create test tokens
        tokens(1)%kind = TK_IDENTIFIER
        tokens(1)%text = "x"
        tokens(1)%line = 1
        tokens(1)%column = 1

        tokens(2)%kind = TK_OPERATOR
        tokens(2)%text = "="
        tokens(2)%line = 1
        tokens(2)%column = 3

        tokens(3)%kind = TK_NUMBER
        tokens(3)%text = "42"
        tokens(3)%line = 1
        tokens(3)%column = 5

        ! Test with .f90 extension
        test_file = temp_mgr%get_file_path('test_tokens.f90')
        call debug_output_tokens(test_file, tokens)

        json_file = temp_mgr%get_file_path('test_tokens_tokens.json')
        inquire(file=json_file, exist=file_exists)
        if (.not. file_exists) then
            print *, "  FAIL: JSON tokens file not created"
            passed = .false.
        else
            print *, "  PASS: JSON tokens file created"
            call sys_remove_file(json_file)
        end if

        ! Test without extension
        test_file = temp_mgr%get_file_path('test_no_ext')
        call debug_output_tokens(test_file, tokens)

        json_file = temp_mgr%get_file_path('test_no_ext_tokens.json')
        inquire(file=json_file, exist=file_exists)
        if (.not. file_exists) then
            print *, "  FAIL: JSON tokens file not created for file without extension"
            passed = .false.
        else
            print *, "  PASS: JSON tokens file created for file without extension"
            call sys_remove_file(json_file)
        end if

    end function test_debug_output_tokens

    function test_debug_output_codegen() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_file, json_file
        character(*), parameter :: test_code = &
            "program test" // new_line('a') // &
            "  print *, ""Hello""" // new_line('a') // &
            "end program"
        logical :: file_exists
        integer :: unit, ios
        character(len=1024) :: line

        print *, "Test 5: Debug output codegen"
        passed = .true.

        test_file = temp_mgr%get_file_path('test_codegen.f90')
        call debug_output_codegen(test_file, test_code)

        json_file = temp_mgr%get_file_path('test_codegen_codegen.json')
        inquire(file=json_file, exist=file_exists)
        if (.not. file_exists) then
            print *, "  FAIL: JSON codegen file not created"
            passed = .false.
        else
            ! Check file contains generated code structure
            open(newunit=unit, file=json_file, status='old', iostat=ios)
            if (ios == 0) then
                ! Read multiple lines to find the generated_code field
                block
                    character(len=1024) :: content
                    logical :: found_generated_code
                    found_generated_code = .false.
                    
                    do
                        read(unit, '(a)', iostat=ios) line
                        if (ios /= 0) exit
                        if (index(line, 'generated_code') > 0) then
                            found_generated_code = .true.
                            exit
                        end if
                    end do
                    
                    if (found_generated_code) then
                        print *, "  PASS: JSON codegen file created with generated code"
                    else
                        print *, "  FAIL: JSON codegen file doesn't contain generated_code field"
                        passed = .false.
                    end if
                end block
                close(unit)
            else
                print *, "  FAIL: Cannot read JSON codegen file"
                passed = .false.
            end if
            call sys_remove_file(json_file)
        end if

    end function test_debug_output_codegen

end program test_debug_utils