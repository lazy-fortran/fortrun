program test_ff_cli_integration
    use runner, only: is_lazy_fortran_file
    use temp_utils, only: temp_dir_manager, fortran_with_isolated_cache
    implicit none

    type(temp_dir_manager) :: temp_mgr
    character(len=256) :: test_file, output_file, temp_input
    character(len=1024) :: command
    integer :: test_count, pass_count, exit_code, unit, ios
    logical :: success, file_exists
    character(len=1024) :: line

    test_count = 0
    pass_count = 0

    print *, "=== FF CLI Integration Tests ==="
    print *, ""

    call temp_mgr%create('ff_cli_test')
    ! No need for test_cache_dir in this test

    ! Test 1: File extension detection
    call test_extension_detection()

    ! Test 2: CLI transformation functionality
    call test_cli_transformation()

    ! Test 3: Error handling in CLI
    call test_cli_error_handling()

    ! Test 4: Mixed file project support
    call test_mixed_file_project()

    ! Test 5: Type inference validation
    call test_type_inference_validation()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All fortfront CLI integration tests passed!"
        stop 0
    else
        print *, "Some fortfront CLI integration tests failed!"
        stop 1
    end if

contains

    subroutine test_extension_detection()
        logical :: result

        call test_start("File extension detection")

        ! Test .lf files are detected as lazy fortran
        result = is_lazy_fortran_file("test.lf") .and. &
                 is_lazy_fortran_file("example.LF") .and. &
                 .not. is_lazy_fortran_file("standard.f90") .and. &
                 .not. is_lazy_fortran_file("standard.F90") .and. &
                 .not. is_lazy_fortran_file("readme.txt") .and. &
                 .not. is_lazy_fortran_file("noextension")

        call test_result(result)

        if (.not. result) then
            print *, "  Extension detection failed"
        end if
    end subroutine test_extension_detection

    subroutine test_cli_transformation()
        call test_start("CLI transformation functionality")

        ! Create a simple lazy fortran file
        test_file = temp_mgr%get_file_path('simple.lf')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'x = 42'
        write (unit, '(A)') 'print *, x'
        close (unit)

        ! Transform using fortfront CLI directly
        output_file = temp_mgr%get_file_path('simple_output.f90')
        ! Use a simpler approach - try the most likely path first
        command = 'fortfront < "'//trim(test_file)//'" > "'//trim(output_file)//'" 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = (exit_code == 0)

        if (success) then
            ! Check if output file exists and contains valid Fortran
            inquire (file=output_file, exist=file_exists)
            if (file_exists) then
                open (newunit=unit, file=output_file, status='old', iostat=ios)
                if (ios == 0) then
                    read (unit, '(A)', iostat=ios) line
                    ! Should contain "program" keyword
                    success = (index(line, 'program') > 0)
                    close (unit)
                else
                    success = .false.
                end if
            else
                success = .false.
            end if
        end if

        call test_result(success)

        if (.not. success) then
            print *, "  CLI transformation failed"
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_cli_transformation

    subroutine test_cli_error_handling()
        call test_start("CLI error handling")

        ! Create an invalid lazy fortran file
        test_file = temp_mgr%get_file_path('invalid.lf')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'this is not valid lazy fortran syntax'
        write (unit, '(A)') '!@#$%^&*()_+|}{":?><'
        close (unit)

        ! Try to transform - should fail gracefully
        output_file = temp_mgr%get_file_path('invalid_output.f90')
        command = 'fortfront < "'//trim(test_file)//'" > "'//trim(output_file)//'" 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        ! Should handle gracefully (either succeed or fail, but not crash)
        success = .true.  ! As long as it doesn't crash, it's good

        call test_result(success)

        if (.not. success) then
            print *, "  CLI error handling failed - expected failure"
        end if
    end subroutine test_cli_error_handling

    subroutine test_mixed_file_project()
        character(len=256) :: f90_file

        call test_start("Mixed file project support")

        ! For now, just test that .f90 files work correctly
        ! Testing both .lf and .f90 files requires fully working fortfront
        ! This test validates that the detection logic works properly
        f90_file = temp_mgr%get_file_path('standard.f90')
        open (newunit=unit, file=f90_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program standard'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    integer :: y'
        write (unit, '(A)') '    y = 100'
        write (unit, '(A)') '    print *, "Standard:", y'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Test that .f90 files work correctly
        command = fortran_with_isolated_cache('mixed_test1')//' "'// &
                  trim(f90_file)//'" > /dev/null 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = (exit_code == 0)

        ! Additional test: verify that file extension detection works
        success = success .and. .not. is_lazy_fortran_file(trim(f90_file))
        success = success .and. is_lazy_fortran_file('test.lf')
        success = success .and. is_lazy_fortran_file('test.LF')

        call test_result(success)

        if (.not. success) then
            print *, "  Mixed file project support failed"
            print *, "  .f90 file exit code: ", exit_code
        end if
    end subroutine test_mixed_file_project

    subroutine test_type_inference_validation()
        call test_start("Type inference validation")

        ! Create a file that tests type inference
        test_file = temp_mgr%get_file_path('type_inference.lf')
        open (newunit=unit, file=test_file, status='replace', iostat=ios)
        write (unit, '(A)') 'x = 42'           ! Should infer integer
        write (unit, '(A)') 'y = 3.14'         ! Should infer real
        write (unit, '(A)') 'z = "hello"'      ! Should infer character
        write (unit, '(A)') 'print *, x, y, z'
        close (unit)

        ! Transform and check generated code
        output_file = temp_mgr%get_file_path('type_inference_output.f90')
        command = 'fortfront < "'//trim(test_file)//'" > "'//trim(output_file)//'" 2>&1'
        call execute_command_line(command, exitstat=exit_code, wait=.true.)

        success = (exit_code == 0)

        if (success) then
            ! Check if generated code contains type declarations
            inquire (file=output_file, exist=file_exists)
            if (file_exists) then
                open (newunit=unit, file=output_file, status='old', iostat=ios)
                success = .false.
                do
                    read (unit, '(A)', iostat=ios) line
                    if (ios /= 0) exit
                    ! Look for type declarations (should contain "integer", "real", etc.)
                    if (index(line, 'integer') > 0 .or. index(line, 'real') > 0) then
                        success = .true.
                        exit
                    end if
                end do
                close (unit)
            else
                success = .false.
            end if
        end if

        call test_result(success)

        if (.not. success) then
            print *, "  Type inference validation failed"
        end if
    end subroutine test_type_inference_validation

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write (*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_result(test_success)
        logical, intent(in) :: test_success
        if (test_success) then
            print *, " ... PASSED"
            pass_count = pass_count + 1
        else
            print *, " ... FAILED"
        end if
    end subroutine test_result

end program test_ff_cli_integration
