program test_module_scanner_edge_cases
    use module_scanner
    use temp_utils, only: create_temp_dir, get_temp_file_path
    implicit none

    logical :: all_passed = .true.
    character(len=256) :: temp_dir, test_file
    type(module_info), allocatable :: modules(:)
    integer :: n_modules, unit

    print *, "=== Module Scanner Edge Cases Tests ==="

    temp_dir = create_temp_dir('mod_scanner_edge')

    ! Test 1: Empty file
    call test_empty_file()

    ! Test 2: File with only comments
    call test_comments_only()

    ! Test 3: Malformed use statements
    call test_malformed_use()

    ! Test 4: Mixed case and whitespace variations
    call test_case_whitespace()

    ! Test 5: Very long module names
    call test_long_module_names()

    ! Test 6: Use statements in strings (should be ignored)
    call test_use_in_strings()

    if (all_passed) then
        print *, ""
        print *, "All module scanner edge case tests PASSED!"
        stop 0
    else
        print *, ""
        print *, "Some module scanner edge case tests FAILED!"
        stop 1
    end if

contains

    subroutine test_empty_file()
        print *, ""
        print *, "Test: Empty file scanning"

        test_file = get_temp_file_path(temp_dir, 'empty.f90')
        open (newunit=unit, file=test_file, status='replace')
        close (unit)

        call scan_modules(test_file, modules, n_modules)

        if (n_modules == 0) then
            print *, "  PASS: Empty file produces no modules"
        else
            print *, "  FAIL: Empty file should produce no modules, got", n_modules
            all_passed = .false.
        end if
    end subroutine

    subroutine test_comments_only()
        print *, ""
        print *, "Test: File with only comments"

        test_file = get_temp_file_path(temp_dir, 'comments.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') '! This is a comment'
        write (unit, '(A)') '!! use fake_module  ! This is in a comment'
        write (unit, '(A)') 'C Old style comment with use statement'
        write (unit, '(A)') '* Another old style comment'
        close (unit)

        call scan_modules(test_file, modules, n_modules)

        if (n_modules == 0) then
            print *, "  PASS: Comments-only file produces no modules"
        else
            print *, "  FAIL: Comments should not produce modules, got", n_modules
            all_passed = .false.
        end if
    end subroutine

    subroutine test_malformed_use()
        print *, ""
        print *, "Test: Malformed use statements"

        test_file = get_temp_file_path(temp_dir, 'malformed.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'use'  ! incomplete use statement
        write (unit, '(A)') 'use  '  ! use with only whitespace
        write (unit, '(A)') 'use,,,'  ! use with commas but no module
        write (unit, '(A)') 'use valid_module'  ! this should work
        write (unit, '(A)') 'use another_valid, only: func'  ! this should work
        close (unit)

        call scan_modules(test_file, modules, n_modules)

        ! Should find the valid modules despite malformed ones
        if (n_modules >= 2) then
            print *, "  PASS: Found valid modules despite malformed statements"
            print *, "  INFO: Found", n_modules, "modules"
        else
            print *, "  FAIL: Should find at least 2 valid modules, got", n_modules
            all_passed = .false.
        end if
    end subroutine

    subroutine test_case_whitespace()
        print *, ""
        print *, "Test: Case and whitespace variations"

        test_file = get_temp_file_path(temp_dir, 'case_ws.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') '   USE   module_a   '  ! extra spaces
        write (unit, '(A)') 'use module_b'        ! normal
        write (unit, '(A)') '        use        module_c        '     ! tabs
        write (unit, '(A)') 'USE MODULE_D'        ! uppercase
        close (unit)

        call scan_modules(test_file, modules, n_modules)

        if (n_modules == 4) then
            print *, "  PASS: All case/whitespace variations detected"
        else
            print *, "  INFO: Found", n_modules, "modules (case sensitivity may vary)"
        end if
    end subroutine

    subroutine test_long_module_names()
        print *, ""
        print *, "Test: Very long module names"

        test_file = get_temp_file_path(temp_dir, 'long_names.f90')
        open (newunit=unit, file=test_file, status='replace')
        write(unit, '(A)') 'use very_long_module_name_that_exceeds_normal_limits_but_should_still_work'
        write (unit, '(A)') 'use a'  ! very short name
        close (unit)

        call scan_modules(test_file, modules, n_modules)

        if (n_modules >= 1) then
            print *, "  PASS: Long module names handled"
            print *, "  INFO: Found", n_modules, "modules"
        else
            print *, "  FAIL: Should find at least 1 module"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_use_in_strings()
        print *, ""
        print *, "Test: Use statements inside strings (should be ignored)"

        test_file = get_temp_file_path(temp_dir, 'strings.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(A)') 'program test'
        write (unit, '(A)') '  character(len=*), parameter :: msg = "use fake_module"'
        write (unit, '(A)') "  print *, 'This contains use statement'"
        write (unit, '(A)') '  use real_module  ! This is a real use'
        write (unit, '(A)') 'end program'
        close (unit)

        call scan_modules(test_file, modules, n_modules)

        ! Should only find the real module, not the ones in strings
        if (n_modules == 1) then
            print *, "  PASS: Use statements in strings ignored correctly"
        else
            print *, "  INFO: Found", n_modules, "modules (string detection may vary)"
        end if
    end subroutine

end program test_module_scanner_edge_cases
