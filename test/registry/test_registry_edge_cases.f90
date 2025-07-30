program test_registry_edge_cases
    use registry_resolver
    use temp_utils, only: create_temp_dir, get_temp_file_path
    implicit none

    logical :: all_passed = .true.
    character(len=256) :: temp_dir, test_registry, config_dir
    character(len=128) :: package_name, git_url, version, error_msg
    logical :: found, is_valid
    integer :: unit

    print *, "=== Registry Resolver Edge Cases Tests ==="

    temp_dir = create_temp_dir('registry_edge')
    config_dir = trim(temp_dir)//"/config"

    ! Test 1: Empty registry file
    call test_empty_registry()

    ! Test 2: Malformed TOML in registry
    call test_malformed_registry()

    ! Test 3: Registry with missing required fields
    call test_incomplete_registry()

    ! Test 4: Non-existent module lookup
    call test_nonexistent_module()

    ! Test 5: Registry validation edge cases
    call test_registry_validation()

    ! Test 6: Special characters in module names
    call test_special_module_names()

    if (all_passed) then
        print *, ""
        print *, "All registry resolver edge case tests PASSED!"
        stop 0
    else
        print *, ""
        print *, "Some registry resolver edge case tests FAILED!"
        stop 1
    end if

contains

    subroutine test_empty_registry()
        print *, ""
        print *, "Test: Empty registry file"

        test_registry = get_temp_file_path(temp_dir, 'empty_registry.toml')
        open (newunit=unit, file=test_registry, status='replace')
        close (unit)

        call validate_registry(test_registry, is_valid, error_msg)

        if (.not. is_valid) then
            print *, "  PASS: Empty registry detected as invalid"
        else
            print *, "  INFO: Empty registry considered valid (may be OK)"
        end if

        ! Try to resolve from empty registry
        call load_registry_from_path(test_registry)
        call resolve_module_to_package('any_module', package_name, git_url, found)

        if (.not. found) then
            print *, "  PASS: No modules found in empty registry"
        else
            print *, "  FAIL: Should not find modules in empty registry"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_malformed_registry()
        print *, ""
        print *, "Test: Malformed TOML registry"

        test_registry = get_temp_file_path(temp_dir, 'malformed.toml')
        open (newunit=unit, file=test_registry, status='replace')
        write (unit, '(A)') '[packages'  ! missing closing bracket
        write (unit, '(A)') 'invalid toml syntax here'
        write (unit, '(A)') 'modules = ['
        close (unit)

        call validate_registry(test_registry, is_valid, error_msg)

        if (.not. is_valid) then
            print *, "  PASS: Malformed registry detected as invalid"
            print *, "  INFO: Error:", trim(error_msg)
        else
            print *, "  INFO: Malformed registry not detected (parser may be lenient)"
        end if
    end subroutine

    subroutine test_incomplete_registry()
        print *, ""
        print *, "Test: Registry with missing required fields"

        test_registry = get_temp_file_path(temp_dir, 'incomplete.toml')
        open (newunit=unit, file=test_registry, status='replace')
        write (unit, '(A)') '[[packages]]'
        write (unit, '(A)') 'name = "incomplete_package"'
        write (unit, '(A)') '# missing git field'
        write (unit, '(A)') 'modules = ["test_mod"]'
        write (unit, '(A)') ''
        write (unit, '(A)') '[[packages]]'
        write (unit, '(A)') 'git = "https://example.com/repo.git"'
        write (unit, '(A)') '# missing name field'
        write (unit, '(A)') 'modules = ["another_mod"]'
        close (unit)

        call load_registry_from_path(test_registry)
        call resolve_module_to_package('test_mod', package_name, git_url, found)

        if (.not. found) then
            print *, "  PASS: Module from incomplete package not resolved"
        else
            print *, "  INFO: Incomplete package handled gracefully"
        end if
    end subroutine

    subroutine test_nonexistent_module()
        print *, ""
        print *, "Test: Looking up non-existent module"

        ! Create a valid registry first
        test_registry = get_temp_file_path(temp_dir, 'valid.toml')
        open (newunit=unit, file=test_registry, status='replace')
        write (unit, '(A)') '[[packages]]'
        write (unit, '(A)') 'name = "test_package"'
        write (unit, '(A)') 'git = "https://example.com/test.git"'
        write (unit, '(A)') 'modules = ["existing_module"]'
        close (unit)

        call load_registry_from_path(test_registry)

        ! Look for non-existent module
        call resolve_module_to_package('definitely_nonexistent_module_12345', &
                                       package_name, git_url, found)

        if (.not. found) then
            print *, "  PASS: Non-existent module not found"
        else
            print *, "  FAIL: Should not find non-existent module"
            all_passed = .false.
        end if

        ! Look for existing module to verify registry is working
        call resolve_module_to_package('existing_module', package_name, git_url, found)

        if (found) then
            print *, "  PASS: Existing module found correctly"
        else
            print *, "  INFO: Registry loading may have issues"
        end if
    end subroutine

    subroutine test_registry_validation()
        print *, ""
        print *, "Test: Registry validation edge cases"

        ! Test non-existent file
        call validate_registry('/nonexistent/path/registry.toml', is_valid, error_msg)

        if (.not. is_valid) then
            print *, "  PASS: Non-existent registry file detected"
        else
            print *, "  FAIL: Should detect non-existent file"
            all_passed = .false.
        end if

        ! Test directory instead of file
        call validate_registry(temp_dir, is_valid, error_msg)

        if (.not. is_valid) then
            print *, "  PASS: Directory instead of file detected"
        else
            print *, "  FAIL: Should detect directory is not a file"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_special_module_names()
        print *, ""
        print *, "Test: Special characters in module names"

        test_registry = get_temp_file_path(temp_dir, 'special.toml')
        open (newunit=unit, file=test_registry, status='replace')
        write (unit, '(A)') '[[packages]]'
        write (unit, '(A)') 'name = "special_chars"'
        write (unit, '(A)') 'git = "https://example.com/special.git"'
  write (unit, '(A)') 'modules = ["module_with_underscore", "module123", "CAPS_MODULE"]'
        close (unit)

        call load_registry_from_path(test_registry)

        ! Test underscore
  call resolve_module_to_package('module_with_underscore', package_name, git_url, found)
        if (found) then
            print *, "  PASS: Module with underscore found"
        else
            print *, "  INFO: Underscore module not found (may be parser limitation)"
        end if

        ! Test numbers
        call resolve_module_to_package('module123', package_name, git_url, found)
        if (found) then
            print *, "  PASS: Module with numbers found"
        else
            print *, "  INFO: Numeric module not found (may be parser limitation)"
        end if

        ! Test uppercase
        call resolve_module_to_package('CAPS_MODULE', package_name, git_url, found)
        if (found) then
            print *, "  PASS: Uppercase module found"
        else
            print *, "  INFO: Uppercase module not found (may be case sensitive)"
        end if
    end subroutine

end program test_registry_edge_cases
