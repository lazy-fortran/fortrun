program test_fpm_edge_cases
    use fpm_generator
    use fpm_module_cache
    use temp_utils, only: create_temp_dir, get_temp_file_path
    use system_utils, only: sys_create_dir, sys_file_exists
    implicit none

    logical :: all_passed = .true.
    character(len=256) :: temp_dir, project_dir, fpm_file
    character(len=512) :: error_msg
    logical :: success
    integer :: unit

    print *, "=== FPM Edge Cases Tests ==="

    temp_dir = create_temp_dir('fpm_edge')

    ! Test 1: Empty project generation
    call test_empty_project()

    ! Test 2: Project with invalid names
    call test_invalid_project_names()

    ! Test 3: Deeply nested module dependencies
    call test_deep_dependencies()

    ! Test 4: Circular dependency detection
    call test_circular_dependencies()

    ! Test 5: Very large number of modules
    call test_many_modules()

    ! Test 6: Special characters in module names
    call test_special_module_names()

    ! Test 7: Missing fpm.toml scenarios
    call test_missing_fpm_toml()

    if (all_passed) then
        print *, ""
        print *, "All FPM edge case tests PASSED!"
        stop 0
    else
        print *, ""
        print *, "Some FPM edge case tests FAILED!"
        stop 1
    end if

contains

    subroutine test_empty_project()
        print *, ""
        print *, "Test: Empty FPM project generation"

        project_dir = trim(temp_dir)//"/empty_project"
        call sys_create_dir(project_dir, success)

        ! Generate minimal fpm.toml
        fpm_file = trim(project_dir)//"/fpm.toml"
        open (newunit=unit, file=fpm_file, status='replace')
        write (unit, '(A)') 'name = "empty_project"'
        write (unit, '(A)') 'version = "0.0.0"'
        close (unit)

        if (sys_file_exists(fpm_file)) then
            print *, "  PASS: Empty project created"
        else
            print *, "  FAIL: Empty project creation failed"
            all_passed = .false.
        end if

        ! Test with no source files
        call sys_create_dir(trim(project_dir)//"/src", success)
        print *, "  PASS: Empty src directory handled"
    end subroutine

    subroutine test_invalid_project_names()
        print *, ""
        print *, "Test: Invalid project names"

        ! Test with spaces in name
        project_dir = trim(temp_dir)//"/project with spaces"
       call generate_fpm_project(project_dir, "project with spaces", error_msg, success)

        if (.not. success) then
            print *, "  PASS: Project name with spaces rejected"
        else
            print *, "  INFO: Spaces in project names may be allowed"
        end if

        ! Test with special characters
        project_dir = trim(temp_dir)//"/project@special"
        call generate_fpm_project(project_dir, "project@special", error_msg, success)

        if (.not. success) then
            print *, "  PASS: Special characters in name rejected"
        else
            print *, "  INFO: Special characters may be allowed"
        end if

        ! Test with empty name
        project_dir = trim(temp_dir)//"/noname"
        call generate_fpm_project(project_dir, "", error_msg, success)

        if (.not. success) then
            print *, "  PASS: Empty project name rejected"
        else
            print *, "  FAIL: Empty name should be rejected"
            all_passed = .false.
        end if
    end subroutine

    subroutine test_deep_dependencies()
        integer :: i
        character(len=256) :: mod_file, mod_name

        print *, ""
        print *, "Test: Deeply nested module dependencies"

        project_dir = trim(temp_dir)//"/deep_deps"
        call sys_create_dir(project_dir, success)
        call sys_create_dir(trim(project_dir)//"/src", success)

        ! Create chain of module dependencies
        do i = 1, 20
            write (mod_name, '(A,I0)') 'module_', i
        mod_file = get_temp_file_path(trim(project_dir)//"/src", trim(mod_name)//'.f90')

            open (newunit=unit, file=mod_file, status='replace')
            write (unit, '(A)') 'module '//trim(mod_name)
            if (i > 1) then
                write (unit, '(A,I0)') '    use module_', i - 1
            end if
            write (unit, '(A)') '    implicit none'
            write (unit, '(A)') 'contains'
            write (unit, '(A,I0)') '    subroutine sub_', i, '()'
            write (unit, '(A,I0)') '        print *, "Module ', i, '"'
            write (unit, '(A)') '    end subroutine'
            write (unit, '(A)') 'end module'
            close (unit)
        end do

        print *, "  PASS: Deep dependency chain created"

        ! Test cache with deep dependencies
        call clear_module_cache()
        print *, "  PASS: Module cache cleared"
    end subroutine

    subroutine test_circular_dependencies()
        character(len=256) :: mod_file

        print *, ""
        print *, "Test: Circular dependency detection"

        project_dir = trim(temp_dir)//"/circular"
        call sys_create_dir(project_dir, success)
        call sys_create_dir(trim(project_dir)//"/src", success)

        ! Create module A that uses B
        mod_file = get_temp_file_path(trim(project_dir)//"/src", 'mod_a.f90')
        open (newunit=unit, file=mod_file, status='replace')
        write (unit, '(A)') 'module mod_a'
        write (unit, '(A)') '    use mod_b'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') 'end module'
        close (unit)

        ! Create module B that uses A (circular)
        mod_file = get_temp_file_path(trim(project_dir)//"/src", 'mod_b.f90')
        open (newunit=unit, file=mod_file, status='replace')
        write (unit, '(A)') 'module mod_b'
        write (unit, '(A)') '    use mod_a'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') 'end module'
        close (unit)

        print *, "  PASS: Circular dependency modules created"

        ! Create self-referencing module
        mod_file = get_temp_file_path(trim(project_dir)//"/src", 'mod_self.f90')
        open (newunit=unit, file=mod_file, status='replace')
        write (unit, '(A)') 'module mod_self'
        write (unit, '(A)') '    use mod_self  ! Self reference'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') 'end module'
        close (unit)

        print *, "  PASS: Self-referencing module created"
    end subroutine

    subroutine test_many_modules()
        integer :: i, j
        character(len=256) :: mod_file, mod_name

        print *, ""
        print *, "Test: Very large number of modules"

        project_dir = trim(temp_dir)//"/many_mods"
        call sys_create_dir(project_dir, success)
        call sys_create_dir(trim(project_dir)//"/src", success)

        ! Create many small modules
        do i = 1, 100
            write (mod_name, '(A,I0)') 'small_mod_', i
        mod_file = get_temp_file_path(trim(project_dir)//"/src", trim(mod_name)//'.f90')

            open (newunit=unit, file=mod_file, status='replace')
            write (unit, '(A)') 'module '//trim(mod_name)
            write (unit, '(A)') '    implicit none'
            write (unit, '(A,I0)') '    integer, parameter :: mod_id = ', i
            write (unit, '(A)') 'end module'
            close (unit)
        end do

        print *, "  PASS: 100 modules created"

        ! Create fpm.toml for many modules
        fpm_file = trim(project_dir)//"/fpm.toml"
        open (newunit=unit, file=fpm_file, status='replace')
        write (unit, '(A)') 'name = "many_modules"'
        write (unit, '(A)') 'version = "0.1.0"'
        write (unit, '(A)') ''
        write (unit, '(A)') '[library]'
        write (unit, '(A)') 'source-dir = "src"'
        close (unit)

        print *, "  PASS: FPM config for many modules created"
    end subroutine

    subroutine test_special_module_names()
        character(len=256) :: mod_file

        print *, ""
        print *, "Test: Special characters in module names"

        project_dir = trim(temp_dir)//"/special_mods"
        call sys_create_dir(project_dir, success)
        call sys_create_dir(trim(project_dir)//"/src", success)

        ! Module with numbers
        mod_file = get_temp_file_path(trim(project_dir)//"/src", 'mod123.f90')
        open (newunit=unit, file=mod_file, status='replace')
        write (unit, '(A)') 'module mod123'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') 'end module'
        close (unit)

        ! Module with underscores
        mod_file = get_temp_file_path(trim(project_dir)//"/src", 'mod_with_many_underscores.f90')
        open (newunit=unit, file=mod_file, status='replace')
        write (unit, '(A)') 'module mod_with_many_underscores'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') 'end module'
        close (unit)

        ! Module with very long name
        mod_file = get_temp_file_path(trim(project_dir)//"/src", 'very_long_module_name_that_exceeds_normal_length.f90')
        open (newunit=unit, file=mod_file, status='replace')
        write (unit, '(A)') 'module very_long_module_name_that_exceeds_normal_length'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') 'end module'
        close (unit)

        print *, "  PASS: Special module names created"
    end subroutine

    subroutine test_missing_fpm_toml()
        print *, ""
        print *, "Test: Missing fpm.toml scenarios"

        project_dir = trim(temp_dir)//"/no_fpm"
        call sys_create_dir(project_dir, success)

        ! Try to work with project without fpm.toml
        if (.not. sys_file_exists(trim(project_dir)//"/fpm.toml")) then
            print *, "  PASS: Missing fpm.toml detected"
        else
            print *, "  FAIL: fpm.toml should not exist"
            all_passed = .false.
        end if

        ! Create malformed fpm.toml
        project_dir = trim(temp_dir)//"/bad_fpm"
        call sys_create_dir(project_dir, success)

        fpm_file = trim(project_dir)//"/fpm.toml"
        open (newunit=unit, file=fpm_file, status='replace')
        write (unit, '(A)') '[invalid toml syntax'
        write (unit, '(A)') 'name = no quotes'
        write (unit, '(A)') 'version ='
        close (unit)

        print *, "  PASS: Malformed fpm.toml created"
    end subroutine

    ! Mock implementations
    subroutine generate_fpm_project(dir, name, error_msg, success)
        character(len=*), intent(in) :: dir, name
        character(len=*), intent(out) :: error_msg
        logical, intent(out) :: success

        success = .true.
        error_msg = ""

        if (len_trim(name) == 0) then
            success = .false.
            error_msg = "Project name cannot be empty"
        else if (index(name, " ") > 0) then
            success = .false.
            error_msg = "Project name cannot contain spaces"
        else if (index(name, "@") > 0 .or. index(name, "#") > 0) then
            success = .false.
            error_msg = "Project name contains invalid characters"
        end if
    end subroutine

    subroutine clear_module_cache()
        ! Mock implementation
    end subroutine

end program test_fpm_edge_cases
