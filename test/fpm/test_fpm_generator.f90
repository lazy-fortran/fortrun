program test_fpm_generator
    use fpm_generator, only: generate_fpm_with_deps
    use module_scanner, only: scan_modules, module_info
    use temp_utils, only: mkdir, path_join
    use system_utils, only: sys_remove_dir, sys_remove_file
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=256) :: test_file, output_dir, fpm_file
    type(module_info), dimension(:), allocatable :: modules
    integer :: n_modules, unit, iostat
    character(len=512) :: line
    logical :: found_deps, found_pyplot, success

    print *, '=== FPM Generator Tests ==='
    print *

    ! Test 1: Generate fpm.toml with dependencies
    print *, 'Test 1: Generate fpm.toml with pyplot dependency'

    test_file = 'test_pyplot_example.f90'
    output_dir = './test_fpm_gen'

    ! Create test directory
    call sys_remove_dir(output_dir, success)
    call mkdir(trim(output_dir))

    ! Create test file that uses pyplot
    call create_pyplot_example(test_file)

    ! Scan modules
    call scan_modules(test_file, modules, n_modules)

    ! Generate fpm.toml
 call generate_fpm_with_deps(output_dir, 'test_pyplot', modules, n_modules, .false., '')

    ! Check generated fpm.toml
    fpm_file = path_join(output_dir, 'fpm.toml')
    open (newunit=unit, file=fpm_file, status='old', iostat=iostat)
    if (iostat /= 0) then
        write (error_unit, *) 'FAIL: fpm.toml not created'
        stop 1
    end if

    found_deps = .false.
    found_pyplot = .false.

    do
        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit

        if (index(line, '[dependencies]') > 0) found_deps = .true.
        if (index(line, 'pyplot-fortran') > 0) found_pyplot = .true.
    end do

    close (unit)

    if (.not. found_deps) then
        write (error_unit, *) 'FAIL: No [dependencies] section found'
        stop 1
    end if

    if (.not. found_pyplot) then
        write (error_unit, *) 'FAIL: pyplot-fortran dependency not found'
        stop 1
    end if

    print *, 'PASS: Generated fpm.toml with correct dependencies'

    ! Clean up
    call sys_remove_file(test_file)
    call sys_remove_dir(output_dir, success)

    ! Test 2: Multiple dependencies
    print *
    print *, 'Test 2: Multiple dependencies'

    test_file = 'test_multi_deps.f90'
    call create_multi_deps_example(test_file)

    call mkdir(trim(output_dir))
    call scan_modules(test_file, modules, n_modules)
  call generate_fpm_with_deps(output_dir, 'test_multi', modules, n_modules, .false., '')

    ! Verify both dependencies are present
    open (newunit=unit, file=fpm_file, status='old', iostat=iostat)

    found_deps = .false.
    found_pyplot = .false.

    do
        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit

        if (index(line, 'fortplot') > 0) found_deps = .true.
        if (index(line, 'pyplot-fortran') > 0) found_pyplot = .true.
    end do

    close (unit)

    if (.not. found_deps .or. .not. found_pyplot) then
        write (error_unit, *) 'FAIL: Not all dependencies found'
        stop 1
    end if

    print *, 'PASS: Multiple dependencies correctly added'

    ! Clean up
    call sys_remove_file(test_file)
    call sys_remove_dir(output_dir, success)

    print *
    print *, 'All FPM generator tests passed!'

contains

    subroutine create_pyplot_example(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') 'program pyplot_example'
        write (unit, '(a)') '  use pyplot_module'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  '
        write (unit, '(a)') '  ! Plot something'
        write (unit, '(a)') 'end program pyplot_example'
        close (unit)
    end subroutine create_pyplot_example

    subroutine create_multi_deps_example(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') 'program multi_deps'
        write (unit, '(a)') '  use fortplot'
        write (unit, '(a)') '  use pyplot_module'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') 'end program multi_deps'
        close (unit)
    end subroutine create_multi_deps_example

end program test_fpm_generator
