program test_module_scanner
  use module_scanner, only: scan_modules, module_info
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  
  type(module_info), dimension(:), allocatable :: modules
  character(len=256) :: test_file
  integer :: n_modules, i
  logical :: found_fortplot, found_pyplot
  
  print *, '=== Module Scanner Tests ==='
  print *
  
  ! Test 1: Simple module use
  print *, 'Test 1: Simple module detection'
  test_file = 'test_simple_modules.f90'
  call create_test_file_simple(test_file)
  
  call scan_modules(test_file, modules, n_modules)
  
  if (n_modules /= 2) then
    write(error_unit, *) 'FAIL: Expected 2 modules, found', n_modules
    stop 1
  end if
  
  found_fortplot = .false.
  found_pyplot = .false.
  
  do i = 1, n_modules
    if (trim(modules(i)%name) == 'fortplot') found_fortplot = .true.
    if (trim(modules(i)%name) == 'pyplot_module') found_pyplot = .true.
  end do
  
  if (.not. found_fortplot) then
    write(error_unit, *) 'FAIL: Module fortplot not found'
    stop 1
  end if
  
  if (.not. found_pyplot) then
    write(error_unit, *) 'FAIL: Module pyplot_module not found'
    stop 1
  end if
  
  print *, 'PASS: Found both modules'
  
  ! Clean up
  call execute_command_line('rm -f ' // trim(test_file))
  
  ! Test 2: Module with only clause
  print *
  print *, 'Test 2: Module with only clause'
  test_file = 'test_only_modules.f90'
  call create_test_file_only(test_file)
  
  call scan_modules(test_file, modules, n_modules)
  
  if (n_modules /= 1) then
    write(error_unit, *) 'FAIL: Expected 1 module, found', n_modules
    stop 1
  end if
  
  if (trim(modules(1)%name) /= 'stdlib_io') then
    write(error_unit, *) 'FAIL: Expected stdlib_io, found ', trim(modules(1)%name)
    stop 1
  end if
  
  print *, 'PASS: Correctly extracted module from only clause'
  
  ! Clean up
  call execute_command_line('rm -f ' // trim(test_file))
  
  ! Test 3: Intrinsic modules (should be ignored)
  print *
  print *, 'Test 3: Intrinsic module filtering'
  test_file = 'test_intrinsic_modules.f90'
  call create_test_file_intrinsic(test_file)
  
  call scan_modules(test_file, modules, n_modules)
  
  if (n_modules /= 1) then
    write(error_unit, *) 'FAIL: Expected 1 non-intrinsic module, found', n_modules
    stop 1
  end if
  
  if (trim(modules(1)%name) /= 'my_module') then
    write(error_unit, *) 'FAIL: Expected my_module, found ', trim(modules(1)%name)
    stop 1
  end if
  
  print *, 'PASS: Intrinsic modules correctly filtered'
  
  ! Clean up
  call execute_command_line('rm -f ' // trim(test_file))
  
  print *
  print *, 'All module scanner tests passed!'
  
contains

  subroutine create_test_file_simple(filename)
    character(len=*), intent(in) :: filename
    integer :: unit
    
    open(newunit=unit, file=filename, status='replace')
    write(unit, '(a)') 'program test_plot'
    write(unit, '(a)') '  use fortplot'
    write(unit, '(a)') '  use pyplot_module'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  '
    write(unit, '(a)') '  ! Plot something'
    write(unit, '(a)') 'end program test_plot'
    close(unit)
  end subroutine create_test_file_simple
  
  subroutine create_test_file_only(filename)
    character(len=*), intent(in) :: filename
    integer :: unit
    
    open(newunit=unit, file=filename, status='replace')
    write(unit, '(a)') 'program test_only'
    write(unit, '(a)') '  use stdlib_io, only: open, close'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') 'end program test_only'
    close(unit)
  end subroutine create_test_file_only
  
  subroutine create_test_file_intrinsic(filename)
    character(len=*), intent(in) :: filename
    integer :: unit
    
    open(newunit=unit, file=filename, status='replace')
    write(unit, '(a)') 'program test_intrinsic'
    write(unit, '(a)') '  use, intrinsic :: iso_fortran_env'
    write(unit, '(a)') '  use, intrinsic :: iso_c_binding'
    write(unit, '(a)') '  use my_module'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') 'end program test_intrinsic'
    close(unit)
  end subroutine create_test_file_intrinsic
  
end program test_module_scanner