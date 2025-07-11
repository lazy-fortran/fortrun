program test_registry_resolver
  use registry_resolver, only: resolve_module_to_package, load_registry_from_path
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  
  character(len=128) :: package_name, git_url
  logical :: found
  
  print *, '=== Registry Resolver Tests ==='
  print *
  
  ! Create a test registry in current directory
  call create_test_registry()
  
  ! Load the registry from the test file
  call load_registry_from_path('test_registry.toml')
  
  ! Test 1: Resolve module with prefix (fortplot)
  print *, 'Test 1: Module with prefix (fortplot)'
  call resolve_module_to_package('fortplot', package_name, git_url, found)
  
  if (.not. found) then
    write(error_unit, *) 'FAIL: fortplot module not resolved'
    stop 1
  end if
  
  if (trim(package_name) /= 'fortplotlib') then
    write(error_unit, *) 'FAIL: Expected fortplotlib, got ', trim(package_name)
    stop 1
  end if
  
  print *, 'PASS: fortplot -> fortplotlib'
  
  ! Test 2: Module with prefix (fortplot_utils)
  print *
  print *, 'Test 2: Module with prefix (fortplot_utils)'
  call resolve_module_to_package('fortplot_utils', package_name, git_url, found)
  
  if (.not. found) then
    write(error_unit, *) 'FAIL: fortplot_utils module not resolved'
    stop 1
  end if
  
  if (trim(package_name) /= 'fortplotlib') then
    write(error_unit, *) 'FAIL: Expected fortplotlib, got ', trim(package_name)
    stop 1
  end if
  
  print *, 'PASS: fortplot_utils -> fortplotlib (via prefix)'
  
  ! Test 3: Module with underscore inference
  print *
  print *, 'Test 3: Module with underscore (pyplot_module)'
  call resolve_module_to_package('pyplot_module', package_name, git_url, found)
  
  if (.not. found) then
    write(error_unit, *) 'FAIL: pyplot_module not resolved'
    stop 1
  end if
  
  if (trim(package_name) /= 'pyplot-fortran') then
    write(error_unit, *) 'FAIL: Expected pyplot-fortran, got ', trim(package_name)
    stop 1
  end if
  
  print *, 'PASS: pyplot_module -> pyplot-fortran'
  
  ! Test 4: Unknown module
  print *
  print *, 'Test 4: Unknown module'
  call resolve_module_to_package('unknown_module', package_name, git_url, found)
  
  if (found) then
    write(error_unit, *) 'FAIL: unknown_module should not be found'
    stop 1
  end if
  
  print *, 'PASS: Unknown module correctly not found'
  
  ! Test 5: Git URL retrieval
  print *
  print *, 'Test 5: Git URL retrieval'
  call resolve_module_to_package('fortplot', package_name, git_url, found)
  
  if (index(git_url, 'github.com/krystophny/fortplotlib') == 0) then
    write(error_unit, *) 'FAIL: Incorrect git URL: ', trim(git_url)
    stop 1
  end if
  
  print *, 'PASS: Correct git URL retrieved'
  
  print *
  print *, 'All registry resolver tests passed!'
  
  ! Clean up test registry
  call execute_command_line('rm -f test_registry.toml')
  
contains

  subroutine create_test_registry()
    integer :: unit
    
    open(newunit=unit, file='test_registry.toml', status='replace')
    write(unit, '(a)') '# Test registry'
    write(unit, '(a)') '[packages]'
    write(unit, '(a)') ''
    write(unit, '(a)') '[packages.fortplotlib]'
    write(unit, '(a)') 'git = "https://github.com/krystophny/fortplotlib"'
    write(unit, '(a)') 'prefix = "fortplot"'
    write(unit, '(a)') ''
    write(unit, '(a)') '[packages.pyplot-fortran]'
    write(unit, '(a)') 'git = "https://github.com/jacobwilliams/pyplot-fortran"'
    close(unit)
  end subroutine create_test_registry
  
end program test_registry_resolver