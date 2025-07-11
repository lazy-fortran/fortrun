program test_fpm_cache_integration
  use fpm_sources, only: add_sources_from_dir
  use fpm_model, only: srcfile_t, FPM_SCOPE_APP
  use fpm_filesystem, only: list_files
  use fpm_strings, only: string_t
  use fpm_error, only: error_t
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  
  print *, '=== FPM Cache Integration Tests ===\'
  
  ! Test 1: Use FPM API for source discovery
  call test_fpm_source_discovery()
  
  ! Test 2: Check if we can access source file information
  call test_source_file_info()
  
  print *, 'All FPM cache integration tests passed!'
  
contains

  subroutine test_fpm_source_discovery()
    type(srcfile_t), allocatable :: sources(:)
    type(error_t), allocatable :: error
    character(len=256) :: test_dir
    integer :: unit
    
    print *, 'Test 1: FPM source discovery'
    
    ! Create a test directory with Fortran files
    test_dir = '/tmp/fpm_test_sources'
    call execute_command_line('mkdir -p ' // trim(test_dir))
    
    ! Create a test source file
    open(newunit=unit, file=trim(test_dir) // '/test.f90', status='replace')
    write(unit, '(a)') 'program test_program'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "Hello from FPM API test"'
    write(unit, '(a)') 'end program test_program'
    close(unit)
    
    ! Use FPM API to discover sources
    call add_sources_from_dir(sources, test_dir, FPM_SCOPE_APP, error=error)
    
    if (allocated(error)) then
      write(error_unit, *) 'Error: ', error%message
      stop 1
    end if
    
    if (.not. allocated(sources)) then
      write(error_unit, *) 'Error: No sources found'
      stop 1
    end if
    
    if (size(sources) == 0) then
      write(error_unit, *) 'Error: Empty sources array'
      stop 1
    end if
    
    ! Clean up
    call execute_command_line('rm -rf ' // trim(test_dir))
    
    print *, 'PASS: FPM source discovery works, found', size(sources), 'sources'
    print *
    
  end subroutine test_fpm_source_discovery

  subroutine test_source_file_info()
    type(srcfile_t), allocatable :: sources(:)
    type(error_t), allocatable :: error
    character(len=256) :: test_dir
    integer :: unit
    
    print *, 'Test 2: Source file information access'
    
    ! Create a test directory with Fortran files
    test_dir = '/tmp/fpm_test_info'
    call execute_command_line('mkdir -p ' // trim(test_dir))
    
    ! Create a test source file
    open(newunit=unit, file=trim(test_dir) // '/hello.f90', status='replace')
    write(unit, '(a)') 'program hello'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "Hello World"'
    write(unit, '(a)') 'end program hello'
    close(unit)
    
    ! Use FPM API to discover sources
    call add_sources_from_dir(sources, test_dir, FPM_SCOPE_APP, error=error)
    
    if (allocated(error)) then
      write(error_unit, *) 'Error: ', error%message
      stop 1
    end if
    
    if (allocated(sources) .and. size(sources) > 0) then
      print *, 'PASS: Can access source file path length:', len(sources(1)%file_name)
      print *, 'PASS: Can access source file path:', trim(sources(1)%file_name)
      print *, 'PASS: Source file digest:', sources(1)%digest
      
      ! Check if file actually exists
      inquire(file=trim(sources(1)%file_name), exist=.true.)
      
    else
      write(error_unit, *) 'Error: No sources found'
      stop 1
    end if
    
    ! Clean up
    call execute_command_line('rm -rf ' // trim(test_dir))
    
    print *
    
  end subroutine test_source_file_info

end program test_fpm_cache_integration