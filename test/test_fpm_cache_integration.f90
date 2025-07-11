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
    character(len=:), allocatable :: test_dir
    character(len=:), allocatable :: test_file
    integer :: unit
    
    print *, 'Test 1: FPM source discovery'
    
    ! Create a test directory with Fortran files (FPM expects app/ subdirectory)
    test_dir = '/tmp/fpm_test'
    test_file = test_dir // '/app/test.f90'
    print *, 'Creating test directory: ', test_dir
    call execute_command_line('rm -rf ' // test_dir // ' && mkdir -p ' // test_dir // '/app')
    
    ! Create a test source file in app/ subdirectory
    print *, 'Creating file: ', test_file
    open(newunit=unit, file=test_file, status='replace')
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
    logical :: exists
    
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
      inquire(file=trim(sources(1)%file_name), exist=exists)
      
    else
      write(error_unit, *) 'Error: No sources found'
      stop 1
    end if
    
    ! Clean up
    call execute_command_line('rm -rf ' // trim(test_dir))
    
    print *
    
  end subroutine test_source_file_info

  function get_timestamp() result(timestamp)
    character(len=20) :: timestamp
    integer :: values(8)
    
    call date_and_time(values=values)
    write(timestamp, '(i4.4,5i2.2)') values(1), values(2), values(3), &
                                      values(5), values(6), values(7)
  end function get_timestamp

end program test_fpm_cache_integration