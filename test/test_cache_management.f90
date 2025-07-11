program test_cache_management
  use cache
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  
  print *, '=== Cache Management Tests ===\'
  
  ! Test 1: Cache structure creation
  call test_cache_structure_creation()
  
  ! Test 2: Cache subdirectory paths
  call test_cache_subdirectory_paths()
  
  print *, 'All cache management tests passed!'
  
contains

  subroutine test_cache_structure_creation()
    character(len=256) :: test_cache_dir
    logical :: success
    
    print *, 'Test 1: Cache structure creation'
    
    test_cache_dir = '/tmp/fortran_cache_mgmt_test'
    
    ! Create structured cache directory
    call ensure_cache_structure(test_cache_dir, success)
    
    if (.not. success) then
      write(error_unit, *) 'Error: Failed to create cache structure'
      stop 1
    end if
    
    ! Verify all subdirectories exist
    call check_directory_exists(trim(test_cache_dir) // '/builds')
    call check_directory_exists(trim(test_cache_dir) // '/modules')
    call check_directory_exists(trim(test_cache_dir) // '/executables')
    call check_directory_exists(trim(test_cache_dir) // '/metadata')
    
    ! Clean up
    call execute_command_line('rm -rf ' // trim(test_cache_dir))
    
    print *, 'PASS: Cache structure created successfully'
    print *
    
  end subroutine test_cache_structure_creation

  subroutine test_cache_subdirectory_paths()
    character(len=256) :: cache_dir
    character(len=512) :: builds_dir, modules_dir, executables_dir, metadata_dir
    
    print *, 'Test 2: Cache subdirectory paths'
    
    cache_dir = '/tmp/test_cache'
    
    ! Get subdirectory paths
    builds_dir = get_cache_subdir(cache_dir, 'builds')
    modules_dir = get_cache_subdir(cache_dir, 'modules')
    executables_dir = get_cache_subdir(cache_dir, 'executables')
    metadata_dir = get_cache_subdir(cache_dir, 'metadata')
    
    ! Verify paths are correct
    if (trim(builds_dir) /= '/tmp/test_cache/builds') then
      write(error_unit, *) 'Error: Incorrect builds directory path: ', trim(builds_dir)
      stop 1
    end if
    
    if (trim(modules_dir) /= '/tmp/test_cache/modules') then
      write(error_unit, *) 'Error: Incorrect modules directory path: ', trim(modules_dir)
      stop 1
    end if
    
    if (trim(executables_dir) /= '/tmp/test_cache/executables') then
      write(error_unit, *) 'Error: Incorrect executables directory path: ', trim(executables_dir)
      stop 1
    end if
    
    if (trim(metadata_dir) /= '/tmp/test_cache/metadata') then
      write(error_unit, *) 'Error: Incorrect metadata directory path: ', trim(metadata_dir)
      stop 1
    end if
    
    print *, 'PASS: Cache subdirectory paths correct'
    print *
    
  end subroutine test_cache_subdirectory_paths

  subroutine check_directory_exists(dir_path)
    character(len=*), intent(in) :: dir_path
    logical :: exists
    
    inquire(file=trim(dir_path), exist=exists)
    
    if (.not. exists) then
      write(error_unit, *) 'Error: Directory does not exist: ', trim(dir_path)
      stop 1
    end if
    
  end subroutine check_directory_exists

end program test_cache_management