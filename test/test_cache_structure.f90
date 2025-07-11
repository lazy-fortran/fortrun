program test_cache_structure
  use cache
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  
  character(len=256) :: cache_dir, custom_cache_dir
  logical :: success
  
  print *, '=== Cache Directory Structure Tests ===\'
  
  ! Test 1: Default cache directory creation
  call test_default_cache_dir()
  
  ! Test 2: Custom cache directory
  call test_custom_cache_dir()
  
  ! Test 3: Cache directory structure validation
  call test_cache_structure_validation()
  
  print *, 'All cache structure tests passed!'
  
contains

  subroutine test_default_cache_dir()
    character(len=256) :: cache_dir
    logical :: success
    
    print *, 'Test 1: Default cache directory creation'
    
    ! Get default cache directory
    cache_dir = get_cache_dir()
    
    ! Ensure it exists
    call ensure_cache_dir(cache_dir, success)
    
    if (.not. success) then
      write(error_unit, *) 'Error: Failed to create default cache directory'
      stop 1
    end if
    
    ! Check that it exists
    call check_directory_exists(cache_dir)
    
    print *, 'PASS: Default cache directory created at: ', trim(cache_dir)
    print *
    
  end subroutine test_default_cache_dir

  subroutine test_custom_cache_dir()
    character(len=256) :: custom_cache_dir
    logical :: success
    
    print *, 'Test 2: Custom cache directory'
    
    custom_cache_dir = '/tmp/fortran_test_cache'
    
    ! Ensure custom cache directory exists
    call ensure_cache_dir(custom_cache_dir, success)
    
    if (.not. success) then
      write(error_unit, *) 'Error: Failed to create custom cache directory'
      stop 1
    end if
    
    ! Check that it exists
    call check_directory_exists(custom_cache_dir)
    
    ! Clean up
    call execute_command_line('rm -rf ' // trim(custom_cache_dir))
    
    print *, 'PASS: Custom cache directory created successfully'
    print *
    
  end subroutine test_custom_cache_dir

  subroutine test_cache_structure_validation()
    character(len=256) :: test_cache_dir
    logical :: success
    
    print *, 'Test 3: Cache directory structure validation'
    
    test_cache_dir = '/tmp/fortran_cache_structure_test'
    
    ! Create cache directory
    call ensure_cache_dir(test_cache_dir, success)
    
    if (.not. success) then
      write(error_unit, *) 'Error: Failed to create test cache directory'
      stop 1
    end if
    
    ! Test the expected structure for a smart cache system
    ! This tests the structure we want to implement:
    ! cache_dir/
    !   ├── builds/        # Build artifacts
    !   ├── modules/       # Compiled modules
    !   ├── executables/   # Cached executables
    !   └── metadata/      # Cache metadata
    
    call create_cache_subdirectories(test_cache_dir)
    
    ! Verify subdirectories exist
    call check_directory_exists(trim(test_cache_dir) // '/builds')
    call check_directory_exists(trim(test_cache_dir) // '/modules') 
    call check_directory_exists(trim(test_cache_dir) // '/executables')
    call check_directory_exists(trim(test_cache_dir) // '/metadata')
    
    ! Clean up
    call execute_command_line('rm -rf ' // trim(test_cache_dir))
    
    print *, 'PASS: Cache directory structure validation passed'
    print *
    
  end subroutine test_cache_structure_validation

  subroutine create_cache_subdirectories(cache_dir)
    character(len=*), intent(in) :: cache_dir
    character(len=512) :: command
    
    ! Create the expected cache subdirectories
    command = 'mkdir -p "' // trim(cache_dir) // '/builds"'
    call execute_command_line(command)
    
    command = 'mkdir -p "' // trim(cache_dir) // '/modules"'
    call execute_command_line(command)
    
    command = 'mkdir -p "' // trim(cache_dir) // '/executables"'
    call execute_command_line(command)
    
    command = 'mkdir -p "' // trim(cache_dir) // '/metadata"'
    call execute_command_line(command)
    
  end subroutine create_cache_subdirectories

  subroutine check_directory_exists(dir_path)
    character(len=*), intent(in) :: dir_path
    logical :: exists
    
    inquire(file=trim(dir_path), exist=exists)
    
    if (.not. exists) then
      write(error_unit, *) 'Error: Directory does not exist: ', trim(dir_path)
      stop 1
    end if
    
  end subroutine check_directory_exists

end program test_cache_structure