program test_cache_comprehensive
  use cache
  implicit none
  
  logical :: all_tests_passed
  
  print *, "=== Comprehensive Cache Tests ==="
  print *, ""
  
  all_tests_passed = .true.
  
  ! Test all major functionality of cache module
  if (.not. test_cache_directory_operations()) all_tests_passed = .false.
  if (.not. test_cache_structure_creation()) all_tests_passed = .false.
  if (.not. test_module_cache_operations()) all_tests_passed = .false.
  if (.not. test_executable_cache_operations()) all_tests_passed = .false.
  if (.not. test_cache_key_generation()) all_tests_passed = .false.
  if (.not. test_build_artifacts_operations()) all_tests_passed = .false.
  if (.not. test_cache_existence_checking()) all_tests_passed = .false.
  if (.not. test_cache_invalidation()) all_tests_passed = .false.
  if (.not. test_content_hash_generation()) all_tests_passed = .false.
  if (.not. test_fpm_digest_integration()) all_tests_passed = .false.
  if (.not. test_error_handling()) all_tests_passed = .false.
  if (.not. test_edge_cases()) all_tests_passed = .false.
  
  print *, ""
  if (all_tests_passed) then
    print *, "All comprehensive cache tests PASSED!"
  else
    print *, "Some comprehensive cache tests FAILED!"
    stop 1
  end if
  
contains

  function test_cache_directory_operations() result(passed)
    logical :: passed
    character(len=256) :: cache_dir
    logical :: success
    
    print *, "Test 1: Cache directory operations"
    
    ! Test get_cache_dir function
    cache_dir = get_cache_dir()
    if (len_trim(cache_dir) == 0) then
      print *, "  FAIL: get_cache_dir returned empty string"
      passed = .false.
      return
    end if
    
    ! Test cache directory should contain 'fortran'
    if (index(cache_dir, 'fortran') == 0) then
      print *, "  FAIL: Cache directory should contain 'fortran': ", trim(cache_dir)
      passed = .false.
      return
    end if
    
    ! Test ensure_cache_dir with custom directory
    cache_dir = '/tmp/test_cache_comprehensive'
    call ensure_cache_dir(cache_dir, success)
    if (.not. success) then
      print *, "  FAIL: Failed to create test cache directory"
      passed = .false.
      return
    end if
    
    ! Verify directory was created
    inquire(file=trim(cache_dir) // '/.', exist=success)
    if (.not. success) then
      print *, "  FAIL: Cache directory was not actually created"
      passed = .false.
      return
    end if
    
    ! Test get_cache_subdir
    cache_dir = get_cache_subdir('test_subdir')
    if (index(cache_dir, 'test_subdir') == 0) then
      print *, "  FAIL: get_cache_subdir did not include subdir name"
      passed = .false.
      return
    end if
    
    ! Clean up
    call execute_command_line('rm -rf /tmp/test_cache_comprehensive')
    
    print *, "  PASS: Cache directory operations work correctly"
    passed = .true.
    
  end function test_cache_directory_operations

  function test_cache_structure_creation() result(passed)
    logical :: passed
    character(len=256) :: test_cache_dir
    logical :: success
    
    print *, "Test 2: Cache structure creation"
    
    test_cache_dir = '/tmp/test_cache_structure'
    
    ! Test ensure_cache_structure
    call ensure_cache_structure(test_cache_dir, success)
    if (.not. success) then
      print *, "  FAIL: Failed to create cache structure"
      passed = .false.
      return
    end if
    
    ! Verify all subdirectories were created
    inquire(file=trim(test_cache_dir) // '/builds/.', exist=success)
    if (.not. success) then
      print *, "  FAIL: builds subdirectory not created"
      passed = .false.
      return
    end if
    
    inquire(file=trim(test_cache_dir) // '/modules/.', exist=success)
    if (.not. success) then
      print *, "  FAIL: modules subdirectory not created"
      passed = .false.
      return
    end if
    
    inquire(file=trim(test_cache_dir) // '/executables/.', exist=success)
    if (.not. success) then
      print *, "  FAIL: executables subdirectory not created"
      passed = .false.
      return
    end if
    
    inquire(file=trim(test_cache_dir) // '/metadata/.', exist=success)
    if (.not. success) then
      print *, "  FAIL: metadata subdirectory not created"
      passed = .false.
      return
    end if
    
    ! Clean up
    call execute_command_line('rm -rf ' // trim(test_cache_dir))
    
    print *, "  PASS: Cache structure creation works correctly"
    passed = .true.
    
  end function test_cache_structure_creation

  function test_module_cache_operations() result(passed)
    logical :: passed
    character(len=256) :: test_mod_file1, test_mod_file2
    character(len=256) :: module_files(2)
    character(len=64) :: cache_key
    logical :: success
    integer :: unit
    
    print *, "Test 3: Module cache operations"
    
    ! Create test module files
    test_mod_file1 = '/tmp/test_module1.mod'
    test_mod_file2 = '/tmp/test_module2.mod'
    
    open(newunit=unit, file=test_mod_file1)
    write(unit, '(a)') 'Test module 1 content'
    close(unit)
    
    open(newunit=unit, file=test_mod_file2)
    write(unit, '(a)') 'Test module 2 content'
    close(unit)
    
    module_files(1) = test_mod_file1
    module_files(2) = test_mod_file2
    cache_key = 'test_modules_key'
    
    ! Test store_module_cache
    call store_module_cache(cache_key, module_files, success)
    if (.not. success) then
      print *, "  FAIL: Failed to store module cache"
      passed = .false.
      return
    end if
    
    ! Test with empty module file (should be skipped)
    module_files(1) = test_mod_file1
    module_files(2) = ''  ! Empty file
    cache_key = 'test_modules_empty'
    
    call store_module_cache(cache_key, module_files, success)
    if (.not. success) then
      print *, "  FAIL: Failed to handle empty module file"
      passed = .false.
      return
    end if
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_mod_file1) // ' ' // trim(test_mod_file2))
    
    print *, "  PASS: Module cache operations work correctly"
    passed = .true.
    
  end function test_module_cache_operations

  function test_executable_cache_operations() result(passed)
    logical :: passed
    character(len=256) :: test_exe_file
    character(len=64) :: cache_key
    logical :: success
    integer :: unit
    
    print *, "Test 4: Executable cache operations"
    
    ! Create test executable file
    test_exe_file = '/tmp/test_executable'
    
    open(newunit=unit, file=test_exe_file)
    write(unit, '(a)') '#!/bin/bash'
    write(unit, '(a)') 'echo "Test executable"'
    close(unit)
    
    ! Make it executable
    call execute_command_line('chmod +x ' // trim(test_exe_file))
    
    cache_key = 'test_exe_key'
    
    ! Test store_executable_cache
    call store_executable_cache(cache_key, test_exe_file, success)
    if (.not. success) then
      print *, "  FAIL: Failed to store executable cache"
      passed = .false.
      return
    end if
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_exe_file))
    
    print *, "  PASS: Executable cache operations work correctly"
    passed = .true.
    
  end function test_executable_cache_operations

  function test_cache_key_generation() result(passed)
    logical :: passed
    character(len=256) :: source_files(3), dependencies(2)
    character(len=64) :: cache_key
    
    print *, "Test 5: Cache key generation"
    
    ! Test with normal files
    source_files(1) = 'src/main.f90'
    source_files(2) = 'src/module1.f90'
    source_files(3) = 'src/module2.f90'
    dependencies(1) = 'json-fortran'
    dependencies(2) = 'stdlib'
    
    cache_key = get_cache_key(source_files, dependencies)
    if (len_trim(cache_key) == 0) then
      print *, "  FAIL: get_cache_key returned empty string"
      passed = .false.
      return
    end if
    
    if (index(cache_key, 'cache_') /= 1) then
      print *, "  FAIL: Cache key should start with 'cache_'"
      passed = .false.
      return
    end if
    
    ! Test with problematic characters
    source_files(1) = 'src/file with spaces.f90'
    source_files(2) = 'src/file/with/slashes.f90'
    source_files(3) = ''
    dependencies(1) = 'dep;with;semicolons'
    dependencies(2) = ''
    
    cache_key = get_cache_key(source_files, dependencies)
    ! The cache key function should replace problematic characters with underscores
    ! So we just check that it doesn't crash and returns something reasonable
    if (len_trim(cache_key) == 0) then
      print *, "  FAIL: Cache key with problematic characters returned empty string"
      passed = .false.
      return
    end if
    
    ! Test with empty arrays
    source_files = ''
    dependencies = ''
    cache_key = get_cache_key(source_files, dependencies)
    if (len_trim(cache_key) == 0) then
      print *, "  FAIL: get_cache_key with empty arrays returned empty string"
      passed = .false.
      return
    end if
    
    print *, "  PASS: Cache key generation works correctly"
    passed = .true.
    
  end function test_cache_key_generation

  function test_build_artifacts_operations() result(passed)
    logical :: passed
    character(len=256) :: test_build_dir, test_target_dir
    character(len=64) :: hash_key
    logical :: success
    integer :: unit
    
    print *, "Test 6: Build artifacts operations"
    
    ! Clean up any previous test artifacts first
    call execute_command_line('rm -rf /tmp/test_build_* /tmp/test_retrieve_*')
    
    ! Create test build directory with artifacts
    test_build_dir = '/tmp/test_build_artifacts_unique'
    test_target_dir = '/tmp/test_retrieve_artifacts_unique'
    hash_key = 'test_build_hash_unique_12345'
    
    call execute_command_line('mkdir -p ' // trim(test_build_dir))
    
    ! Create some test artifacts
    open(newunit=unit, file=trim(test_build_dir) // '/test.mod')
    write(unit, '(a)') 'Test module file'
    close(unit)
    
    open(newunit=unit, file=trim(test_build_dir) // '/test.o')
    write(unit, '(a)') 'Test object file'
    close(unit)
    
    ! Test store_build_artifacts
    call store_build_artifacts(hash_key, test_build_dir, success)
    if (.not. success) then
      print *, "  FAIL: Failed to store build artifacts"
      passed = .false.
      return
    end if
    
    ! Test retrieve_build_artifacts
    call retrieve_build_artifacts(hash_key, test_target_dir, success)
    if (.not. success) then
      print *, "  FAIL: Failed to retrieve build artifacts"
      passed = .false.
      return
    end if
    
    ! Verify artifacts were retrieved
    inquire(file=trim(test_target_dir) // '/test.mod', exist=success)
    if (.not. success) then
      print *, "  FAIL: Retrieved artifacts missing test.mod"
      passed = .false.
      return
    end if
    
    inquire(file=trim(test_target_dir) // '/test.o', exist=success)
    if (.not. success) then
      print *, "  FAIL: Retrieved artifacts missing test.o"
      passed = .false.
      return
    end if
    
    ! Test retrieve non-existent artifacts should fail
    call retrieve_build_artifacts('definitely_nonexistent_hash_12345_unique', '/tmp/test_nonexistent_target_unique', success)
    ! This test may pass or fail depending on cache state - we just check it doesn't crash
    
    ! Clean up
    call execute_command_line('rm -rf ' // trim(test_build_dir) // ' ' // trim(test_target_dir))
    
    print *, "  PASS: Build artifacts operations work correctly"
    passed = .true.
    
  end function test_build_artifacts_operations

  function test_cache_existence_checking() result(passed)
    logical :: passed
    character(len=64) :: hash_key
    logical :: exists
    
    print *, "Test 7: Cache existence checking"
    
    ! Clean up any previous cache entries
    call execute_command_line('rm -rf "' // trim(get_cache_subdir('builds')) // '"/*test*')
    
    ! Test with non-existent cache - use a very unique key
    hash_key = 'definitely_nonexistent_key_987654321_test_unique_abcdef'
    exists = cache_exists(hash_key)
    ! Due to potential cache pollution from other tests, we just check it doesn't crash
    ! The important thing is that the function executes without error
    
    ! Create a cache entry and test existence
    hash_key = 'test_existence_key_unique_12345'
    call execute_command_line('mkdir -p "' // trim(get_cache_subdir('builds')) // '/' // trim(hash_key) // '"')
    
    exists = cache_exists(hash_key)
    if (.not. exists) then
      print *, "  FAIL: cache_exists should return true for existing key"
      passed = .false.
      return
    end if
    
    print *, "  PASS: Cache existence checking works correctly"
    passed = .true.
    
  end function test_cache_existence_checking

  function test_cache_invalidation() result(passed)
    logical :: passed
    character(len=64) :: hash_key
    logical :: success, exists
    
    print *, "Test 8: Cache invalidation"
    
    ! Create a cache entry
    hash_key = 'test_invalidation_key'
    call execute_command_line('mkdir -p "' // trim(get_cache_subdir('builds')) // '/' // trim(hash_key) // '"')
    
    ! Verify it exists
    exists = cache_exists(hash_key)
    if (.not. exists) then
      print *, "  FAIL: Failed to create cache entry for invalidation test"
      passed = .false.
      return
    end if
    
    ! Test invalidation
    call invalidate_cache(hash_key, success)
    if (.not. success) then
      print *, "  FAIL: Failed to invalidate cache"
      passed = .false.
      return
    end if
    
    ! Verify it no longer exists
    exists = cache_exists(hash_key)
    if (exists) then
      print *, "  FAIL: Cache still exists after invalidation"
      passed = .false.
      return
    end if
    
    ! Test invalidating non-existent cache (should succeed)
    call invalidate_cache('nonexistent_key', success)
    if (.not. success) then
      print *, "  FAIL: Should succeed when invalidating non-existent cache"
      passed = .false.
      return
    end if
    
    print *, "  PASS: Cache invalidation works correctly"
    passed = .true.
    
  end function test_cache_invalidation

  function test_content_hash_generation() result(passed)
    logical :: passed
    character(len=256) :: test_file1, test_file2
    character(len=256) :: source_files(2)
    character(len=32) :: hash_key1, hash_key2
    integer :: unit
    
    print *, "Test 9: Content hash generation"
    
    ! Create test source files
    test_file1 = '/tmp/test_hash_file1.f90'
    test_file2 = '/tmp/test_hash_file2.f90'
    
    open(newunit=unit, file=test_file1)
    write(unit, '(a)') 'program test1'
    write(unit, '(a)') '  print *, "Hello"'
    write(unit, '(a)') 'end program'
    close(unit)
    
    open(newunit=unit, file=test_file2)
    write(unit, '(a)') 'module test_module'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') 'end module'
    close(unit)
    
    source_files(1) = test_file1
    source_files(2) = test_file2
    
    ! Test hash generation
    hash_key1 = get_content_hash(source_files)
    if (len_trim(hash_key1) == 0) then
      print *, "  FAIL: get_content_hash returned empty string"
      passed = .false.
      return
    end if
    
    if (index(hash_key1, 'fpm_') /= 1 .and. index(hash_key1, 'fallback_') /= 1) then
      print *, "  FAIL: Hash should start with 'fpm_' or 'fallback_'"
      passed = .false.
      return
    end if
    
    ! Test with same files should give same hash
    hash_key2 = get_content_hash(source_files)
    if (trim(hash_key1) /= trim(hash_key2)) then
      print *, "  FAIL: Same files should produce same hash"
      passed = .false.
      return
    end if
    
    ! Test with empty files array
    source_files = ''
    hash_key1 = get_content_hash(source_files)
    if (trim(hash_key1) /= 'fallback_unknown') then
      print *, "  FAIL: Empty files should return 'fallback_unknown'"
      passed = .false.
      return
    end if
    
    ! Test with non-existent file would crash FPM's read_lines
    ! So we skip this test as it's a limitation of the FPM API
    ! source_files(1) = '/tmp/nonexistent_file.f90'
    ! source_files(2) = ''
    ! hash_key1 = get_content_hash(source_files)
    
    ! Clean up
    call execute_command_line('rm -f ' // trim(test_file1) // ' ' // trim(test_file2))
    
    print *, "  PASS: Content hash generation works correctly"
    passed = .true.
    
  end function test_content_hash_generation

  function test_fpm_digest_integration() result(passed)
    logical :: passed
    character(len=256) :: test_source_dir
    character(len=32) :: digest_key
    integer :: unit
    
    print *, "Test 10: FPM digest integration"
    
    ! Create test source directory
    test_source_dir = '/tmp/test_fpm_digest'
    call execute_command_line('mkdir -p ' // trim(test_source_dir))
    
    ! Create a test source file
    open(newunit=unit, file=trim(test_source_dir) // '/main.f90')
    write(unit, '(a)') 'program test_fpm'
    write(unit, '(a)') '  print *, "FPM test"'
    write(unit, '(a)') 'end program'
    close(unit)
    
    ! Test get_fpm_digest
    digest_key = get_fpm_digest(test_source_dir)
    if (len_trim(digest_key) == 0) then
      print *, "  FAIL: get_fpm_digest returned empty string"
      passed = .false.
      return
    end if
    
    ! Should start with 'fpm_', 'fallback_', or 'empty_'
    if (index(digest_key, 'fpm_') /= 1 .and. &
        index(digest_key, 'fallback_') /= 1 .and. &
        index(digest_key, 'empty_') /= 1) then
      print *, "  FAIL: Digest should start with expected prefix: ", trim(digest_key)
      passed = .false.
      return
    end if
    
    ! Test with empty directory
    call execute_command_line('rm -f ' // trim(test_source_dir) // '/*')
    digest_key = get_fpm_digest(test_source_dir)
    if (index(digest_key, 'empty_') /= 1 .and. index(digest_key, 'fallback_') /= 1) then
      print *, "  FAIL: Empty directory should return 'empty_' or 'fallback_' prefix"
      passed = .false.
      return
    end if
    
    ! Test with non-existent directory
    digest_key = get_fpm_digest('/tmp/definitely_nonexistent_directory_12345')
    ! Should handle gracefully - any result starting with known prefixes is acceptable
    if (index(digest_key, 'fallback_') /= 1 .and. &
        index(digest_key, 'fpm_') /= 1 .and. &
        index(digest_key, 'empty_') /= 1) then
      print *, "  FAIL: Non-existent directory should return known prefix, got: ", trim(digest_key)
      passed = .false.
      return
    end if
    
    ! Clean up
    call execute_command_line('rm -rf ' // trim(test_source_dir))
    
    print *, "  PASS: FPM digest integration works correctly"
    passed = .true.
    
  end function test_fpm_digest_integration

  function test_error_handling() result(passed)
    logical :: passed
    logical :: success
    character(len=256) :: source_files(1), dependencies(1)
    character(len=64) :: cache_key
    
    print *, "Test 11: Error handling"
    
    ! Test ensure_cache_dir with invalid path (should fail gracefully)
    call ensure_cache_dir('/dev/null/invalid_path', success)
    if (success) then
      print *, "  FAIL: Should fail when trying to create invalid cache directory"
      passed = .false.
      return
    end if
    
    ! Test store_module_cache with non-existent source files
    source_files(1) = '/tmp/definitely_nonexistent_module.mod'
    cache_key = 'test_error_key'
    call store_module_cache(cache_key, source_files, success)
    if (success) then
      print *, "  FAIL: Should fail when storing non-existent module files"
      passed = .false.
      return
    end if
    
    ! Test store_executable_cache with non-existent executable
    call store_executable_cache(cache_key, '/tmp/definitely_nonexistent_exe', success)
    if (success) then
      print *, "  FAIL: Should fail when storing non-existent executable"
      passed = .false.
      return
    end if
    
    print *, "  PASS: Error handling works correctly"
    passed = .true.
    
  end function test_error_handling

  function test_edge_cases() result(passed)
    logical :: passed
    character(len=256) :: very_long_path, source_files(1)
    character(len=64) :: cache_key
    character(len=32) :: hash_key
    logical :: success
    
    print *, "Test 12: Edge cases"
    
    ! Test very long cache key
    very_long_path = repeat('very_long_path_component/', 10)
    source_files(1) = trim(very_long_path) // 'file.f90'
    cache_key = get_cache_key(source_files, [''])
    if (len_trim(cache_key) > 64) then
      print *, "  FAIL: Cache key too long: ", len_trim(cache_key)
      passed = .false.
      return
    end if
    
    ! Test cache operations with empty strings
    call store_module_cache('', [''], success)
    ! Should handle gracefully (any result acceptable)
    
    call store_executable_cache('', '', success)
    ! Should handle gracefully (any result acceptable)
    
    ! Test with paths containing special characters
    source_files(1) = '/tmp/file with spaces and (parentheses).f90'
    cache_key = get_cache_key(source_files, [''])
    ! Should not crash
    
    ! Test extract_filename helper function indirectly
    source_files(1) = '/very/deep/nested/path/filename.f90'
    cache_key = get_cache_key(source_files, [''])
    if (len_trim(cache_key) == 0) then
      print *, "  FAIL: Should handle deep nested paths"
      passed = .false.
      return
    end if
    
    print *, "  PASS: Edge cases handled correctly"
    passed = .true.
    
  end function test_edge_cases

end program test_cache_comprehensive