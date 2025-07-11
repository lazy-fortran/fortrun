program test_cache_safety
  implicit none
  
  integer :: n_passed, n_failed
  
  n_passed = 0
  n_failed = 0
  
  print '(a)', '='//repeat('=', 60)
  print '(a)', 'Testing Cache Safety and Limitations'
  print '(a)', '='//repeat('=', 60)
  print *
  
  ! Test current behavior with concurrent access
  call test_current_concurrent_behavior(n_passed, n_failed)
  
  ! Document what would be needed for proper concurrency
  call document_locking_requirements()
  
  ! Summary
  print *
  print '(a)', '='//repeat('=', 60)
  print '(a)', 'Test Summary'
  print '(a)', '='//repeat('=', 60)
  print '(a,i0)', 'Tests passed: ', n_passed
  print '(a,i0)', 'Tests failed: ', n_failed
  
  if (n_failed > 0) then
    stop 1
  end if
  
contains

  subroutine test_current_concurrent_behavior(n_passed, n_failed)
    integer, intent(inout) :: n_passed, n_failed
    character(len=256) :: test_file, cache_dir
    integer :: exit_code1, exit_code2
    
    print '(a)', 'Test 1: Current Concurrent Behavior'
    print '(a)', '-----------------------------------'
    
    ! Create test file
    test_file = '/tmp/safety_test.f90'
    call create_simple_test_file(test_file)
    
    ! Use temporary cache
    cache_dir = '/tmp/safety_test_cache'
    call execute_command_line('rm -rf ' // trim(cache_dir))
    
    ! Note: Currently, if two processes try to build the same project
    ! simultaneously, they might interfere with each other
    print '(a)', 'Current implementation status:'
    print '(a)', '  - Each process creates its own project directory'
    print '(a)', '  - Directory names are based on content hash'
    print '(a)', '  - No explicit locking mechanism'
    print '(a)', '  - FPM handles its own build directory locking'
    
    ! Test sequential access (should work)
    print '(a)', ''
    print '(a)', 'Testing sequential access...'
    call execute_command_line('./build/gfortran_*/app/fortran --cache-dir "' // &
                              trim(cache_dir) // '" "' // trim(test_file) // &
                              '" > /dev/null 2>&1', exitstat=exit_code1)
    
    call execute_command_line('./build/gfortran_*/app/fortran --cache-dir "' // &
                              trim(cache_dir) // '" "' // trim(test_file) // &
                              '" > /dev/null 2>&1', exitstat=exit_code2)
    
    if (exit_code1 == 0 .and. exit_code2 == 0) then
      print '(a)', '  ✓ PASS: Sequential access works correctly'
      n_passed = n_passed + 1
    else
      print '(a)', '  ✗ FAIL: Sequential access failed'
      n_failed = n_failed + 1
    end if
    
    ! Cleanup
    call execute_command_line('rm -rf ' // trim(test_file) // ' ' // trim(cache_dir))
    print *
    
  end subroutine test_current_concurrent_behavior
  
  subroutine document_locking_requirements()
    print '(a)', 'Proper Concurrent Cache Access Requirements'
    print '(a)', '==========================================='
    print '(a)', ''
    print '(a)', 'For safe concurrent access, we would need:'
    print '(a)', ''
    print '(a)', '1. Lock Files:'
    print '(a)', '   - Create .lock files when starting a build'
    print '(a)', '   - Use atomic file operations (O_CREAT | O_EXCL)'
    print '(a)', '   - Include PID to handle stale locks'
    print '(a)', ''
    print '(a)', '2. Wait/Retry Logic:'
    print '(a)', '   - If lock exists, wait or retry'
    print '(a)', '   - Timeout mechanism for stale locks'
    print '(a)', '   - Exponential backoff for retries'
    print '(a)', ''
    print '(a)', '3. Atomic Operations:'
    print '(a)', '   - Use rename() for atomic file moves'
    print '(a)', '   - Create files in temp location first'
    print '(a)', '   - Move atomically when complete'
    print '(a)', ''
    print '(a)', '4. Cache Integrity:'
    print '(a)', '   - Mark builds as incomplete until done'
    print '(a)', '   - Clean up incomplete builds on startup'
    print '(a)', '   - Verify cache integrity before use'
    print '(a)', ''
    print '(a)', 'Implementation Options:'
    print '(a)', '- Use OS-specific locking (fcntl, LockFileEx)'
    print '(a)', '- Implement simple file-based locking'
    print '(a)', '- Leverage FPM''s existing locking if available'
    print '(a)', ''
    print '(a)', 'Current Status: Deferred to future enhancement'
    print '(a)', 'Workaround: Use different cache directories for'
    print '(a)', '            concurrent processes if needed'
    print *
    
  end subroutine document_locking_requirements
  
  subroutine create_simple_test_file(file_path)
    character(len=*), intent(in) :: file_path
    integer :: unit
    
    open(newunit=unit, file=file_path, status='replace')
    write(unit, '(a)') 'program safety_test'
    write(unit, '(a)') '  print *, "Safety test"'
    write(unit, '(a)') 'end program safety_test'
    close(unit)
    
  end subroutine create_simple_test_file
  
end program test_cache_safety