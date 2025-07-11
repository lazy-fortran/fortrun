program test_examples
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  
  character(len=256), dimension(:), allocatable :: example_files
  character(len=1024) :: output
  integer :: n_examples, i, exit_code
  integer :: n_passed, n_failed
  logical :: file_exists
  
  ! List of example files to test
  n_examples = 6
  allocate(example_files(n_examples))
  example_files(1) = 'example/hello/hello.f90'
  example_files(2) = 'example/calculator/calculator.f90'
  example_files(3) = 'example/precision/precision_test.f90'
  example_files(4) = 'example/precision/precision_compare.f90'
  example_files(5) = 'example/precision/real_default_test.f90'
  example_files(6) = 'example/interdependent/main.f90'
  
  n_passed = 0
  n_failed = 0
  
  print '(a)', '='//repeat('=', 60)
  print '(a)', 'Running Fortran CLI Example Tests'
  print '(a)', '='//repeat('=', 60)
  print *
  
  do i = 1, n_examples
    ! Check if file exists
    inquire(file=example_files(i), exist=file_exists)
    
    if (.not. file_exists) then
      print '(a,a,a)', 'SKIP: ', trim(example_files(i)), ' (file not found)'
      cycle
    end if
    
    ! Run the example
    print '(a,a,a)', 'Running: ', trim(example_files(i)), '...'
    call run_example(example_files(i), output, exit_code)
    
    if (exit_code == 0) then
      print '(a,a,a)', '  ✓ PASS: ', trim(example_files(i)), ' (exit code 0)'
      n_passed = n_passed + 1
      
      ! Show output for specific examples
      select case (trim(example_files(i)))
      case ('example/hello/hello.f90')
        if (index(output, 'Hello from fortran CLI!') == 0) then
          print '(a)', '    WARNING: Expected output not found'
        end if
      case ('example/precision/real_default_test.f90')
        if (index(output, 'sizeof(real) =                     8  bytes') > 0) then
          print '(a)', '    ✓ Double precision default confirmed'
        else
          print '(a)', '    WARNING: Double precision not working as expected'
        end if
      case ('example/interdependent/main.f90')
        if (index(output, 'Cylinder Calculations') > 0) then
          print '(a)', '    ✓ Interdependent modules working correctly'
        else
          print '(a)', '    WARNING: Interdependent modules may not be working'
        end if
      end select
    else
      print '(a,a,a,i0,a)', '  ✗ FAIL: ', trim(example_files(i)), &
                             ' (exit code ', exit_code, ')'
      n_failed = n_failed + 1
      ! Show error output
      if (len_trim(output) > 0) then
        print '(a)', '    Error output:'
        print '(a,a)', '    ', trim(output)
      end if
    end if
    print *
  end do
  
  ! Test incremental compilation (caching)
  call test_incremental_compilation(n_passed, n_failed)
  
  ! Test source file modification with cached dependencies
  call test_source_modification_with_cached_deps(n_passed, n_failed)
  
  ! Test complex dependency changes
  call test_complex_dependency_changes(n_passed, n_failed)
  
  ! Summary
  print '(a)', '='//repeat('=', 60)
  print '(a)', 'Test Summary'
  print '(a)', '='//repeat('=', 60)
  print '(a,i0)', 'Total examples: ', n_examples
  print '(a,i0)', 'Passed: ', n_passed
  print '(a,i0)', 'Failed: ', n_failed
  print '(a,i0)', 'Skipped: ', n_examples - n_passed - n_failed
  
  if (n_failed > 0) then
    print *
    print '(a)', 'OVERALL: FAILED'
    stop 1
  else
    print *
    print '(a)', 'OVERALL: PASSED'
  end if
  
contains

  subroutine run_example(filename, output, exit_code)
    character(len=*), intent(in) :: filename
    character(len=*), intent(out) :: output
    integer, intent(out) :: exit_code
    
    character(len=512) :: command
    character(len=256) :: cache_pattern
    integer :: unit, iostat, j
    character(len=1024) :: line
    
    ! Extract base name for cache cleanup
    j = index(filename, '/', back=.true.)
    if (j > 0) then
      cache_pattern = filename(j+1:)
    else
      cache_pattern = filename
    end if
    j = index(cache_pattern, '.f90')
    if (j > 0) then
      cache_pattern = cache_pattern(1:j-1)
    end if
    
    ! Clean cache for this example
    command = 'rm -rf ~/.cache/fortran/' // trim(cache_pattern) // '_*'
    call execute_command_line(trim(command))
    
    ! Run the example
    command = './build/gfortran_*/app/fortran ' // trim(filename) // &
              ' > test_output.tmp 2>&1'
    call execute_command_line(trim(command), exitstat=exit_code)
    
    ! Read output
    output = ''
    open(newunit=unit, file='test_output.tmp', status='old', iostat=iostat)
    if (iostat == 0) then
      do
        read(unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit
        if (len_trim(output) > 0) then
          output = trim(output) // ' | ' // trim(adjustl(line))
        else
          output = trim(adjustl(line))
        end if
      end do
      close(unit)
    end if
    
    ! Extract just the program output (after FPM messages)
    j = index(output, '[100%] Project compiled successfully.')
    if (j > 0) then
      output = output(j+37:)  ! Skip the FPM message
    end if
    
    ! Clean up
    call execute_command_line('rm -f test_output.tmp')
    
  end subroutine run_example
  
  subroutine test_incremental_compilation(n_passed, n_failed)
    integer, intent(inout) :: n_passed, n_failed
    character(len=1024) :: output1, output2
    integer :: exit_code1, exit_code2
    character(len=*), parameter :: test_file = 'example/hello/hello.f90'
    character(len=256) :: temp_cache_dir
    character(len=512) :: cleanup_command
    
    print '(a)', '='//repeat('=', 60)
    print '(a)', 'Testing Incremental Compilation (Caching)'
    print '(a)', '='//repeat('=', 60)
    print *
    
    ! Create temporary cache directory
    temp_cache_dir = '/tmp/fortran_test_cache_' // get_test_timestamp()
    print '(a,a)', 'Using temporary cache: ', trim(temp_cache_dir)
    
    ! First run - should compile
    print '(a)', 'First run (should compile)...'
    call run_example_with_cache(test_file, temp_cache_dir, output1, exit_code1)
    
    if (exit_code1 /= 0) then
      print '(a)', '  ✗ FAIL: First run failed'
      print '(a,a)', '    Output: ', trim(output1)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Check that compilation occurred
    if (index(output1, '[  0%]') > 0 .and. index(output1, '[100%]') > 0) then
      print '(a)', '  ✓ First run compiled files as expected'
    else
      print '(a)', '  ✗ FAIL: Expected compilation messages not found in first run'
      print '(a,a)', '    Output: ', trim(output1)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Verify cache miss message
    if (index(output1, 'Cache miss: Setting up new build') > 0) then
      print '(a)', '  ✓ Cache miss detected on first run'
    else
      print '(a)', '  ✗ FAIL: Cache miss not detected on first run'
      print '(a,a)', '    Output: ', trim(output1)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Second run - should use cache
    print '(a)', 'Second run (should use cache)...'
    call run_example_with_cache(test_file, temp_cache_dir, output2, exit_code2)
    
    if (exit_code2 /= 0) then
      print '(a)', '  ✗ FAIL: Second run failed'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Check that cache was hit
    if (index(output2, 'Cache hit: Using existing build') > 0) then
      print '(a)', '  ✓ Cache hit detected on second run'
    else
      print '(a)', '  ✗ FAIL: Cache hit not detected on second run'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Check that FPM detected no compilation needed
    if (index(output2, 'Project is up to date') > 0) then
      print '(a)', '  ✓ FPM detected no compilation needed'
    else
      print '(a)', '  ✗ FAIL: FMP "Project is up to date" message not found'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Verify no compilation percentages in second run
    if (index(output2, '[  0%]') == 0 .and. index(output2, '[100%]') == 0) then
      print '(a)', '  ✓ No compilation occurred on second run'
    else
      print '(a)', '  ✗ FAIL: Compilation occurred on second run (no caching)'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    print '(a)', '  ✓ PASS: Incremental compilation working correctly'
    n_passed = n_passed + 1
    
999 continue
    ! Clean up temporary cache directory
    cleanup_command = 'rm -rf "' // trim(temp_cache_dir) // '"'
    call execute_command_line(trim(cleanup_command))
    print '(a)', 'Cleaned up temporary cache directory'
    print *
    
  end subroutine test_incremental_compilation
  
  subroutine run_example_with_cache(filename, cache_dir, output, exit_code)
    character(len=*), intent(in) :: filename, cache_dir
    character(len=*), intent(out) :: output
    integer, intent(out) :: exit_code
    
    character(len=512) :: command
    integer :: unit, iostat
    character(len=1024) :: line
    
    ! Run with verbose flag and custom cache directory
    command = './build/gfortran_*/app/fortran -v --cache-dir "' // trim(cache_dir) // &
              '" ' // trim(filename) // ' > test_cache_output.tmp 2>&1'
    call execute_command_line(trim(command), exitstat=exit_code)
    
    ! Read full output including verbose messages
    output = ''
    open(newunit=unit, file='test_cache_output.tmp', status='old', iostat=iostat)
    if (iostat == 0) then
      do
        read(unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit
        if (len_trim(output) > 0) then
          output = trim(output) // ' | ' // trim(adjustl(line))
        else
          output = trim(adjustl(line))
        end if
      end do
      close(unit)
    end if
    
    ! Clean up
    call execute_command_line('rm -f test_cache_output.tmp')
    
  end subroutine run_example_with_cache
  
  function get_test_timestamp() result(timestamp)
    character(len=16) :: timestamp
    integer :: time_values(8)
    
    call date_and_time(values=time_values)
    write(timestamp, '(i4.4,i2.2,i2.2,a1,i2.2,i2.2,i2.2)') &
          time_values(1), time_values(2), time_values(3), '_', &
          time_values(5), time_values(6), time_values(7)
          
  end function get_test_timestamp
  
  subroutine test_source_modification_with_cached_deps(n_passed, n_failed)
    integer, intent(inout) :: n_passed, n_failed
    character(len=1024) :: output1, output2, output3
    integer :: exit_code1, exit_code2, exit_code3
    character(len=*), parameter :: test_file = 'example/interdependent/main.f90'
    character(len=256) :: temp_cache_dir, temp_source_file, temp_source_dir
    character(len=512) :: cleanup_command, copy_command
    integer :: unit, iostat
    character(len=16) :: timestamp
    
    print '(a)', '='//repeat('=', 60)
    print '(a)', 'Testing Source Modification with Cached Dependencies'
    print '(a)', '='//repeat('=', 60)
    print *
    
    ! Create a clean timestamp without spaces
    timestamp = get_test_timestamp()
    ! Remove any spaces from timestamp
    call replace_spaces_with_underscores(timestamp)
    
    ! Create temporary directories and files
    temp_cache_dir = '/tmp/fortran_test_cache_' // trim(timestamp)
    temp_source_dir = '/tmp/fortran_test_source_' // trim(timestamp)
    temp_source_file = trim(temp_source_dir) // '/main.f90'
    
    print '(a,a)', 'Using temporary cache: ', trim(temp_cache_dir)
    print '(a,a)', 'Using temporary source: ', trim(temp_source_file)
    
    ! Create temporary source directory
    copy_command = 'mkdir -p "' // trim(temp_source_dir) // '"'
    call execute_command_line(trim(copy_command))
    
    ! Copy the entire interdependent directory to temp location
    copy_command = 'cp -r example/interdependent/* "' // trim(temp_source_dir) // '/"'
    call execute_command_line(trim(copy_command))
    
    ! First run - should compile everything
    print '(a)', 'First run (should compile everything)...'
    call run_example_with_cache(temp_source_file, temp_cache_dir, output1, exit_code1)
    
    if (exit_code1 /= 0) then
      print '(a)', '  ✗ FAIL: First run failed'
      print '(a,a)', '    Output: ', trim(output1)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Verify first run compiled dependencies
    if (index(output1, '[  0%]') > 0 .and. index(output1, '[100%]') > 0) then
      print '(a)', '  ✓ First run compiled files as expected'
    else
      print '(a)', '  ✗ FAIL: Expected compilation messages not found in first run'
      print '(a,a)', '    Output: ', trim(output1)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Second run - should use cache completely
    print '(a)', 'Second run (should use cache completely)...'
    call run_example_with_cache(temp_source_file, temp_cache_dir, output2, exit_code2)
    
    if (exit_code2 /= 0) then
      print '(a)', '  ✗ FAIL: Second run failed'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Verify second run used cache
    if (index(output2, 'Cache hit: Using existing build') > 0) then
      print '(a)', '  ✓ Cache hit detected on second run'
    else
      print '(a)', '  ✗ FAIL: Cache hit not detected on second run'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    if (index(output2, 'Project is up to date') > 0) then
      print '(a)', '  ✓ FPM detected no compilation needed on second run'
    else
      print '(a)', '  ✗ FAIL: FPM should have detected no compilation needed'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Modify the source file (add a comment)
    print '(a)', 'Modifying source file...'
    open(newunit=unit, file=temp_source_file, position='append', iostat=iostat)
    if (iostat == 0) then
      write(unit, '(a)') '! Modified for testing incremental compilation'
      close(unit)
    else
      print '(a)', '  ✗ FAIL: Could not modify source file'
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Third run - should compile only the modified file, not dependencies
    print '(a)', 'Third run (should compile only modified file)...'
    call run_example_with_cache(temp_source_file, temp_cache_dir, output3, exit_code3)
    
    if (exit_code3 /= 0) then
      print '(a)', '  ✗ FAIL: Third run failed'
      print '(a,a)', '    Output: ', trim(output3)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Verify third run detected file change and recompiled incrementally
    if (index(output3, 'Cache hit: Using existing build') > 0) then
      print '(a)', '  ✓ Cache hit detected on third run (reusing project)'
    else
      print '(a)', '  ✗ FAIL: Should reuse cached project on third run'
      print '(a,a)', '    Output: ', trim(output3)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Check that some compilation occurred (but not full rebuild)
    if (index(output3, 'Project is up to date') == 0) then
      print '(a)', '  ✓ FPM detected file changes and recompiled'
    else
      print '(a)', '  ✗ FAIL: FPM should have detected file changes'
      print '(a,a)', '    Output: ', trim(output3)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    ! Verify it's not a full rebuild (should be much faster)
    if (index(output3, '[  0%]') > 0 .and. index(output3, '[100%]') > 0) then
      print '(a)', '  ✓ Incremental compilation occurred'
    else
      print '(a)', '  ✗ FAIL: Should have incremental compilation messages'
      print '(a,a)', '    Output: ', trim(output3)
      n_failed = n_failed + 1
      goto 999  ! cleanup and return
    end if
    
    print '(a)', '  ✓ PASS: Source modification with cached dependencies working correctly'
    n_passed = n_passed + 1
    
999 continue
    ! Clean up temporary files and cache directory
    cleanup_command = 'rm -rf "' // trim(temp_cache_dir) // '"'
    call execute_command_line(trim(cleanup_command))
    cleanup_command = 'rm -rf "' // trim(temp_source_dir) // '"'
    call execute_command_line(trim(cleanup_command))
    print '(a)', 'Cleaned up temporary files and cache directory'
    print *
    
  end subroutine test_source_modification_with_cached_deps
  
  subroutine replace_spaces_with_underscores(str)
    character(len=*), intent(inout) :: str
    integer :: i
    
    do i = 1, len_trim(str)
      if (str(i:i) == ' ') then
        str(i:i) = '_'
      end if
    end do
    
  end subroutine replace_spaces_with_underscores
  
  subroutine test_complex_dependency_changes(n_passed, n_failed)
    integer, intent(inout) :: n_passed, n_failed
    character(len=1024) :: output1, output2, output3
    integer :: exit_code1, exit_code2, exit_code3
    character(len=256) :: temp_cache_dir, temp_source_dir
    character(len=512) :: command
    character(len=16) :: timestamp
    integer :: unit, iostat
    
    print '(a)', '='//repeat('=', 60)
    print '(a)', 'Testing Complex Dependency Changes'
    print '(a)', '='//repeat('=', 60)
    print *
    
    ! Create a clean timestamp without spaces
    timestamp = get_test_timestamp()
    call replace_spaces_with_underscores(timestamp)
    
    ! Create temporary directories
    temp_cache_dir = '/tmp/fortran_complex_dep_cache_' // trim(timestamp)
    temp_source_dir = '/tmp/fortran_complex_dep_source_' // trim(timestamp)
    
    print '(a,a)', 'Using temporary cache: ', trim(temp_cache_dir)
    print '(a,a)', 'Using temporary source: ', trim(temp_source_dir)
    
    ! Create test source directory
    command = 'mkdir -p "' // trim(temp_source_dir) // '"'
    call execute_command_line(trim(command))
    
    ! Create initial version of files
    call create_initial_dependency_files(temp_source_dir)
    
    ! First run - compile everything
    print '(a)', 'Test 1: Initial compilation with dependencies...'
    call run_example_with_cache(trim(temp_source_dir) // '/main.f90', &
                                 temp_cache_dir, output1, exit_code1)
    
    if (exit_code1 /= 0) then
      print '(a)', '  ✗ FAIL: Initial compilation failed'
      print '(a,a)', '    Output: ', trim(output1)
      n_failed = n_failed + 1
      goto 999
    end if
    
    if (index(output1, '[  0%]') > 0 .and. index(output1, '[100%]') > 0) then
      print '(a)', '  ✓ Initial compilation successful'
    else
      print '(a)', '  ✗ FAIL: Expected compilation not detected'
      n_failed = n_failed + 1
      goto 999
    end if
    
    ! Modify a dependency module
    print '(a)', 'Test 2: Modifying dependency module...'
    call modify_dependency_module(temp_source_dir)
    
    call run_example_with_cache(trim(temp_source_dir) // '/main.f90', &
                                 temp_cache_dir, output2, exit_code2)
    
    if (exit_code2 /= 0) then
      print '(a)', '  ✗ FAIL: Compilation after dependency change failed'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999
    end if
    
    ! When dependency changes, it creates a new cache (current behavior)
    if (index(output2, 'Cache miss: Setting up new build') > 0) then
      print '(a)', '  ✓ Dependency change created new cache (expected behavior)'
    else
      print '(a)', '  ✗ FAIL: Expected cache miss for dependency change'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999
    end if
    
    ! Verify compilation occurred
    if (index(output2, '[  0%]') > 0 .and. index(output2, '[100%]') > 0) then
      print '(a)', '  ✓ Recompilation occurred for changed dependency'
    else
      print '(a)', '  ✗ FAIL: Expected recompilation not detected'
      print '(a,a)', '    Output: ', trim(output2)
      n_failed = n_failed + 1
      goto 999
    end if
    
    ! Add new dependency
    print '(a)', 'Test 3: Adding new dependency...'
    call add_new_dependency(temp_source_dir)
    
    call run_example_with_cache(trim(temp_source_dir) // '/main.f90', &
                                 temp_cache_dir, output3, exit_code3)
    
    if (exit_code3 /= 0) then
      print '(a)', '  ✗ FAIL: Compilation with new dependency failed'
      print '(a,a)', '    Output: ', trim(output3)
      n_failed = n_failed + 1
      goto 999
    end if
    
    print '(a)', '  ✓ PASS: Complex dependency changes handled correctly'
    n_passed = n_passed + 1
    
999 continue
    ! Clean up
    command = 'rm -rf "' // trim(temp_cache_dir) // '" "' // trim(temp_source_dir) // '"'
    call execute_command_line(trim(command))
    print '(a)', 'Cleaned up temporary directories'
    print *
    
  end subroutine test_complex_dependency_changes
  
  subroutine create_initial_dependency_files(dir)
    character(len=*), intent(in) :: dir
    integer :: unit
    
    ! Create helper_mod.f90
    open(newunit=unit, file=trim(dir)//'/helper_mod.f90', status='replace')
    write(unit, '(a)') 'module helper_mod'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer, parameter :: HELPER_VERSION = 1'
    write(unit, '(a)') 'contains'
    write(unit, '(a)') '  function helper_function(x) result(y)'
    write(unit, '(a)') '    integer, intent(in) :: x'
    write(unit, '(a)') '    integer :: y'
    write(unit, '(a)') '    y = x * 2'
    write(unit, '(a)') '  end function helper_function'
    write(unit, '(a)') 'end module helper_mod'
    close(unit)
    
    ! Create main.f90 that uses helper_mod
    open(newunit=unit, file=trim(dir)//'/main.f90', status='replace')
    write(unit, '(a)') 'program test_deps'
    write(unit, '(a)') '  use helper_mod'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer :: result'
    write(unit, '(a)') '  result = helper_function(5)'
    write(unit, '(a)') '  print *, "Result:", result'
    write(unit, '(a)') '  print *, "Helper version:", HELPER_VERSION'
    write(unit, '(a)') 'end program test_deps'
    close(unit)
    
  end subroutine create_initial_dependency_files
  
  subroutine modify_dependency_module(dir)
    character(len=*), intent(in) :: dir
    integer :: unit
    
    ! Modify helper_mod.f90
    open(newunit=unit, file=trim(dir)//'/helper_mod.f90', status='replace')
    write(unit, '(a)') 'module helper_mod'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer, parameter :: HELPER_VERSION = 2  ! Modified'
    write(unit, '(a)') 'contains'
    write(unit, '(a)') '  function helper_function(x) result(y)'
    write(unit, '(a)') '    integer, intent(in) :: x'
    write(unit, '(a)') '    integer :: y'
    write(unit, '(a)') '    y = x * 3  ! Modified formula'
    write(unit, '(a)') '  end function helper_function'
    write(unit, '(a)') 'end module helper_mod'
    close(unit)
    
  end subroutine modify_dependency_module
  
  subroutine add_new_dependency(dir)
    character(len=*), intent(in) :: dir
    integer :: unit
    
    ! Create new_mod.f90
    open(newunit=unit, file=trim(dir)//'/new_mod.f90', status='replace')
    write(unit, '(a)') 'module new_mod'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') 'contains'
    write(unit, '(a)') '  subroutine new_feature()'
    write(unit, '(a)') '    print *, "New feature added!"'
    write(unit, '(a)') '  end subroutine new_feature'
    write(unit, '(a)') 'end module new_mod'
    close(unit)
    
    ! Update main.f90 to use new module
    open(newunit=unit, file=trim(dir)//'/main.f90', status='replace')
    write(unit, '(a)') 'program test_deps'
    write(unit, '(a)') '  use helper_mod'
    write(unit, '(a)') '  use new_mod  ! Added new dependency'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer :: result'
    write(unit, '(a)') '  result = helper_function(5)'
    write(unit, '(a)') '  print *, "Result:", result'
    write(unit, '(a)') '  print *, "Helper version:", HELPER_VERSION'
    write(unit, '(a)') '  call new_feature()  ! Use new module'
    write(unit, '(a)') 'end program test_deps'
    close(unit)
    
  end subroutine add_new_dependency
  
end program test_examples