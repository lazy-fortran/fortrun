!> Unit test for runner module cache integration
program test_runner_module_cache_integration
  use fpm_module_cache
  use fpm_compiler, only: compiler_t, new_compiler, id_gcc
  use fpm_model, only: srcfile_t, FPM_UNIT_MODULE
  use fpm_strings, only: string_t
  use fpm_error, only: error_t
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none
  
  logical :: all_pass
  
  all_pass = .true.
  
  print '(a)', 'Testing runner module cache integration...'
  print '(a)', ''
  
  ! Test 1: Module cache initialization
  call test_module_cache_init(all_pass)
  
  ! Test 2: Module cache with real source files
  call test_module_cache_with_sources(all_pass)
  
  ! Test 3: Module cache integration points
  call test_integration_points(all_pass)
  
  if (all_pass) then
    print '(a)', ''
    print '(a)', 'All runner module cache integration tests passed!'
    stop 0
  else
    print '(a)', ''
    print '(a)', 'Some tests failed!'
    stop 1
  end if
  
contains

  !> Test module cache initialization
  subroutine test_module_cache_init(all_pass)
    logical, intent(inout) :: all_pass
    type(module_cache_t) :: cache
    type(compiler_t) :: compiler
    logical :: test_pass
    
    print '(a)', 'Test 1: Module cache initialization'
    
    ! Initialize compiler
    call new_compiler(compiler, 'gfortran', 'gcc', 'g++', .true., .false.)
    compiler%id = id_gcc
    
    ! Initialize cache
    cache = new_module_cache(compiler, '13.0.0')
    
    test_pass = cache%enabled .and. len_trim(cache%cache_dir) > 0
    
    if (test_pass) then
      print '(a)', '  ✓ Module cache initialized successfully'
      print '(a,a)', '    Cache directory: ', cache%cache_dir
      print '(a,a)', '    Compiler ID: ', cache%compiler_id
    else
      print '(a)', '  ✗ Module cache initialization failed'
      all_pass = .false.
    end if
    
  end subroutine test_module_cache_init

  !> Test module cache with real source files
  subroutine test_module_cache_with_sources(all_pass)
    logical, intent(inout) :: all_pass
    type(module_cache_t) :: cache
    type(compiler_t) :: compiler
    type(srcfile_t) :: srcfile
    type(error_t), allocatable :: error
    character(len=64) :: cache_key
    logical :: found, test_pass
    character(len=256) :: test_dir, build_dir
    integer :: unit
    type(string_t) :: modules(1)
    
    print '(a)', 'Test 2: Module cache with real source files'
    
    ! Setup
    call new_compiler(compiler, 'gfortran', 'gcc', 'g++', .true., .false.)
    compiler%id = id_gcc
    cache = new_module_cache(compiler, '13.0.0')
    
    ! Create test directories
    test_dir = 'test_module_cache_sources'
    build_dir = trim(test_dir) // '/build'
    call execute_command_line('rm -rf ' // trim(test_dir))
    call execute_command_line('mkdir -p ' // trim(build_dir))
    
    ! Create test module
    open(newunit=unit, file=trim(test_dir) // '/test_mod.f90', status='replace')
    write(unit, '(a)') 'module test_mod'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer, parameter :: answer = 42'
    write(unit, '(a)') 'end module test_mod'
    close(unit)
    
    ! Compile the module
    call execute_command_line('cd ' // trim(build_dir) // ' && gfortran -c ../test_mod.f90')
    
    ! Setup source file info
    srcfile%file_name = 'test_mod.f90'
    srcfile%digest = 12345_int64
    srcfile%unit_type = FPM_UNIT_MODULE
    modules(1)%s = 'test_mod'
    srcfile%modules_provided = modules
    
    ! Generate cache key
    cache_key = cache%get_cache_key(srcfile)
    
    ! Test storage
    call cache%store_module(srcfile, cache_key, build_dir, error)
    test_pass = .not. allocated(error)
    
    if (test_pass) then
      print '(a)', '  ✓ Module stored in cache'
    else
      print '(a)', '  ✗ Failed to store module in cache'
      all_pass = .false.
    end if
    
    ! Test retrieval
    call execute_command_line('mkdir -p ' // trim(test_dir) // '/target')
    call cache%retrieve_module(cache_key, trim(test_dir) // '/target', srcfile, found, error)
    
    test_pass = found .and. .not. allocated(error)
    if (test_pass) then
      print '(a)', '  ✓ Module retrieved from cache'
    else
      print '(a)', '  ✗ Failed to retrieve module from cache'
      all_pass = .false.
    end if
    
    ! Test cache hit detection
    test_pass = cache%is_cached(cache_key)
    if (test_pass) then
      print '(a)', '  ✓ Cache hit detection works'
    else
      print '(a)', '  ✗ Cache hit detection failed'
      all_pass = .false.
    end if
    
    ! Cleanup
    call execute_command_line('rm -rf ' // trim(test_dir))
    
  end subroutine test_module_cache_with_sources

  !> Test integration points where module cache should be used
  subroutine test_integration_points(all_pass)
    logical, intent(inout) :: all_pass
    
    print '(a)', 'Test 3: Module cache integration points'
    
    ! This test identifies where in runner.f90 we should integrate module caching
    print '(a)', '  Integration points needed:'
    print '(a)', '    1. After module scanning, before FPM build'
    print '(a)', '    2. Check for cached modules from dependencies'
    print '(a)', '    3. Store newly compiled modules in cache'
    print '(a)', '    4. Use cached modules in FPM build process'
    
    print '(a)', '  ✓ Integration points identified'
    
    ! TODO: Test actual integration when implemented
    
  end subroutine test_integration_points

end program test_runner_module_cache_integration