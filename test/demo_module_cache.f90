!> Demonstration of module-level caching concept
program demo_module_cache
  implicit none
  
  character(:), allocatable :: test_dir, cache_dir
  integer :: i
  real :: time1, time2
  
  print '(a)', 'Module-Level Caching Demonstration'
  print '(a)', '=================================='
  print '(a)', ''
  
  test_dir = '/tmp/fortran_module_demo_' // trim(adjustl(get_timestamp()))
  cache_dir = trim(test_dir) // '/.module_cache'
  
  ! Create test directory structure
  call setup_demo_project(test_dir)
  
  ! Scenario 1: First build - no cache
  print '(a)', 'Scenario 1: Initial build (no cache)'
  call cpu_time(time1)
  call build_project(test_dir, cache_dir, .false.)
  call cpu_time(time2)
  print '(a,f6.3,a)', '  Build time: ', time2-time1, ' seconds'
  print '(a)', ''
  
  ! Scenario 2: Rebuild with module cache
  print '(a)', 'Scenario 2: Rebuild with module cache'
  call cpu_time(time1)
  call build_project(test_dir, cache_dir, .true.)
  call cpu_time(time2)
  print '(a,f6.3,a)', '  Build time: ', time2-time1, ' seconds'
  print '(a)', ''
  
  ! Scenario 3: Modify main program only
  print '(a)', 'Scenario 3: Modify main program only'
  call modify_main_program(test_dir)
  call cpu_time(time1)
  call build_project(test_dir, cache_dir, .true.)
  call cpu_time(time2)
  print '(a,f6.3,a)', '  Build time: ', time2-time1, ' seconds'
  print '(a)', '  (Modules should be retrieved from cache)'
  print '(a)', ''
  
  ! Show cache statistics
  call show_cache_stats(cache_dir)
  
  ! Cleanup
  call execute_command_line('rm -rf ' // test_dir)
  
  print '(a)', ''
  print '(a)', 'Demonstration completed!'
  
contains

  subroutine setup_demo_project(dir)
    character(*), intent(in) :: dir
    integer :: unit
    
    call execute_command_line('mkdir -p ' // dir // '/src')
    
    ! Create math module
    open(newunit=unit, file=dir // '/src/math_utils.f90', status='replace')
    write(unit, '(a)') 'module math_utils'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  private'
    write(unit, '(a)') '  public :: add, multiply'
    write(unit, '(a)') 'contains'
    write(unit, '(a)') '  pure function add(a, b) result(c)'
    write(unit, '(a)') '    real, intent(in) :: a, b'
    write(unit, '(a)') '    real :: c'
    write(unit, '(a)') '    c = a + b'
    write(unit, '(a)') '  end function add'
    write(unit, '(a)') '  pure function multiply(a, b) result(c)'
    write(unit, '(a)') '    real, intent(in) :: a, b'
    write(unit, '(a)') '    real :: c'
    write(unit, '(a)') '    c = a * b'
    write(unit, '(a)') '  end function multiply'
    write(unit, '(a)') 'end module math_utils'
    close(unit)
    
    ! Create string module
    open(newunit=unit, file=dir // '/src/string_utils.f90', status='replace')
    write(unit, '(a)') 'module string_utils'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  private'
    write(unit, '(a)') '  public :: concat, repeat_string'
    write(unit, '(a)') 'contains'
    write(unit, '(a)') '  function concat(s1, s2) result(s3)'
    write(unit, '(a)') '    character(*), intent(in) :: s1, s2'
    write(unit, '(a)') '    character(:), allocatable :: s3'
    write(unit, '(a)') '    s3 = s1 // s2'
    write(unit, '(a)') '  end function concat'
    write(unit, '(a)') '  function repeat_string(s, n) result(repeated)'
    write(unit, '(a)') '    character(*), intent(in) :: s'
    write(unit, '(a)') '    integer, intent(in) :: n'
    write(unit, '(a)') '    character(:), allocatable :: repeated'
    write(unit, '(a)') '    integer :: i'
    write(unit, '(a)') '    repeated = ""'
    write(unit, '(a)') '    do i = 1, n'
    write(unit, '(a)') '      repeated = repeated // s'
    write(unit, '(a)') '    end do'
    write(unit, '(a)') '  end function repeat_string'
    write(unit, '(a)') 'end module string_utils'
    close(unit)
    
    ! Create main program
    open(newunit=unit, file=dir // '/main.f90', status='replace')
    write(unit, '(a)') 'program main'
    write(unit, '(a)') '  use math_utils'
    write(unit, '(a)') '  use string_utils'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "2 + 3 =", add(2.0, 3.0)'
    write(unit, '(a)') '  print *, "Hello " // concat("World", "!")'
    write(unit, '(a)') 'end program main'
    close(unit)
    
  end subroutine setup_demo_project
  
  subroutine build_project(project_dir, cache_dir, use_cache)
    character(*), intent(in) :: project_dir, cache_dir
    logical, intent(in) :: use_cache
    character(:), allocatable :: build_dir, cmd
    
    build_dir = project_dir // '/build'
    call execute_command_line('mkdir -p ' // build_dir)
    
    if (use_cache) then
      print '(a)', '  Checking module cache...'
      
      ! Simulate cache check for modules
      if (module_in_cache(cache_dir, 'math_utils')) then
        print '(a)', '    ✓ math_utils.mod found in cache'
        call retrieve_from_cache(cache_dir, 'math_utils', build_dir)
      else
        print '(a)', '    ✗ math_utils.mod not in cache - compiling'
        call compile_module(project_dir, 'math_utils', build_dir)
        call store_in_cache(cache_dir, 'math_utils', build_dir)
      end if
      
      if (module_in_cache(cache_dir, 'string_utils')) then
        print '(a)', '    ✓ string_utils.mod found in cache'
        call retrieve_from_cache(cache_dir, 'string_utils', build_dir)
      else
        print '(a)', '    ✗ string_utils.mod not in cache - compiling'
        call compile_module(project_dir, 'string_utils', build_dir)
        call store_in_cache(cache_dir, 'string_utils', build_dir)
      end if
    else
      print '(a)', '  Compiling all modules...'
      call compile_module(project_dir, 'math_utils', build_dir)
      call compile_module(project_dir, 'string_utils', build_dir)
    end if
    
    ! Always compile main program
    print '(a)', '  Compiling main program...'
    cmd = 'cd ' // build_dir // ' && gfortran -c ' // project_dir // '/main.f90'
    call execute_command_line(cmd, exitstat=i)
    
    ! Link
    print '(a)', '  Linking...'
    cmd = 'cd ' // build_dir // ' && gfortran -o main main.o math_utils.o string_utils.o'
    call execute_command_line(cmd, exitstat=i)
    
  end subroutine build_project
  
  function module_in_cache(cache_dir, module_name) result(exists)
    character(*), intent(in) :: cache_dir, module_name
    logical :: exists
    
    inquire(file=cache_dir // '/' // module_name // '.mod', exist=exists)
    
  end function module_in_cache
  
  subroutine compile_module(project_dir, module_name, build_dir)
    character(*), intent(in) :: project_dir, module_name, build_dir
    character(:), allocatable :: cmd
    
    cmd = 'cd ' // build_dir // ' && gfortran -c ' // &
          project_dir // '/src/' // module_name // '.f90'
    call execute_command_line(cmd)
    
  end subroutine compile_module
  
  subroutine store_in_cache(cache_dir, module_name, build_dir)
    character(*), intent(in) :: cache_dir, module_name, build_dir
    
    call execute_command_line('mkdir -p ' // cache_dir)
    call execute_command_line('cp ' // build_dir // '/' // module_name // '.mod ' // &
                              cache_dir // '/')
    call execute_command_line('cp ' // build_dir // '/' // module_name // '.o ' // &
                              cache_dir // '/')
    
  end subroutine store_in_cache
  
  subroutine retrieve_from_cache(cache_dir, module_name, build_dir)
    character(*), intent(in) :: cache_dir, module_name, build_dir
    
    call execute_command_line('cp ' // cache_dir // '/' // module_name // '.mod ' // &
                              build_dir // '/')
    call execute_command_line('cp ' // cache_dir // '/' // module_name // '.o ' // &
                              build_dir // '/')
    
  end subroutine retrieve_from_cache
  
  subroutine modify_main_program(dir)
    character(*), intent(in) :: dir
    integer :: unit
    
    open(newunit=unit, file=dir // '/main.f90', status='replace')
    write(unit, '(a)') 'program main'
    write(unit, '(a)') '  use math_utils'
    write(unit, '(a)') '  use string_utils'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  ! Modified version'
    write(unit, '(a)') '  print *, "5 * 6 =", multiply(5.0, 6.0)'
    write(unit, '(a)') '  print *, repeat_string("Hi! ", 3)'
    write(unit, '(a)') 'end program main'
    close(unit)
    
  end subroutine modify_main_program
  
  subroutine show_cache_stats(cache_dir)
    character(*), intent(in) :: cache_dir
    character(:), allocatable :: cmd
    integer :: unit, num_files
    
    print '(a)', 'Cache Statistics:'
    
    ! Count cached modules
    cmd = 'ls ' // cache_dir // '/*.mod 2>/dev/null | wc -l > /tmp/cache_count.tmp'
    call execute_command_line(cmd)
    
    open(newunit=unit, file='/tmp/cache_count.tmp', status='old')
    read(unit, *) num_files
    close(unit)
    
    print '(a,i0)', '  Cached modules: ', num_files
    print '(a,a)', '  Cache directory: ', cache_dir
    
    call execute_command_line('rm -f /tmp/cache_count.tmp')
    
  end subroutine show_cache_stats
  
  function get_timestamp() result(timestamp)
    character(len=16) :: timestamp
    integer :: values(8)
    
    call date_and_time(values=values)
    write(timestamp, '(i4.4,5i2.2)') values(1), values(2), values(3), &
                                      values(5), values(6), values(7)
  end function get_timestamp

end program demo_module_cache