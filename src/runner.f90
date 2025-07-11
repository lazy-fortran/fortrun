module runner
  use cache, only: get_cache_dir, ensure_cache_dir
  use module_scanner, only: scan_modules, module_info
  use fpm_generator, only: generate_fpm_with_deps
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none
  private
  public :: run_fortran_file
  
contains

  subroutine run_fortran_file(filename, exit_code, verbose_level, custom_cache_dir)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: exit_code
    integer, intent(in) :: verbose_level
    character(len=*), intent(in) :: custom_cache_dir
    
    logical :: file_exists, success
    character(len=256) :: cache_dir, project_dir, basename
    character(len=512) :: command
    character(len=256) :: absolute_path
    integer :: exitstat, cmdstat
    integer :: i, last_slash
    
    exit_code = 0
    
    ! Check if file exists
    inquire(file=filename, exist=file_exists)
    if (.not. file_exists) then
      print '(a,a)', 'Error: File not found: ', trim(filename)
      exit_code = 1
      return
    end if
    
    ! Check file extension
    if (index(filename, '.f90') == 0 .and. index(filename, '.F90') == 0) then
      print '(a)', 'Error: Input file must have .f90 or .F90 extension'
      exit_code = 1
      return
    end if
    
    ! Get absolute path
    call get_absolute_path(filename, absolute_path)
    
    ! Extract basename without extension
    call get_basename(filename, basename)
    
    ! Get cache directory (use custom if provided)
    if (len_trim(custom_cache_dir) > 0) then
      cache_dir = custom_cache_dir
    else
      cache_dir = get_cache_dir()
    end if
    call ensure_cache_dir(cache_dir, success)
    if (.not. success) then
      print '(a)', 'Error: Failed to create cache directory'
      exit_code = 1
      return
    end if
    
    ! Create project directory in cache
    project_dir = trim(cache_dir) // '/' // trim(basename) // '_' // get_timestamp()
    command = 'mkdir -p "' // trim(project_dir) // '/app"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    
    if (cmdstat /= 0 .or. exitstat /= 0) then
      print '(a)', 'Error: Failed to create project directory'
      exit_code = 1
      return
    end if
    
    ! Copy the source file to app directory
    command = 'cp "' // trim(absolute_path) // '" "' // trim(project_dir) // '/app/main.f90"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    
    if (cmdstat /= 0 .or. exitstat /= 0) then
      print '(a)', 'Error: Failed to copy source file'
      exit_code = 1
      return
    end if
    
    ! Copy all other .f90 files from the same directory to src/
    call copy_local_modules(absolute_path, project_dir)
    
    ! Scan for module dependencies
    call generate_fpm_with_dependencies(absolute_path, project_dir, basename, verbose_level)
    
    ! Build first
    if (verbose_level == 0) then
      ! Quiet mode: build silently
      command = 'cd "' // trim(project_dir) // '" && ' // &
                'fpm build --flag "-fdefault-real-8 -fdefault-double-8" > /dev/null 2>&1'
    else if (verbose_level >= 2) then
      ! Very verbose: show detailed build output
      command = 'cd "' // trim(project_dir) // '" && ' // &
                'fpm build --verbose --flag "-fdefault-real-8 -fdefault-double-8"'
    else
      ! Normal verbose: show build progress
      command = 'cd "' // trim(project_dir) // '" && ' // &
                'fpm build --flag "-fdefault-real-8 -fdefault-double-8"'
    end if
    
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat, wait=.true.)
    
    if (cmdstat /= 0 .or. exitstat /= 0) then
      if (verbose_level == 0) then
        print '(a)', 'Error: Build failed. Run with -v to see details.'
      end if
      exit_code = 1
      return
    end if
    
    ! Run the executable directly
    command = trim(project_dir) // '/build/gfortran_*/app/' // trim(basename)
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat, wait=.true.)
    
    if (cmdstat /= 0) then
      print '(a)', 'Error: Failed to execute fpm'
      exit_code = 1
    else if (exitstat /= 0) then
      ! FPM returned non-zero, likely compilation error
      exit_code = exitstat
    end if
    
    ! Clean up (optional for now - we might want to keep for caching)
    ! command = 'rm -rf "' // trim(project_dir) // '"'
    ! call execute_command_line(command)
    
  end subroutine run_fortran_file
  
  subroutine get_absolute_path(filename, absolute_path)
    character(len=*), intent(in) :: filename
    character(len=*), intent(out) :: absolute_path
    character(len=512) :: command
    integer :: unit, iostat
    
    ! Use realpath command to get absolute path
    command = 'realpath "' // trim(filename) // '" > /tmp/fortran_path.tmp'
    call execute_command_line(command)
    
    open(newunit=unit, file='/tmp/fortran_path.tmp', status='old', iostat=iostat)
    if (iostat == 0) then
      read(unit, '(a)') absolute_path
      close(unit)
      call execute_command_line('rm -f /tmp/fortran_path.tmp')
    else
      ! Fallback to original filename
      absolute_path = filename
    end if
    
  end subroutine get_absolute_path
  
  subroutine get_basename(filename, basename)
    character(len=*), intent(in) :: filename
    character(len=*), intent(out) :: basename
    integer :: i, last_slash, last_dot
    
    ! Find last slash
    last_slash = 0
    do i = len_trim(filename), 1, -1
      if (filename(i:i) == '/') then
        last_slash = i
        exit
      end if
    end do
    
    ! Find last dot after the slash
    last_dot = 0
    do i = len_trim(filename), last_slash+1, -1
      if (filename(i:i) == '.') then
        last_dot = i
        exit
      end if
    end do
    
    if (last_dot > last_slash) then
      basename = filename(last_slash+1:last_dot-1)
    else
      basename = filename(last_slash+1:)
    end if
    
  end subroutine get_basename
  
  function get_timestamp() result(timestamp)
    character(len=16) :: timestamp
    integer :: values(8)
    
    call date_and_time(values=values)
    write(timestamp, '(i0,5(i2.2))') values(1), values(2), values(3), &
                                      values(5), values(6), values(7)
    
  end function get_timestamp
  
  subroutine generate_fpm_with_dependencies(source_file, project_dir, name, verbose_level)
    character(len=*), intent(in) :: source_file, project_dir, name
    integer, intent(in) :: verbose_level
    
    type(module_info), dimension(:), allocatable :: modules
    integer :: n_modules
    
    ! Scan the source file for module dependencies
    call scan_modules(source_file, modules, n_modules)
    
    if (verbose_level >= 1 .and. n_modules > 0) then
      print '(a,i0,a)', 'Found ', n_modules, ' external module dependencies'
    end if
    
    ! Generate fpm.toml with dependencies
    call generate_fpm_with_deps(project_dir, name, modules, n_modules)
    
  end subroutine generate_fpm_with_dependencies
  
  subroutine copy_local_modules(main_file, project_dir)
    character(len=*), intent(in) :: main_file, project_dir
    character(len=256) :: source_dir
    character(len=512) :: command
    integer :: i, last_slash
    
    ! Extract directory from main file path
    last_slash = 0
    do i = len_trim(main_file), 1, -1
      if (main_file(i:i) == '/') then
        last_slash = i
        exit
      end if
    end do
    
    if (last_slash > 0) then
      source_dir = main_file(1:last_slash-1)
    else
      source_dir = '.'
    end if
    
    ! Create src directory
    command = 'mkdir -p "' // trim(project_dir) // '/src"'
    call execute_command_line(command)
    
    ! Copy all .f90 files except the main file
    command = 'find "' // trim(source_dir) // '" -maxdepth 1 -name "*.f90" -o -name "*.F90" | ' // &
              'while read f; do ' // &
              '  if [ "$f" != "' // trim(main_file) // '" ]; then ' // &
              '    cp "$f" "' // trim(project_dir) // '/src/"; ' // &
              '  fi; ' // &
              'done'
    call execute_command_line(command)
    
  end subroutine copy_local_modules
  
end module runner