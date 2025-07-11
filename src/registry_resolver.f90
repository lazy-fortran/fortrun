module registry_resolver
  use config, only: get_registry_path, get_config_dir, ensure_config_dir
  implicit none
  private
  public :: resolve_module_to_package, load_registry, ensure_registry_exists, load_registry_from_path, ensure_registry_exists_in_dir
  
  type :: package_info
    character(len=128) :: name
    character(len=256) :: git_url
    character(len=64) :: prefix
  end type package_info
  
  type(package_info), dimension(:), allocatable :: packages
  integer :: n_packages
  
contains

  subroutine load_registry()
    integer :: unit, iostat
    character(len=512) :: line, registry_path
    character(len=128) :: current_package
    logical :: in_package
    
    n_packages = 0
    if (allocated(packages)) deallocate(packages)
    allocate(packages(10))  ! Start with space for 10 packages
    
    registry_path = get_registry_path()
    
    open(newunit=unit, file=registry_path, status='old', iostat=iostat)
    if (iostat /= 0) then
      print *, 'Warning: Cannot open registry at ', trim(registry_path)
      return
    end if
    
    in_package = .false.
    current_package = ''
    
    do
      read(unit, '(a)', iostat=iostat) line
      if (iostat /= 0) exit
      
      line = adjustl(line)
      
      ! Skip empty lines and comments
      if (len_trim(line) == 0) cycle
      if (line(1:1) == '#') cycle
      
      ! Check for package section
      if (line(1:10) == '[packages.') then
        ! Extract package name
        n_packages = n_packages + 1
        current_package = extract_between(line, '[packages.', ']')
        packages(n_packages)%name = current_package
        in_package = .true.
      else if (in_package) then
        ! Parse package properties
        if (index(line, 'git =') > 0) then
          packages(n_packages)%git_url = extract_quoted(line)
        else if (index(line, 'prefix =') > 0) then
          packages(n_packages)%prefix = extract_quoted(line)
        end if
      end if
    end do
    
    close(unit)
    
  end subroutine load_registry
  
  subroutine load_registry_from_path(registry_path)
    character(len=*), intent(in) :: registry_path
    integer :: unit, iostat
    character(len=512) :: line
    character(len=128) :: current_package
    logical :: in_package
    
    n_packages = 0
    if (allocated(packages)) deallocate(packages)
    allocate(packages(10))  ! Start with space for 10 packages
    
    open(newunit=unit, file=registry_path, status='old', iostat=iostat)
    if (iostat /= 0) then
      print *, 'Warning: Cannot open registry at ', trim(registry_path)
      return
    end if
    
    in_package = .false.
    current_package = ''
    
    do
      read(unit, '(a)', iostat=iostat) line
      if (iostat /= 0) exit
      
      line = adjustl(line)
      
      ! Skip empty lines and comments
      if (len_trim(line) == 0) cycle
      if (line(1:1) == '#') cycle
      
      ! Check for package section
      if (line(1:10) == '[packages.') then
        ! Extract package name
        n_packages = n_packages + 1
        current_package = extract_between(line, '[packages.', ']')
        packages(n_packages)%name = current_package
        in_package = .true.
      else if (in_package) then
        ! Parse package properties
        if (index(line, 'git =') > 0) then
          packages(n_packages)%git_url = extract_quoted(line)
        else if (index(line, 'prefix =') > 0) then
          packages(n_packages)%prefix = extract_quoted(line)
        end if
      end if
    end do
    
    close(unit)
    
  end subroutine load_registry_from_path
  
  subroutine resolve_module_to_package(module_name, package_name, git_url, found)
    character(len=*), intent(in) :: module_name
    character(len=*), intent(out) :: package_name
    character(len=*), intent(out) :: git_url
    logical, intent(out) :: found
    
    integer :: i, underscore_pos
    character(len=128) :: inferred_package
    
    found = .false.
    package_name = ''
    git_url = ''
    
    ! First check prefixes
    do i = 1, n_packages
      if (len_trim(packages(i)%prefix) > 0) then
        if (index(module_name, trim(packages(i)%prefix)) == 1) then
          package_name = packages(i)%name
          git_url = packages(i)%git_url
          found = .true.
          return
        end if
      end if
    end do
    
    ! Try underscore inference
    underscore_pos = index(module_name, '_')
    if (underscore_pos > 0) then
      inferred_package = module_name(1:underscore_pos-1) // '-fortran'
    else
      inferred_package = module_name
    end if
    
    ! Look for inferred package name
    do i = 1, n_packages
      if (trim(packages(i)%name) == trim(inferred_package)) then
        package_name = packages(i)%name
        git_url = packages(i)%git_url
        found = .true.
        return
      end if
    end do
    
  end subroutine resolve_module_to_package
  
  function extract_between(str, start_str, end_str) result(extracted)
    character(len=*), intent(in) :: str, start_str, end_str
    character(len=128) :: extracted
    integer :: start_pos, end_pos
    
    extracted = ''
    start_pos = index(str, start_str)
    if (start_pos > 0) then
      start_pos = start_pos + len(start_str)
      end_pos = index(str(start_pos:), end_str)
      if (end_pos > 0) then
        extracted = str(start_pos:start_pos+end_pos-2)
      end if
    end if
  end function extract_between
  
  function extract_quoted(str) result(extracted)
    character(len=*), intent(in) :: str
    character(len=256) :: extracted
    integer :: quote1, quote2
    
    extracted = ''
    quote1 = index(str, '"')
    if (quote1 > 0) then
      quote2 = index(str(quote1+1:), '"')
      if (quote2 > 0) then
        extracted = str(quote1+1:quote1+quote2-1)
      end if
    end if
  end function extract_quoted
  
  subroutine ensure_registry_exists()
    character(len=256) :: config_dir
    character(len=512) :: registry_path
    logical :: registry_exists, success
    integer :: unit
    
    ! Get paths
    config_dir = get_config_dir()
    registry_path = get_registry_path()
    
    ! Ensure config directory exists
    call ensure_config_dir(config_dir, success)
    if (.not. success) then
      print *, 'Error: Cannot create config directory: ', trim(config_dir)
      return
    end if
    
    ! Check if registry already exists
    inquire(file=registry_path, exist=registry_exists)
    if (registry_exists) return
    
    ! Create default registry
    open(newunit=unit, file=registry_path, status='replace')
    write(unit, '(a)') '# Fortran Package Registry'
    write(unit, '(a)') '# Maps module names to their packages for automatic dependency resolution'
    write(unit, '(a)') '#'
    write(unit, '(a)') '# Module resolution rules:'
    write(unit, '(a)') '# 1. Check explicit modules list'
    write(unit, '(a)') '# 2. Check if module starts with a custom prefix'
    write(unit, '(a)') '# 3. Use default behavior: package name = module name before first underscore'
    write(unit, '(a)') '# 4. If no underscore, package name = module name itself'
    write(unit, '(a)') ''
    write(unit, '(a)') '[packages]'
    write(unit, '(a)') ''
    write(unit, '(a)') '[packages.fortplotlib]'
    write(unit, '(a)') 'git = "https://github.com/krystophny/fortplotlib"'
    write(unit, '(a)') 'prefix = "fortplot"  # Any module starting with "fortplot" belongs to this package'
    write(unit, '(a)') ''
    write(unit, '(a)') '[packages.pyplot-fortran]'
    write(unit, '(a)') 'git = "https://github.com/jacobwilliams/pyplot-fortran"'
    write(unit, '(a)') '# No prefix specified, will use default behavior:'
    write(unit, '(a)') '# pyplot_module -> package name = "pyplot" (before underscore)'
    close(unit)
    
    print *, 'Created default registry at: ', trim(registry_path)
    
  end subroutine ensure_registry_exists
  
  subroutine ensure_registry_exists_in_dir(config_dir)
    character(len=*), intent(in) :: config_dir
    character(len=512) :: registry_path
    logical :: registry_exists, success
    integer :: unit
    
    ! Get registry path in custom config directory
    registry_path = trim(config_dir) // '/registry.toml'
    
    ! Ensure config directory exists
    call ensure_config_dir(config_dir, success)
    if (.not. success) then
      print *, 'Error: Cannot create config directory: ', trim(config_dir)
      return
    end if
    
    ! Check if registry already exists
    inquire(file=registry_path, exist=registry_exists)
    if (registry_exists) return
    
    ! Create default registry
    open(newunit=unit, file=registry_path, status='replace')
    write(unit, '(a)') '# Fortran Package Registry'
    write(unit, '(a)') '# Maps module names to their packages for automatic dependency resolution'
    write(unit, '(a)') '#'
    write(unit, '(a)') '# Module resolution rules:'
    write(unit, '(a)') '# 1. Check explicit modules list'
    write(unit, '(a)') '# 2. Check if module starts with a custom prefix'
    write(unit, '(a)') '# 3. Use default behavior: package name = module name before first underscore'
    write(unit, '(a)') '# 4. If no underscore, package name = module name itself'
    write(unit, '(a)') ''
    write(unit, '(a)') '[packages]'
    write(unit, '(a)') ''
    write(unit, '(a)') '[packages.fortplotlib]'
    write(unit, '(a)') 'git = "https://github.com/krystophny/fortplotlib"'
    write(unit, '(a)') 'prefix = "fortplot"  # Any module starting with "fortplot" belongs to this package'
    write(unit, '(a)') ''
    write(unit, '(a)') '[packages.pyplot-fortran]'
    write(unit, '(a)') 'git = "https://github.com/jacobwilliams/pyplot-fortran"'
    write(unit, '(a)') '# No prefix specified, will use default behavior:'
    write(unit, '(a)') '# pyplot_module -> package name = "pyplot" (before underscore)'
    close(unit)
    
    print *, 'Created default registry at: ', trim(registry_path)
    
  end subroutine ensure_registry_exists_in_dir
  
end module registry_resolver