module fpm_generator
  use module_scanner, only: module_info
  use registry_resolver, only: resolve_module_to_package, load_registry, load_registry_from_path, resolve_module_with_version
  implicit none
  private
  public :: generate_fpm_with_deps, generate_fpm_with_deps_from_config
  
contains

  subroutine generate_fpm_with_deps(project_dir, name, modules, n_modules, is_preprocessed_file, custom_flags)
    character(len=*), intent(in) :: project_dir, name
    type(module_info), dimension(:), intent(in) :: modules
    integer, intent(in) :: n_modules
    logical, intent(in), optional :: is_preprocessed_file
    character(len=*), intent(in), optional :: custom_flags
    
    character(len=512) :: toml_path
    character(len=128) :: package_name, package_names(100)
    character(len=256) :: git_url, git_urls(100)
    character(len=32) :: version, versions(100)
    logical :: found
    integer :: unit, i, j, n_deps
    logical :: already_added
    
    ! Load registry if not already loaded
    call load_registry()
    
    toml_path = trim(project_dir) // '/fpm.toml'
    
    ! Resolve dependencies
    n_deps = 0
    do i = 1, n_modules
      call resolve_module_with_version(modules(i)%name, package_name, git_url, version, found)
      
      if (found) then
        ! Print resolution info for test validation
        print '(a,a,a,a)', '  Module ', trim(modules(i)%name), ' -> package ', trim(package_name)
        
        ! Check if already added
        already_added = .false.
        do j = 1, n_deps
          if (trim(package_names(j)) == trim(package_name)) then
            already_added = .true.
            exit
          end if
        end do
        
        if (.not. already_added) then
          n_deps = n_deps + 1
          package_names(n_deps) = package_name
          git_urls(n_deps) = git_url
          versions(n_deps) = version
        end if
      end if
    end do
    
    ! Generate fpm.toml
    open(newunit=unit, file=toml_path, status='replace')
    
    ! Basic metadata
    write(unit, '(a)') 'name = "' // trim(name) // '"'
    write(unit, '(a)') 'version = "0.1.0"'
    write(unit, '(a)') ''
    
    ! Build settings - removed to allow auto-discovery of library sources
    ! FPM will auto-discover sources in src/ directory
    
    ! Fortran settings - conditional based on file type
    write(unit, '(a)') '[fortran]'
    if (present(is_preprocessed_file)) then
      if (is_preprocessed_file) then
        write(unit, '(a)') 'implicit-typing = false  # Enforces implicit none'
      end if
    end if
    write(unit, '(a)') 'implicit-external = false'
    write(unit, '(a)') 'source-form = "free"'
    write(unit, '(a)') ''
    
    ! Executable definition
    write(unit, '(a)') '[[executable]]'
    write(unit, '(a)') 'name = "' // trim(name) // '"'
    write(unit, '(a)') 'main = "main.f90"'
    write(unit, '(a)') ''
    
    ! Dependencies
    if (n_deps > 0) then
      write(unit, '(a)') '[dependencies]'
      do i = 1, n_deps
        if (len_trim(versions(i)) > 0) then
          write(unit, '(a)') trim(package_names(i)) // ' = { git = "' // &
                            trim(git_urls(i)) // '", tag = "' // trim(versions(i)) // '" }'
        else
          write(unit, '(a)') trim(package_names(i)) // ' = { git = "' // &
                            trim(git_urls(i)) // '" }'
        end if
      end do
    else
      write(unit, '(a)') '[executable.dependencies]'
    end if
    
    close(unit)
    
  end subroutine generate_fpm_with_deps
  
  subroutine generate_fpm_with_deps_from_config(project_dir, name, modules, n_modules, config_dir, &
                                                 is_preprocessed_file, custom_flags)
    character(len=*), intent(in) :: project_dir, name, config_dir
    type(module_info), dimension(:), intent(in) :: modules
    integer, intent(in) :: n_modules
    logical, intent(in), optional :: is_preprocessed_file
    character(len=*), intent(in), optional :: custom_flags
    
    character(len=512) :: toml_path, registry_path
    character(len=128) :: package_name, package_names(100)
    character(len=256) :: git_url, git_urls(100)
    character(len=32) :: version, versions(100)
    logical :: found
    integer :: unit, i, j, n_deps
    logical :: already_added
    
    ! Load registry from custom config directory
    registry_path = trim(config_dir) // '/registry.toml'
    call load_registry_from_path(registry_path)
    
    toml_path = trim(project_dir) // '/fpm.toml'
    
    ! Resolve dependencies
    n_deps = 0
    do i = 1, n_modules
      call resolve_module_with_version(modules(i)%name, package_name, git_url, version, found)
      
      if (found) then
        ! Print resolution info for test validation
        print '(a,a,a,a)', '  Module ', trim(modules(i)%name), ' -> package ', trim(package_name)
        
        ! Check if already added
        already_added = .false.
        do j = 1, n_deps
          if (trim(package_names(j)) == trim(package_name)) then
            already_added = .true.
            exit
          end if
        end do
        
        if (.not. already_added) then
          n_deps = n_deps + 1
          package_names(n_deps) = package_name
          git_urls(n_deps) = git_url
          versions(n_deps) = version
        end if
      end if
    end do
    
    ! Generate fpm.toml
    open(newunit=unit, file=toml_path, status='replace')
    
    ! Basic metadata
    write(unit, '(a)') 'name = "' // trim(name) // '"'
    write(unit, '(a)') 'version = "0.1.0"'
    write(unit, '(a)') ''
    
    ! Build settings - removed to allow auto-discovery of library sources
    ! FPM will auto-discover sources in src/ directory
    
    ! Fortran settings - conditional based on file type
    write(unit, '(a)') '[fortran]'
    if (present(is_preprocessed_file)) then
      if (is_preprocessed_file) then
        write(unit, '(a)') 'implicit-typing = false  # Enforces implicit none'
      end if
    end if
    write(unit, '(a)') 'implicit-external = false'
    write(unit, '(a)') 'source-form = "free"'
    write(unit, '(a)') ''
    
    ! Executable definition
    write(unit, '(a)') '[[executable]]'
    write(unit, '(a)') 'name = "' // trim(name) // '"'
    write(unit, '(a)') 'main = "main.f90"'
    write(unit, '(a)') ''
    
    ! Dependencies
    if (n_deps > 0) then
      write(unit, '(a)') '[dependencies]'
      do i = 1, n_deps
        if (len_trim(versions(i)) > 0) then
          write(unit, '(a)') trim(package_names(i)) // ' = { git = "' // &
                            trim(git_urls(i)) // '", tag = "' // trim(versions(i)) // '" }'
        else
          write(unit, '(a)') trim(package_names(i)) // ' = { git = "' // &
                            trim(git_urls(i)) // '" }'
        end if
      end do
    else
      write(unit, '(a)') '[executable.dependencies]'
    end if
    
    close(unit)
    
  end subroutine generate_fpm_with_deps_from_config
  
end module fpm_generator