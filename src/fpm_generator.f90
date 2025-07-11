module fpm_generator
  use module_scanner, only: module_info
  use registry_resolver, only: resolve_module_to_package, load_registry, load_registry_from_path
  implicit none
  private
  public :: generate_fpm_with_deps, generate_fpm_with_deps_from_config
  
contains

  subroutine generate_fpm_with_deps(project_dir, name, modules, n_modules)
    character(len=*), intent(in) :: project_dir, name
    type(module_info), dimension(:), intent(in) :: modules
    integer, intent(in) :: n_modules
    
    character(len=512) :: toml_path
    character(len=128) :: package_name, package_names(100)
    character(len=256) :: git_url, git_urls(100)
    logical :: found
    integer :: unit, i, j, n_deps
    logical :: already_added
    
    ! Load registry if not already loaded
    call load_registry()
    
    toml_path = trim(project_dir) // '/fpm.toml'
    
    ! Resolve dependencies
    n_deps = 0
    do i = 1, n_modules
      call resolve_module_to_package(modules(i)%name, package_name, git_url, found)
      
      if (found) then
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
        end if
      end if
    end do
    
    ! Generate fpm.toml
    open(newunit=unit, file=toml_path, status='replace')
    
    ! Basic metadata
    write(unit, '(a)') 'name = "' // trim(name) // '"'
    write(unit, '(a)') 'version = "0.1.0"'
    write(unit, '(a)') ''
    
    ! Build settings
    write(unit, '(a)') '[build]'
    write(unit, '(a)') 'auto-executables = false'
    write(unit, '(a)') 'auto-tests = false'
    write(unit, '(a)') 'auto-examples = false'
    write(unit, '(a)') ''
    
    ! Fortran settings
    write(unit, '(a)') '[fortran]'
    write(unit, '(a)') 'implicit-typing = false  # Enforces implicit none'
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
        write(unit, '(a)') trim(package_names(i)) // ' = { git = "' // &
                          trim(git_urls(i)) // '" }'
      end do
    else
      write(unit, '(a)') '[executable.dependencies]'
    end if
    
    close(unit)
    
  end subroutine generate_fpm_with_deps
  
  subroutine generate_fpm_with_deps_from_config(project_dir, name, modules, n_modules, config_dir)
    character(len=*), intent(in) :: project_dir, name, config_dir
    type(module_info), dimension(:), intent(in) :: modules
    integer, intent(in) :: n_modules
    
    character(len=512) :: toml_path, registry_path
    character(len=128) :: package_name, package_names(100)
    character(len=256) :: git_url, git_urls(100)
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
      call resolve_module_to_package(modules(i)%name, package_name, git_url, found)
      
      if (found) then
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
        end if
      end if
    end do
    
    ! Generate fpm.toml
    open(newunit=unit, file=toml_path, status='replace')
    
    ! Basic metadata
    write(unit, '(a)') 'name = "' // trim(name) // '"'
    write(unit, '(a)') 'version = "0.1.0"'
    write(unit, '(a)') ''
    
    ! Build settings
    write(unit, '(a)') '[build]'
    write(unit, '(a)') 'auto-executables = false'
    write(unit, '(a)') 'auto-tests = false'
    write(unit, '(a)') 'auto-examples = false'
    write(unit, '(a)') ''
    
    ! Fortran settings
    write(unit, '(a)') '[fortran]'
    write(unit, '(a)') 'implicit-typing = false  # Enforces implicit none'
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
        write(unit, '(a)') trim(package_names(i)) // ' = { git = "' // &
                          trim(git_urls(i)) // '" }'
      end do
    else
      write(unit, '(a)') '[executable.dependencies]'
    end if
    
    close(unit)
    
  end subroutine generate_fpm_with_deps_from_config
  
end module fpm_generator