!> Module-level caching for FPM builds
!>
!> This module implements a shared cache for compiled Fortran modules (.mod and .o files)
!> that can be reused across different projects. It follows FPM conventions and integrates
!> with the FPM source model.
!>
!> The cache structure is:
!>   ~/.cache/fortran/modules/
!>     <compiler_id>/
!>       <compiler_version>/
!>         <module_hash>/
!>           module.mod
!>           module.o
!>           metadata.json
!>
module fpm_module_cache
  use, intrinsic :: iso_fortran_env, only: int64
  use fpm_filesystem, only: exists, mkdir, join_path, list_files
  use fpm_strings, only: string_t, str
  use fpm_environment, only: get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
  use fpm_compiler, only: compiler_t, id_gcc, id_intel_classic_nix, &
                          id_intel_classic_mac, id_intel_classic_windows, &
                          id_intel_llvm_nix, id_intel_llvm_windows, &
                          id_nvhpc, id_nag, id_lfortran
  use fpm_model, only: srcfile_t
  use fpm_error, only: error_t
  implicit none
  private
  
  public :: module_cache_t
  public :: new_module_cache
  public :: get_module_cache_dir
  
  !> Type for managing module-level caching
  type :: module_cache_t
    !> Base cache directory
    character(:), allocatable :: cache_dir
    
    !> Compiler identifier for cache segregation
    character(:), allocatable :: compiler_id
    
    !> Compiler version for cache segregation
    character(:), allocatable :: compiler_version
    
    !> Whether caching is enabled
    logical :: enabled = .true.
    
  contains
    procedure :: init => cache_init
    procedure :: get_cache_key => cache_get_key
    procedure :: store_module => cache_store_module
    procedure :: retrieve_module => cache_retrieve_module
    procedure :: is_cached => cache_is_cached
    procedure :: get_module_dir => cache_get_module_dir
    procedure :: clean_stale => cache_clean_stale
  end type module_cache_t
  
contains

  !> Create a new module cache instance
  function new_module_cache(compiler, compiler_version) result(cache)
    type(compiler_t), intent(in) :: compiler
    character(*), intent(in), optional :: compiler_version
    type(module_cache_t) :: cache
    
    call cache%init(compiler, compiler_version)
    
  end function new_module_cache
  
  !> Get the default module cache directory
  function get_module_cache_dir() result(dir)
    character(:), allocatable :: dir
    character(len=256) :: home_dir
    integer :: stat
    
    ! Try XDG_CACHE_HOME first
    call get_environment_variable('XDG_CACHE_HOME', home_dir, status=stat)
    
    if (stat == 0 .and. len_trim(home_dir) > 0) then
      dir = trim(home_dir) // '/fortran/modules'
    else
      ! Fallback to HOME
      call get_environment_variable('HOME', home_dir, status=stat)
      
      if (stat == 0) then
        select case(get_os_type())
        case(OS_LINUX, OS_MACOS)
          dir = trim(home_dir) // '/.cache/fortran/modules'
        case(OS_WINDOWS)
          call get_environment_variable('LOCALAPPDATA', home_dir, status=stat)
          if (stat == 0) then
            dir = trim(home_dir) // '\fortran\cache\modules'
          else
            dir = '.fortran-module-cache'
          end if
        case default
          dir = '.fortran-module-cache'
        end select
      else
        dir = '.fortran-module-cache'
      end if
    end if
    
  end function get_module_cache_dir
  
  !> Initialize the module cache
  subroutine cache_init(this, compiler, compiler_version)
    class(module_cache_t), intent(inout) :: this
    type(compiler_t), intent(in) :: compiler
    character(*), intent(in), optional :: compiler_version
    character(:), allocatable :: env_val
    integer :: stat
    
    ! Set cache directory
    this%cache_dir = get_module_cache_dir()
    
    ! Extract compiler ID
    select case(compiler%id)
    case(id_gcc)
      this%compiler_id = 'gfortran'
    case(id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows)
      this%compiler_id = 'ifort'
    case(id_intel_llvm_nix, id_intel_llvm_windows)
      this%compiler_id = 'ifx'
    case(id_nvhpc)
      this%compiler_id = 'nvfortran'
    case(id_nag)
      this%compiler_id = 'nagfor'
    case(id_lfortran)
      this%compiler_id = 'lfortran'
    case default
      this%compiler_id = 'unknown'
    end select
    
    ! Set compiler version
    if (present(compiler_version)) then
      this%compiler_version = compiler_version
    else
      this%compiler_version = 'default'
    end if
    
    ! Check if caching is disabled via environment
    call get_environment_variable('FPM_NO_MODULE_CACHE', env_val, status=stat)
    if (stat == 0 .and. len_trim(env_val) > 0) then
      this%enabled = .false.
    end if
    
    ! Create cache directory structure if enabled
    if (this%enabled) then
      call ensure_cache_dirs(this)
    end if
    
  end subroutine cache_init
  
  !> Generate cache key for a source file
  function cache_get_key(this, srcfile, dependencies) result(key)
    class(module_cache_t), intent(in) :: this
    type(srcfile_t), intent(in) :: srcfile
    type(srcfile_t), intent(in), optional :: dependencies(:)
    character(len=64) :: key
    integer(int64) :: hash_val
    character(len=16) :: hex_str
    integer :: i
    
    ! Start with source file digest
    hash_val = srcfile%digest
    
    ! Include dependency digests if provided
    if (present(dependencies)) then
      do i = 1, size(dependencies)
        hash_val = ieor(hash_val, dependencies(i)%digest)
      end do
    end if
    
    ! Include compiler flags in hash (simplified for now)
    ! In a full implementation, we'd hash relevant compiler flags
    
    ! Convert to hex string
    write(hex_str, '(z16.16)') hash_val
    key = hex_str
    
  end function cache_get_key
  
  !> Store a compiled module in the cache
  subroutine cache_store_module(this, srcfile, cache_key, build_dir, error)
    class(module_cache_t), intent(in) :: this
    type(srcfile_t), intent(in) :: srcfile
    character(*), intent(in) :: cache_key
    character(*), intent(in) :: build_dir
    type(error_t), allocatable, intent(out) :: error
    
    character(:), allocatable :: module_dir, src_file, dst_file
    type(string_t), allocatable :: module_files(:)
    integer :: i, unit, iostat
    logical :: found_files
    
    if (.not. this%enabled) return
    
    ! Get cache directory for this module
    module_dir = this%get_module_dir(cache_key)
    
    ! Create directory
    call mkdir(module_dir)
    
    found_files = .false.
    
    ! Copy .mod files for each provided module
    if (allocated(srcfile%modules_provided)) then
      do i = 1, size(srcfile%modules_provided)
        ! Look for .mod file
        src_file = join_path(build_dir, srcfile%modules_provided(i)%s // '.mod')
        if (exists(src_file)) then
          dst_file = join_path(module_dir, srcfile%modules_provided(i)%s // '.mod')
          call copy_file(src_file, dst_file, iostat)
          if (iostat == 0) found_files = .true.
        end if
        
        ! Also look for .smod file (submodules)
        src_file = join_path(build_dir, srcfile%modules_provided(i)%s // '.smod')
        if (exists(src_file)) then
          dst_file = join_path(module_dir, srcfile%modules_provided(i)%s // '.smod')
          call copy_file(src_file, dst_file, iostat)
          if (iostat == 0) found_files = .true.
        end if
      end do
    end if
    
    ! Copy object file
    src_file = get_object_name(srcfile%file_name, build_dir)
    if (exists(src_file)) then
      dst_file = join_path(module_dir, 'module.o')
      call copy_file(src_file, dst_file, iostat)
      if (iostat == 0) found_files = .true.
    end if
    
    ! Write metadata
    if (found_files) then
      call write_cache_metadata(module_dir, srcfile, cache_key)
    end if
    
  end subroutine cache_store_module
  
  !> Retrieve a cached module
  subroutine cache_retrieve_module(this, cache_key, target_dir, srcfile, found, error)
    class(module_cache_t), intent(in) :: this
    character(*), intent(in) :: cache_key
    character(*), intent(in) :: target_dir
    type(srcfile_t), intent(in) :: srcfile
    logical, intent(out) :: found
    type(error_t), allocatable, intent(out) :: error
    
    character(:), allocatable :: module_dir, src_file, dst_file
    type(string_t), allocatable :: cached_files(:)
    integer :: i, iostat
    
    found = .false.
    if (.not. this%enabled) return
    
    module_dir = this%get_module_dir(cache_key)
    
    ! Check if cache directory exists
    if (.not. exists(module_dir)) return
    
    ! Copy module files
    if (allocated(srcfile%modules_provided)) then
      do i = 1, size(srcfile%modules_provided)
        ! Copy .mod file
        src_file = join_path(module_dir, srcfile%modules_provided(i)%s // '.mod')
        if (exists(src_file)) then
          dst_file = join_path(target_dir, srcfile%modules_provided(i)%s // '.mod')
          call copy_file(src_file, dst_file, iostat)
          if (iostat == 0) found = .true.
        end if
        
        ! Copy .smod file if exists
        src_file = join_path(module_dir, srcfile%modules_provided(i)%s // '.smod')
        if (exists(src_file)) then
          dst_file = join_path(target_dir, srcfile%modules_provided(i)%s // '.smod')
          call copy_file(src_file, dst_file, iostat)
        end if
      end do
    end if
    
    ! Copy object file
    src_file = join_path(module_dir, 'module.o')
    if (exists(src_file)) then
      dst_file = get_object_name(srcfile%file_name, target_dir)
      call copy_file(src_file, dst_file, iostat)
      if (iostat == 0) found = .true.
    end if
    
  end subroutine cache_retrieve_module
  
  !> Check if a module is cached
  function cache_is_cached(this, cache_key) result(is_cached)
    class(module_cache_t), intent(in) :: this
    character(*), intent(in) :: cache_key
    logical :: is_cached
    character(:), allocatable :: module_dir
    
    is_cached = .false.
    if (.not. this%enabled) return
    
    module_dir = this%get_module_dir(cache_key)
    is_cached = exists(module_dir)
    
    ! Could add more validation here (check metadata, etc.)
    
  end function cache_is_cached
  
  !> Get the cache directory for a specific module
  function cache_get_module_dir(this, cache_key) result(dir)
    class(module_cache_t), intent(in) :: this
    character(*), intent(in) :: cache_key
    character(:), allocatable :: dir
    
    dir = join_path(this%cache_dir, this%compiler_id)
    dir = join_path(dir, this%compiler_version)
    dir = join_path(dir, cache_key)
    
  end function cache_get_module_dir
  
  !> Clean stale cache entries
  subroutine cache_clean_stale(this, max_age_days, error)
    class(module_cache_t), intent(in) :: this
    integer, intent(in) :: max_age_days
    type(error_t), allocatable, intent(out) :: error
    
    ! Implementation would check metadata timestamps
    ! and remove entries older than max_age_days
    ! For now, this is a placeholder
    
  end subroutine cache_clean_stale
  
  !> Ensure cache directory structure exists
  subroutine ensure_cache_dirs(cache)
    type(module_cache_t), intent(in) :: cache
    character(:), allocatable :: dir
    
    ! Create base cache directory
    call mkdir(cache%cache_dir)
    
    ! Create compiler-specific directory
    dir = join_path(cache%cache_dir, cache%compiler_id)
    call mkdir(dir)
    
    ! Create version-specific directory
    dir = join_path(dir, cache%compiler_version)
    call mkdir(dir)
    
  end subroutine ensure_cache_dirs
  
  !> Copy a file
  subroutine copy_file(src, dst, iostat)
    character(*), intent(in) :: src, dst
    integer, intent(out) :: iostat
    character(len=512) :: command
    
    ! Use system copy command
    select case(get_os_type())
    case(OS_WINDOWS)
      write(command, '(a,a,a,a,a)') 'copy "', src, '" "', dst, '" >nul 2>&1'
    case default
      write(command, '(a,a,a,a,a)') 'cp "', src, '" "', dst, '" 2>/dev/null'
    end select
    
    call execute_command_line(command, exitstat=iostat)
    
  end subroutine copy_file
  
  !> Get object file name from source file name
  function get_object_name(source_file, build_dir) result(object_file)
    character(*), intent(in) :: source_file, build_dir
    character(:), allocatable :: object_file
    integer :: i
    
    ! Extract base name and change extension
    i = index(source_file, '.', back=.true.)
    if (i > 0) then
      object_file = join_path(build_dir, source_file(1:i-1) // '.o')
    else
      object_file = join_path(build_dir, source_file // '.o')
    end if
    
    ! Handle path separators
    do i = 1, len(object_file)
      if (object_file(i:i) == '/' .or. object_file(i:i) == '\') then
        object_file(i:i) = '_'
      end if
    end do
    
  end function get_object_name
  
  !> Write cache metadata file
  subroutine write_cache_metadata(cache_dir, srcfile, cache_key)
    character(*), intent(in) :: cache_dir
    type(srcfile_t), intent(in) :: srcfile
    character(*), intent(in) :: cache_key
    
    character(:), allocatable :: metadata_file
    integer :: unit, i
    integer :: timestamp(8)
    
    metadata_file = join_path(cache_dir, 'metadata.txt')
    
    open(newunit=unit, file=metadata_file, status='replace', action='write')
    
    ! Write timestamp
    call date_and_time(values=timestamp)
    write(unit, '(a,i0,5(a,i2.2))') 'timestamp: ', timestamp(1), '-', timestamp(2), &
                                     '-', timestamp(3), ' ', timestamp(5), ':', &
                                     timestamp(6), ':', timestamp(7)
    
    ! Write cache key
    write(unit, '(a,a)') 'cache_key: ', cache_key
    
    ! Write source file
    write(unit, '(a,a)') 'source_file: ', srcfile%file_name
    
    ! Write digest
    write(unit, '(a,z16.16)') 'digest: ', srcfile%digest
    
    ! Write provided modules
    if (allocated(srcfile%modules_provided)) then
      write(unit, '(a)') 'modules_provided:'
      do i = 1, size(srcfile%modules_provided)
        write(unit, '(a,a)') '  - ', srcfile%modules_provided(i)%s
      end do
    end if
    
    close(unit)
    
  end subroutine write_cache_metadata

end module fpm_module_cache