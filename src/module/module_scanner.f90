module module_scanner
  use logger_utils, only: print_error
  implicit none
  private
  public :: scan_modules, module_info
  
  type :: module_info
    character(len=128) :: name
  end type module_info
  
  ! List of intrinsic modules to ignore
  character(len=32), dimension(10), parameter :: intrinsic_modules = [ &
    'iso_fortran_env  ', &
    'iso_c_binding    ', &
    'ieee_arithmetic  ', &
    'ieee_exceptions  ', &
    'ieee_features    ', &
    'omp_lib          ', &
    'openacc          ', &
    'mpi              ', &
    'mpi_f08          ', &
    'coarray_intrinsic' &
  ]
  
contains

  subroutine scan_modules(filename, modules, n_modules)
    character(len=*), intent(in) :: filename
    type(module_info), dimension(:), allocatable, intent(out) :: modules
    integer, intent(out) :: n_modules
    
    integer :: unit, iostat, i
    character(len=512) :: line
    character(len=128) :: module_name
    type(module_info), dimension(:), allocatable :: temp_modules
    integer :: max_modules
    logical :: is_intrinsic
    
    max_modules = 100
    allocate(temp_modules(max_modules))
    n_modules = 0
    
    open(newunit=unit, file=filename, status='old', iostat=iostat)
    if (iostat /= 0) then
      call print_error('Cannot open file '//trim(filename))
      return
    end if
    
    do
      read(unit, '(a)', iostat=iostat) line
      if (iostat /= 0) exit
      
      ! Process the line
      call process_use_statement(line, module_name)
      
      if (len_trim(module_name) > 0) then
        ! Check if it's an intrinsic module
        is_intrinsic = .false.
        do i = 1, size(intrinsic_modules)
          if (trim(module_name) == trim(intrinsic_modules(i))) then
            is_intrinsic = .true.
            exit
          end if
        end do
        
        if (.not. is_intrinsic) then
          n_modules = n_modules + 1
          if (n_modules <= max_modules) then
            temp_modules(n_modules)%name = module_name
          end if
        end if
      end if
    end do
    
    close(unit)
    
    ! Allocate output array
    if (n_modules > 0) then
      allocate(modules(n_modules))
      modules = temp_modules(1:n_modules)
    else
      allocate(modules(0))
    end if
    
    deallocate(temp_modules)
    
  end subroutine scan_modules
  
  subroutine process_use_statement(line, module_name)
    character(len=*), intent(in) :: line
    character(len=*), intent(out) :: module_name
    
    character(len=512) :: trimmed_line
    integer :: use_pos, comma_pos, only_pos
    integer :: start_pos, end_pos
    logical :: is_intrinsic_decl
    
    module_name = ''
    trimmed_line = adjustl(line)
    
    ! Skip comments and empty lines
    if (len_trim(trimmed_line) == 0) return
    if (trimmed_line(1:1) == '!') return
    
    ! Find 'use' keyword
    use_pos = index(trimmed_line, 'use ')
    if (use_pos == 0) return
    
    ! Check if it's at the beginning (allowing for whitespace)
    if (use_pos > 1) then
      if (trimmed_line(1:use_pos-1) /= ' ') return
    end if
    
    ! Check for 'intrinsic' keyword
    is_intrinsic_decl = index(trimmed_line, 'intrinsic') > 0
    
    ! Extract module name
    start_pos = use_pos + 4  ! After 'use '
    
    ! Skip 'intrinsic ::' if present
    if (is_intrinsic_decl) then
      start_pos = index(trimmed_line, '::')
      if (start_pos > 0) then
        start_pos = start_pos + 2
      else
        ! No :: found, skip to after 'intrinsic'
        start_pos = index(trimmed_line, 'intrinsic') + 9
      end if
    end if
    
    ! Skip whitespace
    do while (start_pos <= len_trim(trimmed_line) .and. &
              trimmed_line(start_pos:start_pos) == ' ')
      start_pos = start_pos + 1
    end do
    
    ! Find end of module name (comma or 'only' or end of line)
    comma_pos = index(trimmed_line(start_pos:), ',')
    only_pos = index(trimmed_line(start_pos:), ' only')
    
    if (comma_pos > 0 .and. (only_pos == 0 .or. comma_pos < only_pos)) then
      end_pos = start_pos + comma_pos - 2
    else if (only_pos > 0) then
      end_pos = start_pos + only_pos - 2
    else
      end_pos = len_trim(trimmed_line)
    end if
    
    ! Remove trailing whitespace
    do while (end_pos > start_pos .and. &
              trimmed_line(end_pos:end_pos) == ' ')
      end_pos = end_pos - 1
    end do
    
    if (end_pos >= start_pos) then
      module_name = trimmed_line(start_pos:end_pos)
    end if
    
  end subroutine process_use_statement
  
end module module_scanner