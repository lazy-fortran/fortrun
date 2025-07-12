module type_system
  implicit none
  private
  
  ! Public constants for type kinds
  integer, parameter, public :: TYPE_UNKNOWN = 0
  integer, parameter, public :: TYPE_INTEGER = 1
  integer, parameter, public :: TYPE_REAL = 2
  integer, parameter, public :: TYPE_LOGICAL = 3
  integer, parameter, public :: TYPE_CHARACTER = 4
  integer, parameter, public :: TYPE_COMPLEX = 5
  
  ! Type information structure
  type, public :: type_info
    integer :: base_type = TYPE_UNKNOWN
    integer :: kind = 4  ! Default kinds
    integer :: char_len = -1  ! For character types (-1 = not applicable)
    logical :: is_array = .false.
    integer :: array_rank = 0
    integer, dimension(7) :: array_shape = -1  ! Up to 7D arrays
  end type type_info
  
  ! Public procedures
  public :: create_type_info
  public :: is_numeric_type
  public :: can_promote_types
  public :: promote_types
  public :: types_compatible
  public :: type_to_string
  
contains

  function create_type_info(base_type, kind, char_len) result(tinfo)
    integer, intent(in) :: base_type
    integer, intent(in), optional :: kind
    integer, intent(in), optional :: char_len
    type(type_info) :: tinfo
    
    tinfo%base_type = base_type
    tinfo%kind = 4  ! Default
    tinfo%char_len = -1
    tinfo%is_array = .false.
    tinfo%array_rank = 0
    tinfo%array_shape = -1
    
    if (present(kind)) tinfo%kind = kind
    if (present(char_len)) tinfo%char_len = char_len
    
  end function create_type_info

  function is_numeric_type(tinfo) result(is_numeric)
    type(type_info), intent(in) :: tinfo
    logical :: is_numeric
    
    is_numeric = (tinfo%base_type == TYPE_INTEGER .or. &
                  tinfo%base_type == TYPE_REAL .or. &
                  tinfo%base_type == TYPE_COMPLEX)
    
  end function is_numeric_type

  function can_promote_types(type1, type2) result(can_promote)
    type(type_info), intent(in) :: type1, type2
    logical :: can_promote
    
    ! Can promote if both are numeric
    can_promote = is_numeric_type(type1) .and. is_numeric_type(type2)
    
  end function can_promote_types

  function promote_types(type1, type2) result(result_type)
    type(type_info), intent(in) :: type1, type2
    type(type_info) :: result_type
    
    ! Type promotion rules
    if (type1%base_type == TYPE_REAL .or. type2%base_type == TYPE_REAL) then
      result_type = create_type_info(TYPE_REAL, 8)
    else if (type1%base_type == TYPE_INTEGER .and. type2%base_type == TYPE_INTEGER) then
      result_type = create_type_info(TYPE_INTEGER, max(type1%kind, type2%kind))
    else
      ! Default to unknown for incompatible types
      result_type = create_type_info(TYPE_UNKNOWN)
    end if
    
  end function promote_types

  function types_compatible(type1, type2) result(compatible)
    type(type_info), intent(in) :: type1, type2
    logical :: compatible
    
    ! Types are compatible if they're the same or can be promoted
    compatible = (type1%base_type == type2%base_type) .or. &
                 can_promote_types(type1, type2)
    
  end function types_compatible

  function type_to_string(tinfo) result(type_str)
    type(type_info), intent(in) :: tinfo
    character(len=64) :: type_str
    
    select case (tinfo%base_type)
    case (TYPE_INTEGER)
      if (tinfo%kind == 4) then
        type_str = 'integer'
      else
        write(type_str, '(a,i0,a)') 'integer(', tinfo%kind, ')'
      end if
      
    case (TYPE_REAL)
      if (tinfo%kind == 4) then
        type_str = 'real'
      else
        write(type_str, '(a,i0,a)') 'real(', tinfo%kind, ')'
      end if
      
    case (TYPE_LOGICAL)
      type_str = 'logical'
      
    case (TYPE_CHARACTER)
      if (tinfo%char_len >= 0) then
        write(type_str, '(a,i0,a)') 'character(len=', tinfo%char_len, ')'
      else
        type_str = 'character(len=*)'
      end if
      
    case (TYPE_COMPLEX)
      if (tinfo%kind == 4) then
        type_str = 'complex'
      else
        write(type_str, '(a,i0,a)') 'complex(', tinfo%kind, ')'
      end if
      
    case default
      type_str = 'unknown'
    end select
    
  end function type_to_string

end module type_system