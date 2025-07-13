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
  integer, parameter, public :: TYPE_DERIVED = 6
  
  ! Type information structure
  type, public :: type_info
    integer :: base_type = TYPE_UNKNOWN
    integer :: kind = 4  ! Default kinds
    integer :: char_len = -1  ! For character types (-1 = not applicable)
    logical :: is_array = .false.
    integer :: array_rank = 0
    integer, dimension(7) :: array_shape = -1  ! Up to 7D arrays
    character(len=63) :: type_name = ''  ! For derived types
  end type type_info
  
  ! Public procedures
  public :: create_type_info
  public :: create_array_type_info
  public :: create_derived_type_info
  public :: is_numeric_type
  public :: can_promote_types
  public :: promote_types
  public :: types_compatible
  public :: type_to_string
  public :: array_shapes_compatible
  
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

  function create_array_type_info(base_type, kind, shape_array) result(tinfo)
    integer, intent(in) :: base_type
    integer, intent(in), optional :: kind
    integer, dimension(:), intent(in) :: shape_array
    type(type_info) :: tinfo
    
    integer :: i
    
    tinfo = create_type_info(base_type, kind)
    tinfo%is_array = .true.
    tinfo%array_rank = size(shape_array)
    
    do i = 1, min(size(shape_array), 7)
      tinfo%array_shape(i) = shape_array(i)
    end do
    
  end function create_array_type_info

  function create_derived_type_info(type_name) result(tinfo)
    character(len=*), intent(in) :: type_name
    type(type_info) :: tinfo
    
    tinfo = create_type_info(TYPE_DERIVED)
    tinfo%type_name = type_name
    
  end function create_derived_type_info

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
    character(len=32) :: base_str
    
    ! Get base type string
    select case (tinfo%base_type)
    case (TYPE_INTEGER)
      if (tinfo%kind == 4) then
        base_str = 'integer'
      else
        write(base_str, '(a,i0,a)') 'integer(', tinfo%kind, ')'
      end if
      
    case (TYPE_REAL)
      if (tinfo%kind == 4) then
        base_str = 'real'
      else
        write(base_str, '(a,i0,a)') 'real(', tinfo%kind, ')'
      end if
      
    case (TYPE_LOGICAL)
      base_str = 'logical'
      
    case (TYPE_CHARACTER)
      if (tinfo%char_len >= 0) then
        write(base_str, '(a,i0,a)') 'character(len=', tinfo%char_len, ')'
      else
        base_str = 'character(len=*)'
      end if
      
    case (TYPE_COMPLEX)
      if (tinfo%kind == 4) then
        base_str = 'complex'
      else
        write(base_str, '(a,i0,a)') 'complex(', tinfo%kind, ')'
      end if
      
    case (TYPE_DERIVED)
      if (len_trim(tinfo%type_name) > 0) then
        write(base_str, '(a,a,a)') 'type(', trim(tinfo%type_name), ')'
      else
        base_str = 'type(?)'
      end if
      
    case default
      base_str = 'unknown'
    end select
    
    ! Add array dimensions if needed
    if (tinfo%is_array) then
      if (tinfo%array_rank == 1) then
        write(type_str, '(a,a,i0,a)') trim(base_str), ', dimension(', tinfo%array_shape(1), ')'
      else if (tinfo%array_rank == 2) then
        write(type_str, '(a,a,i0,a,i0,a)') trim(base_str), ', dimension(', &
          tinfo%array_shape(1), ',', tinfo%array_shape(2), ')'
      else
        write(type_str, '(a,a)') trim(base_str), ', dimension(?)'
      end if
    else
      type_str = base_str
    end if
    
  end function type_to_string

  function array_shapes_compatible(type1, type2) result(compatible)
    type(type_info), intent(in) :: type1, type2
    logical :: compatible
    
    integer :: i
    
    compatible = .false.
    
    if (type1%array_rank /= type2%array_rank) return
    
    do i = 1, type1%array_rank
      if (type1%array_shape(i) /= type2%array_shape(i)) return
    end do
    
    compatible = .true.
    
  end function array_shapes_compatible

end module type_system