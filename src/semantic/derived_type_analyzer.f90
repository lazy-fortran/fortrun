module derived_type_analyzer
  use type_system
  use type_environment
  use literal_analyzer
  implicit none
  private
  
  ! Public procedures
  public :: analyze_derived_type_expression
  public :: is_field_access
  public :: is_constructor_call
  public :: extract_field_info
  public :: extract_constructor_info
  
contains

  subroutine analyze_derived_type_expression(expr, result_type, env)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Check for field access patterns
    if (is_field_access(expr, result_type, env)) then
      ! Type already set by is_field_access
    else if (is_constructor_call(expr, result_type, env)) then
      ! Type already set by is_constructor_call
    end if
    
  end subroutine analyze_derived_type_expression

  function is_field_access(expr, result_type, env) result(is_field)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    logical :: is_field
    
    character(len=len(expr)) :: trimmed_expr
    integer :: dot_pos, equals_pos
    character(len=64) :: variable_name, field_name, value_expr, derived_type_name
    type(type_info) :: value_type
    logical :: is_literal
    
    is_field = .false.
    result_type = create_type_info(TYPE_UNKNOWN)
    trimmed_expr = trim(adjustl(expr))
    
    ! Look for assignment pattern: variable.field = value
    equals_pos = index(trimmed_expr, '=')
    if (equals_pos == 0) return
    
    ! Extract left side (variable.field) and right side (value)
    variable_name = trim(adjustl(trimmed_expr(1:equals_pos-1)))
    value_expr = trim(adjustl(trimmed_expr(equals_pos+1:)))
    
    ! Check if left side has dot notation
    dot_pos = index(variable_name, '.')
    if (dot_pos == 0) return
    
    ! Extract variable and field names
    variable_name = trim(adjustl(variable_name(1:dot_pos-1)))
    field_name = trim(adjustl(variable_name(dot_pos+1:)))
    
    ! Analyze the value type
    call analyze_literal(value_expr, value_type, is_literal)
    
    ! If not a simple literal, check for array literal pattern
    if (.not. is_literal) then
      if (index(value_expr, '[') == 1 .and. index(value_expr, ']') > 1) then
        ! Simple array literal detected
        value_type = create_type_info(TYPE_INTEGER, 4)  ! Assume integer array
        is_literal = .true.
      end if
    end if
    
    ! For now, create a derived type based on the variable name and field
    if (value_type%base_type /= TYPE_UNKNOWN) then
      ! Create derived type name based on variable
      write(derived_type_name, '(a,a)') trim(variable_name), '_type'
      result_type = create_derived_type_info(derived_type_name)
      is_field = .true.
    end if
    
  end function is_field_access

  function is_constructor_call(expr, result_type, env) result(is_constructor)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    logical :: is_constructor
    
    character(len=len(expr)) :: trimmed_expr
    integer :: equals_pos, paren_pos
    character(len=64) :: variable_name, constructor_expr, type_name
    
    is_constructor = .false.
    result_type = create_type_info(TYPE_UNKNOWN)
    trimmed_expr = trim(adjustl(expr))
    
    ! Look for assignment pattern: variable = type_name(...)
    equals_pos = index(trimmed_expr, '=')
    if (equals_pos == 0) return
    
    variable_name = trim(adjustl(trimmed_expr(1:equals_pos-1)))
    constructor_expr = trim(adjustl(trimmed_expr(equals_pos+1:)))
    
    ! Check if right side looks like constructor: type_name(...)
    paren_pos = index(constructor_expr, '(')
    if (paren_pos == 0) return
    
    type_name = trim(adjustl(constructor_expr(1:paren_pos-1)))
    
    ! Simple pattern check - if it ends with _t, it's likely a constructor
    if (index(type_name, '_t') > 0) then
      result_type = create_derived_type_info(type_name)
      is_constructor = .true.
    end if
    
  end function is_constructor_call

  subroutine extract_field_info(expr, variable_name, field_name, value_type)
    character(len=*), intent(in) :: expr
    character(len=*), intent(out) :: variable_name, field_name
    type(type_info), intent(out) :: value_type
    
    character(len=len(expr)) :: trimmed_expr, left_side, value_expr
    integer :: equals_pos, dot_pos
    logical :: is_literal
    
    trimmed_expr = trim(adjustl(expr))
    equals_pos = index(trimmed_expr, '=')
    
    if (equals_pos > 0) then
      left_side = trim(adjustl(trimmed_expr(1:equals_pos-1)))
      value_expr = trim(adjustl(trimmed_expr(equals_pos+1:)))
      
      dot_pos = index(left_side, '.')
      if (dot_pos > 0) then
        variable_name = trim(adjustl(left_side(1:dot_pos-1)))
        field_name = trim(adjustl(left_side(dot_pos+1:)))
        
        call analyze_literal(value_expr, value_type, is_literal)
      end if
    end if
    
  end subroutine extract_field_info

  subroutine extract_constructor_info(expr, variable_name, type_name, args)
    character(len=*), intent(in) :: expr
    character(len=*), intent(out) :: variable_name, type_name
    character(len=*), intent(out) :: args
    
    character(len=len(expr)) :: trimmed_expr, constructor_expr
    integer :: equals_pos, paren_start, paren_end
    
    trimmed_expr = trim(adjustl(expr))
    equals_pos = index(trimmed_expr, '=')
    
    if (equals_pos > 0) then
      variable_name = trim(adjustl(trimmed_expr(1:equals_pos-1)))
      constructor_expr = trim(adjustl(trimmed_expr(equals_pos+1:)))
      
      paren_start = index(constructor_expr, '(')
      paren_end = index(constructor_expr, ')', back=.true.)
      
      if (paren_start > 0 .and. paren_end > paren_start) then
        type_name = trim(adjustl(constructor_expr(1:paren_start-1)))
        args = constructor_expr(paren_start+1:paren_end-1)
      end if
    end if
    
  end subroutine extract_constructor_info

end module derived_type_analyzer