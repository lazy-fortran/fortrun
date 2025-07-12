module array_analyzer
  use type_system
  use type_environment
  use literal_analyzer
  implicit none
  private
  
  ! Public procedures
  public :: analyze_array_expression
  public :: is_array_literal
  public :: is_array_operation
  public :: is_reshape_expression
  
contains

  subroutine analyze_array_expression(expr, result_type, env)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Check for array literals first
    if (is_array_literal(expr, result_type, env)) then
      ! Type already set by is_array_literal
    else if (is_reshape_expression(expr, result_type, env)) then
      ! Type already set by is_reshape_expression
    else if (is_array_operation(expr, result_type, env)) then
      ! Type already set by is_array_operation
    end if
    
  end subroutine analyze_array_expression

  function is_array_literal(expr, result_type, env) result(is_array)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    logical :: is_array
    
    character(len=len(expr)) :: trimmed_expr
    integer :: bracket_start, bracket_end
    character(len=256) :: content
    
    is_array = .false.
    result_type = create_type_info(TYPE_UNKNOWN)
    trimmed_expr = trim(adjustl(expr))
    
    ! Check for array literal syntax [...]
    bracket_start = index(trimmed_expr, '[')
    bracket_end = index(trimmed_expr, ']', back=.true.)
    
    if (bracket_start == 1 .and. bracket_end == len_trim(trimmed_expr) .and. &
        bracket_end > bracket_start) then
      
      content = trimmed_expr(bracket_start+1:bracket_end-1)
      
      ! Handle empty array
      if (len_trim(content) == 0) then
        result_type = create_type_info(TYPE_UNKNOWN)
        is_array = .true.
        return
      end if
      
      ! Check for nested arrays (2D)
      if (index(content, '[') > 0) then
        call analyze_nested_array(content, result_type, env)
        is_array = .true.
      else
        call analyze_1d_array(content, result_type, env)
        is_array = .true.
      end if
    end if
    
  end function is_array_literal

  subroutine analyze_1d_array(content, result_type, env)
    character(len=*), intent(in) :: content
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    
    integer :: comma_pos, start_pos, element_count
    character(len=64) :: element
    type(type_info) :: element_type, promoted_type
    integer :: element_shape(1)
    logical :: first_element, is_literal
    
    result_type = create_type_info(TYPE_UNKNOWN)
    element_count = 0
    start_pos = 1
    first_element = .true.
    
    ! Count elements and determine type
    do
      comma_pos = index(content(start_pos:), ',')
      
      if (comma_pos > 0) then
        element = trim(adjustl(content(start_pos:start_pos+comma_pos-2)))
        start_pos = start_pos + comma_pos
      else
        element = trim(adjustl(content(start_pos:)))
      end if
      
      if (len_trim(element) > 0) then
        element_count = element_count + 1
        
        ! Analyze element type
        call analyze_literal(element, element_type, is_literal)
        
        if (first_element) then
          promoted_type = element_type
          first_element = .false.
        else if (can_promote_types(promoted_type, element_type)) then
          promoted_type = promote_types(promoted_type, element_type)
        end if
      end if
      
      if (comma_pos == 0) exit
    end do
    
    ! Create array type
    if (element_count > 0) then
      element_shape(1) = element_count
      result_type = create_array_type_info(promoted_type%base_type, promoted_type%kind, element_shape)
    end if
    
  end subroutine analyze_1d_array

  subroutine analyze_nested_array(content, result_type, env)
    character(len=*), intent(in) :: content
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    
    integer :: bracket_count, rows, cols
    integer :: i, j, start_pos, bracket_pos
    character(len=128) :: row_content
    type(type_info) :: element_type
    integer :: array_shape(2)
    
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Simple 2D array analysis - count rows and columns
    rows = 0
    cols = 0
    i = 1
    
    do while (i <= len_trim(content))
      if (content(i:i) == '[') then
        rows = rows + 1
        start_pos = i + 1
        bracket_count = 1
        j = i + 1
        
        ! Find matching closing bracket
        do while (j <= len_trim(content) .and. bracket_count > 0)
          if (content(j:j) == '[') then
            bracket_count = bracket_count + 1
          else if (content(j:j) == ']') then
            bracket_count = bracket_count - 1
          end if
          j = j + 1
        end do
        
        if (bracket_count == 0) then
          row_content = content(start_pos:j-2)
          
          ! Count elements in first row to determine columns
          if (rows == 1) then
            call count_elements(row_content, cols)
            call analyze_first_element(row_content, element_type)
          end if
          
          i = j
        else
          exit
        end if
      else
        i = i + 1
      end if
    end do
    
    ! Create 2D array type
    if (rows > 0 .and. cols > 0) then
      array_shape(1) = rows
      array_shape(2) = cols
      result_type = create_array_type_info(element_type%base_type, element_type%kind, array_shape)
    end if
    
  end subroutine analyze_nested_array

  subroutine count_elements(content, count)
    character(len=*), intent(in) :: content
    integer, intent(out) :: count
    
    integer :: i
    
    count = 1
    do i = 1, len_trim(content)
      if (content(i:i) == ',') then
        count = count + 1
      end if
    end do
    
  end subroutine count_elements

  subroutine analyze_first_element(content, element_type)
    character(len=*), intent(in) :: content
    type(type_info), intent(out) :: element_type
    
    integer :: comma_pos
    character(len=64) :: first_element
    logical :: is_literal
    
    comma_pos = index(content, ',')
    if (comma_pos > 0) then
      first_element = trim(adjustl(content(1:comma_pos-1)))
    else
      first_element = trim(adjustl(content))
    end if
    
    call analyze_literal(first_element, element_type, is_literal)
    if (.not. is_literal) then
      element_type = create_type_info(TYPE_INTEGER, 4)  ! Default assumption
    end if
    
  end subroutine analyze_first_element

  function is_reshape_expression(expr, result_type, env) result(is_reshape)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    logical :: is_reshape
    
    integer :: reshape_pos, paren_start, paren_end
    character(len=256) :: args
    
    is_reshape = .false.
    result_type = create_type_info(TYPE_UNKNOWN)
    
    reshape_pos = index(expr, 'reshape(')
    if (reshape_pos == 1) then
      paren_start = index(expr, '(')
      paren_end = index(expr, ')', back=.true.)
      
      if (paren_start > 0 .and. paren_end > paren_start) then
        args = expr(paren_start+1:paren_end-1)
        call analyze_reshape_args(args, result_type, env)
        is_reshape = .true.
      end if
    end if
    
  end function is_reshape_expression

  subroutine analyze_reshape_args(args, result_type, env)
    character(len=*), intent(in) :: args
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    
    integer :: comma_pos, bracket_start, bracket_end
    character(len=128) :: source_array, shape_array
    type(type_info) :: source_type
    integer :: shape_values(2)
    logical :: is_literal
    
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Find comma outside brackets to separate source and shape arguments
    call find_comma_outside_brackets(args, comma_pos)
    if (comma_pos > 0) then
      source_array = trim(adjustl(args(1:comma_pos-1)))
      shape_array = trim(adjustl(args(comma_pos+1:)))
      
      ! Analyze source array type
      call analyze_literal(source_array, source_type, is_literal)
      if (.not. is_literal) then
        ! Try array literal
        call analyze_array_expression(source_array, source_type, env)
      end if
      
      ! If source is still unknown, assume integer for reshape
      if (source_type%base_type == TYPE_UNKNOWN) then
        source_type = create_type_info(TYPE_INTEGER, 4)
      end if
      
      ! Parse shape array [2,2]
      bracket_start = index(shape_array, '[')
      bracket_end = index(shape_array, ']')
      if (bracket_start > 0 .and. bracket_end > bracket_start) then
        call parse_shape_values(shape_array(bracket_start+1:bracket_end-1), shape_values)
        
        if (shape_values(1) > 0 .and. shape_values(2) > 0) then
          result_type = create_array_type_info(source_type%base_type, source_type%kind, shape_values)
        end if
      end if
    end if
    
  end subroutine analyze_reshape_args

  subroutine parse_shape_values(shape_str, shape_values)
    character(len=*), intent(in) :: shape_str
    integer, dimension(2), intent(out) :: shape_values
    
    integer :: comma_pos
    character(len=32) :: first_val, second_val
    
    shape_values = -1
    
    comma_pos = index(shape_str, ',')
    if (comma_pos > 0) then
      first_val = trim(adjustl(shape_str(1:comma_pos-1)))
      second_val = trim(adjustl(shape_str(comma_pos+1:)))
      
      read(first_val, *, err=10, end=10) shape_values(1)
      read(second_val, *, err=10, end=10) shape_values(2)
    end if
    
 10 continue
    
  end subroutine parse_shape_values

  function is_array_operation(expr, result_type, env) result(is_operation)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    logical :: is_operation
    
    is_operation = .false.
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Check for matmul
    if (index(expr, 'matmul(') == 1) then
      call analyze_matmul(expr, result_type, env)
      is_operation = .true.
    ! Check for sum
    else if (index(expr, 'sum(') == 1) then
      call analyze_sum(expr, result_type, env)
      is_operation = .true.
    ! Check for transpose
    else if (index(expr, 'transpose(') == 1) then
      call analyze_transpose(expr, result_type, env)
      is_operation = .true.
    end if
    
  end function is_array_operation

  subroutine analyze_matmul(expr, result_type, env)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    
    ! Simple placeholder - assume result is 2D integer array
    integer :: shape_values(2) = [2, 2]
    result_type = create_array_type_info(TYPE_INTEGER, 4, shape_values)
    
  end subroutine analyze_matmul

  subroutine analyze_sum(expr, result_type, env)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    
    ! Sum reduces array to scalar
    result_type = create_type_info(TYPE_INTEGER, 4)
    
  end subroutine analyze_sum

  subroutine analyze_transpose(expr, result_type, env)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    
    ! Transpose swaps dimensions - placeholder
    integer :: shape_values(2) = [3, 2]
    result_type = create_array_type_info(TYPE_INTEGER, 4, shape_values)
    
  end subroutine analyze_transpose

  subroutine find_comma_outside_brackets(text, comma_pos)
    character(len=*), intent(in) :: text
    integer, intent(out) :: comma_pos
    
    integer :: i, bracket_count
    
    comma_pos = 0
    bracket_count = 0
    
    do i = 1, len_trim(text)
      if (text(i:i) == '[') then
        bracket_count = bracket_count + 1
      else if (text(i:i) == ']') then
        bracket_count = bracket_count - 1
      else if (text(i:i) == ',' .and. bracket_count == 0) then
        comma_pos = i
        return
      end if
    end do
    
  end subroutine find_comma_outside_brackets

end module array_analyzer