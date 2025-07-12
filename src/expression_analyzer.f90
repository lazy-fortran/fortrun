module expression_analyzer
  use type_system
  use type_environment
  use literal_analyzer
  implicit none
  private
  
  ! Public procedures
  public :: analyze_expression
  public :: is_arithmetic_expression
  public :: is_comparison_expression
  public :: is_logical_expression
  public :: is_intrinsic_function
  public :: strip_comment
  
contains

  subroutine analyze_expression(expr, result_type, env)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    
    character(len=len(expr)) :: trimmed_expr
    logical :: is_literal, found
    
    ! Initialize to unknown
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Trim whitespace and remove comments
    trimmed_expr = adjustl(expr)
    call strip_comment(trimmed_expr)
    
    ! Check expressions first (they have precedence over literals)
    if (is_comparison_expression(trimmed_expr, result_type, env)) then
      ! Type already set by is_comparison_expression (always logical)
    else if (is_logical_expression(trimmed_expr, result_type, env)) then
      ! Type already set by is_logical_expression (always logical)
    else if (is_arithmetic_expression(trimmed_expr, result_type, env)) then
      ! Type already set by is_arithmetic_expression
    else if (is_intrinsic_function(trimmed_expr, result_type, env)) then
      ! Type already set by is_intrinsic_function
    else
      ! Check for literals
      call analyze_literal(trimmed_expr, result_type, is_literal)
      
      if (.not. is_literal) then
        ! Check if it's a known variable
        if (present(env)) then
          call get_variable_type(env, trimmed_expr, result_type, found)
          if (.not. found) then
            result_type = create_type_info(TYPE_UNKNOWN)
          end if
        else
          result_type = create_type_info(TYPE_UNKNOWN)
        end if
      end if
    end if
    
  end subroutine analyze_expression

  function is_arithmetic_expression(expr, result_type, env) result(is_arith)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    logical :: is_arith
    
    integer :: plus_pos, minus_pos, mult_pos, div_pos, power_pos
    type(type_info) :: left_type, right_type
    character(len=256) :: left_expr, right_expr
    
    is_arith = .false.
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Simple check for arithmetic operators
    plus_pos = index(expr, '+')
    minus_pos = index(expr, '-', back=.true.)  ! back to avoid negative numbers
    mult_pos = index(expr, '*')
    div_pos = index(expr, '/')
    power_pos = index(expr, '**')
    
    ! Check if division is actually part of /= operator
    if (div_pos > 0 .and. div_pos < len_trim(expr)) then
      if (expr(div_pos+1:div_pos+1) == '=') then
        div_pos = 0  ! Not a division operator
      end if
    end if
    
    ! Check if mult_pos is actually part of ** operator
    if (power_pos > 0 .and. mult_pos == power_pos) then
      mult_pos = 0  ! Part of power operator, not multiplication
    end if
    
    ! Handle different binary operations (check power first)
    if (power_pos > 1) then
      left_expr = adjustl(expr(1:power_pos-1))
      right_expr = adjustl(expr(power_pos+2:))
    else if (plus_pos > 1) then
      left_expr = adjustl(expr(1:plus_pos-1))
      right_expr = adjustl(expr(plus_pos+1:))
    else if (minus_pos > 1) then
      left_expr = adjustl(expr(1:minus_pos-1))
      right_expr = adjustl(expr(minus_pos+1:))
    else if (mult_pos > 1) then
      left_expr = adjustl(expr(1:mult_pos-1))
      right_expr = adjustl(expr(mult_pos+1:))
    else if (div_pos > 1) then
      left_expr = adjustl(expr(1:div_pos-1))
      right_expr = adjustl(expr(div_pos+1:))
    end if
    
    ! If we found an operator, analyze the operands
    if (power_pos > 1 .or. plus_pos > 1 .or. minus_pos > 1 .or. mult_pos > 1 .or. div_pos > 1) then
      call analyze_expression(left_expr, left_type, env)
      call analyze_expression(right_expr, right_type, env)
      
      if (left_type%base_type /= TYPE_UNKNOWN .and. &
          right_type%base_type /= TYPE_UNKNOWN) then
        is_arith = .true.
        ! Type promotion rules
        result_type = promote_types(left_type, right_type)
      end if
    end if
    
  end function is_arithmetic_expression

  function is_comparison_expression(expr, result_type, env) result(is_comp)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    logical :: is_comp
    
    integer :: gt_pos, lt_pos, eq_pos, ne_pos, ge_pos, le_pos
    integer :: op_pos
    character(len=256) :: left_expr, right_expr
    type(type_info) :: left_type, right_type
    
    is_comp = .false.
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Check for comparison operators
    gt_pos = 0; lt_pos = 0; ge_pos = 0; le_pos = 0; eq_pos = 0; ne_pos = 0
    
    ! First check for two-character operators
    ge_pos = index(expr, '>=')
    le_pos = index(expr, '<=')
    eq_pos = index(expr, '==')
    ne_pos = index(expr, '/=')
    
    ! Then check for single-character operators (but not if part of >=, <=)
    if (ge_pos == 0) gt_pos = index(expr, '>')
    if (le_pos == 0) lt_pos = index(expr, '<')
    
    ! Check for Fortran-style operators
    if (gt_pos == 0 .and. ge_pos == 0) gt_pos = index(expr, '.gt.')
    if (lt_pos == 0 .and. le_pos == 0) lt_pos = index(expr, '.lt.')
    if (eq_pos == 0) eq_pos = index(expr, '.eq.')
    if (ne_pos == 0) ne_pos = index(expr, '.ne.')
    if (ge_pos == 0) ge_pos = index(expr, '.ge.')
    if (le_pos == 0) le_pos = index(expr, '.le.')
    
    ! Find which operator is present and its position
    op_pos = 0
    if (gt_pos > 0) op_pos = gt_pos
    if (lt_pos > 0 .and. (op_pos == 0 .or. lt_pos < op_pos)) op_pos = lt_pos
    if (eq_pos > 0 .and. (op_pos == 0 .or. eq_pos < op_pos)) op_pos = eq_pos
    if (ne_pos > 0 .and. (op_pos == 0 .or. ne_pos < op_pos)) op_pos = ne_pos
    if (ge_pos > 0 .and. (op_pos == 0 .or. ge_pos < op_pos)) op_pos = ge_pos
    if (le_pos > 0 .and. (op_pos == 0 .or. le_pos < op_pos)) op_pos = le_pos
    
    if (op_pos > 1) then
      ! Extract left and right expressions
      left_expr = adjustl(expr(1:op_pos-1))
      
      ! Determine operator length and extract right expression
      if (op_pos == ge_pos .or. op_pos == le_pos) then
        right_expr = adjustl(expr(op_pos+2:))  ! Two-character operator >= or <=
      else if (op_pos == eq_pos .or. op_pos == ne_pos) then
        right_expr = adjustl(expr(op_pos+2:))  ! Two-character operator == or /=
      else if (index(expr(op_pos:), '.gt.') == 1 .or. &
               index(expr(op_pos:), '.lt.') == 1 .or. &
               index(expr(op_pos:), '.eq.') == 1 .or. &
               index(expr(op_pos:), '.ne.') == 1 .or. &
               index(expr(op_pos:), '.ge.') == 1 .or. &
               index(expr(op_pos:), '.le.') == 1) then
        right_expr = adjustl(expr(op_pos+4:))  ! Four-character operator like .gt.
      else
        right_expr = adjustl(expr(op_pos+1:))  ! Single-character operator > or <
      end if
      
      ! Ensure we have non-empty expressions
      if (len_trim(left_expr) > 0 .and. len_trim(right_expr) > 0) then
        ! Comparison always returns logical
        is_comp = .true.
        result_type = create_type_info(TYPE_LOGICAL, 4)
      end if
    end if
    
  end function is_comparison_expression

  function is_logical_expression(expr, result_type, env) result(is_logical)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    logical :: is_logical
    
    integer :: and_pos, or_pos, not_pos, op_pos
    character(len=256) :: left_expr, right_expr
    
    is_logical = .false.
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Check for logical operators
    and_pos = index(expr, '.and.')
    or_pos = index(expr, '.or.')
    not_pos = index(expr, '.not.')
    
    ! Handle .not. (unary operator)
    if (not_pos == 1) then
      is_logical = .true.
      result_type = create_type_info(TYPE_LOGICAL, 4)
      return
    end if
    
    ! Find first binary logical operator
    op_pos = 0
    if (and_pos > 0) op_pos = and_pos
    if (or_pos > 0 .and. (op_pos == 0 .or. or_pos < op_pos)) op_pos = or_pos
    
    if (op_pos > 1) then
      ! Extract left expression
      left_expr = adjustl(expr(1:op_pos-1))
      
      ! Extract right expression (skip operator)
      if (op_pos == and_pos) then
        right_expr = adjustl(expr(op_pos+5:))  ! Skip '.and.'
      else if (op_pos == or_pos) then
        right_expr = adjustl(expr(op_pos+4:))  ! Skip '.or.'
      end if
      
      ! Ensure we have non-empty expressions
      if (len_trim(left_expr) > 0 .and. len_trim(right_expr) > 0) then
        is_logical = .true.
        result_type = create_type_info(TYPE_LOGICAL, 4)
      end if
    end if
    
  end function is_logical_expression

  function is_intrinsic_function(expr, result_type, env) result(is_intrinsic)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment_t), intent(in), optional :: env
    logical :: is_intrinsic
    
    is_intrinsic = .false.
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Check for common intrinsic functions
    if (index(expr, 'sin(') == 1 .or. &
        index(expr, 'cos(') == 1 .or. &
        index(expr, 'tan(') == 1 .or. &
        index(expr, 'exp(') == 1 .or. &
        index(expr, 'log(') == 1 .or. &
        index(expr, 'sqrt(') == 1) then
      is_intrinsic = .true.
      result_type = create_type_info(TYPE_REAL, 8)
    else if (index(expr, 'abs(') == 1) then
      ! abs can return integer or real depending on argument
      is_intrinsic = .true.
      result_type = create_type_info(TYPE_REAL, 8)  ! Conservative assumption
    else if (index(expr, 'len(') == 1 .or. &
             index(expr, 'len_trim(') == 1) then
      is_intrinsic = .true.
      result_type = create_type_info(TYPE_INTEGER, 4)
    end if
    
  end function is_intrinsic_function

  subroutine strip_comment(expr)
    character(len=*), intent(inout) :: expr
    
    integer :: comment_pos, i, expr_len
    logical :: in_string
    character :: quote_char
    
    ! Find comment position, but ignore ! inside string literals
    comment_pos = 0
    in_string = .false.
    quote_char = ' '
    expr_len = len_trim(expr)
    
    i = 1
    do while (i <= expr_len)
      if (.not. in_string) then
        ! Not in string - check for start of string or comment
        if (expr(i:i) == '"' .or. expr(i:i) == "'") then
          in_string = .true.
          quote_char = expr(i:i)
        else if (expr(i:i) == '!') then
          comment_pos = i
          exit
        end if
      else
        ! In string - check for end of string
        if (expr(i:i) == quote_char) then
          ! Check for doubled quote (escape sequence)
          if (i+1 <= expr_len) then
            if (expr(i+1:i+1) == quote_char) then
              i = i + 1  ! Skip the doubled quote
            else
              in_string = .false.
            end if
          else
            in_string = .false.
          end if
        end if
      end if
      i = i + 1
    end do
    
    ! Remove comment if found
    if (comment_pos > 0) then
      expr = expr(1:comment_pos-1)
    end if
    
    ! Trim trailing whitespace
    expr = trim(expr)
    
  end subroutine strip_comment

end module expression_analyzer