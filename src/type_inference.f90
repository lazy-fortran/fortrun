module type_inference
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
  
  ! Variable entry in type environment
  type :: var_entry
    character(len=63) :: name = ''
    type(type_info) :: var_type
    logical :: in_use = .false.
  end type var_entry
  
  ! Type environment to track variables
  type, public :: type_environment
    type(var_entry), dimension(1000) :: vars  ! Max 1000 variables
    integer :: var_count = 0
  end type type_environment
  
  ! Public procedures
  public :: infer_type_from_expression
  public :: init_type_environment
  public :: cleanup_type_environment
  public :: process_assignment
  public :: get_variable_type
  public :: generate_declarations
  
contains

  subroutine infer_type_from_expression(expr, inferred_type, env)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: inferred_type
    type(type_environment), intent(in), optional :: env
    
    character(len=len(expr)) :: trimmed_expr
    logical :: found
    
    ! Initialize to unknown
    inferred_type%base_type = TYPE_UNKNOWN
    inferred_type%kind = 4
    inferred_type%char_len = -1
    
    ! Trim whitespace and remove comments
    trimmed_expr = adjustl(expr)
    call strip_comment(trimmed_expr)
    
    ! Check for expressions before literals to handle cases like "3.14 > 3"
    ! Comparison expressions must be checked first
    if (is_comparison_expression(trimmed_expr, inferred_type, env)) then
      ! Type already set by is_comparison_expression (always logical)
    else if (is_logical_expression(trimmed_expr, inferred_type, env)) then
      ! Type already set by is_logical_expression (always logical)
    ! Then check for literals
    else if (is_logical_literal(trimmed_expr)) then
      inferred_type%base_type = TYPE_LOGICAL
      inferred_type%kind = 4
    else if (is_character_literal(trimmed_expr, inferred_type%char_len)) then
      inferred_type%base_type = TYPE_CHARACTER
      inferred_type%kind = 1
    else if (is_real_literal(trimmed_expr, inferred_type%kind)) then
      inferred_type%base_type = TYPE_REAL
    else if (is_integer_literal(trimmed_expr, inferred_type%kind)) then
      inferred_type%base_type = TYPE_INTEGER
    else if (is_arithmetic_expression(trimmed_expr, inferred_type, env)) then
      ! Type already set by is_arithmetic_expression
    else if (is_intrinsic_function(trimmed_expr, inferred_type, env)) then
      ! Type already set by is_intrinsic_function
    else
      ! Check if it's a known variable
      if (present(env)) then
        call get_variable_type(env, trimmed_expr, inferred_type, found)
        if (.not. found) then
          inferred_type%base_type = TYPE_UNKNOWN
        end if
      else
        inferred_type%base_type = TYPE_UNKNOWN
      end if
    end if
    
  end subroutine infer_type_from_expression
  
  function is_integer_literal(expr, kind_val) result(is_int)
    character(len=*), intent(in) :: expr
    integer, intent(out) :: kind_val
    logical :: is_int
    
    integer :: i, underscore_pos
    character(len=len(expr)) :: test_expr
    logical :: has_digit
    
    is_int = .false.
    kind_val = 4  ! Default integer kind
    test_expr = expr
    
    ! Check for kind suffix (_4, _8, etc.)
    underscore_pos = index(expr, '_', back=.true.)
    if (underscore_pos > 1) then
      ! Try to read kind value
      read(expr(underscore_pos+1:), *, err=10, end=10) kind_val
      test_expr = expr(1:underscore_pos-1)
    end if
    
 10 continue
    
    ! Check if remaining part is integer
    has_digit = .false.
    do i = 1, len_trim(test_expr)
      if (i == 1 .and. (test_expr(i:i) == '+' .or. test_expr(i:i) == '-')) then
        cycle  ! Allow sign at start
      else if (test_expr(i:i) >= '0' .and. test_expr(i:i) <= '9') then
        has_digit = .true.
      else
        return  ! Non-digit character
      end if
    end do
    
    is_int = has_digit
    
  end function is_integer_literal
  
  function is_real_literal(expr, kind_val) result(is_real)
    character(len=*), intent(in) :: expr
    integer, intent(out) :: kind_val
    logical :: is_real
    
    is_real = .false.
    kind_val = 8  ! Default to double precision
    
    ! Check for decimal point
    if (index(expr, '.') > 0) then
      is_real = .true.
      
      ! Check for d0 suffix (double precision)
      if (index(expr, 'd0') > 0 .or. index(expr, 'D0') > 0) then
        kind_val = 8
      ! Check for explicit kind suffix
      else if (index(expr, '_4') > 0) then
        kind_val = 4
      else if (index(expr, '_8') > 0) then
        kind_val = 8
      end if
      
    ! Check for scientific notation without decimal
    else if (index(expr, 'e') > 0 .or. index(expr, 'E') > 0 .or. &
             index(expr, 'd') > 0 .or. index(expr, 'D') > 0) then
      is_real = .true.
      if (index(expr, 'd') > 0 .or. index(expr, 'D') > 0) then
        kind_val = 8
      end if
    end if
    
  end function is_real_literal
  
  function is_logical_literal(expr) result(is_logical)
    character(len=*), intent(in) :: expr
    logical :: is_logical
    
    is_logical = (trim(expr) == '.true.' .or. trim(expr) == '.TRUE.' .or. &
                  trim(expr) == '.false.' .or. trim(expr) == '.FALSE.')
    
  end function is_logical_literal
  
  function is_character_literal(expr, length) result(is_char)
    character(len=*), intent(in) :: expr
    integer, intent(out) :: length
    logical :: is_char
    
    integer :: expr_len, i, char_count
    character :: quote_char
    
    is_char = .false.
    length = 0
    expr_len = len_trim(expr)
    
    if (expr_len < 2) return
    
    ! Check for single or double quotes
    if (expr(1:1) == "'" .or. expr(1:1) == '"') then
      quote_char = expr(1:1)
      
      ! Scan through string handling doubled quotes
      i = 2
      char_count = 0
      do while (i <= expr_len)
        if (expr(i:i) == quote_char) then
          if (i == expr_len) then
            ! Found closing quote at end
            is_char = .true.
            length = char_count
            return
          else if (i+1 <= expr_len) then
            if (expr(i+1:i+1) == quote_char) then
              ! Doubled quote - counts as one character
              char_count = char_count + 1
              i = i + 2
            else
              ! Quote in middle but not doubled - not a valid string literal
              return
            end if
          else
            ! Quote at end with no next character - valid end
            is_char = .true.
            length = char_count
            return
          end if
        else
          ! Regular character
          char_count = char_count + 1
          i = i + 1
        end if
      end do
    end if
    
  end function is_character_literal
  
  function is_arithmetic_expression(expr, result_type, env) result(is_arith)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment), intent(in), optional :: env
    logical :: is_arith
    
    integer :: plus_pos, minus_pos, mult_pos, div_pos
    type(type_info) :: left_type, right_type
    character(len=256) :: left_expr, right_expr
    
    is_arith = .false.
    
    ! Simple check for arithmetic operators
    plus_pos = index(expr, '+')
    minus_pos = index(expr, '-', back=.true.)  ! back to avoid negative numbers
    mult_pos = index(expr, '*')
    div_pos = index(expr, '/')
    
    ! Check if division is actually part of /= operator
    if (div_pos > 0 .and. div_pos < len_trim(expr)) then
      if (expr(div_pos+1:div_pos+1) == '=') then
        div_pos = 0  ! Not a division operator
      end if
    end if
    
    ! Handle different binary operations
    if (plus_pos > 1) then
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
    if (plus_pos > 1 .or. minus_pos > 1 .or. mult_pos > 1 .or. div_pos > 1) then
      call infer_type_from_expression(left_expr, left_type, env)
      call infer_type_from_expression(right_expr, right_type, env)
      
      if (left_type%base_type /= TYPE_UNKNOWN .and. &
          right_type%base_type /= TYPE_UNKNOWN) then
        is_arith = .true.
        ! Type promotion rules
        if (left_type%base_type == TYPE_REAL .or. right_type%base_type == TYPE_REAL) then
          result_type%base_type = TYPE_REAL
          result_type%kind = 8
        else if (left_type%base_type == TYPE_INTEGER .and. right_type%base_type == TYPE_INTEGER) then
          result_type%base_type = TYPE_INTEGER
          result_type%kind = max(left_type%kind, right_type%kind)
        end if
      end if
    end if
    
  end function is_arithmetic_expression
  
  function is_comparison_expression(expr, result_type, env) result(is_comp)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment), intent(in), optional :: env
    logical :: is_comp
    
    integer :: gt_pos, lt_pos, eq_pos, ne_pos, ge_pos, le_pos
    integer :: op_pos
    character(len=256) :: left_expr, right_expr
    type(type_info) :: left_type, right_type
    
    is_comp = .false.
    result_type%base_type = TYPE_UNKNOWN
    
    ! Check for comparison operators
    ! Initialize all positions
    gt_pos = 0
    lt_pos = 0
    ge_pos = 0
    le_pos = 0
    eq_pos = 0
    ne_pos = 0
    
    ! First check for two-character operators
    ge_pos = index(expr, '>=')
    le_pos = index(expr, '<=')
    eq_pos = index(expr, '==')
    ne_pos = index(expr, '/=')
    
    ! Then check for single-character operators (but not if part of >=, <=)
    if (ge_pos == 0) then
      gt_pos = index(expr, '>')
    end if
    if (le_pos == 0) then
      lt_pos = index(expr, '<')
    end if
    
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
      if (len_trim(left_expr) == 0 .or. len_trim(right_expr) == 0) then
        return  ! Invalid comparison
      end if
      
      ! Infer types of operands
      if (present(env)) then
        call infer_type_from_expression(left_expr, left_type, env)
        call infer_type_from_expression(right_expr, right_type, env)
      else
        call infer_type_from_expression(left_expr, left_type)
        call infer_type_from_expression(right_expr, right_type)
      end if
      
      ! Comparison always returns logical, even if operands are unknown
      ! (we assume variables exist and have compatible types for comparison)
      is_comp = .true.
      result_type%base_type = TYPE_LOGICAL
      result_type%kind = 4  ! Default logical kind
    end if
    
  end function is_comparison_expression
  
  function is_logical_expression(expr, result_type, env) result(is_logical)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment), intent(in), optional :: env
    logical :: is_logical
    
    integer :: and_pos, or_pos, not_pos, op_pos
    character(len=256) :: left_expr, right_expr
    type(type_info) :: left_type, right_type
    
    is_logical = .false.
    result_type%base_type = TYPE_UNKNOWN
    
    ! Check for logical operators
    and_pos = index(expr, '.and.')
    or_pos = index(expr, '.or.')
    not_pos = index(expr, '.not.')
    
    ! Handle .not. (unary operator)
    if (not_pos == 1) then
      is_logical = .true.
      result_type%base_type = TYPE_LOGICAL
      result_type%kind = 4
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
      if (len_trim(left_expr) == 0 .or. len_trim(right_expr) == 0) then
        return  ! Invalid logical expression
      end if
      
      is_logical = .true.
      result_type%base_type = TYPE_LOGICAL
      result_type%kind = 4
    end if
    
  end function is_logical_expression
  
  function is_intrinsic_function(expr, result_type, env) result(is_intrinsic)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    type(type_environment), intent(in), optional :: env
    logical :: is_intrinsic
    
    is_intrinsic = .false.
    
    ! Check for common intrinsic functions
    if (index(expr, 'sin(') == 1 .or. &
        index(expr, 'cos(') == 1 .or. &
        index(expr, 'tan(') == 1 .or. &
        index(expr, 'exp(') == 1 .or. &
        index(expr, 'log(') == 1 .or. &
        index(expr, 'sqrt(') == 1) then
      is_intrinsic = .true.
      result_type%base_type = TYPE_REAL
      result_type%kind = 8
    else if (index(expr, 'abs(') == 1) then
      ! abs can return integer or real depending on argument
      is_intrinsic = .true.
      result_type%base_type = TYPE_REAL  ! Conservative assumption
      result_type%kind = 8
    else if (index(expr, 'len(') == 1 .or. &
             index(expr, 'len_trim(') == 1) then
      is_intrinsic = .true.
      result_type%base_type = TYPE_INTEGER
      result_type%kind = 4
    end if
    
  end function is_intrinsic_function
  
  subroutine init_type_environment(env)
    type(type_environment), intent(out) :: env
    
    env%var_count = 0
    env%vars%in_use = .false.
    
  end subroutine init_type_environment
  
  subroutine cleanup_type_environment(env)
    type(type_environment), intent(inout) :: env
    
    env%var_count = 0
    env%vars%in_use = .false.
    
  end subroutine cleanup_type_environment
  
  subroutine process_assignment(env, var_name, expr)
    type(type_environment), intent(inout) :: env
    character(len=*), intent(in) :: var_name, expr
    
    type(type_info) :: expr_type
    integer :: var_idx
    logical :: found
    
    ! Ignore array subscripts - only process simple variable names
    if (index(var_name, '(') > 0) then
      return
    end if
    
    ! Infer type from expression
    call infer_type_from_expression(expr, expr_type, env)
    
    ! Find or create variable entry
    call find_variable(env, var_name, var_idx, found)
    
    if (.not. found) then
      ! New variable
      if (env%var_count < size(env%vars)) then
        env%var_count = env%var_count + 1
        var_idx = env%var_count
        env%vars(var_idx)%name = var_name
        env%vars(var_idx)%in_use = .true.
      else
        ! Too many variables
        return
      end if
    end if
    
    ! Update or check type
    if (env%vars(var_idx)%var_type%base_type == -1) then
      ! Variable already declared - skip type inference
      return
    else if (env%vars(var_idx)%var_type%base_type == TYPE_UNKNOWN) then
      ! First assignment
      env%vars(var_idx)%var_type = expr_type
    else if (env%vars(var_idx)%var_type%base_type /= expr_type%base_type) then
      ! Type conflict - promote to real if mixing numeric types
      if ((env%vars(var_idx)%var_type%base_type == TYPE_INTEGER .and. &
           expr_type%base_type == TYPE_REAL) .or. &
          (env%vars(var_idx)%var_type%base_type == TYPE_REAL .and. &
           expr_type%base_type == TYPE_INTEGER)) then
        env%vars(var_idx)%var_type%base_type = TYPE_REAL
        env%vars(var_idx)%var_type%kind = 8
      end if
    end if
    
  end subroutine process_assignment
  
  subroutine find_variable(env, var_name, var_idx, found)
    type(type_environment), intent(in) :: env
    character(len=*), intent(in) :: var_name
    integer, intent(out) :: var_idx
    logical, intent(out) :: found
    
    integer :: i
    
    found = .false.
    var_idx = 0
    
    do i = 1, env%var_count
      if (env%vars(i)%in_use .and. trim(env%vars(i)%name) == trim(var_name)) then
        found = .true.
        var_idx = i
        return
      end if
    end do
    
  end subroutine find_variable
  
  subroutine get_variable_type(env, var_name, var_type, found)
    type(type_environment), intent(in) :: env
    character(len=*), intent(in) :: var_name
    type(type_info), intent(out) :: var_type
    logical, intent(out) :: found
    
    integer :: var_idx
    
    call find_variable(env, var_name, var_idx, found)
    
    if (found) then
      var_type = env%vars(var_idx)%var_type
    else
      var_type%base_type = TYPE_UNKNOWN
    end if
    
  end subroutine get_variable_type
  
  subroutine generate_declarations(env, declarations)
    type(type_environment), intent(in) :: env
    character(len=*), intent(out) :: declarations
    
    integer :: i, pos
    character(len=64) :: type_str, var_decl
    
    declarations = ''
    pos = 1
    
    ! Generate declaration for each variable
    do i = 1, env%var_count
      if (env%vars(i)%in_use .and. env%vars(i)%var_type%base_type /= TYPE_UNKNOWN) then
        
        ! Generate type string
        select case (env%vars(i)%var_type%base_type)
        case (TYPE_INTEGER)
          if (env%vars(i)%var_type%kind == 4) then
            type_str = 'integer'
          else
            write(type_str, '(a,i0,a)') 'integer(', env%vars(i)%var_type%kind, ')'
          end if
          
        case (TYPE_REAL)
          if (env%vars(i)%var_type%kind == 4) then
            type_str = 'real'
          else
            write(type_str, '(a,i0,a)') 'real(', env%vars(i)%var_type%kind, ')'
          end if
          
        case (TYPE_LOGICAL)
          type_str = 'logical'
          
        case (TYPE_CHARACTER)
          if (env%vars(i)%var_type%char_len >= 0) then
            write(type_str, '(a,i0,a)') 'character(len=', env%vars(i)%var_type%char_len, ')'
          else
            type_str = 'character(len=*)'
          end if
          
        case default
          cycle  ! Skip unknown types
        end select
        
        ! Build declaration
        write(var_decl, '(a,a,a)') trim(type_str), ' :: ', trim(env%vars(i)%name)
        
        ! Append to declarations (simple concatenation)
        if (pos + len_trim(var_decl) <= len(declarations)) then
          declarations(pos:) = trim(var_decl)
          pos = pos + len_trim(var_decl)
        end if
        
      end if
    end do
    
  end subroutine generate_declarations

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

end module type_inference