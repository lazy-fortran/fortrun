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
    
    ! Trim whitespace
    trimmed_expr = adjustl(expr)
    
    ! Check for literals in order of precedence
    if (is_logical_literal(trimmed_expr)) then
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
    
    integer :: expr_len, quote_start, quote_end
    character :: quote_char
    
    is_char = .false.
    length = 0
    expr_len = len_trim(expr)
    
    if (expr_len < 2) return
    
    ! Check for single or double quotes
    if (expr(1:1) == "'" .or. expr(1:1) == '"') then
      quote_char = expr(1:1)
      quote_start = 1
      
      ! Find matching closing quote
      quote_end = index(expr(2:), quote_char)
      if (quote_end > 0) then
        quote_end = quote_end + 1  ! Adjust for substring
        if (quote_end == expr_len) then
          is_char = .true.
          length = quote_end - quote_start - 1
        end if
      end if
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
    if (env%vars(var_idx)%var_type%base_type == TYPE_UNKNOWN) then
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

end module type_inference