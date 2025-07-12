module literal_analyzer
  use type_system
  implicit none
  private
  
  ! Public procedures
  public :: analyze_literal
  public :: is_integer_literal
  public :: is_real_literal
  public :: is_logical_literal
  public :: is_character_literal
  
contains

  subroutine analyze_literal(expr, result_type, is_literal)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: result_type
    logical, intent(out) :: is_literal
    
    integer :: kind_val, char_len
    
    is_literal = .false.
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Check each literal type
    if (is_logical_literal(expr)) then
      result_type = create_type_info(TYPE_LOGICAL, 4)
      is_literal = .true.
    else if (is_character_literal(expr, char_len)) then
      result_type = create_type_info(TYPE_CHARACTER, 1, char_len)
      is_literal = .true.
    else if (is_real_literal(expr, kind_val)) then
      result_type = create_type_info(TYPE_REAL, kind_val)
      is_literal = .true.
    else if (is_integer_literal(expr, kind_val)) then
      result_type = create_type_info(TYPE_INTEGER, kind_val)
      is_literal = .true.
    end if
    
  end subroutine analyze_literal

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

end module literal_analyzer