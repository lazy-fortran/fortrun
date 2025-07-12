module function_analyzer
  use type_system
  implicit none
  private
  
  ! Public procedures
  public :: analyze_function_return
  public :: analyze_intent_out_argument
  public :: is_subroutine_call
  public :: parse_function_definition
  public :: extract_function_call
  
contains

  subroutine analyze_function_return(assignment_stmt, function_def, result_type)
    character(len=*), intent(in) :: assignment_stmt
    character(len=*), intent(in) :: function_def
    type(type_info), intent(out) :: result_type
    
    character(len=64) :: func_name, var_name, def_func_name
    type(type_info) :: def_return_type
    
    ! Initialize to unknown
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Extract function name from assignment
    call extract_function_call(assignment_stmt, func_name, var_name)
    
    if (len_trim(func_name) == 0) then
      return  ! Not a function call
    end if
    
    ! Check for intrinsic functions first
    if (is_intrinsic_function_name(func_name)) then
      call get_intrinsic_return_type(func_name, result_type)
      return
    end if
    
    ! Parse function definition if provided
    if (len_trim(function_def) > 0) then
      call parse_function_definition(function_def, def_func_name, def_return_type)
      if (trim(def_func_name) == trim(func_name)) then
        result_type = def_return_type
      end if
    end if
    
  end subroutine analyze_function_return

  subroutine analyze_intent_out_argument(call_stmt, var_name, subroutine_def, result_type)
    character(len=*), intent(in) :: call_stmt
    character(len=*), intent(in) :: var_name
    character(len=*), intent(in) :: subroutine_def
    type(type_info), intent(out) :: result_type
    
    character(len=256) :: sub_name, args_list
    character(len=64) :: arg_name
    integer :: arg_pos, def_pos
    
    ! Initialize to unknown
    result_type = create_type_info(TYPE_UNKNOWN)
    
    ! Extract subroutine name and arguments from call
    call extract_subroutine_call(call_stmt, sub_name, args_list)
    
    if (len_trim(sub_name) == 0) then
      return  ! Not a subroutine call
    end if
    
    ! Find position of var_name in argument list
    call find_argument_position(args_list, var_name, arg_pos)
    
    if (arg_pos <= 0) then
      return  ! Variable not found in arguments
    end if
    
    ! Parse subroutine definition to get argument types
    call get_argument_type_from_definition(subroutine_def, arg_pos, result_type)
    
  end subroutine analyze_intent_out_argument

  subroutine is_subroutine_call(stmt, is_sub)
    character(len=*), intent(in) :: stmt
    logical, intent(out) :: is_sub
    
    character(len=256) :: trimmed_stmt
    
    trimmed_stmt = adjustl(stmt)
    is_sub = (index(trimmed_stmt, 'call ') == 1)
    
  end subroutine is_subroutine_call

  subroutine parse_function_definition(func_def, func_name, return_type)
    character(len=*), intent(in) :: func_def
    character(len=*), intent(out) :: func_name
    type(type_info), intent(out) :: return_type
    
    character(len=256) :: trimmed_def
    integer :: function_pos, paren_pos, name_start
    character(len=64) :: type_part
    
    ! Initialize outputs
    func_name = ''
    return_type = create_type_info(TYPE_UNKNOWN)
    
    trimmed_def = adjustl(func_def)
    function_pos = index(trimmed_def, 'function ')
    
    if (function_pos == 0) then
      return  ! Not a function definition
    end if
    
    ! Check if there's a return type before 'function'
    if (function_pos > 1) then
      type_part = adjustl(trimmed_def(1:function_pos-1))
      call parse_return_type(type_part, return_type)
    else
      ! No explicit return type - default to real for compatibility
      return_type = create_type_info(TYPE_REAL, 8)
    end if
    
    ! Extract function name
    ! Search for parenthesis after the function keyword
    paren_pos = index(trimmed_def(function_pos:), '(')
    if (paren_pos > 0) then
      paren_pos = paren_pos + function_pos - 1  ! Adjust to absolute position
      ! Start after 'function ' (9 characters)
      name_start = function_pos + 9
      func_name = trim(adjustl(trimmed_def(name_start:paren_pos-1)))
    end if
    
  end subroutine parse_function_definition

  subroutine extract_function_call(assignment_stmt, func_name, var_name)
    character(len=*), intent(in) :: assignment_stmt
    character(len=*), intent(out) :: func_name
    character(len=*), intent(out) :: var_name
    
    character(len=256) :: trimmed_stmt
    integer :: eq_pos, paren_pos, space_pos
    
    ! Initialize outputs
    func_name = ''
    var_name = ''
    
    trimmed_stmt = adjustl(assignment_stmt)
    eq_pos = index(trimmed_stmt, '=')
    
    if (eq_pos == 0) then
      return  ! Not an assignment
    end if
    
    ! Extract variable name (left side)
    var_name = trim(adjustl(trimmed_stmt(1:eq_pos-1)))
    
    ! Extract function name (right side)
    trimmed_stmt = adjustl(trimmed_stmt(eq_pos+1:))
    paren_pos = index(trimmed_stmt, '(')
    
    if (paren_pos > 1) then
      func_name = trim(adjustl(trimmed_stmt(1:paren_pos-1)))
    end if
    
  end subroutine extract_function_call

  ! Helper procedures
  
  function is_intrinsic_function_name(func_name) result(is_intrinsic)
    character(len=*), intent(in) :: func_name
    logical :: is_intrinsic
    
    character(len=64) :: name
    
    name = trim(adjustl(func_name))
    
    is_intrinsic = (name == 'sin' .or. name == 'cos' .or. name == 'tan' .or. &
                    name == 'exp' .or. name == 'log' .or. name == 'sqrt' .or. &
                    name == 'abs' .or. name == 'len' .or. name == 'len_trim' .or. &
                    name == 'size' .or. name == 'shape' .or. name == 'sum' .or. &
                    name == 'product' .or. name == 'matmul')
    
  end function is_intrinsic_function_name

  subroutine get_intrinsic_return_type(func_name, return_type)
    character(len=*), intent(in) :: func_name
    type(type_info), intent(out) :: return_type
    
    character(len=64) :: name
    
    name = trim(adjustl(func_name))
    
    if (name == 'sin' .or. name == 'cos' .or. name == 'tan' .or. &
        name == 'exp' .or. name == 'log' .or. name == 'sqrt') then
      return_type = create_type_info(TYPE_REAL, 8)
    else if (name == 'abs') then
      return_type = create_type_info(TYPE_REAL, 8)  ! Conservative assumption
    else if (name == 'len' .or. name == 'len_trim' .or. name == 'size') then
      return_type = create_type_info(TYPE_INTEGER, 4)
    else
      return_type = create_type_info(TYPE_UNKNOWN)
    end if
    
  end subroutine get_intrinsic_return_type

  subroutine parse_return_type(type_str, return_type)
    character(len=*), intent(in) :: type_str
    type(type_info), intent(out) :: return_type
    
    character(len=64) :: trimmed_type
    
    trimmed_type = adjustl(type_str)
    
    if (index(trimmed_type, 'integer') == 1) then
      return_type = create_type_info(TYPE_INTEGER, 4)
      ! Check for kind specification
      if (index(trimmed_type, '(8)') > 0) then
        return_type%kind = 8
      end if
    else if (index(trimmed_type, 'real') == 1) then
      return_type = create_type_info(TYPE_REAL, 8)  ! Default to double precision
      if (index(trimmed_type, '(4)') > 0) then
        return_type%kind = 4
      else if (index(trimmed_type, '(8)') > 0) then
        return_type%kind = 8
      end if
    else if (index(trimmed_type, 'logical') == 1) then
      return_type = create_type_info(TYPE_LOGICAL, 4)
    else if (index(trimmed_type, 'character') == 1) then
      return_type = create_type_info(TYPE_CHARACTER, 1)
      return_type%char_len = -1  ! Variable length
    else
      return_type = create_type_info(TYPE_UNKNOWN)
    end if
    
  end subroutine parse_return_type

  subroutine extract_subroutine_call(call_stmt, sub_name, args_list)
    character(len=*), intent(in) :: call_stmt
    character(len=*), intent(out) :: sub_name
    character(len=*), intent(out) :: args_list
    
    character(len=256) :: trimmed_stmt
    integer :: call_pos, paren_pos, end_paren_pos
    
    ! Initialize outputs
    sub_name = ''
    args_list = ''
    
    trimmed_stmt = adjustl(call_stmt)
    call_pos = index(trimmed_stmt, 'call ')
    
    if (call_pos /= 1) then
      return  ! Not a call statement
    end if
    
    ! Extract subroutine name
    paren_pos = index(trimmed_stmt, '(')
    if (paren_pos > 6) then  ! 'call ' is 5 chars + 1
      sub_name = trim(adjustl(trimmed_stmt(6:paren_pos-1)))
      
      ! Extract arguments list
      end_paren_pos = index(trimmed_stmt, ')', back=.true.)
      if (end_paren_pos > paren_pos) then
        args_list = trim(adjustl(trimmed_stmt(paren_pos+1:end_paren_pos-1)))
      end if
    end if
    
  end subroutine extract_subroutine_call

  subroutine find_argument_position(args_list, var_name, position)
    character(len=*), intent(in) :: args_list
    character(len=*), intent(in) :: var_name
    integer, intent(out) :: position
    
    character(len=256) :: trimmed_args
    integer :: i, comma_pos, start_pos
    character(len=64) :: current_arg
    
    position = 0
    trimmed_args = adjustl(args_list)
    
    if (len_trim(trimmed_args) == 0) then
      return
    end if
    
    i = 1
    start_pos = 1
    
    do while (start_pos <= len_trim(trimmed_args))
      comma_pos = index(trimmed_args(start_pos:), ',')
      
      if (comma_pos == 0) then
        ! Last argument
        current_arg = trim(adjustl(trimmed_args(start_pos:)))
      else
        current_arg = trim(adjustl(trimmed_args(start_pos:start_pos+comma_pos-2)))
        start_pos = start_pos + comma_pos
      end if
      
      if (trim(current_arg) == trim(var_name)) then
        position = i
        return
      end if
      
      i = i + 1
      if (comma_pos == 0) exit
    end do
    
  end subroutine find_argument_position

  subroutine get_argument_type_from_definition(sub_def, arg_pos, arg_type)
    character(len=*), intent(in) :: sub_def
    integer, intent(in) :: arg_pos
    type(type_info), intent(out) :: arg_type
    
    character(len=1024) :: lines(20)
    integer :: line_count, i, current_arg
    character(len=256) :: line
    
    ! Initialize to unknown
    arg_type = create_type_info(TYPE_UNKNOWN)
    
    ! Split definition into lines
    call split_into_lines(sub_def, lines, line_count)
    
    current_arg = 0
    
    ! Look for intent(out) declarations
    do i = 1, line_count
      line = adjustl(lines(i))
      
      ! Check if this line contains intent declarations
      if (index(line, 'intent(') > 0) then
        current_arg = current_arg + 1
        
        if (current_arg == arg_pos) then
          ! Check if it's intent(out)
          if (index(line, 'intent(out)') > 0) then
            call extract_type_from_declaration(line, arg_type)
          end if
          return
        end if
      end if
    end do
    
  end subroutine get_argument_type_from_definition

  subroutine split_into_lines(text, lines, line_count)
    character(len=*), intent(in) :: text
    character(len=*), intent(out) :: lines(:)
    integer, intent(out) :: line_count
    
    integer :: i, start_pos, newline_pos
    character(len=1), parameter :: nl = new_line('A')
    
    line_count = 0
    start_pos = 1
    
    do i = 1, size(lines)
      newline_pos = index(text(start_pos:), nl)
      
      if (newline_pos == 0) then
        ! Last line
        if (start_pos <= len_trim(text)) then
          line_count = line_count + 1
          lines(line_count) = text(start_pos:)
        end if
        exit
      else
        line_count = line_count + 1
        lines(line_count) = text(start_pos:start_pos+newline_pos-2)
        start_pos = start_pos + newline_pos
      end if
    end do
    
  end subroutine split_into_lines

  subroutine extract_type_from_declaration(decl_line, var_type)
    character(len=*), intent(in) :: decl_line
    type(type_info), intent(out) :: var_type
    
    character(len=256) :: trimmed_line
    
    trimmed_line = adjustl(decl_line)
    
    if (index(trimmed_line, 'integer') > 0) then
      var_type = create_type_info(TYPE_INTEGER, 4)
    else if (index(trimmed_line, 'real(8)') > 0) then
      var_type = create_type_info(TYPE_REAL, 8)
    else if (index(trimmed_line, 'real') > 0) then
      var_type = create_type_info(TYPE_REAL, 4)
    else if (index(trimmed_line, 'logical') > 0) then
      var_type = create_type_info(TYPE_LOGICAL, 4)
    else if (index(trimmed_line, 'character') > 0) then
      var_type = create_type_info(TYPE_CHARACTER, 1)
      var_type%char_len = -1
    else
      var_type = create_type_info(TYPE_UNKNOWN)
    end if
    
  end subroutine extract_type_from_declaration

end module function_analyzer