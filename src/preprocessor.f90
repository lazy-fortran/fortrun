module preprocessor
  ! Replace the original preprocessor with multi-scope support
  use cache, only: get_cache_dir
  use type_inference_coordinator
  implicit none
  private
  
  public :: preprocess_file, is_preprocessor_file
  
contains

  function is_preprocessor_file(filename) result(is_dot_f)
    character(len=*), intent(in) :: filename
    logical :: is_dot_f
    integer :: ext_pos
    
    ext_pos = index(filename, '.', back=.true.)
    if (ext_pos > 0) then
      is_dot_f = filename(ext_pos:) == '.f'
    else
      is_dot_f = .false.
    end if
  end function is_preprocessor_file
  
  subroutine preprocess_file(input_file, output_file, error_msg)
    character(len=*), intent(in) :: input_file
    character(len=*), intent(in) :: output_file
    character(len=*), intent(out) :: error_msg
    
    character(len=1024) :: line
    integer :: unit_in, unit_out, ios
    integer :: line_num
    logical :: in_subroutine, in_function
    logical :: has_program_statement, contains_written
    logical :: enable_type_inference
    character(len=:), allocatable :: indent
    type(type_environment) :: type_env
    
    ! Multi-scope support
    type(type_environment), dimension(10) :: scope_envs
    integer :: current_scope = 0
    integer :: max_scope = 0
    logical, dimension(10) :: scope_has_vars = .false.
    character(len=1024), dimension(10000) :: output_lines
    integer :: output_line_count = 0
    integer, dimension(10) :: implicit_lines = 0
    integer :: i, j
    
    error_msg = ''
    in_subroutine = .false.
    in_function = .false.
    has_program_statement = .false.
    contains_written = .false.
    enable_type_inference = .true.  ! Enable by default for .f files
    line_num = 0
    
    ! Initialize type inference environment for main scope
    current_scope = 1
    max_scope = 1
    if (enable_type_inference) then
      call init_type_environment(scope_envs(1))
    end if
    
    ! Open input file
    open(newunit=unit_in, file=input_file, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      error_msg = 'Failed to open input file: ' // trim(input_file)
      return
    end if
    
    ! First pass: check if file has program statement
    do
      read(unit_in, '(A)', iostat=ios) line
      if (ios /= 0) exit
      
      if (is_program_statement(line)) then
        has_program_statement = .true.
        exit
      end if
    end do
    
    ! Rewind for actual processing
    rewind(unit_in)
    
    ! If no program statement, wrap everything in a program
    if (.not. has_program_statement) then
      output_line_count = output_line_count + 1
      output_lines(output_line_count) = 'program main'
      output_line_count = output_line_count + 1
      output_lines(output_line_count) = '  implicit none'
      implicit_lines(1) = output_line_count
    end if
    
    ! Process file line by line
    do
      read(unit_in, '(A)', iostat=ios) line
      if (ios /= 0) exit
      
      line_num = line_num + 1
      
      ! Skip empty lines
      if (len_trim(line) == 0) then
        output_line_count = output_line_count + 1
        output_lines(output_line_count) = ''
        cycle
      end if
      
      ! Get indentation of current line
      indent = get_indentation(line)
      
      ! Check for function/subroutine declarations
      if (is_function_declaration(line)) then
        if (.not. has_program_statement .and. .not. contains_written) then
          output_line_count = output_line_count + 1
          output_lines(output_line_count) = 'contains'
          contains_written = .true.
        end if
        in_function = .true.
        current_scope = current_scope + 1
        if (current_scope > max_scope) max_scope = current_scope
        if (enable_type_inference) then
          call init_type_environment(scope_envs(current_scope))
        end if
        output_line_count = output_line_count + 1
        output_lines(output_line_count) = line
      else if (is_subroutine_declaration(line)) then
        if (.not. has_program_statement .and. .not. contains_written) then
          output_line_count = output_line_count + 1
          output_lines(output_line_count) = 'contains'
          contains_written = .true.
        end if
        in_subroutine = .true.
        current_scope = current_scope + 1
        if (current_scope > max_scope) max_scope = current_scope
        if (enable_type_inference) then
          call init_type_environment(scope_envs(current_scope))
        end if
        output_line_count = output_line_count + 1
        output_lines(output_line_count) = line
      else if (is_end_statement(line, 'function')) then
        in_function = .false.
        if (current_scope > 1) current_scope = current_scope - 1
        output_line_count = output_line_count + 1
        output_lines(output_line_count) = line
      else if (is_end_statement(line, 'subroutine')) then
        in_subroutine = .false.
        if (current_scope > 1) current_scope = current_scope - 1
        output_line_count = output_line_count + 1
        output_lines(output_line_count) = line
      else if (index(adjustl(line), 'implicit none') == 1) then
        implicit_lines(current_scope) = output_line_count + 1
        output_line_count = output_line_count + 1
        output_lines(output_line_count) = line
      else
        ! Check for existing declarations
        if (enable_type_inference .and. is_declaration_line(line)) then
          call mark_declared_variables(scope_envs(current_scope), line)
        end if
        
        ! Check for assignments for type inference
        if (enable_type_inference) then
          call detect_and_process_assignment(scope_envs(current_scope), line)
        end if
        
        ! Regular line - add proper indentation if we added program wrapper
        if (.not. has_program_statement .and. .not. in_subroutine .and. .not. in_function) then
          output_line_count = output_line_count + 1
          output_lines(output_line_count) = '  ' // trim(line)
        else
          output_line_count = output_line_count + 1
          output_lines(output_line_count) = line
        end if
      end if
    end do
    
    ! Close program wrapper if we added one
    if (.not. has_program_statement) then
      output_line_count = output_line_count + 1
      output_lines(output_line_count) = 'end program main'
    end if
    
    close(unit_in)
    
    ! Check which scopes have variables
    do i = 1, max_scope
      scope_has_vars(i) = scope_envs(i)%env%var_count > 0
    end do
    
    ! Now write output with injected declarations
    open(newunit=unit_out, file=output_file, status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      error_msg = 'Failed to create output file: ' // trim(output_file)
      return
    end if
    
    do i = 1, output_line_count
      write(unit_out, '(A)') trim(output_lines(i))
      
      ! Check if we need to inject declarations after this line
      do current_scope = 1, 10
        if (implicit_lines(current_scope) == i .and. scope_has_vars(current_scope)) then
          write(unit_out, '(A)') '  '
          write(unit_out, '(A)') '  ! Auto-generated variable declarations:'
          call write_formatted_declarations(unit_out, scope_envs(current_scope))
          write(unit_out, '(A)') '  '
        end if
      end do
    end do
    
    close(unit_out)
    
    ! Cleanup type environments
    if (enable_type_inference) then
      do i = 1, 10
        if (scope_has_vars(i)) then
          call cleanup_type_environment(scope_envs(i))
        end if
      end do
    end if
    
  end subroutine preprocess_file
  
  ! Helper functions remain the same...
  
  function get_indentation(line) result(indent)
    character(len=*), intent(in) :: line
    character(len=:), allocatable :: indent
    integer :: i
    
    do i = 1, len(line)
      if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) exit
    end do
    
    if (i > 1) then
      indent = line(1:i-1)
    else
      indent = ''
    end if
  end function get_indentation
  
  function is_program_statement(line) result(is_program)
    character(len=*), intent(in) :: line
    logical :: is_program
    character(len=256) :: trimmed
    
    trimmed = adjustl(line)
    is_program = index(trimmed, 'program ') == 1 .and. &
                 index(trimmed, 'program main') /= 1 .and. &
                 index(trimmed, 'end program') /= 1
  end function is_program_statement
  
  function is_function_declaration(line) result(is_function)
    character(len=*), intent(in) :: line
    logical :: is_function
    character(len=256) :: trimmed
    
    trimmed = adjustl(line)
    is_function = (index(trimmed, 'function ') == 1 .or. &
                   index(trimmed, 'real function ') == 1 .or. &
                   index(trimmed, 'integer function ') == 1 .or. &
                   index(trimmed, 'logical function ') == 1 .or. &
                   index(trimmed, 'complex function ') == 1 .or. &
                   index(trimmed, 'character function ') == 1) .and. &
                  index(trimmed, 'end function') /= 1
  end function is_function_declaration
  
  function is_subroutine_declaration(line) result(is_subroutine)
    character(len=*), intent(in) :: line
    logical :: is_subroutine
    character(len=256) :: trimmed
    
    trimmed = adjustl(line)
    is_subroutine = index(trimmed, 'subroutine ') == 1 .and. &
                    index(trimmed, 'end subroutine') /= 1
  end function is_subroutine_declaration
  
  function is_end_statement(line, construct_type) result(is_end)
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: construct_type
    logical :: is_end
    character(len=256) :: trimmed
    
    trimmed = adjustl(line)
    is_end = index(trimmed, 'end ' // trim(construct_type)) == 1
  end function is_end_statement
  
  function is_declaration_line(line) result(is_decl)
    character(len=*), intent(in) :: line
    logical :: is_decl
    character(len=256) :: trimmed
    
    trimmed = adjustl(line)
    
    ! Check for type declarations
    is_decl = (index(trimmed, 'integer ') == 1 .or. &
               index(trimmed, 'real ') == 1 .or. &
               index(trimmed, 'logical ') == 1 .or. &
               index(trimmed, 'character ') == 1 .or. &
               index(trimmed, 'complex ') == 1 .or. &
               index(trimmed, 'double precision ') == 1 .or. &
               index(trimmed, 'integer(') == 1 .or. &
               index(trimmed, 'real(') == 1 .or. &
               index(trimmed, 'logical(') == 1 .or. &
               index(trimmed, 'character(') == 1 .or. &
               index(trimmed, 'complex(') == 1 .or. &
               index(trimmed, 'type(') == 1)
  end function is_declaration_line
  
  subroutine mark_declared_variables(type_env, line)
    type(type_environment), intent(inout) :: type_env
    character(len=*), intent(in) :: line
    
    character(len=256) :: trimmed, var_list
    integer :: double_colon_pos, comma_pos, i
    character(len=64) :: var_name
    integer :: var_idx
    logical :: found
    
    trimmed = adjustl(line)
    
    ! Find :: separator (skip if not present - old style declaration)
    double_colon_pos = index(trimmed, '::')
    if (double_colon_pos == 0) then
      ! Old style declaration without :: - skip for now
      return
    end if
    
    ! Get variable list after ::
    var_list = adjustl(trimmed(double_colon_pos+2:))
    
    ! Parse comma-separated variable names
    i = 1
    do while (i <= len_trim(var_list))
      ! Find next comma or end
      comma_pos = index(var_list(i:), ',')
      if (comma_pos == 0) then
        ! Last variable
        var_name = adjustl(trim(var_list(i:)))
      else
        var_name = adjustl(trim(var_list(i:i+comma_pos-2)))
        i = i + comma_pos
      end if
      
      ! Add to type environment as already declared (with special marker)
      if (len_trim(var_name) > 0) then
        ! Create an entry marked as already declared
        if (type_env%env%var_count < size(type_env%env%vars)) then
          type_env%env%var_count = type_env%env%var_count + 1
          var_idx = type_env%env%var_count
          type_env%env%vars(var_idx)%name = var_name
          type_env%env%vars(var_idx)%in_use = .true.
          ! Use a special type to indicate already declared
          type_env%env%vars(var_idx)%var_type%base_type = -1  ! Special marker
        end if
      end if
      
      if (comma_pos == 0) exit
    end do
    
  end subroutine mark_declared_variables
  
  subroutine detect_and_process_assignment(type_env, line)
    type(type_environment), intent(inout) :: type_env
    character(len=*), intent(in) :: line
    
    integer :: eq_pos
    character(len=256) :: var_name, expr
    character(len=256) :: trimmed_line
    
    trimmed_line = adjustl(line)
    
    ! Skip lines that are not assignments (print, write, read, etc.)
    ! Use word boundaries to avoid false matches
    if ((index(trimmed_line, 'print ') == 1 .or. index(trimmed_line, 'print*') == 1) .or. &
        (index(trimmed_line, 'write ') == 1 .or. index(trimmed_line, 'write(') == 1) .or. &
        (index(trimmed_line, 'read ') == 1 .or. index(trimmed_line, 'read(') == 1) .or. &
        (index(trimmed_line, 'call ') == 1) .or. &
        (index(trimmed_line, '!') == 1)) then
      return
    end if
    
    ! Look for assignment operator (=)
    eq_pos = index(trimmed_line, '=')
    if (eq_pos > 1) then
      ! Extract variable name and expression
      var_name = adjustl(trimmed_line(1:eq_pos-1))
      expr = adjustl(trimmed_line(eq_pos+1:))
      
      ! Remove any leading/trailing whitespace
      var_name = trim(var_name)
      expr = trim(expr)
      
      ! Skip if it's not a simple variable (e.g., array access)
      if (index(var_name, '(') == 0 .and. index(var_name, '%') == 0) then
        call process_assignment(type_env, var_name, expr)
      end if
    end if
    
  end subroutine detect_and_process_assignment
  
  subroutine write_formatted_declarations(unit, type_env)
    integer, intent(in) :: unit
    type(type_environment), intent(in) :: type_env
    
    integer :: i
    character(len=64) :: type_str
    
    ! Generate declaration for each variable
    do i = 1, type_env%env%var_count
      if (type_env%env%vars(i)%in_use .and. &
          type_env%env%vars(i)%var_type%base_type /= TYPE_UNKNOWN .and. &
          type_env%env%vars(i)%var_type%base_type /= -1) then  ! Skip already declared
        
        ! Generate type string
        select case (type_env%env%vars(i)%var_type%base_type)
        case (TYPE_INTEGER)
          if (type_env%env%vars(i)%var_type%kind == 4) then
            type_str = 'integer'
          else
            write(type_str, '(a,i0,a)') 'integer(', type_env%env%vars(i)%var_type%kind, ')'
          end if
          
        case (TYPE_REAL)
          if (type_env%env%vars(i)%var_type%kind == 4) then
            type_str = 'real'
          else
            write(type_str, '(a,i0,a)') 'real(', type_env%env%vars(i)%var_type%kind, ')'
          end if
          
        case (TYPE_LOGICAL)
          type_str = 'logical'
          
        case (TYPE_CHARACTER)
          if (type_env%env%vars(i)%var_type%char_len >= 0) then
            write(type_str, '(a,i0,a)') 'character(len=', type_env%env%vars(i)%var_type%char_len, ')'
          else
            type_str = 'character(len=*)'
          end if
          
        case default
          cycle  ! Skip unknown types
        end select
        
        ! Write properly formatted declaration
        write(unit, '(a,a,a,a)') '  ', trim(type_str), ' :: ', trim(type_env%env%vars(i)%name)
        
      end if
    end do
    
  end subroutine write_formatted_declarations

end module preprocessor