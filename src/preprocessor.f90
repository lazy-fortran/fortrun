module preprocessor
  use cache, only: get_cache_dir
  use type_inference
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
    character(len=*), intent(out) :: output_file
    character(len=*), intent(out) :: error_msg
    
    character(len=1024) :: line
    character(len=256) :: cache_dir
    character(len=256) :: base_name
    integer :: unit_in, unit_out, ios
    integer :: line_num, ext_pos
    logical :: in_subroutine, in_function
    logical :: has_program_statement, contains_written
    logical :: enable_type_inference
    character(len=:), allocatable :: indent
    type(type_environment) :: type_env
    character(len=2048) :: declarations
    
    error_msg = ''
    in_subroutine = .false.
    in_function = .false.
    has_program_statement = .false.
    contains_written = .false.
    enable_type_inference = .true.  ! Enable by default for .f files
    line_num = 0
    
    ! Initialize type inference environment
    if (enable_type_inference) then
      call init_type_environment(type_env)
    end if
    
    ! Generate output filename in cache directory
    cache_dir = get_cache_dir()
    ext_pos = index(input_file, '.', back=.true.)
    if (ext_pos > 0) then
      base_name = input_file(1:ext_pos-1)
    else
      base_name = input_file
    end if
    
    ! Extract just the filename without path
    ext_pos = index(base_name, '/', back=.true.)
    if (ext_pos > 0) then
      base_name = base_name(ext_pos+1:)
    end if
    
    write(output_file, '(a,a,a,a)') trim(cache_dir), '/', trim(base_name), '_preprocessed.f90'
    
    ! Open input file
    open(newunit=unit_in, file=input_file, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      error_msg = 'Failed to open input file: ' // trim(input_file)
      return
    end if
    
    ! Open output file
    open(newunit=unit_out, file=output_file, status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      close(unit_in)
      error_msg = 'Failed to create output file: ' // trim(output_file)
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
      write(unit_out, '(A)') 'program main'
      write(unit_out, '(A)') '  implicit none'
    end if
    
    ! Process file line by line
    do
      read(unit_in, '(A)', iostat=ios) line
      if (ios /= 0) exit
      
      line_num = line_num + 1
      
      ! Skip empty lines
      if (len_trim(line) == 0) then
        write(unit_out, '(A)') ''
        cycle
      end if
      
      ! Get indentation of current line
      indent = get_indentation(line)
      
      ! Check for function/subroutine declarations
      if (is_function_declaration(line)) then
        if (.not. has_program_statement .and. .not. contains_written) then
          write(unit_out, '(A)') 'contains'
          contains_written = .true.
        end if
        in_function = .true.
        write(unit_out, '(A)') line
      else if (is_subroutine_declaration(line)) then
        if (.not. has_program_statement .and. .not. contains_written) then
          write(unit_out, '(A)') 'contains'
          contains_written = .true.
        end if
        in_subroutine = .true.
        write(unit_out, '(A)') line
      else if (is_end_statement(line, 'function')) then
        in_function = .false.
        write(unit_out, '(A)') line
      else if (is_end_statement(line, 'subroutine')) then
        in_subroutine = .false.
        write(unit_out, '(A)') line
      else
        ! Check for assignments for type inference
        if (enable_type_inference .and. .not. in_subroutine .and. .not. in_function) then
          call detect_and_process_assignment(type_env, line)
        end if
        
        ! Regular line - add proper indentation if we added program wrapper
        if (.not. has_program_statement .and. .not. in_subroutine .and. .not. in_function) then
          write(unit_out, '(A)') '  ' // trim(line)
        else
          write(unit_out, '(A)') line
        end if
      end if
    end do
    
    ! Close program wrapper if we added one
    if (.not. has_program_statement) then
      write(unit_out, '(A)') 'end program main'
    end if
    
    close(unit_in)
    close(unit_out)
    
    ! Now post-process to add declarations if type inference was used
    if (enable_type_inference .and. type_env%var_count > 0) then
      call inject_declarations(output_file, type_env, error_msg)
    end if
    
    ! Cleanup type environment
    if (enable_type_inference) then
      call cleanup_type_environment(type_env)
    end if
    
  end subroutine preprocess_file
  
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
  
  subroutine detect_and_process_assignment(type_env, line)
    type(type_environment), intent(inout) :: type_env
    character(len=*), intent(in) :: line
    
    integer :: eq_pos
    character(len=256) :: var_name, expr
    character(len=256) :: trimmed_line
    
    trimmed_line = adjustl(line)
    
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
  
  subroutine inject_declarations(filename, type_env, error_msg)
    character(len=*), intent(in) :: filename
    type(type_environment), intent(in) :: type_env
    character(len=*), intent(out) :: error_msg
    
    character(len=2048) :: declarations
    character(len=1024) :: line, temp_filename
    integer :: unit_in, unit_out, ios
    logical :: declarations_added
    
    error_msg = ''
    declarations_added = .false.
    temp_filename = trim(filename) // '.tmp'
    
    ! Generate declarations
    call generate_declarations(type_env, declarations)
    if (len_trim(declarations) == 0) return
    
    ! Open original file for reading
    open(newunit=unit_in, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      error_msg = 'Failed to open file for declaration injection'
      return
    end if
    
    ! Open temporary file for writing
    open(newunit=unit_out, file=temp_filename, status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      close(unit_in)
      error_msg = 'Failed to create temporary file'
      return
    end if
    
    ! Copy file, inserting declarations after implicit none
    do
      read(unit_in, '(A)', iostat=ios) line
      if (ios /= 0) exit
      
      write(unit_out, '(A)') line
      
      ! Insert declarations after implicit none
      if (.not. declarations_added .and. index(adjustl(line), 'implicit none') == 1) then
        write(unit_out, '(A)') '  '  ! Blank line
        write(unit_out, '(A)') '  ! Auto-generated variable declarations:'
        call write_formatted_declarations(unit_out, declarations)
        write(unit_out, '(A)') '  '  ! Blank line
        declarations_added = .true.
      end if
    end do
    
    close(unit_in)
    close(unit_out)
    
    ! Replace original with temporary
    call execute_command_line('mv "' // trim(temp_filename) // '" "' // trim(filename) // '"')
    
  end subroutine inject_declarations
  
  subroutine write_formatted_declarations(unit, declarations)
    integer, intent(in) :: unit
    character(len=*), intent(in) :: declarations
    
    integer :: pos, next_pos
    character(len=256) :: single_decl
    
    pos = 1
    do while (pos <= len_trim(declarations))
      next_pos = index(declarations(pos:), '; ')
      if (next_pos == 0) then
        single_decl = trim(declarations(pos:))
        pos = len_trim(declarations) + 1
      else
        next_pos = pos + next_pos - 1
        single_decl = trim(declarations(pos:next_pos-1))
        pos = next_pos + 2
      end if
      
      if (len_trim(single_decl) > 0) then
        write(unit, '(A)') '  ' // trim(single_decl)
      end if
    end do
    
  end subroutine write_formatted_declarations

end module preprocessor