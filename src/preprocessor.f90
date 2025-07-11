module preprocessor
  use cache, only: get_cache_dir
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
    character(len=:), allocatable :: indent
    
    error_msg = ''
    in_subroutine = .false.
    in_function = .false.
    has_program_statement = .false.
    contains_written = .false.
    line_num = 0
    
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

end module preprocessor