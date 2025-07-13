module declaration_generator
  use type_system
  use type_environment
  implicit none
  private
  
  ! Public procedures
  public :: generate_declarations
  public :: format_declaration
  public :: get_declarations_for_scope
  
contains

  subroutine generate_declarations(env, declarations)
    type(type_environment_t), intent(in) :: env
    character(len=*), intent(out) :: declarations
    
    integer :: i, pos
    character(len=64) :: type_str, var_decl
    
    declarations = ''
    pos = 1
    
    ! Generate declaration for each variable
    do i = 1, env%var_count
      if (env%vars(i)%in_use .and. env%vars(i)%var_type%base_type /= TYPE_UNKNOWN) then
        
        ! Generate type string
        type_str = type_to_string(env%vars(i)%var_type)
        
        ! Skip unknown types
        if (trim(type_str) == 'unknown') cycle
        
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

  function format_declaration(var_name, var_type) result(declaration)
    character(len=*), intent(in) :: var_name
    type(type_info), intent(in) :: var_type
    character(len=128) :: declaration
    
    character(len=64) :: type_str
    
    type_str = type_to_string(var_type)
    write(declaration, '(a,a,a)') trim(type_str), ' :: ', trim(var_name)
    
  end function format_declaration

  subroutine get_declarations_for_scope(env, scope_vars, declarations)
    type(type_environment_t), intent(in) :: env
    character(len=*), dimension(:), intent(in) :: scope_vars
    character(len=*), intent(out) :: declarations
    
    integer :: i, j, pos
    character(len=128) :: var_decl
    logical :: found
    
    declarations = ''
    pos = 1
    
    ! Generate declarations only for variables in the specified scope
    do i = 1, size(scope_vars)
      found = .false.
      
      do j = 1, env%var_count
        if (env%vars(j)%in_use .and. &
            trim(env%vars(j)%name) == trim(scope_vars(i)) .and. &
            env%vars(j)%var_type%base_type /= TYPE_UNKNOWN) then
          
          var_decl = format_declaration(env%vars(j)%name, env%vars(j)%var_type)
          
          ! Append to declarations
          if (pos + len_trim(var_decl) + 1 <= len(declarations)) then
            if (pos > 1) then
              declarations(pos:pos) = char(10)  ! Newline
              pos = pos + 1
            end if
            declarations(pos:) = trim(var_decl)
            pos = pos + len_trim(var_decl)
          end if
          
          found = .true.
          exit
        end if
      end do
    end do
    
  end subroutine get_declarations_for_scope

end module declaration_generator