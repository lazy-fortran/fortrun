module type_inference_coordinator
  use type_system
  use type_environment, type_env_init => init_type_environment, &
                        type_env_cleanup => cleanup_type_environment, &
                        type_env_get_var => get_variable_type
  use literal_analyzer
  use expression_analyzer
  use array_analyzer
  use derived_type_analyzer
  use function_analyzer
  use declaration_generator, decl_gen_generate => generate_declarations
  implicit none
  private
  
  ! Public interface - maintains backward compatibility
  public :: infer_type_from_expression
  public :: init_type_environment
  public :: cleanup_type_environment
  public :: process_assignment
  public :: get_variable_type
  public :: generate_declarations
  
  ! Re-export type for backward compatibility
  public :: type_environment_t
  public :: type_info
  public :: TYPE_UNKNOWN, TYPE_INTEGER, TYPE_REAL, TYPE_LOGICAL, TYPE_CHARACTER, TYPE_COMPLEX
  
  ! Compatibility type alias
  public :: type_environment
  
  ! Type alias for backward compatibility
  type :: type_environment
    type(type_environment_t) :: env
  end type type_environment
  
contains

  ! Main type inference entry point - backward compatible with advanced features
  subroutine infer_type_from_expression(expr, inferred_type, env)
    character(len=*), intent(in) :: expr
    type(type_info), intent(out) :: inferred_type
    type(type_environment), intent(in), optional :: env
    
    ! First try basic expression analysis
    if (present(env)) then
      call analyze_expression(expr, inferred_type, env%env)
    else
      call analyze_expression(expr, inferred_type)
    end if
    
    ! If that failed, try advanced analyzers
    if (inferred_type%base_type == TYPE_UNKNOWN) then
      ! Try function return analysis
      call analyze_function_return(expr, '', inferred_type)
      
      ! Try derived type analysis
      if (inferred_type%base_type == TYPE_UNKNOWN) then
        if (present(env)) then
          call analyze_derived_type_expression(expr, inferred_type, env%env)
        else
          call analyze_derived_type_expression(expr, inferred_type)
        end if
      end if
      
      ! If still unknown, try array analysis
      if (inferred_type%base_type == TYPE_UNKNOWN) then
        if (present(env)) then
          call analyze_array_expression(expr, inferred_type, env%env)
        else
          call analyze_array_expression(expr, inferred_type)
        end if
      end if
    end if
    
  end subroutine infer_type_from_expression

  ! Environment management - backward compatible
  subroutine init_type_environment(env)
    type(type_environment), intent(out) :: env
    
    call type_env_init(env%env)
    
  end subroutine init_type_environment

  subroutine cleanup_type_environment(env)
    type(type_environment), intent(inout) :: env
    
    call type_env_cleanup(env%env)
    
  end subroutine cleanup_type_environment

  ! Assignment processing - backward compatible
  subroutine process_assignment(env, var_name, expr)
    type(type_environment), intent(inout) :: env
    character(len=*), intent(in) :: var_name, expr
    
    type(type_info) :: expr_type, current_type
    logical :: success, found
    
    ! Ignore array subscripts - only process simple variable names
    if (index(var_name, '(') > 0) then
      return
    end if
    
    ! Infer type from expression
    call analyze_expression(expr, expr_type, env%env)
    
    ! Add or update variable
    if (has_variable(env%env, var_name)) then
      ! Update existing variable with type promotion if needed
      call update_variable_type(env%env, var_name, expr_type, success)
    else
      ! Add new variable
      call add_variable(env%env, var_name, expr_type, success)
    end if
    
    ! Handle pre-declared variables (base_type = -1)
    if (success) then
      call type_env_get_var(env%env, var_name, current_type, found)
      if (found .and. current_type%base_type == -1) then
        ! Variable already declared - skip type inference by setting unknown
        call update_variable_type(env%env, var_name, create_type_info(TYPE_UNKNOWN), success)
      end if
    end if
    
  end subroutine process_assignment

  ! Variable type lookup - backward compatible
  subroutine get_variable_type(env, var_name, var_type, found)
    type(type_environment), intent(in) :: env
    character(len=*), intent(in) :: var_name
    type(type_info), intent(out) :: var_type
    logical, intent(out) :: found
    
    call type_env_get_var(env%env, var_name, var_type, found)
    
  end subroutine get_variable_type

  ! Declaration generation - backward compatible
  subroutine generate_declarations(env, declarations)
    type(type_environment), intent(in) :: env
    character(len=*), intent(out) :: declarations
    
    call decl_gen_generate(env%env, declarations)
    
  end subroutine generate_declarations

end module type_inference_coordinator