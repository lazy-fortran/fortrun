module scope_manager
  use type_environment, only: type_environment_t
  implicit none
  private
  
  public :: ScopeManager, init_scope_manager, enter_scope, exit_scope, &
            get_current_scope, get_scope_env, get_max_scope
  
  !> Maximum number of nested scopes supported
  integer, parameter :: MAX_SCOPES = 10
  
  !> Scope management data structure
  type :: ScopeManager
    type(type_environment_t), dimension(MAX_SCOPES) :: scope_envs
    character(len=64), dimension(MAX_SCOPES) :: scope_function_names
    integer, dimension(MAX_SCOPES) :: implicit_lines
    logical, dimension(MAX_SCOPES) :: scope_has_vars
    integer :: current_scope = 0
    integer :: max_scope = 0
  end type ScopeManager
  
contains

  !> Initialize the scope manager
  subroutine init_scope_manager(manager)
    type(ScopeManager), intent(out) :: manager
    
    manager%current_scope = 0
    manager%max_scope = 0
    manager%scope_function_names = ''
    manager%implicit_lines = 0
    manager%scope_has_vars = .false.
  end subroutine init_scope_manager

  !> Enter a new scope (function/subroutine)
  subroutine enter_scope(manager, function_name)
    type(ScopeManager), intent(inout) :: manager
    character(len=*), intent(in), optional :: function_name
    
    if (manager%current_scope < MAX_SCOPES) then
      manager%current_scope = manager%current_scope + 1
      manager%max_scope = max(manager%max_scope, manager%current_scope)
      
      if (present(function_name)) then
        manager%scope_function_names(manager%current_scope) = function_name
      else
        manager%scope_function_names(manager%current_scope) = ''
      end if
    end if
  end subroutine enter_scope

  !> Exit current scope
  subroutine exit_scope(manager)
    type(ScopeManager), intent(inout) :: manager
    
    if (manager%current_scope > 0) then
      manager%current_scope = manager%current_scope - 1
    end if
  end subroutine exit_scope

  !> Get current scope index
  function get_current_scope(manager) result(scope)
    type(ScopeManager), intent(in) :: manager
    integer :: scope
    
    scope = manager%current_scope
  end function get_current_scope

  !> Get type environment for a specific scope
  function get_scope_env(manager, scope_index) result(env)
    type(ScopeManager), intent(in) :: manager
    integer, intent(in) :: scope_index
    type(type_environment_t) :: env
    
    if (scope_index >= 1 .and. scope_index <= MAX_SCOPES) then
      env = manager%scope_envs(scope_index)
    end if
  end function get_scope_env

  !> Get maximum scope reached
  function get_max_scope(manager) result(max_scope)
    type(ScopeManager), intent(in) :: manager
    integer :: max_scope
    
    max_scope = manager%max_scope
  end function get_max_scope

end module scope_manager