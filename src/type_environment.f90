module type_environment
  use type_system
  implicit none
  private
  
  ! Variable entry in type environment
  type :: var_entry
    character(len=63) :: name = ''
    type(type_info) :: var_type
    logical :: in_use = .false.
  end type var_entry
  
  ! Type environment to track variables
  type, public :: type_environment_t
    type(var_entry), dimension(1000) :: vars  ! Max 1000 variables
    integer :: var_count = 0
  end type type_environment_t
  
  ! Public procedures
  public :: init_type_environment
  public :: cleanup_type_environment
  public :: add_variable
  public :: update_variable_type
  public :: find_variable_index
  public :: get_variable_type
  public :: has_variable
  
contains

  subroutine init_type_environment(env)
    type(type_environment_t), intent(out) :: env
    
    env%var_count = 0
    env%vars%in_use = .false.
    
  end subroutine init_type_environment

  subroutine cleanup_type_environment(env)
    type(type_environment_t), intent(inout) :: env
    
    env%var_count = 0
    env%vars%in_use = .false.
    
  end subroutine cleanup_type_environment

  subroutine add_variable(env, var_name, var_type, success)
    type(type_environment_t), intent(inout) :: env
    character(len=*), intent(in) :: var_name
    type(type_info), intent(in) :: var_type
    logical, intent(out) :: success
    
    integer :: var_idx
    logical :: found
    
    success = .false.
    
    ! Check if variable already exists
    call find_variable_index(env, var_name, var_idx, found)
    
    if (found) then
      ! Update existing variable
      env%vars(var_idx)%var_type = var_type
      success = .true.
    else
      ! Add new variable
      if (env%var_count < size(env%vars)) then
        env%var_count = env%var_count + 1
        var_idx = env%var_count
        env%vars(var_idx)%name = var_name
        env%vars(var_idx)%var_type = var_type
        env%vars(var_idx)%in_use = .true.
        success = .true.
      end if
    end if
    
  end subroutine add_variable

  subroutine update_variable_type(env, var_name, new_type, success)
    type(type_environment_t), intent(inout) :: env
    character(len=*), intent(in) :: var_name
    type(type_info), intent(in) :: new_type
    logical, intent(out) :: success
    
    integer :: var_idx
    logical :: found
    
    call find_variable_index(env, var_name, var_idx, found)
    
    if (found) then
      ! Check for type conflicts and handle promotion
      if (env%vars(var_idx)%var_type%base_type == TYPE_UNKNOWN) then
        ! First assignment
        env%vars(var_idx)%var_type = new_type
      else if (can_promote_types(env%vars(var_idx)%var_type, new_type)) then
        ! Type promotion
        env%vars(var_idx)%var_type = promote_types(env%vars(var_idx)%var_type, new_type)
      end if
      success = .true.
    else
      success = .false.
    end if
    
  end subroutine update_variable_type

  subroutine find_variable_index(env, var_name, var_idx, found)
    type(type_environment_t), intent(in) :: env
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
    
  end subroutine find_variable_index

  subroutine get_variable_type(env, var_name, var_type, found)
    type(type_environment_t), intent(in) :: env
    character(len=*), intent(in) :: var_name
    type(type_info), intent(out) :: var_type
    logical, intent(out) :: found
    
    integer :: var_idx
    
    call find_variable_index(env, var_name, var_idx, found)
    
    if (found) then
      var_type = env%vars(var_idx)%var_type
    else
      var_type = create_type_info(TYPE_UNKNOWN)
    end if
    
  end subroutine get_variable_type

  function has_variable(env, var_name) result(has_var)
    type(type_environment_t), intent(in) :: env
    character(len=*), intent(in) :: var_name
    logical :: has_var
    
    integer :: var_idx
    
    call find_variable_index(env, var_name, var_idx, has_var)
    
  end function has_variable

end module type_environment