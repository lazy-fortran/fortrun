module scope_manager
    ! Hierarchical scope management for semantic analysis
    use type_system_hm
    implicit none
    private

    public :: scope_t, scope_stack_t
    public :: create_scope, create_scope_stack

    ! Scope types
    integer, parameter, public :: SCOPE_GLOBAL = 1
    integer, parameter, public :: SCOPE_MODULE = 2
    integer, parameter, public :: SCOPE_FUNCTION = 3
    integer, parameter, public :: SCOPE_SUBROUTINE = 4
    integer, parameter, public :: SCOPE_BLOCK = 5     ! if/do/etc blocks
    integer, parameter, public :: SCOPE_INTERFACE = 6

    ! Single scope with its own environment (no parent pointer - stack handles hierarchy)
    type :: scope_t
        integer :: scope_type = SCOPE_GLOBAL
        character(len=:), allocatable :: name  ! e.g., module name, function name
        type(type_env_t) :: env
        ! No parent pointer - stack handles hierarchy
    contains
        procedure :: lookup => scope_lookup
        procedure :: define => scope_define
        ! Remove lookup_recursive - stack handles traversal
    end type scope_t

    ! Stack of scopes for managing nested scopes (cache-efficient design)
    type :: scope_stack_t
        type(scope_t), allocatable :: scopes(:)  ! Stack of scopes (contiguous memory)
        integer :: depth = 0                     ! Current depth (top of stack)
        integer :: capacity = 0                  ! Array capacity
        ! No current/global pointers - use array indices
    contains
        procedure :: push => stack_push_scope
        procedure :: pop => stack_pop_scope
        procedure :: lookup => stack_lookup
        procedure :: define => stack_define
        procedure :: enter_module => stack_enter_module
        procedure :: enter_function => stack_enter_function
        procedure :: enter_subroutine => stack_enter_subroutine
        procedure :: enter_block => stack_enter_block
        procedure :: enter_interface => stack_enter_interface
        procedure :: leave_scope => stack_leave_scope
        procedure :: get_current_scope_type => stack_get_current_scope_type
        ! Remove finalize - automatic cleanup with allocatable
    end type scope_stack_t

contains

    ! Create a new scope (no parent pointer - stack handles hierarchy)
    function create_scope(scope_type, name) result(scope)
        integer, intent(in) :: scope_type
        character(len=*), intent(in), optional :: name
        type(scope_t) :: scope

        scope%scope_type = scope_type

        if (present(name)) then
            scope%name = name
        else
            scope%name = ""
        end if

        ! No parent pointer - stack handles hierarchy

        ! Initialize empty environment
        scope%env%count = 0
        scope%env%capacity = 10  ! Start with some capacity
        allocate (character(len=256) :: scope%env%names(scope%env%capacity))
        allocate (scope%env%schemes(scope%env%capacity))

    end function create_scope

    ! Create a new scope stack with global scope
    function create_scope_stack() result(stack)
        type(scope_stack_t) :: stack

        ! Initialize capacity and create global scope
        stack%capacity = 10
        allocate (stack%scopes(stack%capacity))
        stack%depth = 1
        stack%scopes(1) = create_scope(SCOPE_GLOBAL, "global")

    end function create_scope_stack

    ! Scope lookup (local only)
    function scope_lookup(this, name) result(scheme)
        class(scope_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), allocatable :: scheme

        ! Safety check: ensure env is properly initialized
        if (this%env%count < 0 .or. this%env%capacity < 0) then
            return
        end if

        scheme = this%env%lookup(name)

    end function scope_lookup

    ! Scope define (add to local scope)
    subroutine scope_define(this, name, scheme)
        class(scope_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), intent(in) :: scheme

        call this%env%extend(name, scheme)

    end subroutine scope_define

    ! Recursive lookup removed - stack handles traversal in stack_lookup

    ! Stack: push a new scope using safe array extension
    subroutine stack_push_scope(this, new_scope)
        class(scope_stack_t), intent(inout) :: this
        type(scope_t), intent(in) :: new_scope
        type(scope_t), allocatable :: temp_scopes(:)
        integer :: new_capacity

        ! Grow array if needed (following CLAUDE.md safe array extension)
        if (this%depth >= this%capacity) then
            new_capacity = this%capacity*2
            if (new_capacity == 0) new_capacity = 10
            allocate (temp_scopes(new_capacity))
            if (this%depth > 0) then
                temp_scopes(1:this%depth) = this%scopes(1:this%depth)
            end if
            this%scopes = temp_scopes
            this%capacity = new_capacity
        end if

        ! Push new scope onto stack
        this%depth = this%depth + 1
        this%scopes(this%depth) = new_scope

    end subroutine stack_push_scope

    ! Stack: pop current scope (simple decrement)
    subroutine stack_pop_scope(this)
        class(scope_stack_t), intent(inout) :: this

        if (this%depth > 1) then
            this%depth = this%depth - 1
        else
            error stop "Cannot pop global scope"
        end if

    end subroutine stack_pop_scope

    ! Stack: lookup with hierarchical search (walk down the stack)
    function stack_lookup(this, name) result(scheme)
        class(scope_stack_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), allocatable :: scheme
        integer :: i

        ! Walk down the stack from current scope to global scope
        do i = this%depth, 1, -1
            scheme = this%scopes(i)%lookup(name)
            if (allocated(scheme)) return
        end do

    end function stack_lookup

    ! Stack: define in current scope (top of stack)
    subroutine stack_define(this, name, scheme)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), intent(in) :: scheme

        if (this%depth > 0) then
            call this%scopes(this%depth)%define(name, scheme)
        else
            error stop "No current scope"
        end if

    end subroutine stack_define

    ! Enter module scope
    subroutine stack_enter_module(this, module_name)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in) :: module_name
        type(scope_t) :: new_scope

        new_scope = create_scope(SCOPE_MODULE, module_name)
        call this%push(new_scope)

    end subroutine stack_enter_module

    ! Enter function scope
    subroutine stack_enter_function(this, function_name)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in) :: function_name
        type(scope_t) :: new_scope

        new_scope = create_scope(SCOPE_FUNCTION, function_name)
        call this%push(new_scope)

    end subroutine stack_enter_function

    ! Enter subroutine scope
    subroutine stack_enter_subroutine(this, subroutine_name)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in) :: subroutine_name
        type(scope_t) :: new_scope

        new_scope = create_scope(SCOPE_SUBROUTINE, subroutine_name)
        call this%push(new_scope)

    end subroutine stack_enter_subroutine

    ! Enter block scope (if/do/etc)
    subroutine stack_enter_block(this)
        class(scope_stack_t), intent(inout) :: this
        type(scope_t) :: new_scope

        new_scope = create_scope(SCOPE_BLOCK, "")
        call this%push(new_scope)

    end subroutine stack_enter_block

    ! Enter interface scope
    subroutine stack_enter_interface(this, interface_name)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in), optional :: interface_name
        type(scope_t) :: new_scope

        if (present(interface_name)) then
            new_scope = create_scope(SCOPE_INTERFACE, interface_name)
        else
            new_scope = create_scope(SCOPE_INTERFACE, "")
        end if
        call this%push(new_scope)

    end subroutine stack_enter_interface

    ! Leave current scope
    subroutine stack_leave_scope(this)
        class(scope_stack_t), intent(inout) :: this

        call this%pop()

    end subroutine stack_leave_scope

    ! Get current scope type
    function stack_get_current_scope_type(this) result(scope_type)
        class(scope_stack_t), intent(in) :: this
        integer :: scope_type

        if (this%depth > 0) then
            scope_type = this%scopes(this%depth)%scope_type
        else
            scope_type = SCOPE_GLOBAL
        end if

    end function stack_get_current_scope_type

    ! Finalization removed - automatic cleanup with allocatable arrays

end module scope_manager
