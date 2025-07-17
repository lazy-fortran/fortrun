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

    ! Single scope with its own environment
    type :: scope_t
        integer :: scope_type = SCOPE_GLOBAL
        character(len=:), allocatable :: name  ! e.g., module name, function name
        type(type_env_t) :: env
        type(scope_t), pointer :: parent => null()
    contains
        procedure :: lookup => scope_lookup
        procedure :: define => scope_define
        procedure :: lookup_recursive => scope_lookup_recursive
    end type scope_t

    ! Stack of scopes for managing nested scopes
    type :: scope_stack_t
        type(scope_t), pointer :: current => null()
        type(scope_t), pointer :: global => null()
        integer :: depth = 0
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
        procedure :: finalize => stack_finalize
    end type scope_stack_t

contains

    ! Create a new scope
    function create_scope(scope_type, name, parent) result(scope)
        integer, intent(in) :: scope_type
        character(len=*), intent(in), optional :: name
        type(scope_t), pointer, intent(in), optional :: parent
        type(scope_t), pointer :: scope

        allocate (scope)
        scope%scope_type = scope_type

        if (present(name)) then
            scope%name = name
        else
            scope%name = ""
        end if

        if (present(parent)) then
            scope%parent => parent
        end if

        ! Initialize empty environment
        scope%env%count = 0
        scope%env%capacity = 10  ! Start with some capacity
        allocate (character(len=256) :: scope%env%names(scope%env%capacity))
        allocate (scope%env%schemes(scope%env%capacity))

    end function create_scope

    ! Create a new scope stack
    function create_scope_stack() result(stack)
        type(scope_stack_t) :: stack

        ! Create global scope
        stack%global => create_scope(SCOPE_GLOBAL, "global")
        stack%current => stack%global
        stack%depth = 1

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

    ! Recursive lookup through parent scopes
    function scope_lookup_recursive(this, name) result(scheme)
        class(scope_t), intent(in), target :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), allocatable :: scheme
        type(scope_t), pointer :: current

        current => this

        do while (associated(current))
            scheme = current%lookup(name)
            if (allocated(scheme)) return

            current => current%parent
        end do

    end function scope_lookup_recursive

    ! Stack: push a new scope
    subroutine stack_push_scope(this, new_scope)
        class(scope_stack_t), intent(inout) :: this
        type(scope_t), pointer, intent(in) :: new_scope

        new_scope%parent => this%current
        this%current => new_scope
        this%depth = this%depth + 1

    end subroutine stack_push_scope

    ! Stack: pop current scope
    subroutine stack_pop_scope(this)
        class(scope_stack_t), intent(inout) :: this

        if (associated(this%current%parent)) then
            this%current => this%current%parent
            this%depth = this%depth - 1
        else
            error stop "Cannot pop global scope"
        end if

    end subroutine stack_pop_scope

    ! Stack: lookup with hierarchical search
    function stack_lookup(this, name) result(scheme)
        class(scope_stack_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), allocatable :: scheme

        if (associated(this%current)) then
            scheme = this%current%lookup_recursive(name)
        end if

    end function stack_lookup

    ! Stack: define in current scope
    subroutine stack_define(this, name, scheme)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), intent(in) :: scheme

        if (associated(this%current)) then
            call this%current%define(name, scheme)
        else
            error stop "No current scope"
        end if

    end subroutine stack_define

    ! Enter module scope
    subroutine stack_enter_module(this, module_name)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in) :: module_name
        type(scope_t), pointer :: new_scope

        new_scope => create_scope(SCOPE_MODULE, module_name, this%current)
        call this%push(new_scope)

    end subroutine stack_enter_module

    ! Enter function scope
    subroutine stack_enter_function(this, function_name)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in) :: function_name
        type(scope_t), pointer :: new_scope

        new_scope => create_scope(SCOPE_FUNCTION, function_name, this%current)
        call this%push(new_scope)

    end subroutine stack_enter_function

    ! Enter subroutine scope
    subroutine stack_enter_subroutine(this, subroutine_name)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in) :: subroutine_name
        type(scope_t), pointer :: new_scope

        new_scope => create_scope(SCOPE_SUBROUTINE, subroutine_name, this%current)
        call this%push(new_scope)

    end subroutine stack_enter_subroutine

    ! Enter block scope (if/do/etc)
    subroutine stack_enter_block(this)
        class(scope_stack_t), intent(inout) :: this
        type(scope_t), pointer :: new_scope

        new_scope => create_scope(SCOPE_BLOCK, "", this%current)
        call this%push(new_scope)

    end subroutine stack_enter_block

    ! Enter interface scope
    subroutine stack_enter_interface(this, interface_name)
        class(scope_stack_t), intent(inout) :: this
        character(len=*), intent(in), optional :: interface_name
        type(scope_t), pointer :: new_scope

        if (present(interface_name)) then
            new_scope => create_scope(SCOPE_INTERFACE, interface_name, this%current)
        else
            new_scope => create_scope(SCOPE_INTERFACE, "", this%current)
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

        if (associated(this%current)) then
            scope_type = this%current%scope_type
        else
            scope_type = SCOPE_GLOBAL
        end if

    end function stack_get_current_scope_type

    ! Finalize scope stack (deallocate all scopes)
    subroutine stack_finalize(this)
        class(scope_stack_t), intent(inout) :: this
        type(scope_t), pointer :: current, next

        ! Start from global scope and deallocate all
        current => this%global
        do while (associated(current))
            ! Find next scope (need to traverse tree)
            ! For now, just nullify pointers
            current => null()
        end do

        this%current => null()
        this%global => null()
        this%depth = 0

    end subroutine stack_finalize

end module scope_manager
