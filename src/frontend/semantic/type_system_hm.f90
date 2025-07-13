module type_system_hm
    ! Hindley-Milner type system for lazy fortran compiler frontend
    implicit none
    private
    
    ! Public types and interfaces
    public :: type_var_t, mono_type_t, poly_type_t, type_env_t, substitution_t
    public :: create_type_var, create_mono_type, create_poly_type
    public :: apply_substitution, compose_substitutions
    public :: occurs_check, free_type_vars
    
    ! Type kinds for Hindley-Milner system
    integer, parameter, public :: TVAR = 1      ! Type variable
    integer, parameter, public :: TINT = 2      ! Integer type
    integer, parameter, public :: TREAL = 3     ! Real type  
    integer, parameter, public :: TCHAR = 4     ! Character type
    integer, parameter, public :: TFUN = 5      ! Function type
    integer, parameter, public :: TARRAY = 6    ! Array type
    
    ! Type variable
    type :: type_var_t
        integer :: id
        character(len=:), allocatable :: name  ! e.g., 'a, 'b
    end type type_var_t
    
    ! Monomorphic type
    type :: mono_type_t
        integer :: kind  ! TVAR, TINT, TREAL, etc.
        type(type_var_t) :: var  ! for TVAR
        class(mono_type_t), allocatable :: args(:)  ! for TFUN (arg, result), TARRAY (element type)
        integer :: size  ! for TCHAR(len=size), TARRAY(size)
    contains
        procedure :: equals => mono_type_equals
        procedure :: to_string => mono_type_to_string
        procedure :: deep_copy => mono_type_deep_copy
    end type mono_type_t
    
    ! Polymorphic type (type scheme)
    type :: poly_type_t
        type(type_var_t), allocatable :: forall(:)  ! quantified variables
        type(mono_type_t) :: mono  ! the monomorphic type
    contains
        procedure :: to_string => poly_type_to_string
    end type poly_type_t
    
    ! Type substitution (maps type variables to types)
    type :: substitution_t
        type(type_var_t), allocatable :: vars(:)
        type(mono_type_t), allocatable :: types(:)
        integer :: count = 0
    contains
        procedure :: add => subst_add
        procedure :: lookup => subst_lookup
        procedure :: apply => subst_apply_to_mono
        procedure :: apply_to_poly => subst_apply_to_poly
    end type substitution_t
    
    ! Type environment (maps identifiers to type schemes)
    type :: type_env_t
        character(len=:), allocatable :: names(:)
        type(poly_type_t), allocatable :: schemes(:)
        integer :: count = 0
        integer :: capacity = 0
    contains
        procedure :: lookup => env_lookup
        procedure :: extend => env_extend
        procedure :: extend_many => env_extend_many
        procedure :: remove => env_remove
        procedure :: apply_subst => env_apply_subst
    end type type_env_t
    
contains
    
    ! Constructor for type variable
    function create_type_var(id, name) result(tv)
        integer, intent(in) :: id
        character(len=*), intent(in), optional :: name
        type(type_var_t) :: tv
        
        tv%id = id
        if (present(name)) then
            tv%name = name
        else
            ! Generate name from id: 'a, 'b, ..., 'z, 'a1, 'b1, ...
            if (id <= 26) then
                tv%name = "'" // achar(iachar('a') + id - 1)
            else
                block
                    character(len=10) :: num_str
                    write(num_str, '(i0)') (id - 1) / 26
                    tv%name = "'" // achar(iachar('a') + mod(id - 1, 26)) // trim(num_str)
                end block
            end if
        end if
    end function create_type_var
    
    ! Constructor for monomorphic type
    function create_mono_type(kind, var, args, char_size) result(mt)
        integer, intent(in) :: kind
        type(type_var_t), intent(in), optional :: var
        type(mono_type_t), intent(in), optional :: args(:)
        integer, intent(in), optional :: char_size
        type(mono_type_t) :: mt
        integer :: i
        
        mt%kind = kind
        
        if (present(var)) mt%var = var
        
        if (present(args)) then
            allocate(mt%args(size(args)))
            do i = 1, size(args)
                block
                    type(mono_type_t), allocatable :: temp_copy
                    allocate(temp_copy, source=args(i)%deep_copy())
                    call move_alloc(temp_copy, mt%args(i))
                end block
            end do
        end if
        
        if (present(char_size)) mt%size = char_size
        
        ! Set defaults
        if (kind == TCHAR .and. .not. present(char_size)) mt%size = 1
        if (kind == TARRAY) mt%size = 0  ! arrays don't use size field same way
        
    end function create_mono_type
    
    ! Constructor for polymorphic type
    function create_poly_type(forall_vars, mono) result(pt)
        type(type_var_t), intent(in) :: forall_vars(:)
        type(mono_type_t), intent(in) :: mono
        type(poly_type_t) :: pt
        
        if (size(forall_vars) > 0) then
            allocate(pt%forall(size(forall_vars)))
            pt%forall = forall_vars
        end if
        pt%mono = mono%deep_copy()
    end function create_poly_type
    
    ! Check if two monomorphic types are equal
    recursive logical function mono_type_equals(this, other) result(equal)
        class(mono_type_t), intent(in) :: this, other
        integer :: i
        
        equal = .false.
        
        if (this%kind /= other%kind) return
        
        select case (this%kind)
        case (TVAR)
            equal = (this%var%id == other%var%id)
        case (TINT, TREAL)
            equal = .true.
        case (TCHAR)
            equal = (this%size == other%size)
        case (TFUN, TARRAY)
            if (.not. allocated(this%args) .or. .not. allocated(other%args)) then
                equal = .false.
                return
            end if
            if (size(this%args) /= size(other%args)) return
            equal = .true.
            do i = 1, size(this%args)
                if (.not. this%args(i)%equals(other%args(i))) then
                    equal = .false.
                    return
                end if
            end do
            if (this%kind == TARRAY) then
                equal = equal .and. (this%size == other%size)
            end if
        end select
    end function mono_type_equals
    
    ! Convert monomorphic type to string
    recursive function mono_type_to_string(this) result(str)
        class(mono_type_t), intent(in) :: this
        character(len=:), allocatable :: str
        
        select case (this%kind)
        case (TVAR)
            str = this%var%name
        case (TINT)
            str = "integer"
        case (TREAL)
            str = "real(8)"
        case (TCHAR)
            if (this%size > 0) then
                block
                    character(len=20) :: size_str
                    write(size_str, '(i0)') this%size
                    str = "character(len=" // trim(size_str) // ")"
                end block
            else
                str = "character(*)"
            end if
        case (TFUN)
            if (allocated(this%args) .and. size(this%args) >= 2) then
                str = this%args(1)%to_string() // " -> " // this%args(2)%to_string()
            else
                str = "<invalid function type>"
            end if
        case (TARRAY)
            if (allocated(this%args) .and. size(this%args) >= 1) then
                if (this%size > 0) then
                    block
                        character(len=20) :: size_str
                        write(size_str, '(i0)') this%size
                        str = this%args(1)%to_string() // "(" // trim(size_str) // ")"
                    end block
                else
                    str = this%args(1)%to_string() // "(:)"
                end if
            else
                str = "<invalid array type>"
            end if
        case default
            str = "<unknown type>"
        end select
    end function mono_type_to_string
    
    ! Deep copy a monomorphic type
    recursive function mono_type_deep_copy(this) result(copy)
        class(mono_type_t), intent(in) :: this
        type(mono_type_t) :: copy
        integer :: i
        
        copy%kind = this%kind
        copy%var = this%var
        copy%size = this%size
        
        if (allocated(this%args)) then
            allocate(copy%args(size(this%args)))
            do i = 1, size(this%args)
                allocate(copy%args(i), source=this%args(i)%deep_copy())
            end do
        end if
    end function mono_type_deep_copy
    
    ! Convert polymorphic type to string
    function poly_type_to_string(this) result(str)
        class(poly_type_t), intent(in) :: this
        character(len=:), allocatable :: str
        integer :: i
        
        if (allocated(this%forall) .and. size(this%forall) > 0) then
            str = "forall"
            do i = 1, size(this%forall)
                str = str // " " // this%forall(i)%name
            end do
            str = str // ". " // this%mono%to_string()
        else
            str = this%mono%to_string()
        end if
    end function poly_type_to_string
    
    ! Add a substitution
    subroutine subst_add(this, var, typ)
        class(substitution_t), intent(inout) :: this
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: typ
        type(type_var_t), allocatable :: temp_vars(:)
        type(mono_type_t), allocatable :: temp_types(:)
        
        ! Grow arrays if needed
        if (this%count == 0) then
            allocate(this%vars(10))
            allocate(this%types(10))
        else if (this%count >= size(this%vars)) then
            allocate(temp_vars(size(this%vars) * 2))
            allocate(temp_types(size(this%types) * 2))
            temp_vars(1:this%count) = this%vars(1:this%count)
            temp_types(1:this%count) = this%types(1:this%count)
            call move_alloc(temp_vars, this%vars)
            call move_alloc(temp_types, this%types)
        end if
        
        this%count = this%count + 1
        this%vars(this%count) = var
        this%types(this%count) = typ%deep_copy()
    end subroutine subst_add
    
    ! Lookup a type variable in substitution
    function subst_lookup(this, var) result(typ)
        class(substitution_t), intent(in) :: this
        type(type_var_t), intent(in) :: var
        type(mono_type_t), allocatable :: typ
        integer :: i
        
        do i = 1, this%count
            if (this%vars(i)%id == var%id) then
                typ = this%types(i)%deep_copy()
                return
            end if
        end do
    end function subst_lookup
    
    ! Apply substitution to monomorphic type
    recursive function subst_apply_to_mono(this, typ) result(result_typ)
        class(substitution_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: result_typ
        type(mono_type_t), allocatable :: lookup_result
        integer :: i
        
        select case (typ%kind)
        case (TVAR)
            lookup_result = this%lookup(typ%var)
            if (allocated(lookup_result)) then
                result_typ = lookup_result
            else
                result_typ = typ%deep_copy()
            end if
            
        case (TFUN, TARRAY)
            result_typ%kind = typ%kind
            result_typ%size = typ%size
            if (allocated(typ%args)) then
                allocate(result_typ%args(size(typ%args)))
                do i = 1, size(typ%args)
                    allocate(result_typ%args(i), source=this%apply(typ%args(i)))
                end do
            end if
            
        case default
            result_typ = typ%deep_copy()
        end select
    end function subst_apply_to_mono
    
    ! Apply substitution to polymorphic type
    function subst_apply_to_poly(this, scheme) result(result_scheme)
        class(substitution_t), intent(in) :: this
        type(poly_type_t), intent(in) :: scheme
        type(poly_type_t) :: result_scheme
        type(substitution_t) :: filtered_subst
        integer :: i, j
        logical :: should_include
        
        ! Filter out substitutions for quantified variables
        filtered_subst%count = 0
        do i = 1, this%count
            should_include = .true.
            if (allocated(scheme%forall)) then
                do j = 1, size(scheme%forall)
                    if (this%vars(i)%id == scheme%forall(j)%id) then
                        should_include = .false.
                        exit
                    end if
                end do
            end if
            if (should_include) then
                call filtered_subst%add(this%vars(i), this%types(i))
            end if
        end do
        
        ! Apply filtered substitution
        if (allocated(scheme%forall)) then
            allocate(result_scheme%forall(size(scheme%forall)))
            result_scheme%forall = scheme%forall
        end if
        result_scheme%mono = filtered_subst%apply(scheme%mono)
    end function subst_apply_to_poly
    
    ! Apply substitution (general interface)
    function apply_substitution(subst, typ) result(result_typ)
        type(substitution_t), intent(in) :: subst
        class(*), intent(in) :: typ
        type(mono_type_t) :: result_typ
        
        select type (typ)
        type is (mono_type_t)
            result_typ = subst%apply(typ)
        class default
            error stop "apply_substitution: unsupported type"
        end select
    end function apply_substitution
    
    ! Compose two substitutions
    function compose_substitutions(s1, s2) result(comp)
        type(substitution_t), intent(in) :: s1, s2
        type(substitution_t) :: comp
        integer :: i
        
        ! First apply s1 to all types in s2
        do i = 1, s2%count
            call comp%add(s2%vars(i), s1%apply(s2%types(i)))
        end do
        
        ! Then add all mappings from s1 that are not in s2
        do i = 1, s1%count
            block
                logical :: found
                integer :: j
                found = .false.
                do j = 1, s2%count
                    if (s1%vars(i)%id == s2%vars(j)%id) then
                        found = .true.
                        exit
                    end if
                end do
                if (.not. found) then
                    call comp%add(s1%vars(i), s1%types(i))
                end if
            end block
        end do
    end function compose_substitutions
    
    ! Occurs check - check if variable occurs in type
    recursive logical function occurs_check(var, typ) result(occurs)
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: typ
        integer :: i
        
        occurs = .false.
        
        select case (typ%kind)
        case (TVAR)
            occurs = (var%id == typ%var%id)
        case (TFUN, TARRAY)
            if (allocated(typ%args)) then
                do i = 1, size(typ%args)
                    if (occurs_check(var, typ%args(i))) then
                        occurs = .true.
                        return
                    end if
                end do
            end if
        end select
    end function occurs_check
    
    ! Get free type variables in a type
    function free_type_vars(typ) result(vars)
        type(mono_type_t), intent(in) :: typ
        type(type_var_t), allocatable :: vars(:)
        type(type_var_t), allocatable :: temp_vars(:)
        integer :: count, i, j
        logical :: found
        
        allocate(temp_vars(100))  ! Temporary storage
        count = 0
        
        call collect_vars(typ)
        
        ! Return exact size array
        if (count > 0) then
            allocate(vars(count))
            vars = temp_vars(1:count)
        else
            allocate(vars(0))
        end if
        
    contains
        recursive subroutine collect_vars(t)
            type(mono_type_t), intent(in) :: t
            integer :: k
            
            select case (t%kind)
            case (TVAR)
                ! Check if already collected
                found = .false.
                do j = 1, count
                    if (temp_vars(j)%id == t%var%id) then
                        found = .true.
                        exit
                    end if
                end do
                if (.not. found) then
                    count = count + 1
                    temp_vars(count) = t%var
                end if
            case (TFUN, TARRAY)
                if (allocated(t%args)) then
                    do k = 1, size(t%args)
                        call collect_vars(t%args(k))
                    end do
                end if
            end select
        end subroutine collect_vars
    end function free_type_vars
    
    ! Type environment lookup
    function env_lookup(this, name) result(scheme)
        class(type_env_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), allocatable :: scheme
        integer :: i
        
        do i = 1, this%count
            if (this%names(i) == name) then
                scheme = this%schemes(i)
                return
            end if
        end do
    end function env_lookup
    
    ! Extend type environment with single binding
    subroutine env_extend(this, name, scheme)
        class(type_env_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), intent(in) :: scheme
        character(len=:), allocatable :: temp_names(:)
        type(poly_type_t), allocatable :: temp_schemes(:)
        integer :: new_capacity
        
        ! Initialize or grow arrays if needed
        if (this%capacity == 0) then
            this%capacity = 10
            allocate(character(len=256) :: this%names(this%capacity))
            allocate(this%schemes(this%capacity))
        else if (this%count >= this%capacity) then
            new_capacity = this%capacity * 2
            allocate(character(len=256) :: temp_names(new_capacity))
            allocate(temp_schemes(new_capacity))
            temp_names(1:this%count) = this%names(1:this%count)
            temp_schemes(1:this%count) = this%schemes(1:this%count)
            call move_alloc(temp_names, this%names)
            call move_alloc(temp_schemes, this%schemes)
            this%capacity = new_capacity
        end if
        
        ! Add new binding
        this%count = this%count + 1
        this%names(this%count) = name
        this%schemes(this%count) = scheme
    end subroutine env_extend
    
    ! Extend type environment with multiple bindings
    subroutine env_extend_many(this, names, schemes)
        class(type_env_t), intent(inout) :: this
        character(len=*), intent(in) :: names(:)
        type(poly_type_t), intent(in) :: schemes(:)
        integer :: i
        
        do i = 1, size(names)
            call this%extend(names(i), schemes(i))
        end do
    end subroutine env_extend_many
    
    ! Remove binding from environment
    function env_remove(this, name) result(new_env)
        class(type_env_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(type_env_t) :: new_env
        integer :: i
        
        new_env%capacity = this%capacity
        if (new_env%capacity > 0) then
            allocate(character(len=256) :: new_env%names(new_env%capacity))
            allocate(new_env%schemes(new_env%capacity))
        end if
        
        new_env%count = 0
        do i = 1, this%count
            if (this%names(i) /= name) then
                new_env%count = new_env%count + 1
                new_env%names(new_env%count) = this%names(i)
                new_env%schemes(new_env%count) = this%schemes(i)
            end if
        end do
    end function env_remove
    
    ! Apply substitution to environment
    function env_apply_subst(this, subst) result(new_env)
        class(type_env_t), intent(in) :: this
        type(substitution_t), intent(in) :: subst
        type(type_env_t) :: new_env
        integer :: i
        
        new_env%capacity = this%capacity
        new_env%count = this%count
        
        if (new_env%capacity > 0) then
            allocate(character(len=256) :: new_env%names(new_env%capacity))
            allocate(new_env%schemes(new_env%capacity))
            
            do i = 1, this%count
                new_env%names(i) = this%names(i)
                new_env%schemes(i) = subst%apply_to_poly(this%schemes(i))
            end do
        end if
    end function env_apply_subst
    
end module type_system_hm