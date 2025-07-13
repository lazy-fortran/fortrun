module semantic_analyzer
    ! Hindley-Milner type inference (Algorithm W) for lazy fortran
    use type_system_hm
    use ast_core
    use ast_lazy_fortran
    use ast_extensions, only: ast_type_map_t, create_type_map, set_node_type, get_node_type
    implicit none
    private
    
    public :: semantic_context_t, create_semantic_context
    public :: analyze_program
    
    ! Semantic analysis context
    type :: semantic_context_t
        type(type_env_t) :: env
        integer :: next_var_id = 0
        type(substitution_t) :: subst
        type(ast_type_map_t) :: type_map  ! Store type information separately
    contains
        procedure :: infer => infer_type
        procedure :: infer_stmt => infer_statement_type
        procedure :: unify => unify_types
        procedure :: instantiate => instantiate_type_scheme
        procedure :: generalize => generalize_type
        procedure :: fresh_type_var => generate_fresh_type_var
        procedure :: apply_subst_to_type => apply_current_substitution
        procedure :: get_builtin_function_type
        procedure :: compose_with_subst
    end type semantic_context_t
    
contains
    
    ! Create a new semantic context with builtin functions
    function create_semantic_context() result(ctx)
        type(semantic_context_t) :: ctx
        type(poly_type_t) :: scheme
        type(mono_type_t) :: real_type, fun_type
        type(type_var_t) :: a
        
        ! Initialize type map
        ctx%type_map = create_type_map()
        
        ! Initialize with builtin functions
        real_type = create_mono_type(TREAL)
        
        ! sqrt: real -> real
        fun_type = create_mono_type(TFUN, args=[create_mono_type(TREAL), create_mono_type(TREAL)])
        scheme = create_poly_type(forall_vars=[type_var_t::], mono=fun_type)
        call ctx%env%extend("sqrt", scheme)
        
        ! sin, cos, tan: real -> real
        call ctx%env%extend("sin", scheme)
        call ctx%env%extend("cos", scheme)
        call ctx%env%extend("tan", scheme)
        
        ! exp, log: real -> real
        call ctx%env%extend("exp", scheme)
        call ctx%env%extend("log", scheme)
        
        ! abs: 'a -> 'a (polymorphic)
        a = create_type_var(1, "'a")
        fun_type = create_mono_type(TFUN, args=[&
            create_mono_type(TVAR, var=a), &
            create_mono_type(TVAR, var=a)])
        scheme = create_poly_type(forall_vars=[a], mono=fun_type)
        call ctx%env%extend("abs", scheme)
        
        ctx%next_var_id = 2  ! Start after 'a
    end function create_semantic_context
    
    ! Main entry point: analyze entire program
    subroutine analyze_program(ctx, ast)
        type(semantic_context_t), intent(inout) :: ctx
        class(ast_node), intent(inout) :: ast
        
        ! Temporary fix: disable type inference to avoid segfaults
        ! Just return without doing analysis
        return
        
        select type (ast)
        type is (program_node)
            call analyze_program_node(ctx, ast)
        type is (lf_program_node)
            call analyze_program_node(ctx, ast%program_node)
        class default
            ! Single statement/expression
            call infer_and_store_type(ctx, ast)
        end select
    end subroutine analyze_program
    
    ! Analyze a program node
    subroutine analyze_program_node(ctx, prog)
        type(semantic_context_t), intent(inout) :: ctx
        type(program_node), intent(inout) :: prog
        integer :: i
        
        if (allocated(prog%body)) then
            do i = 1, size(prog%body)
                call infer_and_store_type(ctx, prog%body(i))
            end do
        end if
    end subroutine analyze_program_node
    
    ! Infer type and store in AST node
    subroutine infer_and_store_type(ctx, node)
        type(semantic_context_t), intent(inout) :: ctx
        class(ast_node), intent(inout) :: node
        type(mono_type_t) :: inferred
        
        inferred = ctx%infer_stmt(node)
        
        ! Store inferred type in type map
        call set_node_type(ctx%type_map, node, inferred)
    end subroutine infer_and_store_type
    
    ! Infer type of a statement
    function infer_statement_type(this, stmt) result(typ)
        class(semantic_context_t), intent(inout) :: this
        class(ast_node), intent(inout) :: stmt
        type(mono_type_t) :: typ
        
        select type (stmt)
        type is (assignment_node)
            typ = infer_assignment(this, stmt)
        type is (lf_assignment_node)
            typ = infer_assignment(this, stmt%assignment_node)
        type is (print_statement_node)
            typ = create_mono_type(TINT)  ! print returns unit/void, use int
        class default
            ! For expressions, use general inference
            typ = this%infer(stmt)
        end select
    end function infer_statement_type
    
    ! Infer type of assignment and update environment
    function infer_assignment(ctx, assign) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(assignment_node), intent(inout) :: assign
        type(mono_type_t) :: typ
        type(poly_type_t) :: scheme
        character(len=:), allocatable :: var_name
        
        ! Infer type of RHS
        typ = ctx%infer(assign%value)
        
        ! Store type in value node
        call set_node_type(ctx%type_map, assign%value, typ)
        
        ! Get variable name from target
        select type (target => assign%target)
        type is (identifier_node)
            var_name = target%name
            
            ! Store type in target node
            call set_node_type(ctx%type_map, target, typ)
        class default
            error stop "Assignment target must be identifier"
        end select
        
        ! Generalize and add to environment
        scheme = ctx%generalize(typ)
        call ctx%env%extend(var_name, scheme)
    end function infer_assignment
    
    ! Main type inference function (Algorithm W)
    recursive function infer_type(this, expr) result(typ)
        class(semantic_context_t), intent(inout) :: this
        class(ast_node), intent(inout) :: expr
        type(mono_type_t) :: typ
        
        select type (expr)
        type is (literal_node)
            typ = infer_literal(this, expr)
            
        type is (identifier_node)
            typ = infer_identifier(this, expr)
            
        type is (binary_op_node)
            typ = infer_binary_op(this, expr)
            
        type is (function_call_node)
            typ = infer_function_call(this, expr)
            
        type is (assignment_node)
            typ = infer_assignment(this, expr)
            
        class default
            error stop "Unsupported expression type in type inference"
        end select
        
        ! Apply current substitution
        typ = this%apply_subst_to_type(typ)
        
        ! Store inferred type in node
        call set_node_type(this%type_map, expr, typ)
    end function infer_type
    
    ! Infer type of literal
    function infer_literal(ctx, lit) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(literal_node), intent(in) :: lit
        type(mono_type_t) :: typ
        
        select case (lit%literal_kind)
        case (LITERAL_INTEGER)
            typ = create_mono_type(TINT)
        case (LITERAL_REAL)
            typ = create_mono_type(TREAL)
        case (LITERAL_STRING)
            ! Calculate string length (subtract 2 for quotes)
            typ = create_mono_type(TCHAR, char_size=len_trim(lit%value)-2)
        case default
            error stop "Unknown literal kind"
        end select
    end function infer_literal
    
    ! Infer type of identifier
    function infer_identifier(ctx, ident) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(identifier_node), intent(in) :: ident
        type(mono_type_t) :: typ
        
        ! Safety check: ensure identifier name is allocated and not empty
        if (.not. allocated(ident%name) .or. len_trim(ident%name) == 0) then
            typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
            return
        end if
        
        ! Temporary fix: assign a fresh type variable for all identifiers
        ! This avoids the segfault in environment lookup while we debug
        typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
    end function infer_identifier
    
    ! Infer type of binary operation
    function infer_binary_op(ctx, binop) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(binary_op_node), intent(inout) :: binop
        type(mono_type_t) :: typ
        type(mono_type_t) :: left_typ, right_typ, result_typ
        type(substitution_t) :: s1, s2, s3
        
        ! Infer left operand type
        left_typ = ctx%infer(binop%left)
        
        ! Infer right operand type  
        right_typ = ctx%infer(binop%right)
        
        ! Determine result type based on operator
        select case (trim(binop%operator))
        case ("+", "-", "*", "/", "**")
            ! Numeric operations: both operands and result have same type
            result_typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
            
            ! Unify left with result
            s1 = ctx%unify(left_typ, result_typ)
            call ctx%compose_with_subst(s1)
            
            ! Apply s1 to right_typ before unifying
            right_typ = s1%apply(right_typ)
            result_typ = s1%apply(result_typ)
            
            ! Unify right with result
            s2 = ctx%unify(right_typ, result_typ)
            call ctx%compose_with_subst(s2)
            
            ! Final result type
            result_typ = s2%apply(result_typ)
            
        case ("<", ">", "<=", ">=", "==", "/=")
            ! Comparison operations: operands same type, result is boolean (int)
            s1 = ctx%unify(left_typ, right_typ)
            call ctx%compose_with_subst(s1)
            result_typ = create_mono_type(TINT)  ! Boolean as integer
            
        case (".and.", ".or.")
            ! Logical operations: all integer (boolean)
            s1 = ctx%unify(left_typ, create_mono_type(TINT))
            call ctx%compose_with_subst(s1)
            s2 = ctx%unify(right_typ, create_mono_type(TINT))
            call ctx%compose_with_subst(s2)
            result_typ = create_mono_type(TINT)
            
        case default
            error stop "Unknown binary operator: " // trim(binop%operator)
        end select
        
        typ = ctx%apply_subst_to_type(result_typ)
    end function infer_binary_op
    
    ! Infer type of function call
    function infer_function_call(ctx, call_node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(function_call_node), intent(inout) :: call_node
        type(mono_type_t) :: typ
        type(mono_type_t) :: fun_typ, arg_typ, result_typ
        type(mono_type_t), allocatable :: arg_types(:)
        type(substitution_t) :: s
        integer :: i
        
        ! Get function type
        fun_typ = ctx%get_builtin_function_type(call_node%name)
        
        if (fun_typ%kind == 0) then
            ! Unknown function - look up in environment
            block
                type(identifier_node) :: fun_ident
                fun_ident = create_identifier(call_node%name, &
                    call_node%line, call_node%column)
                fun_typ = infer_identifier(ctx, fun_ident)
            end block
        end if
        
        ! Process arguments
        if (allocated(call_node%args)) then
            allocate(arg_types(size(call_node%args)))
            
            ! Infer all argument types
            do i = 1, size(call_node%args)
                arg_types(i) = ctx%infer(call_node%args(i))
            end do
            
            ! Unify with function type
            result_typ = fun_typ
            do i = 1, size(arg_types)
                ! Create expected function type: arg -> result
                block
                    type(type_var_t) :: tv
                    type(mono_type_t) :: expected_fun_type, new_result_typ
                    
                    tv = ctx%fresh_type_var()
                    new_result_typ = create_mono_type(TVAR, var=tv)
                    expected_fun_type = create_mono_type(TFUN, &
                        args=[arg_types(i), new_result_typ])
                    
                    ! Unify current function type with expected
                    s = ctx%unify(result_typ, expected_fun_type)
                    call ctx%compose_with_subst(s)
                    
                    ! Update result type
                    result_typ = s%apply(new_result_typ)
                end block
            end do
            
            typ = result_typ
        else
            ! No arguments - function type is the result
            if (fun_typ%kind == TFUN .and. allocated(fun_typ%args)) then
                typ = fun_typ%args(size(fun_typ%args))  ! Last element is return type
            else
                typ = fun_typ
            end if
        end if
        
        typ = ctx%apply_subst_to_type(typ)
    end function infer_function_call
    
    ! Type unification
    recursive function unify_types(this, t1, t2) result(subst)
        class(semantic_context_t), intent(inout) :: this
        type(mono_type_t), intent(in) :: t1, t2
        type(substitution_t) :: subst
        type(mono_type_t) :: t1_subst, t2_subst
        
        ! Apply current substitution first
        t1_subst = this%apply_subst_to_type(t1)
        t2_subst = this%apply_subst_to_type(t2)
        
        ! Initialize empty substitution
        subst%count = 0
        
        ! Handle type variables
        if (t1_subst%kind == TVAR) then
            if (t1_subst%var%id == t2_subst%var%id .and. t2_subst%kind == TVAR) then
                ! Same variable - empty substitution
                return
            else if (occurs_check(t1_subst%var, t2_subst)) then
                error stop "Occurs check failed - infinite type"
            else
                call subst%add(t1_subst%var, t2_subst)
            end if
            return
        else if (t2_subst%kind == TVAR) then
            if (occurs_check(t2_subst%var, t1_subst)) then
                error stop "Occurs check failed - infinite type"
            else
                call subst%add(t2_subst%var, t1_subst)
            end if
            return
        end if
        
        ! Both are concrete types
        if (t1_subst%kind /= t2_subst%kind) then
            error stop "Type mismatch: cannot unify " // &
                t1_subst%to_string() // " with " // t2_subst%to_string()
        end if
        
        select case (t1_subst%kind)
        case (TINT, TREAL)
            ! Base types unify if equal (already checked kind)
            
        case (TCHAR)
            if (t1_subst%size /= t2_subst%size) then
                error stop "Cannot unify character types of different lengths"
            end if
            
        case (TFUN)
            if (.not. allocated(t1_subst%args) .or. .not. allocated(t2_subst%args)) then
                error stop "Invalid function types"
            end if
            if (size(t1_subst%args) /= size(t2_subst%args)) then
                error stop "Function arity mismatch"
            end if
            
            ! Unify arguments pairwise
            block
                integer :: i
                type(substitution_t) :: s
                
                do i = 1, size(t1_subst%args)
                    s = this%unify(this%apply_subst_to_type(t1_subst%args(i)), &
                                   this%apply_subst_to_type(t2_subst%args(i)))
                    subst = compose_substitutions(s, subst)
                    call this%compose_with_subst(s)
                end do
            end block
            
        case (TARRAY)
            if (.not. allocated(t1_subst%args) .or. .not. allocated(t2_subst%args)) then
                error stop "Invalid array types"
            end if
            
            ! Unify element types
            subst = this%unify(t1_subst%args(1), t2_subst%args(1))
            
            ! Check sizes if known
            if (t1_subst%size > 0 .and. t2_subst%size > 0) then
                if (t1_subst%size /= t2_subst%size) then
                    error stop "Cannot unify arrays of different sizes"
                end if
            end if
            
        case default
            error stop "Unknown type kind in unification"
        end select
    end function unify_types
    
    ! Instantiate a type scheme
    function instantiate_type_scheme(this, scheme) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(poly_type_t), intent(in) :: scheme
        type(mono_type_t) :: typ
        type(substitution_t) :: subst
        integer :: i
        
        ! Create fresh type variables for all quantified variables
        subst%count = 0
        if (allocated(scheme%forall)) then
            do i = 1, size(scheme%forall)
                call subst%add(scheme%forall(i), &
                    create_mono_type(TVAR, var=this%fresh_type_var()))
            end do
        end if
        
        ! Apply substitution to get instance
        typ = subst%apply(scheme%mono)
    end function instantiate_type_scheme
    
    ! Generalize a type to a type scheme
    function generalize_type(this, typ) result(scheme)
        class(semantic_context_t), intent(inout) :: this
        type(mono_type_t), intent(in) :: typ
        type(poly_type_t) :: scheme
        type(type_var_t), allocatable :: free_vars(:), env_vars(:), gen_vars(:)
        integer :: i, j, count
        logical :: in_env
        
        ! Get free variables in type
        free_vars = free_type_vars(typ)
        
        if (size(free_vars) == 0) then
            ! No free variables - monomorphic type
            scheme = create_poly_type(forall_vars=[type_var_t::], mono=typ)
            return
        end if
        
        ! Get free variables in environment
        env_vars = get_env_free_vars(this%env)
        
        ! Find variables to generalize (in type but not in env)
        allocate(gen_vars(size(free_vars)))
        count = 0
        
        do i = 1, size(free_vars)
            in_env = .false.
            do j = 1, size(env_vars)
                if (free_vars(i)%id == env_vars(j)%id) then
                    in_env = .true.
                    exit
                end if
            end do
            
            if (.not. in_env) then
                count = count + 1
                gen_vars(count) = free_vars(i)
            end if
        end do
        
        ! Create type scheme
        if (count > 0) then
            scheme = create_poly_type(forall_vars=gen_vars(1:count), mono=typ)
        else
            scheme = create_poly_type(forall_vars=[type_var_t::], mono=typ)
        end if
    end function generalize_type
    
    ! Generate fresh type variable
    function generate_fresh_type_var(this) result(tv)
        class(semantic_context_t), intent(inout) :: this
        type(type_var_t) :: tv
        
        this%next_var_id = this%next_var_id + 1
        tv = create_type_var(this%next_var_id)
    end function generate_fresh_type_var
    
    ! Apply current substitution to a type
    function apply_current_substitution(this, typ) result(result_typ)
        class(semantic_context_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: result_typ
        
        result_typ = this%subst%apply(typ)
    end function apply_current_substitution
    
    ! Compose a substitution with the current one
    subroutine compose_with_subst(this, s)
        class(semantic_context_t), intent(inout) :: this
        type(substitution_t), intent(in) :: s
        
        this%subst = compose_substitutions(s, this%subst)
    end subroutine compose_with_subst
    
    ! Get builtin function type
    function get_builtin_function_type(this, name) result(typ)
        class(semantic_context_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(mono_type_t) :: typ
        
        ! Safety check: ensure name is not empty
        if (len_trim(name) == 0) then
            typ%kind = 0
            return
        end if
        
        ! For now, return a simple real->real function type for mathematical functions
        ! This is a temporary fix to avoid segfaults while we debug the environment
        select case (trim(name))
        case ("sqrt", "sin", "cos", "tan", "exp", "log", "abs")
            typ = create_mono_type(TFUN, args=[create_mono_type(TREAL), create_mono_type(TREAL)])
        case default
            ! Return empty type to indicate not found
            typ%kind = 0
        end select
    end function get_builtin_function_type
    
    ! Get free type variables in environment
    function get_env_free_vars(env) result(vars)
        type(type_env_t), intent(in) :: env
        type(type_var_t), allocatable :: vars(:), temp_vars(:), scheme_vars(:)
        integer :: i, j, k, count
        logical :: found
        
        allocate(temp_vars(1000))  ! Temporary storage
        count = 0
        
        ! Collect all free variables from all schemes
        do i = 1, env%count
            scheme_vars = get_scheme_free_vars(env%schemes(i))
            
            do j = 1, size(scheme_vars)
                ! Check if already collected
                found = .false.
                do k = 1, count
                    if (temp_vars(k)%id == scheme_vars(j)%id) then
                        found = .true.
                        exit
                    end if
                end do
                
                if (.not. found) then
                    count = count + 1
                    temp_vars(count) = scheme_vars(j)
                end if
            end do
        end do
        
        ! Return exact size array
        if (count > 0) then
            allocate(vars(count))
            vars = temp_vars(1:count)
        else
            allocate(vars(0))
        end if
    end function get_env_free_vars
    
    ! Get free variables in a type scheme
    function get_scheme_free_vars(scheme) result(vars)
        type(poly_type_t), intent(in) :: scheme
        type(type_var_t), allocatable :: vars(:), mono_vars(:)
        integer :: i, j, count
        logical :: quantified
        
        ! Get free variables in monotype
        mono_vars = free_type_vars(scheme%mono)
        
        if (.not. allocated(scheme%forall) .or. size(scheme%forall) == 0) then
            vars = mono_vars
            return
        end if
        
        ! Filter out quantified variables
        allocate(vars(size(mono_vars)))
        count = 0
        
        do i = 1, size(mono_vars)
            quantified = .false.
            do j = 1, size(scheme%forall)
                if (mono_vars(i)%id == scheme%forall(j)%id) then
                    quantified = .true.
                    exit
                end if
            end do
            
            if (.not. quantified) then
                count = count + 1
                vars(count) = mono_vars(i)
            end if
        end do
        
        ! Return exact size
        if (count > 0) then
            vars = vars(1:count)
        else
            allocate(vars(0))
        end if
    end function get_scheme_free_vars
    
end module semantic_analyzer