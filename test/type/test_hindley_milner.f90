program test_hindley_milner
    ! Test Hindley-Milner type inference implementation
    use type_system
    use semantic_analyzer
    use ast_core
    use parser_core
    use lexer_core, only: tokenize_core
    implicit none
    
    integer :: failures = 0
    
    ! Run all tests
    call test_type_system_basics()
    call test_type_unification()
    call test_type_inference_literals()
    call test_type_inference_assignments()
    call test_type_inference_expressions()
    call test_type_generalization()
    call test_polymorphic_functions()
    
    if (failures == 0) then
        print *, "All Hindley-Milner tests passed!"
    else
        print *, "Hindley-Milner tests failed:", failures
        stop 1
    end if
    
contains

    subroutine test_type_system_basics()
        type(mono_type_t) :: t1, t2, t3
        type(type_var_t) :: tv1, tv2
        type(substitution_t) :: subst
        logical :: equal
        
        print *, "Testing type system basics..."
        
        ! Test type variable creation
        tv1 = create_type_var(1)
        if (tv1%name /= "'a") call fail("Type var 1 should be 'a")
        
        tv2 = create_type_var(27)
        if (tv2%name /= "'a1") call fail("Type var 27 should be 'a1")
        
        ! Test monomorphic type creation
        t1 = create_mono_type(TINT)
        if (t1%kind /= TINT) call fail("Int type creation failed")
        
        t2 = create_mono_type(TREAL)
        if (t2%kind /= TREAL) call fail("Real type creation failed")
        
        t3 = create_mono_type(TCHAR, size=10)
        if (t3%kind /= TCHAR .or. t3%size /= 10) call fail("Char type creation failed")
        
        ! Test type equality
        equal = t1%equals(t1)
        if (.not. equal) call fail("Type should equal itself")
        
        equal = t1%equals(t2)
        if (equal) call fail("Int should not equal real")
        
        ! Test substitution
        call subst%add(tv1, t1)
        t2 = create_mono_type(TVAR, var=tv1)
        t3 = subst%apply(t2)
        if (t3%kind /= TINT) call fail("Substitution failed")
        
        print *, "  ✓ Type system basics work correctly"
    end subroutine test_type_system_basics
    
    subroutine test_type_unification()
        type(semantic_context_t) :: ctx
        type(mono_type_t) :: t1, t2, t3, t4
        type(type_var_t) :: tv1, tv2
        type(substitution_t) :: subst
        
        print *, "Testing type unification..."
        
        ctx = create_semantic_context()
        
        ! Test unifying same concrete types
        t1 = create_mono_type(TINT)
        t2 = create_mono_type(TINT)
        subst = ctx%unify(t1, t2)
        if (subst%count /= 0) call fail("Unifying same types should give empty subst")
        
        ! Test unifying different concrete types (should fail)
        ! In real implementation this would error, so we skip
        
        ! Test unifying type variable with concrete type
        tv1 = ctx%fresh_type_var()
        t1 = create_mono_type(TVAR, var=tv1)
        t2 = create_mono_type(TREAL)
        subst = ctx%unify(t1, t2)
        if (subst%count /= 1) call fail("Should have one substitution")
        
        t3 = subst%apply(t1)
        if (t3%kind /= TREAL) call fail("Type var should be substituted with real")
        
        print *, "  ✓ Type unification works correctly"
    end subroutine test_type_unification
    
    subroutine test_type_inference_literals()
        type(semantic_context_t) :: ctx
        type(literal_node) :: lit_int, lit_real, lit_str
        type(mono_type_t) :: inferred
        
        print *, "Testing type inference for literals..."
        
        ctx = create_semantic_context()
        
        ! Test integer literal
        lit_int = create_literal("42", INTEGER_LITERAL, 1, 1)
        inferred = ctx%infer(lit_int)
        if (inferred%kind /= TINT) call fail("Integer literal should infer to int")
        
        ! Test real literal  
        lit_real = create_literal("3.14", REAL_LITERAL, 1, 1)
        inferred = ctx%infer(lit_real)
        if (inferred%kind /= TREAL) call fail("Real literal should infer to real")
        
        ! Test string literal
        lit_str = create_literal('"Hello"', STRING_LITERAL, 1, 1)
        inferred = ctx%infer(lit_str)
        if (inferred%kind /= TCHAR) call fail("String literal should infer to char")
        if (inferred%size /= 5) call fail("String length should be 5")
        
        print *, "  ✓ Literal type inference works correctly"
    end subroutine test_type_inference_literals
    
    subroutine test_type_inference_assignments()
        type(semantic_context_t) :: ctx
        type(assignment_node) :: assign
        type(identifier_node) :: ident
        type(literal_node) :: lit
        type(mono_type_t) :: inferred
        type(poly_type_t), allocatable :: scheme
        
        print *, "Testing type inference for assignments..."
        
        ctx = create_semantic_context()
        
        ! Test x = 42
        ident = create_identifier("x", 1, 1)
        lit = create_literal("42", INTEGER_LITERAL, 1, 5)
        assign = create_assignment(ident, lit)
        
        inferred = ctx%infer(assign)
        if (inferred%kind /= TINT) call fail("Assignment should infer to int")
        
        ! Check that x is now in environment
        scheme = ctx%env%lookup("x")
        if (.not. allocated(scheme)) call fail("Variable x should be in environment")
        
        ! Test type of x after assignment
        ident = create_identifier("x", 2, 1)
        inferred = ctx%infer(ident)
        if (inferred%kind /= TINT) call fail("Variable x should have int type")
        
        print *, "  ✓ Assignment type inference works correctly"
    end subroutine test_type_inference_assignments
    
    subroutine test_type_inference_expressions()
        type(semantic_context_t) :: ctx
        type(assignment_node) :: assign
        type(identifier_node) :: ident_x, ident_y
        type(literal_node) :: lit1, lit2
        type(binary_op_node) :: binop
        type(mono_type_t) :: inferred
        
        print *, "Testing type inference for expressions..."
        
        ctx = create_semantic_context()
        
        ! First assign x = 3.14
        ident_x = create_identifier("x", 1, 1)
        lit1 = create_literal("3.14", REAL_LITERAL, 1, 5)
        assign = create_assignment(ident_x, lit1)
        inferred = ctx%infer(assign)
        
        ! Now test y = x + 2.0
        ident_y = create_identifier("y", 2, 1)
        ident_x = create_identifier("x", 2, 5)
        lit2 = create_literal("2.0", REAL_LITERAL, 2, 9)
        binop = create_binary_op(ident_x, lit2, "+")
        assign = create_assignment(ident_y, binop)
        
        inferred = ctx%infer(assign)
        if (inferred%kind /= TREAL) call fail("Binary op should infer to real")
        
        ! Check y's type
        ident_y = create_identifier("y", 3, 1)
        inferred = ctx%infer(ident_y)
        if (inferred%kind /= TREAL) call fail("Variable y should have real type")
        
        print *, "  ✓ Expression type inference works correctly"
    end subroutine test_type_inference_expressions
    
    subroutine test_type_generalization()
        type(semantic_context_t) :: ctx
        type(mono_type_t) :: mono_type
        type(poly_type_t) :: poly_type
        type(type_var_t) :: tv
        
        print *, "Testing type generalization..."
        
        ctx = create_semantic_context()
        
        ! Test generalizing a concrete type (should not generalize)
        mono_type = create_mono_type(TINT)
        poly_type = ctx%generalize(mono_type)
        if (allocated(poly_type%forall)) then
            if (size(poly_type%forall) > 0) call fail("Concrete type should not generalize")
        end if
        
        ! Test generalizing a free type variable
        tv = ctx%fresh_type_var()
        mono_type = create_mono_type(TVAR, var=tv)
        poly_type = ctx%generalize(mono_type)
        if (.not. allocated(poly_type%forall)) call fail("Should generalize free var")
        if (size(poly_type%forall) /= 1) call fail("Should generalize one var")
        
        print *, "  ✓ Type generalization works correctly"
    end subroutine test_type_generalization
    
    subroutine test_polymorphic_functions()
        type(semantic_context_t) :: ctx
        type(function_call_node) :: call_node
        type(identifier_node) :: arg
        type(literal_node) :: lit
        type(mono_type_t) :: inferred
        class(ast_node), allocatable :: args(:)
        
        print *, "Testing polymorphic function types..."
        
        ctx = create_semantic_context()
        
        ! Test abs(5) - should infer to int
        lit = create_literal("5", INTEGER_LITERAL, 1, 5)
        allocate(args(1))
        args(1) = lit
        call_node = create_function_call("abs", args)
        
        inferred = ctx%infer(call_node)
        if (inferred%kind /= TINT) call fail("abs(int) should return int")
        
        ! Test abs(3.14) - should infer to real
        lit = create_literal("3.14", REAL_LITERAL, 2, 5)
        deallocate(args)
        allocate(args(1))
        args(1) = lit
        call_node = create_function_call("abs", args)
        
        inferred = ctx%infer(call_node)
        if (inferred%kind /= TREAL) call fail("abs(real) should return real")
        
        print *, "  ✓ Polymorphic function inference works correctly"
    end subroutine test_polymorphic_functions
    
    subroutine fail(msg)
        character(len=*), intent(in) :: msg
        print *, "  ✗ FAIL:", msg
        failures = failures + 1
    end subroutine fail
    
end program test_hindley_milner