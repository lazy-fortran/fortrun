program test_implied_do_codegen
    use ast_core
    use ast_types
    use ast_factory
    use codegen_core
    implicit none
    
    logical :: all_passed
    type(ast_arena_t) :: arena
    
    all_passed = .true.
    
    print *, '=== Implied Do Loop Code Generation Unit Tests ==='
    print *
    
    if (.not. test_simple_implied_do()) all_passed = .false.
    if (.not. test_implied_do_with_step()) all_passed = .false.
    if (.not. test_implied_do_expression()) all_passed = .false.
    if (.not. test_nested_implied_do()) all_passed = .false.
    if (.not. test_array_literal_standard()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, 'All implied do codegen tests passed!'
        stop 0
    else
        print *, 'Some implied do codegen tests failed!'
        stop 1
    end if
    
contains

    logical function test_simple_implied_do()
        integer :: arr_idx, do_idx, i_idx, start_idx, end_idx
        integer, allocatable :: body_indices(:), elem_indices(:)
        character(len=:), allocatable :: code
        
        test_simple_implied_do = .true.
        print *, 'Testing code generation for [(i, i=1,10)]...'
        
        ! Build AST: i
        i_idx = push_identifier(arena, "i", 1, 1)
        
        ! Build AST: 1
        start_idx = push_literal(arena, "1", LITERAL_INTEGER, 1, 1)
        
        ! Build AST: 10
        end_idx = push_literal(arena, "10", LITERAL_INTEGER, 1, 1)
        
        ! Build do loop with i as body
        allocate(body_indices(1))
        body_indices(1) = i_idx
        do_idx = push_do_loop(arena, "i", start_idx, end_idx, 0, body_indices, 1, 1)
        
        ! Build array literal with do loop
        allocate(elem_indices(1))
        elem_indices(1) = do_idx
        arr_idx = push_array_literal(arena, elem_indices, 1, 1)
        
        ! Generate code
        code = generate_code_from_arena(arena, arr_idx)
        
        print *, '  Generated: ', code
        
        if (index(code, "(/ (i, i=1,10) /)") > 0) then
            print *, '  PASS: Correct implied do syntax'
        else
            print *, '  FAIL: Expected "(/ (i, i=1,10) /)"'
            test_simple_implied_do = .false.
        end if
        
    end function test_simple_implied_do
    
    logical function test_implied_do_with_step()
        integer :: arr_idx, do_idx, i_idx, start_idx, end_idx, step_idx
        integer, allocatable :: body_indices(:), elem_indices(:)
        character(len=:), allocatable :: code
        
        test_implied_do_with_step = .true.
        print *, 'Testing code generation for [(i, i=1,10,2)]...'
        
        ! Build AST
        i_idx = push_identifier(arena, "i", 1, 1)
        start_idx = push_literal(arena, "1", LITERAL_INTEGER, 1, 1)
        end_idx = push_literal(arena, "10", LITERAL_INTEGER, 1, 1)
        step_idx = push_literal(arena, "2", LITERAL_INTEGER, 1, 1)
        
        allocate(body_indices(1))
        body_indices(1) = i_idx
        do_idx = push_do_loop(arena, "i", start_idx, end_idx, step_idx, body_indices, 1, 1)
        
        allocate(elem_indices(1))
        elem_indices(1) = do_idx
        arr_idx = push_array_literal(arena, elem_indices, 1, 1)
        
        code = generate_code_from_arena(arena, arr_idx)
        
        print *, '  Generated: ', code
        
        if (index(code, "(/ (i, i=1,10,2) /)") > 0) then
            print *, '  PASS: Correct implied do with step'
        else
            print *, '  FAIL: Expected "(/ (i, i=1,10,2) /)"'
            test_implied_do_with_step = .false.
        end if
        
    end function test_implied_do_with_step
    
    logical function test_implied_do_expression()
        integer :: arr_idx, do_idx, i_idx, mult_idx, start_idx, end_idx
        integer, allocatable :: body_indices(:), elem_indices(:)
        character(len=:), allocatable :: code
        
        test_implied_do_expression = .true.
        print *, 'Testing code generation for [(i*i, i=1,5)]...'
        
        ! Build AST: i*i
        i_idx = push_identifier(arena, "i", 1, 1)
        mult_idx = push_binary_op(arena, i_idx, i_idx, "*", 1, 1)
        
        start_idx = push_literal(arena, "1", LITERAL_INTEGER, 1, 1)
        end_idx = push_literal(arena, "5", LITERAL_INTEGER, 1, 1)
        
        allocate(body_indices(1))
        body_indices(1) = mult_idx
        do_idx = push_do_loop(arena, "i", start_idx, end_idx, 0, body_indices, 1, 1)
        
        allocate(elem_indices(1))
        elem_indices(1) = do_idx
        arr_idx = push_array_literal(arena, elem_indices, 1, 1)
        
        code = generate_code_from_arena(arena, arr_idx)
        
        print *, '  Generated: ', code
        
        if (index(code, "(/ (i*i, i=1,5) /)") > 0 .or. &
            index(code, "(/ (i * i, i=1,5) /)") > 0) then
            print *, '  PASS: Correct implied do with expression'
        else
            print *, '  FAIL: Expected "(/ (i*i, i=1,5) /)"'
            test_implied_do_expression = .false.
        end if
        
    end function test_implied_do_expression
    
    logical function test_nested_implied_do()
        integer :: arr_idx, do_idx, expr_idx, i_idx, j_idx
        integer :: start_idx, end_i, end_j
        integer, allocatable :: body_indices(:), elem_indices(:)
        character(len=:), allocatable :: code
        
        test_nested_implied_do = .true.
        print *, 'Testing nested expression [(i+j, i=1,3)]...'
        
        ! Build AST: i+j
        i_idx = push_identifier(arena, "i", 1, 1)
        j_idx = push_identifier(arena, "j", 1, 1)
        expr_idx = push_binary_op(arena, i_idx, j_idx, "+", 1, 1)
        
        start_idx = push_literal(arena, "1", LITERAL_INTEGER, 1, 1)
        end_i = push_literal(arena, "3", LITERAL_INTEGER, 1, 1)
        
        allocate(body_indices(1))
        body_indices(1) = expr_idx
        do_idx = push_do_loop(arena, "i", start_idx, end_i, 0, body_indices, 1, 1)
        
        allocate(elem_indices(1))
        elem_indices(1) = do_idx
        arr_idx = push_array_literal(arena, elem_indices, 1, 1)
        
        code = generate_code_from_arena(arena, arr_idx)
        
        print *, '  Generated: ', code
        
        if (index(code, "(/ (i+j, i=1,3) /)") > 0 .or. &
            index(code, "(/ (i + j, i=1,3) /)") > 0) then
            print *, '  PASS: Correct nested expression'
        else
            print *, '  FAIL: Wrong output for nested expression'
            test_nested_implied_do = .false.
        end if
        
    end function test_nested_implied_do
    
    logical function test_array_literal_standard()
        integer :: arr_idx
        integer :: idx1, idx2, idx3
        integer, allocatable :: elem_indices(:)
        character(len=:), allocatable :: code
        
        test_array_literal_standard = .true.
        print *, 'Testing standard array literal [1, 2, 3]...'
        
        ! Build AST
        idx1 = push_literal(arena, "1", LITERAL_INTEGER, 1, 1)
        idx2 = push_literal(arena, "2", LITERAL_INTEGER, 1, 1)
        idx3 = push_literal(arena, "3", LITERAL_INTEGER, 1, 1)
        
        allocate(elem_indices(3))
        elem_indices = [idx1, idx2, idx3]
        arr_idx = push_array_literal(arena, elem_indices, 1, 1)
        
        code = generate_code_from_arena(arena, arr_idx)
        
        print *, '  Generated: ', code
        
        if (index(code, "(/ 1, 2, 3 /)") > 0) then
            print *, '  PASS: Correct array literal syntax'
        else
            print *, '  FAIL: Expected "(/ 1, 2, 3 /)"'
            test_array_literal_standard = .false.
        end if
        
    end function test_array_literal_standard
    
end program test_implied_do_codegen