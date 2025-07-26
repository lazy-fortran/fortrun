program debug_slice_generation
    use ast_core
    use ast_factory
    use codegen_core, only: generate_code_from_arena
    implicit none
    
    type(ast_arena_t) :: arena
    integer :: prog_idx, assign_idx, lhs_idx, rhs_idx, arg_idx
    integer, allocatable :: body_indices(:), arg_indices(:)
    character(len=:), allocatable :: code
    
    ! Create AST arena
    arena = create_ast_stack()
    
    ! Create identifier for lhs: sub
    lhs_idx = push_identifier(arena, "sub", 1, 1)
    
    ! Create range expression: 2:4
    block
        integer :: left_idx, right_idx
        left_idx = push_literal(arena, "2", LITERAL_INTEGER, 1, 7)
        right_idx = push_literal(arena, "4", LITERAL_INTEGER, 1, 9)
        arg_idx = push_binary_op(arena, left_idx, right_idx, ":", 1, 8)
    end block
    
    ! Create call_or_subscript: arr(2:4)
    allocate(arg_indices(1))
    arg_indices(1) = arg_idx
    rhs_idx = push_call_or_subscript(arena, "arr", arg_indices, 1, 5)
    
    ! Create assignment: sub = arr(2:4)
    assign_idx = push_assignment(arena, lhs_idx, rhs_idx, 1, 1)
    
    ! Generate code
    code = generate_code_from_arena(arena, assign_idx)
    print *, "Generated assignment: '", trim(code), "'"
    
    ! Also generate just the RHS
    code = generate_code_from_arena(arena, rhs_idx)
    print *, "Generated RHS: '", trim(code), "'"
    
    ! And just the argument
    code = generate_code_from_arena(arena, arg_idx)
    print *, "Generated arg: '", trim(code), "'"
    
end program debug_slice_generation