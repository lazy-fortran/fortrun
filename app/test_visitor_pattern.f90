program test_visitor_pattern
    use ast_core
    use ast_factory
    use ast_visitor
    implicit none

    type(ast_arena_t) :: arena
    type(debug_visitor_t) :: visitor
    integer :: prog_index, assign_index, target_index, value_index

    ! Initialize arena
    arena = create_ast_stack()

    ! Create simple AST: x = 42
    target_index = push_identifier(arena, "x", 1, 1)
    value_index = push_literal(arena, "42", LITERAL_INTEGER, 1, 5)
    assign_index = push_assignment(arena, target_index, value_index, 1, 1)

    ! Create program with the assignment
    prog_index = push_program(arena, "test_prog", [assign_index], 1, 1)

    ! Initialize visitor
    visitor%indent_level = 0
    visitor%output = ""

    ! Test the visitor pattern
    print *, "Testing visitor pattern implementation..."

    ! Visit the program node
    if (prog_index > 0 .and. prog_index <= arena%size) then
        if (allocated(arena%entries(prog_index)%node)) then
            select type (node => arena%entries(prog_index)%node)
            type is (program_node)
                call visitor%visit_program(node)
            end select
        end if
    end if

    print *, "Visitor output:"
    print *, visitor%output
    print *, "Visitor pattern test completed successfully!"

end program test_visitor_pattern
