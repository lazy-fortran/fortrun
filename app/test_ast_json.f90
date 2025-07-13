program test_ast_json
    use lexer_core, only: token_t, tokenize_core
    use parser_core, only: parse_statement
    use json_writer, only: json_write_ast_to_file
    use ast_core
    implicit none
    
    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    class(ast_node), allocatable :: ast
    
    ! Test assignment parsing
    source = "x = 42"
    call tokenize_core(source, tokens)
    
    ast = parse_statement(tokens)
    
    select type(ast)
    type is (assignment_node)
        print *, "Successfully parsed assignment!"
        call json_write_ast_to_file(ast, "test_assignment.json")
        print *, "AST written to test_assignment.json"
    class default
        print *, "Failed to parse as assignment"
    end select
    
end program test_ast_json