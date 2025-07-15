program debug_statement_parsing
    use lexer_core, only: tokenize_core, token_t, TK_EOF
    use parser_core, only: parse_statement
    use ast_core, only: ast_node, literal_node, assignment_node
    implicit none
    
    type(token_t), allocatable :: tokens(:)
    class(ast_node), allocatable :: stmt
    character(len=*), parameter :: test_stmt = "real :: x"
    
    print *, '=== Statement Parsing Debug ==='
    print *, 'Input statement: "' // test_stmt // '"'
    print *
    
    ! Tokenize
    call tokenize_core(test_stmt, tokens)
    print *, 'Tokenized', size(tokens), 'tokens'
    
    ! Parse statement
    stmt = parse_statement(tokens)
    
    if (allocated(stmt)) then
        print *, 'Successfully parsed statement'
        select type(stmt)
        type is (literal_node)
            print *, 'Parsed as literal_node with value: "' // stmt%value // '"'
        type is (assignment_node)
            print *, 'Parsed as assignment_node'
        ! No declaration_node available
        class default
            print *, 'Parsed as different node type'
        end select
    else
        print *, 'Failed to parse statement'
    end if
    
end program debug_statement_parsing