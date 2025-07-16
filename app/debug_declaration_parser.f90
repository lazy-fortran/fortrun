program debug_declaration_parser
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    print '(a)', "Debug: Testing declaration parsing"

    call test_simple_declaration()

contains

    subroutine test_simple_declaration()
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        print '(a)', "Debug: Testing 'integer :: x'"

        ! Tokenize
        call tokenize_core("integer :: x", tokens)
        print '(a,i0)', "Debug: Number of tokens: ", size(tokens)

        ! Create parser state
        block
            type(parser_state_t) :: parser
            parser = create_parser_state(tokens)

            print '(a)', "Debug: Calling parse_statement..."
            ast_result = parse_statement(tokens)
            print '(a)', "Debug: parse_statement returned"
        end block

        print '(a)', "Debug: Test complete"
    end subroutine test_simple_declaration

end program debug_declaration_parser
