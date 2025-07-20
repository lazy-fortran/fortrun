program test_param_reversal
    use lexer_core
    use parser_state_module
    use parser_statements_module
    use ast_core
    use ast_factory
    use codegen_core
    implicit none

    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    type(parser_state_t) :: parser
    integer :: func_index, i
    character(len=:), allocatable :: code, generated_code

    ! Test function with 3 parameters
    print *, "=== Testing parameter order ==="
    print *, "Testing: function test(a, b, c)"
    print *

    ! Create tokens for: function test(real(8), intent(in) :: a, b, c)
    allocate (tokens(24))
    tokens(1) = token_t(TK_KEYWORD, "function", 1, 1)
    tokens(2) = token_t(TK_IDENTIFIER, "test", 1, 10)
    tokens(3) = token_t(TK_OPERATOR, "(", 1, 14)
    tokens(4) = token_t(TK_KEYWORD, "real", 1, 15)
    tokens(5) = token_t(TK_OPERATOR, "(", 1, 19)
    tokens(6) = token_t(TK_NUMBER, "8", 1, 20)
    tokens(7) = token_t(TK_OPERATOR, ")", 1, 21)
    tokens(8) = token_t(TK_OPERATOR, ",", 1, 22)
    tokens(9) = token_t(TK_KEYWORD, "intent", 1, 24)
    tokens(10) = token_t(TK_OPERATOR, "(", 1, 30)
    tokens(11) = token_t(TK_KEYWORD, "in", 1, 31)
    tokens(12) = token_t(TK_OPERATOR, ")", 1, 33)
    tokens(13) = token_t(TK_OPERATOR, "::", 1, 35)
    tokens(14) = token_t(TK_IDENTIFIER, "a", 1, 38)
    tokens(15) = token_t(TK_OPERATOR, ",", 1, 39)
    tokens(16) = token_t(TK_IDENTIFIER, "b", 1, 41)
    tokens(17) = token_t(TK_OPERATOR, ",", 1, 42)
    tokens(18) = token_t(TK_IDENTIFIER, "c", 1, 44)
    tokens(19) = token_t(TK_OPERATOR, ")", 1, 45)
    tokens(20) = token_t(TK_KEYWORD, "end", 2, 1)
    tokens(21) = token_t(TK_KEYWORD, "function", 2, 5)
    tokens(22) = token_t(TK_IDENTIFIER, "test", 2, 14)
    tokens(23) = token_t(TK_EOF, "", 3, 1)
    tokens(24) = token_t(TK_EOF, "", 3, 1)

    ! Create arena and parser
    arena = create_ast_stack()
    parser = create_parser_state(tokens)

    ! Parse function
    func_index = parse_function_definition(parser, arena)

    print *, "Function parsed, index:", func_index

    ! Check the function node
    if (func_index > 0 .and. allocated(arena%entries(func_index)%node)) then
        select type (node => arena%entries(func_index)%node)
        type is (function_def_node)
            print *, "Function name:", trim(node%name)
            print *, "Number of parameters:", size(node%param_indices)

            print *, "Parameter indices:"
            do i = 1, size(node%param_indices)
                print *, "  Param", i, "index:", node%param_indices(i)

                ! Print parameter details
                if (node%param_indices(i) > 0 .and. allocated(arena%entries(node%param_indices(i))%node)) then
                    select type (param => arena%entries(node%param_indices(i))%node)
                    type is (identifier_node)
                        print *, "    Name:", trim(param%name)
                    type is (parameter_declaration_node)
                        print *, "    Param decl name:", trim(param%name)
                    end select
                end if
            end do

            ! Check body indices if allocated
            if (allocated(node%body_indices)) then
                print *, "Body indices count:", size(node%body_indices)
                do i = 1, size(node%body_indices)
                    print *, "  Body", i, "index:", node%body_indices(i)
                    if (node%body_indices(i) > 0 .and. allocated(arena%entries(node%body_indices(i))%node)) then
                        select type (stmt => arena%entries(node%body_indices(i))%node)
                        type is (parameter_declaration_node)
                            print *, "    Param decl in body:", trim(stmt%name)
                        end select
                    end if
                end do
            else
                print *, "Body indices: not allocated"
            end if

            ! Generate code before standardization
            generated_code = generate_code_from_arena(arena, func_index)
            print *
            print *, "Generated code BEFORE standardization:"
            print *, generated_code

        end select
    end if

end program test_param_reversal
