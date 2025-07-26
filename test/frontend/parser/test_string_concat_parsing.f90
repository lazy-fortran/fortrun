program test_string_concat_parsing
    use ast_core
    use ast_types
    use lexer_core, only: tokenize_core, token_t
    use parser_expressions, only: parse_expression
    implicit none
    
    logical :: all_passed
    type(ast_arena_t) :: arena
    
    all_passed = .true.
    
    print *, '=== String Concatenation Parser Unit Tests ==='
    print *
    
    if (.not. test_simple_concat()) all_passed = .false.
    if (.not. test_multiple_concat()) all_passed = .false.
    if (.not. test_concat_with_spaces()) all_passed = .false.
    if (.not. test_concat_in_expr()) all_passed = .false.
    if (.not. test_empty_string_concat()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, 'All string concatenation parser tests passed!'
        stop 0
    else
        print *, 'Some string concatenation parser tests failed!'
        stop 1
    end if
    
contains

    logical function test_simple_concat()
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_simple_concat = .true.
        print *, 'Testing simple concatenation "hello" // "world"...'
        
        call tokenize_core('"hello" // "world"', tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index > 0) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                type is (binary_op_node)
                    if (node%op == "//") then
                        print *, '  PASS: Parsed as concatenation operator'
                    else
                        print *, '  FAIL: Wrong operator:', node%op
                        test_simple_concat = .false.
                    end if
                class default
                    print *, '  FAIL: Not parsed as binary_op_node'
                    test_simple_concat = .false.
                end select
            else
                print *, '  FAIL: Node not allocated'
                test_simple_concat = .false.
            end if
        else
            print *, '  FAIL: Parse failed'
            test_simple_concat = .false.
        end if
        
    end function test_simple_concat
    
    logical function test_multiple_concat()
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_multiple_concat = .true.
        print *, 'Testing multiple concatenation "a" // "b" // "c"...'
        
        call tokenize_core('"a" // "b" // "c"', tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index > 0) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                type is (binary_op_node)
                    if (node%op == "//") then
                        ! Check if left side is also concatenation
                        if (node%left_index > 0 .and. &
                            allocated(arena%entries(node%left_index)%node)) then
                            select type (left => arena%entries(node%left_index)%node)
                            type is (binary_op_node)
                                if (left%op == "//") then
                                    print *, '  PASS: Left-associative concatenation'
                                else
                                    print *, '  FAIL: Left not concatenation'
                                    test_multiple_concat = .false.
                                end if
                            class default
                                print *, '  FAIL: Left not binary op'
                                test_multiple_concat = .false.
                            end select
                        end if
                    else
                        print *, '  FAIL: Wrong operator:', node%op
                        test_multiple_concat = .false.
                    end if
                class default
                    print *, '  FAIL: Not parsed as binary_op_node'
                    test_multiple_concat = .false.
                end select
            end if
        else
            print *, '  FAIL: Parse failed'
            test_multiple_concat = .false.
        end if
        
    end function test_multiple_concat
    
    logical function test_concat_with_spaces()
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_concat_with_spaces = .true.
        print *, 'Testing concatenation with spaces...'
        
        call tokenize_core('"hello " // " " // "world"', tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index > 0) then
            print *, '  PASS: Parsed concatenation with space strings'
        else
            print *, '  FAIL: Parse failed'
            test_concat_with_spaces = .false.
        end if
        
    end function test_concat_with_spaces
    
    logical function test_concat_in_expr()
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_concat_in_expr = .true.
        print *, 'Testing concatenation in assignment msg = "Hi" // name...'
        
        call tokenize_core('"Hi" // name', tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index > 0) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                type is (binary_op_node)
                    if (node%op == "//") then
                        ! Check right side is identifier
                        if (node%right_index > 0 .and. &
                            allocated(arena%entries(node%right_index)%node)) then
                            select type (right => arena%entries(node%right_index)%node)
                            type is (identifier_node)
                                if (right%name == "name") then
                                    print *, '  PASS: String concat with identifier'
                                else
                                    print *, '  FAIL: Wrong identifier:', right%name
                                    test_concat_in_expr = .false.
                                end if
                            class default
                                print *, '  FAIL: Right not identifier'
                                test_concat_in_expr = .false.
                            end select
                        end if
                    end if
                class default
                    print *, '  FAIL: Not parsed as binary_op_node'
                    test_concat_in_expr = .false.
                end select
            end if
        else
            print *, '  FAIL: Parse failed'
            test_concat_in_expr = .false.
        end if
        
    end function test_concat_in_expr
    
    logical function test_empty_string_concat()
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_empty_string_concat = .true.
        print *, 'Testing empty string concatenation "" // "text"...'
        
        call tokenize_core('"" // "text"', tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index > 0) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                type is (binary_op_node)
                    if (node%op == "//") then
                        ! Check left is empty string
                        if (node%left_index > 0 .and. &
                            allocated(arena%entries(node%left_index)%node)) then
                            select type (left => arena%entries(node%left_index)%node)
                            type is (literal_node)
                                if (left%literal_type == LITERAL_STRING) then
                                    print *, '  PASS: Empty string concatenation'
                                else
                                    print *, '  FAIL: Left not string literal'
                                    test_empty_string_concat = .false.
                                end if
                            class default
                                print *, '  FAIL: Left not literal'
                                test_empty_string_concat = .false.
                            end select
                        end if
                    end if
                class default
                    print *, '  FAIL: Not parsed as binary_op_node'
                    test_empty_string_concat = .false.
                end select
            end if
        else
            print *, '  FAIL: Parse failed'
            test_empty_string_concat = .false.
        end if
        
    end function test_empty_string_concat
    
end program test_string_concat_parsing