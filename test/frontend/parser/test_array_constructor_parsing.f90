program test_array_constructor_parsing
    use ast_core
    use ast_types
    use ast_factory
    use lexer_core, only: tokenize_core, token_t
    use parser_expressions, only: parse_expression
    implicit none
    
    logical :: all_passed
    type(ast_arena_t) :: arena
    
    all_passed = .true.
    
    print *, '=== Array Constructor Parser Unit Tests ==='
    print *
    
    if (.not. test_simple_array_literal()) all_passed = .false.
    if (.not. test_empty_array_literal()) all_passed = .false.
    if (.not. test_nested_expressions()) all_passed = .false.
    if (.not. test_implied_do_simple()) all_passed = .false.
    if (.not. test_implied_do_with_step()) all_passed = .false.
    if (.not. test_implied_do_expression()) all_passed = .false.
    if (.not. test_mixed_array_elements()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, 'All array constructor parser tests passed!'
        stop 0
    else
        print *, 'Some array constructor parser tests failed!'
        stop 1
    end if
    
contains

    logical function test_simple_array_literal()
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_simple_array_literal = .true.
        print *, 'Testing simple array literal [1, 2, 3]...'
        
        call tokenize_core("[1, 2, 3]", tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index > 0) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                type is (array_literal_node)
                    if (allocated(node%element_indices)) then
                        if (size(node%element_indices) == 3) then
                            print *, '  PASS: Parsed 3 elements'
                        else
                            print *, '  FAIL: Expected 3 elements, got', size(node%element_indices)
                            test_simple_array_literal = .false.
                        end if
                    else
                        print *, '  FAIL: No elements allocated'
                        test_simple_array_literal = .false.
                    end if
                class default
                    print *, '  FAIL: Not parsed as array_literal_node'
                    test_simple_array_literal = .false.
                end select
            else
                print *, '  FAIL: Node not allocated'
                test_simple_array_literal = .false.
            end if
        else
            print *, '  FAIL: Parse failed'
            test_simple_array_literal = .false.
        end if
        
    end function test_simple_array_literal
    
    logical function test_empty_array_literal()
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_empty_array_literal = .true.
        print *, 'Testing empty array literal []...'
        
        call tokenize_core("[]", tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index > 0) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                type is (array_literal_node)
                    if (allocated(node%element_indices)) then
                        if (size(node%element_indices) == 0) then
                            print *, '  PASS: Parsed empty array'
                        else
                            print *, '  FAIL: Expected 0 elements, got', size(node%element_indices)
                            test_empty_array_literal = .false.
                        end if
                    else
                        print *, '  PASS: Empty array (no elements allocated)'
                    end if
                class default
                    print *, '  FAIL: Not parsed as array_literal_node'
                    test_empty_array_literal = .false.
                end select
            end if
        else
            print *, '  FAIL: Parse failed'
            test_empty_array_literal = .false.
        end if
        
    end function test_empty_array_literal
    
    logical function test_nested_expressions()
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_nested_expressions = .true.
        print *, 'Testing array with expressions [1+2, 3*4, 5-1]...'
        
        call tokenize_core("[1+2, 3*4, 5-1]", tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index > 0) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                type is (array_literal_node)
                    if (allocated(node%element_indices) .and. size(node%element_indices) == 3) then
                        ! Check if elements are binary operations
                        block
                            integer :: i
                            logical :: all_binary
                            all_binary = .true.
                            
                            do i = 1, 3
                                if (node%element_indices(i) > 0 .and. &
                                    allocated(arena%entries(node%element_indices(i))%node)) then
                                    select type (elem => arena%entries(node%element_indices(i))%node)
                                    type is (binary_op_node)
                                        ! Good
                                    class default
                                        all_binary = .false.
                                    end select
                                end if
                            end do
                            
                            if (all_binary) then
                                print *, '  PASS: All elements are binary operations'
                            else
                                print *, '  FAIL: Not all elements are binary operations'
                                test_nested_expressions = .false.
                            end if
                        end block
                    else
                        print *, '  FAIL: Wrong number of elements'
                        test_nested_expressions = .false.
                    end if
                class default
                    print *, '  FAIL: Not parsed as array_literal_node'
                    test_nested_expressions = .false.
                end select
            end if
        else
            print *, '  FAIL: Parse failed'
            test_nested_expressions = .false.
        end if
        
    end function test_nested_expressions
    
    logical function test_implied_do_simple()
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_implied_do_simple = .true.
        print *, 'Testing simple implied do [(i, i=1,10)]...'
        
        call tokenize_core("[(i, i=1,10)]", tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index > 0) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                type is (array_literal_node)
                    if (allocated(node%element_indices) .and. size(node%element_indices) == 1) then
                        ! Check if element is a do_loop_node
                        if (node%element_indices(1) > 0 .and. &
                            allocated(arena%entries(node%element_indices(1))%node)) then
                            select type (elem => arena%entries(node%element_indices(1))%node)
                            type is (do_loop_node)
                                if (elem%var_name == "i") then
                                    print *, '  PASS: Implied do loop with variable "i"'
                                else
                                    print *, '  FAIL: Wrong loop variable:', elem%var_name
                                    test_implied_do_simple = .false.
                                end if
                            class default
                                print *, '  FAIL: Element is not a do_loop_node'
                                test_implied_do_simple = .false.
                            end select
                        end if
                    else
                        print *, '  FAIL: Wrong number of elements'
                        test_implied_do_simple = .false.
                    end if
                class default
                    print *, '  FAIL: Not parsed as array_literal_node'
                    test_implied_do_simple = .false.
                end select
            end if
        else
            print *, '  FAIL: Parse failed'
            test_implied_do_simple = .false.
        end if
        
    end function test_implied_do_simple
    
    logical function test_implied_do_with_step()
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_implied_do_with_step = .true.
        print *, 'Testing implied do with step [(i, i=1,10,2)]...'
        
        call tokenize_core("[(i, i=1,10,2)]", tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index > 0) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                type is (array_literal_node)
                    if (allocated(node%element_indices) .and. size(node%element_indices) == 1) then
                        if (node%element_indices(1) > 0 .and. &
                            allocated(arena%entries(node%element_indices(1))%node)) then
                            select type (elem => arena%entries(node%element_indices(1))%node)
                            type is (do_loop_node)
                                if (elem%step_expr_index > 0) then
                                    print *, '  PASS: Implied do loop has step expression'
                                else
                                    print *, '  FAIL: No step expression found'
                                    test_implied_do_with_step = .false.
                                end if
                            class default
                                print *, '  FAIL: Element is not a do_loop_node'
                                test_implied_do_with_step = .false.
                            end select
                        end if
                    end if
                class default
                    print *, '  FAIL: Not parsed as array_literal_node'
                    test_implied_do_with_step = .false.
                end select
            end if
        else
            print *, '  FAIL: Parse failed'
            test_implied_do_with_step = .false.
        end if
        
    end function test_implied_do_with_step
    
    logical function test_implied_do_expression()
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_implied_do_expression = .true.
        print *, 'Testing implied do with expression [(i*i, i=1,5)]...'
        
        call tokenize_core("[(i*i, i=1,5)]", tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index > 0) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                type is (array_literal_node)
                    if (allocated(node%element_indices) .and. size(node%element_indices) == 1) then
                        if (node%element_indices(1) > 0 .and. &
                            allocated(arena%entries(node%element_indices(1))%node)) then
                            select type (elem => arena%entries(node%element_indices(1))%node)
                            type is (do_loop_node)
                                if (allocated(elem%body_indices) .and. size(elem%body_indices) > 0) then
                                    ! Check if body is a binary operation
                                    if (elem%body_indices(1) > 0 .and. &
                                        allocated(arena%entries(elem%body_indices(1))%node)) then
                                        select type (body => arena%entries(elem%body_indices(1))%node)
                                        type is (binary_op_node)
                                            if (body%op == "*") then
                                                print *, '  PASS: Body is multiplication'
                                            else
                                                print *, '  FAIL: Wrong operation:', body%op
                                                test_implied_do_expression = .false.
                                            end if
                                        class default
                                            print *, '  FAIL: Body is not a binary operation'
                                            test_implied_do_expression = .false.
                                        end select
                                    end if
                                else
                                    print *, '  FAIL: No body expression'
                                    test_implied_do_expression = .false.
                                end if
                            class default
                                print *, '  FAIL: Element is not a do_loop_node'
                                test_implied_do_expression = .false.
                            end select
                        end if
                    end if
                class default
                    print *, '  FAIL: Not parsed as array_literal_node'
                    test_implied_do_expression = .false.
                end select
            end if
        else
            print *, '  FAIL: Parse failed'
            test_implied_do_expression = .false.
        end if
        
    end function test_implied_do_expression
    
    logical function test_mixed_array_elements()
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_mixed_array_elements = .true.
        print *, 'Testing mixed array [1, "hello", 3.14]...'
        
        call tokenize_core('[1, "hello", 3.14]', tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index > 0) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                type is (array_literal_node)
                    if (allocated(node%element_indices) .and. size(node%element_indices) == 3) then
                        print *, '  PASS: Parsed 3 mixed-type elements'
                        ! Could check individual element types here
                    else
                        print *, '  FAIL: Wrong number of elements'
                        test_mixed_array_elements = .false.
                    end if
                class default
                    print *, '  FAIL: Not parsed as array_literal_node'
                    test_mixed_array_elements = .false.
                end select
            end if
        else
            print *, '  FAIL: Parse failed'
            test_mixed_array_elements = .false.
        end if
        
    end function test_mixed_array_elements
    
end program test_array_constructor_parsing