program debug_function_parsing
    use lexer_core, only: tokenize_core, token_t, TK_EOF
    use frontend, only: find_program_unit_boundary, is_function_start, parse_program_unit
    use ast_core, only: ast_node, function_def_node
    implicit none
    
    type(token_t), allocatable :: tokens(:)
    integer :: unit_start, unit_end, i
    class(ast_node), allocatable :: unit
    character(len=*), parameter :: test_code = &
        "real function compute(x)" // new_line('a') // &
        "  real :: x" // new_line('a') // &
        "  compute = x * x" // new_line('a') // &
        "end function"
    
    print *, '=== Function Parsing Debug ==='
    print *, 'Input code:'
    print *, test_code
    print *
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    print *, 'Tokenized', size(tokens), 'tokens'
    
    ! Test is_function_start
    print *, 'is_function_start(tokens, 1) =', is_function_start(tokens, 1)
    
    ! Test program unit boundary detection
    call find_program_unit_boundary(tokens, 1, unit_start, unit_end)
    print *, 'Program unit boundary: start=', unit_start, 'end=', unit_end
    
    ! Test program unit parsing
    if (unit_end >= unit_start) then
        print *, 'Parsing program unit...'
        block
            type(token_t), allocatable :: unit_tokens(:)
            allocate(unit_tokens(unit_end - unit_start + 2))
            unit_tokens(1:unit_end - unit_start + 1) = tokens(unit_start:unit_end)
            ! Add EOF
            unit_tokens(unit_end - unit_start + 2)%kind = TK_EOF
            unit_tokens(unit_end - unit_start + 2)%text = ""
            
            unit = parse_program_unit(unit_tokens)
            
            if (allocated(unit)) then
                print *, 'Successfully parsed program unit'
                select type(unit)
                type is (function_def_node)
                    print *, 'Parsed as function_def_node with name:', unit%name
                class default
                    print *, 'Parsed as different node type'
                end select
            else
                print *, 'Failed to parse program unit'
            end if
        end block
    else
        print *, 'Failed to detect program unit boundary'
    end if
    
end program debug_function_parsing