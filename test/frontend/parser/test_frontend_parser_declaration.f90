program test_frontend_parser_declaration
    use lexer_core, only: tokenize_core, token_t
    use parser_core, only: parse_statement
    use ast_core, only: ast_node, literal_node, LITERAL_STRING
    implicit none
    
    type(token_t), allocatable :: tokens(:)
    class(ast_node), allocatable :: ast
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    ! Test 1: Real declaration "real :: x"
    call test_real_declaration()
    
    if (all_tests_passed) then
        print *, "All parser declaration tests passed!"
        stop 0
    else
        print *, "Some parser declaration tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_real_declaration()
        character(len=*), parameter :: input = "real :: x"
        
        print *, "Testing: '", input, "'"
        
        ! First tokenize
        call tokenize_core(input, tokens)
        
        ! Then parse
        ast = parse_statement(tokens)
        
        ! Check what we got
        if (.not. allocated(ast)) then
            print *, "FAIL: No AST node created"
            all_tests_passed = .false.
            return
        end if
        
        ! For now, declarations are skipped and return empty literal
        select type (ast)
        type is (literal_node)
            if (ast%value == "" .and. ast%literal_kind == LITERAL_STRING) then
                print *, "PASS: Declaration skipped as expected (returns empty literal)"
            else
                print *, "FAIL: Expected empty literal, got '", ast%value, "'"
                all_tests_passed = .false.
            end if
        class default
            print *, "FAIL: Expected literal node for skipped declaration"
            all_tests_passed = .false.
        end select
        
    end subroutine test_real_declaration
    
end program test_frontend_parser_declaration