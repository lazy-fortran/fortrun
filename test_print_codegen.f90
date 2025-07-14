program test_print_codegen
    use ast_core
    use codegen_core
    implicit none
    
    type(print_statement_node) :: print_stmt
    type(identifier_node) :: var
    class(ast_node), allocatable :: args(:)
    character(len=:), allocatable :: code
    
    ! Create print statement with one argument
    var = create_identifier("result", 1, 7)
    allocate(args, source=[var])
    print_stmt = create_print_statement(args, line=1, column=1)
    
    ! Generate code
    code = generate_code(print_stmt)
    
    print *, "Generated code: '", code, "'"
    
end program test_print_codegen