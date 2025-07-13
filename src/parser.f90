module parser
    ! Main parser interface that delegates to appropriate dialect implementation
    use parser_core
    use ast
    implicit none
    private

    ! Re-export core parsing functionality
    public :: parse_expression, parse_statement
    public :: parser_state, create_parser_state

contains

    ! Note: For now, we just use core parsing
    ! In the future, this will delegate to dialect-specific parsers
    ! based on the input or configuration

end module parser