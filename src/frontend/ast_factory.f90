module ast_factory
    use ast_core
    implicit none
    private

    ! Public interface for creating AST nodes in stack-based system
public :: create_program_in_stack, create_assignment_in_stack, create_binary_op_in_stack
    public :: create_function_call_in_stack, create_identifier_in_stack, create_literal_in_stack
    public :: build_ast_from_nodes

contains

    ! Create program node and add to stack
    function create_program_in_stack(stack, name, body_indices, line, column) result(prog_index)
        type(ast_stack_t), intent(inout) :: stack
        character(len=*), intent(in) :: name
        integer, intent(in) :: body_indices(:)
        integer, intent(in), optional :: line, column
        integer :: prog_index
        type(program_node) :: prog

        prog = create_program(name, body_indices, line, column)
        call stack%push(prog, "program")
        prog_index = stack%size
    end function create_program_in_stack

    ! Create assignment node and add to stack
    function create_assignment_in_stack(stack, target_index, value_index, line, column, parent_index) result(assign_index)
        type(ast_stack_t), intent(inout) :: stack
        integer, intent(in) :: target_index, value_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: assign_index
        type(assignment_node) :: assign

        assign = create_assignment(target_index, value_index, line, column)
        call stack%push(assign, "assignment", parent_index)
        assign_index = stack%size
    end function create_assignment_in_stack

    ! Create binary operation node and add to stack
    function create_binary_op_in_stack(stack, left_index, right_index, operator, line, column, parent_index) result(binop_index)
        type(ast_stack_t), intent(inout) :: stack
        integer, intent(in) :: left_index, right_index
        character(len=*), intent(in) :: operator
        integer, intent(in), optional :: line, column, parent_index
        integer :: binop_index
        type(binary_op_node) :: binop

        binop = create_binary_op(left_index, right_index, operator, line, column)
        call stack%push(binop, "binary_op", parent_index)
        binop_index = stack%size
    end function create_binary_op_in_stack

    ! Create function call node and add to stack
    function create_function_call_in_stack(stack, name, arg_indices, line, column, parent_index) result(call_index)
        type(ast_stack_t), intent(inout) :: stack
        character(len=*), intent(in) :: name
        integer, intent(in) :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: call_index
        type(function_call_node) :: call_node

        call_node = create_function_call(name, arg_indices, line, column)
        call stack%push(call_node, "function_call", parent_index)
        call_index = stack%size
    end function create_function_call_in_stack

    ! Create identifier node and add to stack
    function create_identifier_in_stack(stack, name, line, column, parent_index) result(id_index)
        type(ast_stack_t), intent(inout) :: stack
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column, parent_index
        integer :: id_index
        type(identifier_node) :: id

        id = create_identifier(name, line, column)
        call stack%push(id, "identifier", parent_index)
        id_index = stack%size
    end function create_identifier_in_stack

    ! Create literal node and add to stack
    function create_literal_in_stack(stack, value, kind, line, column, parent_index) result(lit_index)
        type(ast_stack_t), intent(inout) :: stack
        character(len=*), intent(in) :: value
        integer, intent(in) :: kind
        integer, intent(in), optional :: line, column, parent_index
        integer :: lit_index
        type(literal_node) :: lit

        lit = create_literal(value, kind, line, column)
        call stack%push(lit, "literal", parent_index)
        lit_index = stack%size
    end function create_literal_in_stack

    ! Build AST from individual nodes (helper function)
    subroutine build_ast_from_nodes(stack, node_specs, indices)
        type(ast_stack_t), intent(inout) :: stack
        character(len=*), intent(in) :: node_specs(:)  ! Array of "type:name" specs
        integer, intent(out) :: indices(:)  ! Output indices
        integer :: i

        do i = 1, size(node_specs)
            block
                character(len=:), allocatable :: spec
                integer :: colon_pos
                character(len=:), allocatable :: node_type, node_name

                spec = trim(node_specs(i))
                colon_pos = index(spec, ':')

                if (colon_pos > 0) then
                    node_type = spec(1:colon_pos - 1)
                    node_name = spec(colon_pos + 1:)

                    select case (trim(node_type))
                    case ('identifier')
                        indices(i) = create_identifier_in_stack(stack, node_name, i, 1)
                    case ('literal_int')
           indices(i) = create_literal_in_stack(stack, node_name, LITERAL_INTEGER, i, 1)
                    case ('literal_real')
              indices(i) = create_literal_in_stack(stack, node_name, LITERAL_REAL, i, 1)
                    case ('literal_string')
            indices(i) = create_literal_in_stack(stack, node_name, LITERAL_STRING, i, 1)
                    case default
                        indices(i) = create_identifier_in_stack(stack, node_name, i, 1)
                    end select
                else
                    indices(i) = create_identifier_in_stack(stack, spec, i, 1)
                end if
            end block
        end do
    end subroutine build_ast_from_nodes

end module ast_factory
