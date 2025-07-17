module codegen_core
    use ast_core
    use type_system_hm
    use codegen_declarations
    implicit none
    private

    ! Context for function indentation
    logical :: context_has_executable_before_contains = .false.

    ! Public interface for code generation
public :: generate_code_from_arena, generate_code_polymorphic, generate_code_declaration

contains

    ! Generate code from AST arena
    function generate_code_from_arena(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = ""
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (literal_node)
            code = generate_code_literal(node)
        type is (identifier_node)
            code = generate_code_identifier(node)
        type is (assignment_node)
            code = generate_code_assignment(arena, node, node_index)
        type is (binary_op_node)
            code = generate_code_binary_op(arena, node, node_index)
        type is (program_node)
            code = generate_code_program(arena, node, node_index)
        type is (function_call_node)
            code = generate_code_function_call(arena, node, node_index)
        class default
            code = "! Unknown node type"
        end select
    end function generate_code_from_arena

    ! Generate code for literal node
    function generate_code_literal(node) result(code)
        type(literal_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Return the literal value with proper formatting
        select case (node%literal_kind)
        case (LITERAL_STRING)
            ! String literals need quotes if not already present
            if (len_trim(node%value) == 0) then
                code = ""  ! Skip empty literals (parser placeholders)
            else if (len(node%value) > 0 .and. node%value(1:1) /= '"' .and. &
                     node%value(1:1) /= "'") then
                code = '"'//node%value//'"'
            else
                code = node%value
            end if
        case (LITERAL_REAL)
            ! For real literals, ensure double precision by adding 'd0' suffix if needed
            if (index(node%value, 'd') == 0 .and. index(node%value, 'D') == 0 .and. &
                index(node%value, '_') == 0) then
                code = node%value//"d0"
            else
                code = node%value
            end if
        case default
            ! Handle invalid/empty literals safely
            if (allocated(node%value) .and. len_trim(node%value) > 0) then
                code = node%value
            else
                code = "! Invalid literal node"
            end if
        end select
    end function generate_code_literal

    ! Generate code for identifier node
    function generate_code_identifier(node) result(code)
        type(identifier_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Simply return the identifier name
        code = node%name
    end function generate_code_identifier

    ! Generate code for assignment node
    function generate_code_assignment(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: target_code, value_code

        ! Generate code for target
        if (node%target_index > 0 .and. node%target_index <= arena%size) then
            target_code = generate_code_from_arena(arena, node%target_index)
        else
            target_code = "???"
        end if

        ! Generate code for value
        if (node%value_index > 0 .and. node%value_index <= arena%size) then
            value_code = generate_code_from_arena(arena, node%value_index)
        else
            value_code = "???"
        end if

        ! Combine target and value
        code = target_code//" = "//value_code
    end function generate_code_assignment

    ! Generate code for binary operation node
    function generate_code_binary_op(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: left_code, right_code

        ! Generate code for operands
        if (node%left_index > 0 .and. node%left_index <= arena%size) then
            left_code = generate_code_from_arena(arena, node%left_index)
        else
            left_code = "???"
        end if

        if (node%right_index > 0 .and. node%right_index <= arena%size) then
            right_code = generate_code_from_arena(arena, node%right_index)
        else
            right_code = "???"
        end if

        ! Combine with operator
        code = left_code//" "//node%operator//" "//right_code
    end function generate_code_binary_op

    ! Generate code for program node
    function generate_code_program(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: declarations, body_code
        integer, allocatable :: child_indices(:)
        integer :: i

        ! Generate variable declarations
        declarations = generate_variable_declarations(arena, node_index)

        ! Generate body code
        body_code = ""
        child_indices = arena%get_children(node_index)

        do i = 1, size(child_indices)
            if (len(body_code) > 0) then
                body_code = body_code//new_line('A')
            end if
        body_code = body_code//"    "//generate_code_from_arena(arena, child_indices(i))
        end do

        ! Combine everything
        code = "program "//node%name//new_line('A')
        code = code//"    implicit none"//new_line('A')

        if (len(declarations) > 0) then
            code = code//declarations//new_line('A')
        end if

        if (len(body_code) > 0) then
            code = code//body_code//new_line('A')
        end if

        code = code//"end program "//node%name
    end function generate_code_program

    ! Generate code for function call node
    function generate_code_function_call(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(function_call_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i

        ! Generate arguments
        args_code = ""
        if (allocated(node%arg_indices)) then
            do i = 1, size(node%arg_indices)
                if (len(args_code) > 0) then
                    args_code = args_code//", "
                end if
               if (node%arg_indices(i) > 0 .and. node%arg_indices(i) <= arena%size) then
             args_code = args_code//generate_code_from_arena(arena, node%arg_indices(i))
                else
                    args_code = args_code//"???"
                end if
            end do
        end if

        ! Combine function name and arguments
        code = node%name//"("//args_code//")"
    end function generate_code_function_call

    ! Polymorphic code generation interface
    function generate_code_polymorphic(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = generate_code_from_arena(arena, node_index)
    end function generate_code_polymorphic

    ! Declaration code generation interface
    function generate_code_declaration(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = generate_variable_declarations(arena, node_index)
    end function generate_code_declaration

end module codegen_core
