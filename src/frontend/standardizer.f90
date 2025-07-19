module standardizer
    ! AST Standardization Stage - transforms lowercase fortran AST to standard Fortran AST
    ! This stage:
    ! 1. Decides program vs module based on content
    ! 2. Inserts 'contains' statements where needed
    ! 3. Handles implicit program wrapping
    ! 4. Transforms dialect-specific constructs to standard Fortran
    ! 5. Generates variable declarations from inferred types

    use ast_core
    use ast_factory
    use type_system_hm
    use json_module, only: json_core, json_value, json_file
    implicit none
    private

    public :: standardize_ast
    public :: standardize_ast_json

contains

    ! Main standardization entry point
    subroutine standardize_ast(arena, root_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        select type (node => arena%entries(root_index)%node)
        type is (program_node)
            call standardize_program(arena, node, root_index)
        class default
            ! For other node types, no standardization needed yet
        end select

    end subroutine standardize_ast

    ! Standardize a program node
    subroutine standardize_program(arena, prog, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index
        logical :: has_functions, has_subroutines, has_use_statements
        logical :: has_executable_statements
        logical :: should_be_module
        integer :: contains_index
        integer, allocatable :: new_body_indices(:)
        integer :: i, n_statements, insert_pos

        ! Analyze the program to determine if it should be a module
        call analyze_program_content(arena, prog, has_functions, has_subroutines, &
                        has_use_statements, has_executable_statements, should_be_module)

        if (should_be_module) then
            ! TODO: Transform to module_node
            ! For now, just handle contains insertion
        end if

        ! Always insert implicit none and variable declarations for programs
        call insert_variable_declarations(arena, prog, prog_index)

        ! Check if we need to insert a contains statement
        if (has_functions .or. has_subroutines) then
            ! Find where to insert contains (before first function/subroutine)
            insert_pos = find_contains_insertion_point(arena, prog)

            if (insert_pos > 0) then
                ! Create new body with contains statement
                call insert_contains_statement(arena, prog, prog_index, insert_pos)
            end if
        end if

    end subroutine standardize_program

    ! Analyze program content to determine its nature
    subroutine analyze_program_content(arena, prog, has_functions, has_subroutines, &
                        has_use_statements, has_executable_statements, should_be_module)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        logical, intent(out) :: has_functions, has_subroutines, has_use_statements
        logical, intent(out) :: has_executable_statements, should_be_module
        integer :: i

        has_functions = .false.
        has_subroutines = .false.
        has_use_statements = .false.
        has_executable_statements = .false.
        should_be_module = .false.

        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (function_def_node)
                        has_functions = .true.
                    type is (subroutine_def_node)
                        has_subroutines = .true.
                    type is (use_statement_node)
                        has_use_statements = .true.
                    type is (assignment_node)
                        has_executable_statements = .true.
                    type is (call_or_subscript_node)
                        has_executable_statements = .true.
                    type is (subroutine_call_node)
                        has_executable_statements = .true.
                    type is (print_statement_node)
                        has_executable_statements = .true.
                    type is (if_node)
                        has_executable_statements = .true.
                    type is (do_loop_node)
                        has_executable_statements = .true.
                    type is (do_while_node)
                        has_executable_statements = .true.
                    type is (select_case_node)
                        has_executable_statements = .true.
                    end select
                end if
            end if
        end do

        ! Decision logic: if only functions/subroutines and use statements, it's likely a module
        ! For now, keep it simple - presence of multiple procedures suggests module
        should_be_module = (has_functions .or. has_subroutines) .and. &
                   (count([has_functions, has_subroutines]) > 1 .or. has_use_statements)

    end subroutine analyze_program_content

    ! Find where to insert the contains statement
    function find_contains_insertion_point(arena, prog) result(pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: pos
        integer :: i

        pos = 0
        if (.not. allocated(prog%body_indices)) return

        ! Find the first function or subroutine
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (function_def_node)
                        pos = i
                        return
                    type is (subroutine_def_node)
                        pos = i
                        return
                    end select
                end if
            end if
        end do

    end function find_contains_insertion_point

    ! Insert a contains statement at the specified position
    subroutine insert_contains_statement(arena, prog, prog_index, insert_pos)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index, insert_pos
        integer, allocatable :: new_body_indices(:)
        integer :: contains_index, i, j
        type(contains_node) :: contains_stmt

        if (.not. allocated(prog%body_indices)) return

        ! Create contains node
        contains_stmt%line = 1  ! Line number will be adjusted later
        contains_stmt%column = 1

        ! Add contains node to arena
        call arena%push(contains_stmt, "contains", prog_index)
        contains_index = arena%size

        ! Create new body indices array with contains inserted
        allocate (new_body_indices(size(prog%body_indices) + 1))

        ! Copy statements before the insertion point
        j = 1
        do i = 1, insert_pos - 1
            new_body_indices(j) = prog%body_indices(i)
            j = j + 1
        end do

        ! Insert contains
        new_body_indices(j) = contains_index
        j = j + 1

        ! Copy remaining statements
        do i = insert_pos, size(prog%body_indices)
            new_body_indices(j) = prog%body_indices(i)
            j = j + 1
        end do

        ! Replace body indices
        prog%body_indices = new_body_indices

        ! Update the arena entry
        arena%entries(prog_index)%node = prog

    end subroutine insert_contains_statement

    ! JSON interface for standardization
    subroutine standardize_ast_json(json_file_path, output_file, error_msg)
        character(len=*), intent(in) :: json_file_path
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg
        ! TODO: Implement JSON standardization
        ! type(json_file) :: json
        ! type(json_value), pointer :: root
        ! type(ast_arena_t) :: arena
        ! integer :: root_index
        ! character(len=:), allocatable :: output_json

        error_msg = "JSON to arena conversion not yet implemented for standardizer"
        return

    end subroutine standardize_ast_json

    ! Insert variable declarations and implicit none for a program
    subroutine insert_variable_declarations(arena, prog, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index
        integer, allocatable :: new_body_indices(:)
        integer :: implicit_none_index
        integer, allocatable :: declaration_indices(:)
        integer :: i, j, insert_pos, n_declarations
        type(literal_node) :: implicit_none_node

        if (.not. allocated(prog%body_indices)) return

        ! Find insertion point (after use statements, before executable statements)
        insert_pos = find_declaration_insertion_point(arena, prog)
        if (insert_pos == 0) insert_pos = 1  ! Default to beginning if no use statements

        ! Create implicit none node
        implicit_none_node%value = "implicit none"
        implicit_none_node%literal_kind = LITERAL_STRING
        implicit_none_node%line = 1
        implicit_none_node%column = 1
        call arena%push(implicit_none_node, "implicit_none", prog_index)
        implicit_none_index = arena%size

        ! Collect and generate variable declarations
     call generate_and_insert_declarations(arena, prog, prog_index, declaration_indices)
        n_declarations = 0
        if (allocated(declaration_indices)) n_declarations = size(declaration_indices)

        ! Create new body indices with implicit none and declarations
        allocate (new_body_indices(size(prog%body_indices) + 1 + n_declarations))

        ! Copy use statements
        j = 1
        do i = 1, insert_pos - 1
            new_body_indices(j) = prog%body_indices(i)
            j = j + 1
        end do

        ! Insert implicit none
        new_body_indices(j) = implicit_none_index
        j = j + 1

        ! Insert declarations
        do i = 1, n_declarations
            new_body_indices(j) = declaration_indices(i)
            j = j + 1
        end do

        ! Copy remaining statements
        do i = insert_pos, size(prog%body_indices)
            new_body_indices(j) = prog%body_indices(i)
            j = j + 1
        end do

        ! Update program body
        prog%body_indices = new_body_indices

        ! Update the arena entry
        arena%entries(prog_index)%node = prog

    end subroutine insert_variable_declarations

    ! Find where to insert declarations (after use statements)
    function find_declaration_insertion_point(arena, prog) result(pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: pos
        integer :: i

        pos = 1  ! Default to beginning
        if (.not. allocated(prog%body_indices)) return

        ! Find the last use statement
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (use_statement_node)
                        pos = i + 1  ! Insert after this use statement
                    class default
                        ! First non-use statement, stop looking
                        exit
                    end select
                end if
            end if
        end do

    end function find_declaration_insertion_point

    ! Generate and insert variable declarations from inferred types
    subroutine generate_and_insert_declarations(arena, prog, prog_index, declaration_indices)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer, intent(in) :: prog_index
        integer, allocatable, intent(out) :: declaration_indices(:)
        character(len=64), allocatable :: var_names(:)
        character(len=64), allocatable :: var_types(:)
        logical, allocatable :: var_declared(:)
        integer :: i, var_count
        type(declaration_node) :: decl_node

        allocate (var_names(100))
        allocate (var_types(100))
        allocate (var_declared(100))
        var_declared = .false.
        var_count = 0

        ! Collect all variables that need declarations
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
             if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (assignment_node)
                            call collect_assignment_vars(arena, prog%body_indices(i), &
                                          var_names, var_types, var_declared, var_count)
                        end select
                    end if
                end if
            end do
        end if

        ! Create declaration nodes
        if (var_count > 0) then
            allocate (declaration_indices(var_count))
            do i = 1, var_count
                if (var_declared(i)) then
                    ! Create declaration node
                    decl_node%type_name = trim(var_types(i))
                    decl_node%var_name = trim(var_names(i))
                    decl_node%is_array = .false.
                    decl_node%has_kind = .false.
                    decl_node%initializer_index = 0
                    decl_node%line = 1
                    decl_node%column = 1

                    call arena%push(decl_node, "declaration", prog_index)
                    declaration_indices(i) = arena%size
                end if
            end do
        else
            allocate (declaration_indices(0))
        end if

    end subroutine generate_and_insert_declarations

    ! Collect variables from assignment node
    subroutine collect_assignment_vars(arena, assign_index, var_names, var_types, var_declared, var_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: assign_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count

        if (assign_index <= 0 .or. assign_index > arena%size) return
        if (.not. allocated(arena%entries(assign_index)%node)) return

        select type (assign => arena%entries(assign_index)%node)
        type is (assignment_node)
            ! Get target node
            if (assign%target_index > 0 .and. assign%target_index <= arena%size) then
                if (allocated(arena%entries(assign%target_index)%node)) then
                    select type (target => arena%entries(assign%target_index)%node)
                    type is (identifier_node)
      call collect_identifier_var(target, var_names, var_types, var_declared, var_count)
                    end select
                end if
            end if
        end select
    end subroutine collect_assignment_vars

    ! Collect variable from identifier node
    subroutine collect_identifier_var(identifier, var_names, var_types, var_declared, var_count)
        type(identifier_node), intent(in) :: identifier
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        integer :: i
        logical :: found

        ! Check if variable already exists
        found = .false.
        do i = 1, var_count
            if (trim(var_names(i)) == trim(identifier%name)) then
                found = .true.
                exit
            end if
        end do

        if (.not. found) then
            var_count = var_count + 1
            if (var_count <= size(var_names)) then
                var_names(var_count) = identifier%name

                ! Determine type from inferred_type if available
                if (allocated(identifier%inferred_type)) then
                var_types(var_count) = get_fortran_type_string(identifier%inferred_type)
                else
                    var_types(var_count) = "real(8)"  ! Default type
                end if

                var_declared(var_count) = .true.
            end if
        end if
    end subroutine collect_identifier_var

    ! Convert mono_type_t to Fortran type string
    function get_fortran_type_string(mono_type) result(type_str)
        type(mono_type_t), intent(in) :: mono_type
        character(len=:), allocatable :: type_str

        select case (mono_type%kind)
        case (TINT)
            type_str = "integer"
        case (TREAL)
            type_str = "real(8)"
        case (TCHAR)
            if (mono_type%size > 0) then
                block
                    character(len=20) :: size_str
                    write (size_str, '(i0)') mono_type%size
                    type_str = "character(len="//trim(size_str)//")"
                end block
            else
                type_str = "character(*)"
            end if
        case default
            type_str = "real(8)"  ! Default fallback
        end select
    end function get_fortran_type_string

end module standardizer
