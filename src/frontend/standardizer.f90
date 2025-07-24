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
    use lexer_core, only: TK_EOF, TK_KEYWORD
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_core, only: parse_function_definition
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use logger, only: log_verbose
    implicit none
    private

    public :: standardize_ast
    public :: standardize_ast_json
    public :: standardize_file

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
        type is (function_def_node)
            ! Wrap standalone function in a program
            call wrap_function_in_program(arena, root_index)
        type is (subroutine_def_node)
            ! Wrap standalone subroutine in a program
            call wrap_subroutine_in_program(arena, root_index)
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

        ! Standardize existing declarations (e.g., real -> real(8))
        call standardize_declarations(arena, prog)

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

        ! Standardize function and subroutine definitions
        call standardize_subprograms(arena, prog)

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

    ! File-based standardization interface
    subroutine standardize_file(input_file, output_file, error_msg)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg
        
        character(len=10240) :: file_content, output_content
        integer :: input_unit, output_unit, ios
        character(len=512) :: line
        logical :: in_function, found_function_start
        character(len=64) :: function_name, var_name, function_return_type
        integer :: equals_pos, paren_pos

        error_msg = ""
        file_content = ""
        output_content = ""
        in_function = .false.
        found_function_start = .false.
        function_name = ""
        function_return_type = ""

        ! Read input file
        open(newunit=input_unit, file=input_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            error_msg = "Cannot open input file: " // trim(input_file)
            return
        end if

        ! First pass: scan for function definitions to determine return types
        do
            read(input_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line = trim(line)
            if (len_trim(line) == 0) cycle
            
            ! Track function return types
            if (index(line, 'function ') > 0) then
                paren_pos = index(line, '(')
                if (paren_pos > 0) then
                    function_name = trim(adjustl(line(index(line, 'function ') + 9:paren_pos-1)))
                    if (index(line, 'real function') > 0) then
                        function_return_type = "real(8)"
                    else if (index(line, 'integer function') > 0) then
                        function_return_type = "integer"
                    end if
                end if
            end if
        end do
        
        ! Rewind and process again
        rewind(input_unit)
        
        ! Process line by line
        do
            read(input_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            line = trim(line)
            if (len_trim(line) == 0) cycle

            ! Check for function definition
            if (index(line, 'function ') > 0) then
                in_function = .true.
                found_function_start = .true.
                ! Extract function name
                paren_pos = index(line, '(')
                if (paren_pos > 0) then
                    function_name = trim(adjustl(line(index(line, 'function ') + 9:paren_pos-1)))
                end if
                
                ! Transform function header and track return type
                if (index(line, 'real function') > 0) then
                    line = 'real(8)' // line(5:)  ! Replace 'real' with 'real(8)'
                    function_return_type = "real(8)"
                else if (index(line, 'integer function') > 0) then
                    function_return_type = "integer"
                    ! Keep integer as is
                end if
                
                ! Add to output: wrap in program with contains
                if (len_trim(output_content) == 0) then
                    output_content = "program main" // new_line('A') // &
                                   "    implicit none" // new_line('A') // new_line('A') // &
                                   "contains" // new_line('A') // &
                                   "    " // trim(line) // new_line('A')
                else
                    output_content = trim(output_content) // "    " // trim(line) // new_line('A')
                end if
            else if (index(line, 'end function') > 0 .or. index(line, 'end') == 1) then
                ! End of function
                if (in_function) then
                    output_content = trim(output_content) // "    " // trim(line) // new_line('A')
                    in_function = .false.
                else
                    output_content = trim(output_content) // trim(line) // new_line('A')
                end if
            else if (in_function) then
                ! Inside function - process parameter declarations and body
                if (index(line, 'integer') > 0 .and. index(line, '::') > 0) then
                    ! Parameter declaration - add intent(in)
                    if (index(line, 'intent') == 0) then
                        ! Insert intent(in) before ::
                        equals_pos = index(line, '::')
                        output_content = trim(output_content) // "        " // &
                                       trim(line(1:equals_pos-1)) // ", intent(in) " // trim(line(equals_pos:)) // new_line('A')
                    else
                        output_content = trim(output_content) // "        " // trim(line) // new_line('A')
                    end if
                else if (index(line, 'real') > 0 .and. index(line, '::') > 0) then
                    ! Real parameter declaration - convert to real(8) and add intent(in)
                    if (index(line, 'intent') == 0) then
                        equals_pos = index(line, '::')
                        output_content = trim(output_content) // "        real(8), intent(in) " // &
                                       trim(line(equals_pos:)) // new_line('A')
                    else
                        ! Replace real with real(8)
                        output_content = trim(output_content) // "        " // &
                                       "real(8)" // line(5:) // new_line('A')
                    end if
                else
                    ! Regular function body
                    output_content = trim(output_content) // "        " // trim(line) // new_line('A')
                end if
            else
                ! Not in function - handle variable assignments and declarations
                if (index(line, '=') > 0 .and. index(line, 'function') == 0) then
                    ! Variable assignment - determine type and add declaration
                    equals_pos = index(line, '=')
                    var_name = trim(adjustl(line(1:equals_pos-1)))
                    
                    ! Look at RHS to determine type based on function return type
                    if (index(line(equals_pos+1:), trim(function_name)) > 0) then
                        ! Assignment from function call - use function return type
                        if (len_trim(output_content) == 0) then
                            output_content = "program main" // new_line('A') // &
                                           "    implicit none" // new_line('A') // &
                                           "    " // trim(function_return_type) // " :: " // trim(var_name) // &
                                           new_line('A') // new_line('A') // &
                                           "    " // trim(line) // new_line('A') // new_line('A')
                        else
                            ! Insert declaration before first executable statement  
                            output_content = "program main" // new_line('A') // &
                                           "    implicit none" // new_line('A') // &
                                           "    " // trim(function_return_type) // " :: " // trim(var_name) // &
                                           new_line('A') // new_line('A') // &
                                           "    " // trim(line) // new_line('A') // new_line('A')
                        end if
                    else
                        output_content = trim(output_content) // "    " // trim(line) // new_line('A')
                    end if
                else
                    output_content = trim(output_content) // trim(line) // new_line('A')
                end if
            end if
        end do
        close(input_unit)

        ! Add end program if we added a program wrapper
        if (found_function_start) then
            output_content = trim(output_content) // "end program main" // new_line('A')
        end if

        ! Write output file
        open(newunit=output_unit, file=output_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            error_msg = "Cannot create output file: " // trim(output_file)
            return
        end if

        write(output_unit, '(A)', iostat=ios) trim(output_content)
        if (ios /= 0) then
            error_msg = "Error writing to output file: " // trim(output_file)
        end if
        close(output_unit)

    end subroutine standardize_file

    ! Get the path to the fortran executable
    function get_fortran_executable_path() result(path)
        character(len=512) :: path
        logical :: exists
        
        ! Try common build paths
        path = './build/gfortran_1612FAD99E3DA210/app/fortran'
        inquire(file=path, exist=exists)
        if (exists) return
        
        ! Try other possible paths
        path = './fortran'
        inquire(file=path, exist=exists)
        if (exists) return
        
        ! Fallback - try to find any fortran executable in build
        block
            integer :: unit, ios, exit_status
            call execute_command_line('find build -name fortran -type f | head -1 > /tmp/fortran_path', exitstat=exit_status)
            if (exit_status == 0) then
                open(newunit=unit, file='/tmp/fortran_path', status='old', action='read', iostat=ios)
                if (ios == 0) then
                    read(unit, '(A)', iostat=ios) path
                    close(unit)
                    if (ios == 0 .and. len_trim(path) > 0) then
                        path = './' // trim(path)
                        return
                    end if
                end if
            end if
        end block
        
        ! Final fallback
        path = 'fortran'
    end function get_fortran_executable_path

    ! Internal parsing function copied from frontend
    subroutine parse_tokens_internal(tokens, arena, prog_index, error_msg)
        use lexer_core, only: token_t
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: prog_index
        character(len=*), intent(out) :: error_msg

        ! Local variables for arena-based parsing
        integer, allocatable :: body_indices(:)
        integer :: stmt_index
        integer :: i, unit_start, unit_end, stmt_count
        type(token_t), allocatable :: unit_tokens(:)
        logical :: has_explicit_program_unit

        error_msg = ""
        stmt_count = 0
        allocate (body_indices(0))
        has_explicit_program_unit = .false.

        ! Check if file starts with explicit 'program', 'module', 'function', or 'subroutine' statement
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_KEYWORD) then
                if (tokens(i)%text == "program" .or. tokens(i)%text == "module") then
                    has_explicit_program_unit = .true.
                    exit  ! Found explicit program unit
                else if (tokens(i)%text == "function" .or. tokens(i)%text == "subroutine") then
                    ! Check if it's a function/subroutine definition (not a call)
                    if (i == 1) then
                        has_explicit_program_unit = .true.
                        exit
                    else if (i > 1 .and. tokens(i - 1)%line < tokens(i)%line) then
                        ! At start of file or start of new line
                        has_explicit_program_unit = .true.
                        exit
                    else if (i > 1 .and. tokens(i - 1)%kind == TK_KEYWORD .and. &
                           (tokens(i - 1)%text == "real" .or. tokens(i - 1)%text == "integer" .or. &
                            tokens(i - 1)%text == "logical" .or. tokens(i - 1)%text == "character")) then
                        ! Type prefixed function/subroutine
                        has_explicit_program_unit = .true.
                        exit
                    end if
                else
                    ! Found other keyword, not a program unit
                    exit
                end if
            else if (tokens(i)%kind /= TK_EOF) then
                exit  ! Stop at first non-EOF token
            end if
        end do

        ! Parse program units, not individual lines
        i = 1
        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) exit

            ! Skip empty lines (just EOF tokens)
            if (i < size(tokens) .and. tokens(i)%kind == TK_EOF) then
                i = i + 1
                cycle
            end if

            ! Find program unit boundary
            call find_program_unit_boundary_internal(tokens, i, unit_start, unit_end, has_explicit_program_unit)

            ! Skip empty units, units with just EOF, or single-token keywords that are part of larger constructs
            if (unit_end >= unit_start .and. &
               .not. (unit_end == unit_start .and. tokens(unit_start)%kind == TK_EOF) .and. &
               .not. (unit_end == unit_start .and. tokens(unit_start)%kind == TK_KEYWORD .and. &
                     (tokens(unit_start)%text == "real" .or. tokens(unit_start)%text == "integer" .or. &
                      tokens(unit_start)%text == "logical" .or. tokens(unit_start)%text == "character" .or. &
                      tokens(unit_start)%text == "function" .or. tokens(unit_start)%text == "subroutine" .or. &
                      tokens(unit_start)%text == "module" .or. tokens(unit_start)%text == "end" .or. &
                      tokens(unit_start)%text == "else" .or. tokens(unit_start)%text == "elseif"))) then
                ! Extract unit tokens and add EOF
                allocate (unit_tokens(unit_end - unit_start + 2))
                unit_tokens(1:unit_end - unit_start + 1) = tokens(unit_start:unit_end)
                ! Add EOF token
                unit_tokens(unit_end - unit_start + 2)%kind = TK_EOF
                unit_tokens(unit_end - unit_start + 2)%text = ""
                unit_tokens(unit_end - unit_start + 2)%line = tokens(unit_end)%line
                unit_tokens(unit_end - unit_start + 2)%column = tokens(unit_end)%column + 1

                ! Parse the program unit
                stmt_index = parse_program_unit_internal(unit_tokens, arena)

                if (stmt_index > 0) then
                    ! Add to body indices
                    body_indices = [body_indices, stmt_index]
                    stmt_count = stmt_count + 1
                end if

                deallocate (unit_tokens)
            end if

            i = unit_end + 1

            ! For lowercase fortran without explicit program units,
            ! parse_all_statements has already processed everything
            if (.not. has_explicit_program_unit .and. unit_end >= size(tokens) - 1) then
                exit  ! We've processed all tokens
            end if
        end do

        ! Create program node with collected body indices
        ! Only wrap in implicit program if there's no explicit program unit (program/module/function/subroutine)
        if (.not. has_explicit_program_unit) then
            ! For lowercase fortran, parse_all_statements already created the program node
            if (stmt_count > 0) then
                prog_index = body_indices(1)  ! This is the program node from parse_all_statements
            else
                error_msg = "No statements found in file"
                prog_index = 0
            end if
        else if (stmt_count > 0) then
            ! Use the first (and should be only) statement as the program unit
            prog_index = body_indices(1)
        else
            ! No program unit found
            error_msg = "No program unit found in file"
            prog_index = 0
        end if
    end subroutine parse_tokens_internal

    ! Find program unit boundary - simplified version
    subroutine find_program_unit_boundary_internal(tokens, start_pos, unit_start, unit_end, has_explicit_program)
        use lexer_core, only: token_t
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: unit_start, unit_end
        logical, intent(in) :: has_explicit_program
        integer :: current_line, i

        unit_start = start_pos
        unit_end = start_pos

        ! For lowercase fortran without explicit program units,
        ! parse the entire remaining file as one unit
        if (.not. has_explicit_program) then
            ! Find end of all tokens (excluding final EOF)
            unit_end = size(tokens)
            do while (unit_end > start_pos .and. tokens(unit_end)%kind == TK_EOF)
                unit_end = unit_end - 1
            end do
        else
            ! Single line construct - find end of current line
            current_line = tokens(start_pos)%line
            i = start_pos
            do while (i <= size(tokens))
                if (tokens(i)%line == current_line) then
                    unit_end = i
                    i = i + 1
                else
                    exit
                end if
            end do
        end if
    end subroutine find_program_unit_boundary_internal

    ! Parse a program unit - simplified version
    function parse_program_unit_internal(tokens, arena) result(unit_index)
        use lexer_core, only: token_t
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: unit_index

        ! For lowercase fortran, we need to parse ALL statements in the token array
        unit_index = parse_all_statements_internal(tokens, arena)
    end function parse_program_unit_internal

    ! Parse all statements in a token array (for lowercase fortran) - simplified version
    function parse_all_statements_internal(tokens, arena) result(prog_index)
        use lexer_core, only: token_t
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: prog_index
        integer, allocatable :: body_indices(:)
        integer :: i, stmt_start, stmt_end, stmt_index
        type(token_t), allocatable :: stmt_tokens(:)

        allocate (body_indices(0))

        ! Parse each statement
        i = 1
        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) then
                exit
            end if

            ! Find statement boundary
            call find_statement_boundary_internal(tokens, i, stmt_start, stmt_end)

            if (stmt_end >= stmt_start) then
                ! Extract statement tokens
                allocate (stmt_tokens(stmt_end - stmt_start + 2))
                stmt_tokens(1:stmt_end - stmt_start + 1) = tokens(stmt_start:stmt_end)
                ! Add EOF token
                stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                stmt_tokens(stmt_end - stmt_start + 2)%text = ""
                stmt_tokens(stmt_end - stmt_start + 2)%line = tokens(stmt_end)%line
                stmt_tokens(stmt_end - stmt_start + 2)%column = tokens(stmt_end)%column + 1

                ! Parse the statement
                stmt_index = parse_statement_dispatcher(stmt_tokens, arena)
                if (stmt_index > 0) then
                    body_indices = [body_indices, stmt_index]
                end if

                deallocate (stmt_tokens)
            end if

            i = stmt_end + 1

            ! Check bounds
            if (i > size(tokens)) then
                exit
            end if
        end do

        ! Create program node with all statements
        prog_index = push_program(arena, "main", body_indices, 1, 1)
    end function parse_all_statements_internal

    ! Find statement boundary - simplified version
    subroutine find_statement_boundary_internal(tokens, start_pos, stmt_start, stmt_end)
        use lexer_core, only: token_t
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: stmt_start, stmt_end
        integer :: i

        stmt_start = start_pos
        stmt_end = start_pos

        ! Single-line statement - find end of line
        i = start_pos
        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) then
                stmt_end = i - 1
                exit
            else if (i < size(tokens) .and. tokens(i)%line < tokens(i + 1)%line) then
                stmt_end = i
                exit
            else
                stmt_end = i
                i = i + 1
            end if
        end do
    end subroutine find_statement_boundary_internal

    ! Helper to write output file
    subroutine write_output_file(filename, content, error_msg)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: content
        character(len=*), intent(out) :: error_msg
        integer :: unit, ios

        error_msg = ""
        open(newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            error_msg = "Unable to open output file: " // filename
            return
        end if

        write(unit, '(A)', iostat=ios) content
        if (ios /= 0) then
            error_msg = "Error writing to output file: " // filename
        end if

        close(unit)
    end subroutine write_output_file

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
        character(len=64), allocatable :: function_names(:)
        integer :: i, var_count, func_count
        type(declaration_node) :: decl_node

        allocate (var_names(100))
        allocate (var_types(100))
        allocate (var_declared(100))
        allocate (function_names(100))
        var_declared = .false.
        var_count = 0
        func_count = 0

        ! First pass: collect function names
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
             if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (function_def_node)
                            if (func_count < size(function_names)) then
                                func_count = func_count + 1
                                function_names(func_count) = stmt%name
                            end if
                        end select
                    end if
                end if
            end do
        end if

        ! Collect all variables that need declarations
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
             if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        call collect_statement_vars(arena, prog%body_indices(i), &
                                        var_names, var_types, var_declared, var_count, &
                                                    function_names, func_count)
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

    ! Collect variables from any statement type
    recursive subroutine collect_statement_vars(arena, stmt_index, var_names, var_types, var_declared, var_count, &
                                                function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        integer :: i

        if (stmt_index <= 0 .or. stmt_index > arena%size) return
        if (.not. allocated(arena%entries(stmt_index)%node)) return

        select type (stmt => arena%entries(stmt_index)%node)
        type is (assignment_node)
            call collect_assignment_vars(arena, stmt_index, var_names, var_types, var_declared, var_count, &
                                         function_names, func_count)
        type is (do_loop_node)
            ! Collect loop variable
            call add_variable(stmt%var_name, "integer", var_names, var_types, var_declared, var_count, &
                              function_names, func_count)
            ! Collect variables from body
            if (allocated(stmt%body_indices)) then
                do i = 1, size(stmt%body_indices)
                    call collect_statement_vars(arena, stmt%body_indices(i), &
                                        var_names, var_types, var_declared, var_count, &
                                                function_names, func_count)
                end do
            end if
        type is (do_while_node)
            ! Collect variables from body
            if (allocated(stmt%body_indices)) then
                do i = 1, size(stmt%body_indices)
                    call collect_statement_vars(arena, stmt%body_indices(i), &
                                        var_names, var_types, var_declared, var_count, &
                                                function_names, func_count)
                end do
            end if
        type is (if_node)
            ! Collect variables from then and else branches
            if (allocated(stmt%then_body_indices)) then
                do i = 1, size(stmt%then_body_indices)
                    call collect_statement_vars(arena, stmt%then_body_indices(i), &
                                        var_names, var_types, var_declared, var_count, &
                                                function_names, func_count)
                end do
            end if
            if (allocated(stmt%else_body_indices)) then
                do i = 1, size(stmt%else_body_indices)
                    call collect_statement_vars(arena, stmt%else_body_indices(i), &
                                        var_names, var_types, var_declared, var_count, &
                                                function_names, func_count)
                end do
            end if
        type is (select_case_node)
            ! TODO: Handle select case when implemented
        end select
    end subroutine collect_statement_vars

    ! Collect variables from assignment node
    subroutine collect_assignment_vars(arena, assign_index, var_names, var_types, var_declared, var_count, &
                                       function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: assign_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count

        if (assign_index <= 0 .or. assign_index > arena%size) return
        if (.not. allocated(arena%entries(assign_index)%node)) return

        select type (assign => arena%entries(assign_index)%node)
        type is (assignment_node)
            ! Get target node
            if (assign%target_index > 0 .and. assign%target_index <= arena%size) then
                if (allocated(arena%entries(assign%target_index)%node)) then
                    select type (target => arena%entries(assign%target_index)%node)
                    type is (identifier_node)
    call collect_identifier_var(target, var_names, var_types, var_declared, var_count, &
                                                    function_names, func_count)
                    end select
                end if
            end if
        end select
    end subroutine collect_assignment_vars

    ! Collect variable from identifier node
    subroutine collect_identifier_var(identifier, var_names, var_types, var_declared, var_count, &
                                      function_names, func_count)
        type(identifier_node), intent(in) :: identifier
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        integer :: i
        logical :: found, is_function

        ! Check if this identifier is a function name
        is_function = .false.
        do i = 1, func_count
            if (trim(function_names(i)) == trim(identifier%name)) then
                is_function = .true.
                exit
            end if
        end do

        ! Skip if it's a function
        if (is_function) return

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

    ! Add a variable to the collection list
    subroutine add_variable(var_name, var_type, var_names, var_types, var_declared, var_count, &
                            function_names, func_count)
        character(len=*), intent(in) :: var_name
        character(len=*), intent(in) :: var_type
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        integer :: i
        logical :: found, is_function

        ! Check if this is a function name
        is_function = .false.
        do i = 1, func_count
            if (trim(function_names(i)) == trim(var_name)) then
                is_function = .true.
                exit
            end if
        end do

        ! Skip if it's a function
        if (is_function) return

        ! Check if variable already exists
        found = .false.
        do i = 1, var_count
            if (trim(var_names(i)) == trim(var_name)) then
                found = .true.
                exit
            end if
        end do

        if (.not. found) then
            var_count = var_count + 1
            if (var_count <= size(var_names)) then
                var_names(var_count) = var_name
                var_types(var_count) = var_type
                var_declared(var_count) = .true.
            end if
        end if
    end subroutine add_variable

    ! Convert mono_type_t to Fortran type string
    function get_fortran_type_string(mono_type) result(type_str)
        type(mono_type_t), intent(in) :: mono_type
        character(len=:), allocatable :: type_str

        select case (mono_type%kind)
        case (TINT)
            type_str = "integer"
        case (TREAL)
            type_str = "real(8)"
        case (TLOGICAL)
            type_str = "logical"
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

    ! Standardize existing declaration nodes (e.g., real -> real(8))
    subroutine standardize_declarations(arena, prog)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer :: i

        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (declaration_node)
                        ! Standardize the type name
                        if (stmt%type_name == "real") then
                            stmt%type_name = "real"
                            stmt%has_kind = .true.
                            stmt%kind_value = 8
                        end if
                        ! Update the node in the arena
                        arena%entries(prog%body_indices(i))%node = stmt
                    end select
                end if
            end if
        end do
    end subroutine standardize_declarations

    ! Standardize function and subroutine definitions
    subroutine standardize_subprograms(arena, prog)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer :: i

        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (function_def_node)
                        call standardize_function_def(arena, stmt, prog%body_indices(i))
                    type is (subroutine_def_node)
                        ! TODO: Implement subroutine standardization
                    end select
                end if
            end if
        end do
    end subroutine standardize_subprograms

    ! Standardize a function definition
    subroutine standardize_function_def(arena, func_def, func_index)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        integer, allocatable :: new_body_indices(:)
        integer :: implicit_none_index, i, j
        type(literal_node) :: implicit_none_node
        character(len=:), allocatable :: return_type_str

        ! Standardize return type
        if (allocated(func_def%return_type)) then
            if (func_def%return_type == "real") then
                func_def%return_type = "real(8)"
            end if
        else
            ! Default to real(8) if no return type specified
            func_def%return_type = "real(8)"
        end if

        ! Add implicit none at the beginning of function body
        if (allocated(func_def%body_indices)) then
            ! Create implicit none node
            implicit_none_node%value = "implicit none"
            implicit_none_node%literal_kind = LITERAL_STRING
            implicit_none_node%line = 1
            implicit_none_node%column = 1
            call arena%push(implicit_none_node, "implicit_none", func_index)
            implicit_none_index = arena%size

            ! Create new body with implicit none at the beginning
            allocate (new_body_indices(size(func_def%body_indices) + 1))
            new_body_indices(1) = implicit_none_index
            do i = 1, size(func_def%body_indices)
                new_body_indices(i + 1) = func_def%body_indices(i)
            end do
            func_def%body_indices = new_body_indices
        end if

        ! Standardize parameter declarations
        call standardize_function_parameters(arena, func_def, func_index)

        ! Update the arena entry
        arena%entries(func_index)%node = func_def
    end subroutine standardize_function_def

    ! Standardize function parameters by updating existing declarations or adding new ones
    subroutine standardize_function_parameters(arena, func_def, func_index)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        type(declaration_node) :: param_decl
        integer, allocatable :: new_body_indices(:)
        integer, allocatable :: param_names_found(:)
        integer :: i, j, n_params, n_body, param_idx
        character(len=64) :: param_name
        character(len=64), allocatable :: param_names(:)
        logical :: is_param_decl, param_updated

        if (.not. allocated(func_def%param_indices)) return
        n_params = size(func_def%param_indices)
        if (n_params == 0) return

        ! Get parameter names
        allocate (param_names(n_params))
        allocate (param_names_found(n_params))
        param_names_found = 0

        do i = 1, n_params
   if (func_def%param_indices(i) > 0 .and. func_def%param_indices(i) <= arena%size) then
                if (allocated(arena%entries(func_def%param_indices(i))%node)) then
                    select type (param => arena%entries(func_def%param_indices(i))%node)
                    type is (identifier_node)
                        param_names(i) = param%name
                    end select
                end if
            end if
        end do

        ! Update existing parameter declarations and track what we find
        if (allocated(func_def%body_indices)) then
            do i = 1, size(func_def%body_indices)
     if (func_def%body_indices(i) > 0 .and. func_def%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(func_def%body_indices(i))%node)) then
                      select type (stmt => arena%entries(func_def%body_indices(i))%node)
                        type is (declaration_node)
                            ! Check if this declaration is for a parameter
                            is_param_decl = .false.
                            param_idx = 0
                            do j = 1, n_params
                                if (stmt%var_name == param_names(j)) then
                                    is_param_decl = .true.
                                    param_idx = j
                                    exit
                                end if
                            end do

                            if (is_param_decl) then
                                ! Update the declaration to have intent(in) and preserve/enhance type
                                if (stmt%type_name == "real") then
                                    stmt%type_name = "real"
                                    stmt%has_kind = .true.
                                    stmt%kind_value = 8
                                ! Keep integer, logical, character as-is
                                end if
                                stmt%intent = "in"
                                stmt%has_intent = .true.
                                param_names_found(param_idx) = func_def%body_indices(i)
                                ! Update in arena
                                arena%entries(func_def%body_indices(i))%node = stmt
                            end if
                        end select
                    end if
                end if
            end do
        end if

        ! Add declarations for parameters not found
        n_body = 0
        if (allocated(func_def%body_indices)) n_body = size(func_def%body_indices)

        ! Count how many new declarations we need
        j = 0
        do i = 1, n_params
            if (param_names_found(i) == 0) j = j + 1
        end do

        if (j > 0) then
            ! We need to add some parameter declarations
            allocate (new_body_indices(n_body + j))

            ! Copy implicit none (should be first) if it exists
            if (n_body > 0) then
                new_body_indices(1) = func_def%body_indices(1)
                j = 2
            else
                j = 1
            end if

            ! Add missing parameter declarations
            do i = 1, n_params
                if (param_names_found(i) == 0) then
                    ! Create declaration node with intent(in)
                    param_decl%type_name = "real"
                    param_decl%var_name = param_names(i)
                    param_decl%has_kind = .true.
                    param_decl%kind_value = 8
                    param_decl%intent = "in"
                    param_decl%has_intent = .true.
                    param_decl%line = 1
                    param_decl%column = 1
                    call arena%push(param_decl, "param_decl", func_index)
                    new_body_indices(j) = arena%size
                    j = j + 1
                end if
            end do

            ! Copy rest of body (skip first if it was implicit none)
            if (n_body > 1) then
                do i = 2, n_body
                    new_body_indices(j) = func_def%body_indices(i)
                    j = j + 1
                end do
            end if

            func_def%body_indices = new_body_indices
        end if
    end subroutine standardize_function_parameters

    ! Wrap a standalone function in a program
    subroutine wrap_function_in_program(arena, func_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: func_index
        type(program_node) :: prog
        type(literal_node) :: implicit_none_node
        type(contains_node) :: contains_stmt
        integer :: prog_index, implicit_none_index, contains_index
        integer, allocatable :: body_indices(:)

        ! Create program node
        prog%name = "main"
        prog%line = 1
        prog%column = 1

        ! Create implicit none
        implicit_none_node%value = "implicit none"
        implicit_none_node%literal_kind = LITERAL_STRING
        implicit_none_node%line = 1
        implicit_none_node%column = 1
        call arena%push(implicit_none_node, "implicit_none", 0)
        implicit_none_index = arena%size

        ! Create contains statement
        contains_stmt%line = 1
        contains_stmt%column = 1
        call arena%push(contains_stmt, "contains", 0)
        contains_index = arena%size

        ! Standardize the function first
        select type (func => arena%entries(func_index)%node)
        type is (function_def_node)
            call standardize_function_def(arena, func, func_index)
        end select

        ! Build program body: implicit none, contains, function
        allocate (body_indices(3))
        body_indices(1) = implicit_none_index
        body_indices(2) = contains_index
        body_indices(3) = func_index
        prog%body_indices = body_indices

        ! Add program to arena
        call arena%push(prog, "program", 0)
        prog_index = arena%size

        ! Update parent references
        arena%entries(implicit_none_index)%parent_index = prog_index
        arena%entries(contains_index)%parent_index = prog_index
        arena%entries(func_index)%parent_index = prog_index

        ! Update root index to point to the program
        func_index = prog_index
    end subroutine wrap_function_in_program

    ! Wrap a standalone subroutine in a program
    subroutine wrap_subroutine_in_program(arena, sub_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: sub_index
        type(program_node) :: prog
        type(literal_node) :: implicit_none_node
        type(contains_node) :: contains_stmt
        integer :: prog_index, implicit_none_index, contains_index
        integer, allocatable :: body_indices(:)

        ! Create program node
        prog%name = "main"
        prog%line = 1
        prog%column = 1

        ! Create implicit none
        implicit_none_node%value = "implicit none"
        implicit_none_node%literal_kind = LITERAL_STRING
        implicit_none_node%line = 1
        implicit_none_node%column = 1
        call arena%push(implicit_none_node, "implicit_none", 0)
        implicit_none_index = arena%size

        ! Create contains statement
        contains_stmt%line = 1
        contains_stmt%column = 1
        call arena%push(contains_stmt, "contains", 0)
        contains_index = arena%size

        ! TODO: Standardize the subroutine if needed

        ! Build program body: implicit none, contains, subroutine
        allocate (body_indices(3))
        body_indices(1) = implicit_none_index
        body_indices(2) = contains_index
        body_indices(3) = sub_index
        prog%body_indices = body_indices

        ! Add program to arena
        call arena%push(prog, "program", 0)
        prog_index = arena%size

        ! Update parent references
        arena%entries(implicit_none_index)%parent_index = prog_index
        arena%entries(contains_index)%parent_index = prog_index
        arena%entries(sub_index)%parent_index = prog_index

        ! Update root index to point to the program
        sub_index = prog_index
    end subroutine wrap_subroutine_in_program

end module standardizer
