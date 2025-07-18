module simple_json
    implicit none
    private

    public :: simple_json_write_tokens_to_file
    public :: simple_json_write_tokens_to_string
    public :: simple_json_write_ast_to_file
    public :: simple_json_write_ast_to_string

contains

    ! Write tokens array to JSON file
    subroutine simple_json_write_tokens_to_file(tokens, filename)
        use lexer_core, only: token_t, token_type_name
        type(token_t), intent(in) :: tokens(:)
        character(len=*), intent(in) :: filename

        integer :: unit, i

        open (newunit=unit, file=filename, status='replace', action='write')

        write (unit, '(A)') '{'
        write (unit, '(A)') '  "tokens": ['

        do i = 1, size(tokens)
            write (unit, '(A)', advance='no') '    {'
            write(unit, '(A)', advance='no') '"type": "' // trim(token_type_name(tokens(i)%kind)) // '",'
            write (unit, '(A)', advance='no') '"text": "'//trim(tokens(i)%text)//'",'
            write(unit, '(A)', advance='no') '"line": ' // trim(adjustl(int_to_string(tokens(i)%line))) // ','
            write(unit, '(A)', advance='no') '"column": ' // trim(adjustl(int_to_string(tokens(i)%column)))

            if (i < size(tokens)) then
                write (unit, '(A)') '},'
            else
                write (unit, '(A)') '}'
            end if
        end do

        write (unit, '(A)') '  ]'
        write (unit, '(A)') '}'

        close (unit)
    end subroutine simple_json_write_tokens_to_file

    ! Convert tokens array to JSON string
    function simple_json_write_tokens_to_string(tokens) result(json_str)
        use lexer_core, only: token_t, token_type_name
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: json_str

        character(len=32) :: temp_filename
        integer :: unit, file_size, i

        ! Create temporary file
        write (temp_filename, '(A,I0,A)') 'temp_json_', int(rand()*100000), '.json'

        call simple_json_write_tokens_to_file(tokens, temp_filename)

        ! Read back as string
        open (newunit=unit, file=temp_filename, status='old', action='read')
        inquire (unit=unit, size=file_size)
        allocate (character(len=file_size) :: json_str)
        read (unit, '(A)') json_str
        close (unit, status='delete')

    end function simple_json_write_tokens_to_string

    ! Write AST to JSON file (simplified - just writes placeholder)
    subroutine simple_json_write_ast_to_file(ast, filename)
        use ast_core, only: ast_node
        class(ast_node), intent(in) :: ast
        character(len=*), intent(in) :: filename

        integer :: unit

        open (newunit=unit, file=filename, status='replace', action='write')
        write(unit, '(A)') '{"type": "ast", "note": "AST serialization not implemented in simple_json"}'
        close (unit)
    end subroutine simple_json_write_ast_to_file

    ! Convert AST to JSON string (simplified - just returns placeholder)
    function simple_json_write_ast_to_string(ast) result(json_str)
        use ast_core, only: ast_node
        class(ast_node), intent(in) :: ast
        character(len=:), allocatable :: json_str

json_str = '{"type": "ast", "note": "AST serialization not implemented in simple_json"}'
    end function simple_json_write_ast_to_string

    ! Helper function to convert integer to string
    function int_to_string(int_val) result(str)
        integer, intent(in) :: int_val
        character(len=32) :: str

        write (str, '(I0)') int_val
    end function int_to_string

end module simple_json
