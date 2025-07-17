program test_minimal_ast_inferred_type
    use ast_core
    use type_system_hm
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_create_identifier_with_inferred_type()
    call test_create_literal_with_inferred_type()
    call test_assignment_with_inferred_type()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_create_identifier_with_inferred_type()
        type(identifier_node) :: ident
        type(mono_type_t) :: int_type

        test_count = test_count + 1

        ! Create identifier
        ident%name = "x"
        ident%line = 1
        ident%column = 1

        ! Create and assign inferred type
        int_type = create_mono_type(TINT)
        allocate (ident%inferred_type)
        ident%inferred_type = int_type

        ! Check if inferred type is properly set
        if (allocated(ident%inferred_type)) then
            if (ident%inferred_type%kind == TINT) then
                pass_count = pass_count + 1
                write (*, '(A)') "PASS: Identifier with inferred type"
            else
                write (*, '(A)') "FAIL: Identifier inferred type wrong kind"
            end if
        else
            write (*, '(A)') "FAIL: Identifier inferred type not allocated"
        end if
    end subroutine test_create_identifier_with_inferred_type

    subroutine test_create_literal_with_inferred_type()
        type(literal_node) :: lit
        type(mono_type_t) :: real_type

        test_count = test_count + 1

        ! Create literal
        lit%value = "3.14"
        lit%literal_kind = LITERAL_REAL
        lit%line = 1
        lit%column = 1

        ! Create and assign inferred type
        real_type = create_mono_type(TREAL)
        allocate (lit%inferred_type)
        lit%inferred_type = real_type

        ! Check if inferred type is properly set
        if (allocated(lit%inferred_type)) then
            if (lit%inferred_type%kind == TREAL) then
                pass_count = pass_count + 1
                write (*, '(A)') "PASS: Literal with inferred type"
            else
                write (*, '(A)') "FAIL: Literal inferred type wrong kind"
            end if
        else
            write (*, '(A)') "FAIL: Literal inferred type not allocated"
        end if
    end subroutine test_create_literal_with_inferred_type

    subroutine test_assignment_with_inferred_type()
        type(assignment_node) :: assign
        type(identifier_node) :: target
        type(literal_node) :: value
        type(mono_type_t) :: real_type

        test_count = test_count + 1

        ! Create target identifier
        target%name = "y"
        target%line = 1
        target%column = 1

        ! Create value literal
        value%value = "2.71"
        value%literal_kind = LITERAL_REAL
        value%line = 1
        value%column = 5

        ! Create assignment
        assign%line = 1
        assign%column = 1
        allocate (assign%target, source=target)
        allocate (assign%value, source=value)

        ! Create and assign inferred type to assignment
        real_type = create_mono_type(TREAL)
        allocate (assign%inferred_type)
        assign%inferred_type = real_type

        ! Check if inferred type is properly set
        if (allocated(assign%inferred_type)) then
            if (assign%inferred_type%kind == TREAL) then
                pass_count = pass_count + 1
                write (*, '(A)') "PASS: Assignment with inferred type"
            else
                write (*, '(A)') "FAIL: Assignment inferred type wrong kind"
            end if
        else
            write (*, '(A)') "FAIL: Assignment inferred type not allocated"
        end if
    end subroutine test_assignment_with_inferred_type

end program test_minimal_ast_inferred_type
