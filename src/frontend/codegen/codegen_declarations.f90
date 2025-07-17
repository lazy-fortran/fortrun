module codegen_declarations
    use ast_core
    use type_system_hm
    implicit none
    private

    public :: generate_variable_declarations

contains

    ! Generate variable declarations from AST with inferred types
    function generate_variable_declarations(prog) result(declarations)
        class(program_node), intent(in) :: prog
        character(len=:), allocatable :: declarations
        character(len=64), allocatable :: var_names(:)
        character(len=64), allocatable :: var_types(:)
        logical, allocatable :: var_declared(:)
        integer :: i, j, var_count

        declarations = ""
        allocate (var_names(100))
        allocate (var_types(100))
        allocate (var_declared(100))
        var_declared = .false.
        var_count = 0

        ! Safety check
        if (.not. allocated(prog%body)) then
            return
        end if

        ! Collect all variables that need declarations
        do i = 1, size(prog%body)
            if (.not. allocated(prog%body(i)%node)) cycle

            select type (stmt => prog%body(i)%node)
            type is (assignment_node)
       call collect_assignment_vars(stmt, var_names, var_types, var_declared, var_count)
            type is (do_loop_node)
          call collect_do_loop_vars(stmt, var_names, var_types, var_declared, var_count)
            type is (do_while_node)
                if (allocated(stmt%body)) then
        call collect_body_vars(stmt%body, var_names, var_types, var_declared, var_count)
                end if
            type is (if_node)
                if (allocated(stmt%then_body)) then
   call collect_body_vars(stmt%then_body, var_names, var_types, var_declared, var_count)
                end if
                if (allocated(stmt%else_body)) then
   call collect_body_vars(stmt%else_body, var_names, var_types, var_declared, var_count)
                end if
            end select
        end do

        ! Generate declaration statements
        do i = 1, var_count
            if (var_declared(i)) then
                declarations = declarations//"    "//trim(var_types(i))//" :: "// &
                               trim(var_names(i))//new_line('a')
            end if
        end do

    end function generate_variable_declarations

    ! Collect variables from assignment statements
    subroutine collect_assignment_vars(assign, var_names, var_types, var_declared, var_count)
        type(assignment_node), intent(in) :: assign
        character(len=64), intent(inout) :: var_names(:), var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=:), allocatable :: var_name, var_type
        integer :: existing_idx

        ! Get variable name from target
        select type (target => assign%target)
        type is (identifier_node)
            var_name = target%name

            ! Check if variable already tracked
            existing_idx = find_variable(var_name, var_names, var_count)

            if (existing_idx == 0) then
                ! New variable - get type from inferred_type
                if (allocated(target%inferred_type)) then
                    var_type = mono_type_to_fortran(target%inferred_type)
                else
                    ! Fallback - should not happen if semantic analysis was run
                    var_type = "real(8)"
                end if

                var_count = var_count + 1
                var_names(var_count) = var_name
                var_types(var_count) = var_type
                var_declared(var_count) = .true.
            end if
        end select
    end subroutine collect_assignment_vars

    ! Collect variables from do loop
    subroutine collect_do_loop_vars(loop, var_names, var_types, var_declared, var_count)
        type(do_loop_node), intent(in) :: loop
        character(len=64), intent(inout) :: var_names(:), var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        integer :: existing_idx

        ! Check if loop variable already declared
        existing_idx = find_variable(loop%var_name, var_names, var_count)

        if (existing_idx == 0) then
            var_count = var_count + 1
            var_names(var_count) = loop%var_name
            var_types(var_count) = "integer"  ! Loop variables are always integer
            var_declared(var_count) = .true.
        end if

        ! Also process loop body
        if (allocated(loop%body)) then
        call collect_body_vars(loop%body, var_names, var_types, var_declared, var_count)
        end if
    end subroutine collect_do_loop_vars

    ! Collect variables from a body (array of statements)
    recursive subroutine collect_body_vars(body, var_names, var_types, var_declared, var_count)
        type(ast_node_wrapper), intent(in) :: body(:)
        character(len=64), intent(inout) :: var_names(:), var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        integer :: i

        do i = 1, size(body)
            if (.not. allocated(body(i)%node)) cycle

            select type (stmt => body(i)%node)
            type is (assignment_node)
       call collect_assignment_vars(stmt, var_names, var_types, var_declared, var_count)
            type is (do_loop_node)
          call collect_do_loop_vars(stmt, var_names, var_types, var_declared, var_count)
            type is (do_while_node)
                if (allocated(stmt%body)) then
        call collect_body_vars(stmt%body, var_names, var_types, var_declared, var_count)
                end if
            type is (if_node)
                if (allocated(stmt%then_body)) then
   call collect_body_vars(stmt%then_body, var_names, var_types, var_declared, var_count)
                end if
                if (allocated(stmt%else_body)) then
   call collect_body_vars(stmt%else_body, var_names, var_types, var_declared, var_count)
                end if
            end select
        end do
    end subroutine collect_body_vars

    ! Find variable in list
    function find_variable(name, var_names, var_count) result(idx)
        character(len=*), intent(in) :: name
        character(len=64), intent(in) :: var_names(:)
        integer, intent(in) :: var_count
        integer :: idx, i

        idx = 0
        do i = 1, var_count
            if (trim(var_names(i)) == trim(name)) then
                idx = i
                exit
            end if
        end do
    end function find_variable

    ! Convert mono_type_t to Fortran type declaration
    recursive function mono_type_to_fortran(mono_type) result(fortran_type)
        type(mono_type_t), intent(in) :: mono_type
        character(len=:), allocatable :: fortran_type

        select case (mono_type%kind)
        case (TINT)
            fortran_type = "integer"
        case (TREAL)
            fortran_type = "real(8)"
        case (TCHAR)
            if (mono_type%size > 0) then
                block
                    character(len=10) :: size_str
                    write (size_str, '(I0)') mono_type%size
                    fortran_type = "character(len="//trim(adjustl(size_str))//")"
                end block
            else
                fortran_type = "character(len=*)"
            end if
        case (TARRAY)
            ! For arrays, we just need the element type here
            ! Array dimensions are handled separately
            if (allocated(mono_type%args) .and. size(mono_type%args) > 0) then
                fortran_type = mono_type_to_fortran(mono_type%args(1))
            else
                fortran_type = "real(8)"  ! Fallback
            end if
        case default
            fortran_type = "real(8)"  ! Fallback
        end select
    end function mono_type_to_fortran

end module codegen_declarations
