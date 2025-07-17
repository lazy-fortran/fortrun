module codegen_declarations
    use ast_core
    use type_system_hm
    implicit none
    private

    public :: generate_variable_declarations

contains

    ! Generate variable declarations from AST stack with inferred types
    function generate_variable_declarations(stack, prog_index) result(declarations)
        type(ast_stack_t), intent(in) :: stack
        integer, intent(in) :: prog_index
        character(len=:), allocatable :: declarations
        character(len=64), allocatable :: var_names(:)
        character(len=64), allocatable :: var_types(:)
        logical, allocatable :: var_declared(:)
        integer :: i, var_count
        integer, allocatable :: child_indices(:)
        class(ast_node), allocatable :: node

        declarations = ""
        allocate (var_names(100))
        allocate (var_types(100))
        allocate (var_declared(100))
        var_declared = .false.
        var_count = 0

        ! Safety check
        if (prog_index <= 0 .or. prog_index > stack%size) then
            return
        end if

        ! Get program node
        if (allocated(stack%entries(prog_index)%node)) then
            select type (prog => stack%entries(prog_index)%node)
            type is (program_node)
                ! Get children of program node
                child_indices = stack%get_children(prog_index)

                ! Collect all variables that need declarations
                do i = 1, size(child_indices)
                    if (allocated(stack%entries(child_indices(i))%node)) then
                        select type (stmt => stack%entries(child_indices(i))%node)
                        type is (assignment_node)
                            call collect_assignment_vars(stack, child_indices(i), var_names, var_types, var_declared, var_count)
                        type is (identifier_node)
        call collect_identifier_var(stmt, var_names, var_types, var_declared, var_count)
                        end select
                    end if
                end do
            end select
        end if

        ! Generate declarations
        do i = 1, var_count
            if (var_declared(i)) then
                if (len(declarations) > 0) then
                    declarations = declarations//new_line('A')
                end if
     declarations = declarations//"    "//trim(var_types(i))//" :: "//trim(var_names(i))
            end if
        end do

    end function generate_variable_declarations

    ! Collect variables from assignment node
    subroutine collect_assignment_vars(stack, assign_index, var_names, var_types, var_declared, var_count)
        type(ast_stack_t), intent(in) :: stack
        integer, intent(in) :: assign_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count

        if (assign_index <= 0 .or. assign_index > stack%size) return
        if (.not. allocated(stack%entries(assign_index)%node)) return

        select type (assign => stack%entries(assign_index)%node)
        type is (assignment_node)
            ! Get target node
            if (assign%target_index > 0 .and. assign%target_index <= stack%size) then
                if (allocated(stack%entries(assign%target_index)%node)) then
                    select type (target => stack%entries(assign%target_index)%node)
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

end module codegen_declarations
