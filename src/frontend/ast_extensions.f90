module ast_extensions
    ! Extensions to AST nodes for the compiler frontend
    ! Provides a way to attach type information without modifying core AST
    
    use ast_core
    use type_system_hm, only: mono_type_t
    implicit none
    private
    
    public :: ast_type_map_t, create_type_map, set_node_type, get_node_type
    
    ! Type map to associate AST nodes with their inferred types
    ! This avoids modifying the core AST structure
    type :: type_entry_t
        class(ast_node), allocatable :: node
        type(mono_type_t), allocatable :: inferred_type
    end type type_entry_t
    
    type :: ast_type_map_t
        type(type_entry_t), allocatable :: entries(:)
        integer :: count = 0
        integer :: capacity = 0
    contains
        procedure :: set_type => map_set_type
        procedure :: get_type => map_get_type
        procedure :: clear => map_clear
    end type ast_type_map_t
    
contains

    ! Create a new type map
    function create_type_map() result(map)
        type(ast_type_map_t) :: map
        
        map%capacity = 100
        map%count = 0
        allocate(map%entries(map%capacity))
    end function create_type_map
    
    ! Set type for a node
    subroutine set_node_type(map, node, typ)
        type(ast_type_map_t), intent(inout) :: map
        class(ast_node), intent(in) :: node
        type(mono_type_t), intent(in) :: typ
        
        call map%set_type(node, typ)
    end subroutine set_node_type
    
    ! Get type for a node
    function get_node_type(map, node) result(typ)
        type(ast_type_map_t), intent(in) :: map
        class(ast_node), intent(in) :: node
        type(mono_type_t), allocatable :: typ
        
        typ = map%get_type(node)
    end function get_node_type
    
    ! Internal: Set type in map
    subroutine map_set_type(this, node, typ)
        class(ast_type_map_t), intent(inout) :: this
        class(ast_node), intent(in) :: node
        type(mono_type_t), intent(in) :: typ
        integer :: i
        
        ! Check if node already has a type
        do i = 1, this%count
            if (same_node(this%entries(i)%node, node)) then
                ! Update existing type
                this%entries(i)%inferred_type = typ
                return
            end if
        end do
        
        ! Add new entry
        if (this%count >= this%capacity) then
            call grow_map(this)
        end if
        
        this%count = this%count + 1
        allocate(this%entries(this%count)%node, source=node)
        allocate(this%entries(this%count)%inferred_type, source=typ)
    end subroutine map_set_type
    
    ! Internal: Get type from map
    function map_get_type(this, node) result(typ)
        class(ast_type_map_t), intent(in) :: this
        class(ast_node), intent(in) :: node
        type(mono_type_t), allocatable :: typ
        integer :: i
        
        do i = 1, this%count
            if (same_node(this%entries(i)%node, node)) then
                if (allocated(this%entries(i)%inferred_type)) then
                    allocate(typ, source=this%entries(i)%inferred_type)
                end if
                return
            end if
        end do
    end function map_get_type
    
    ! Internal: Clear the map
    subroutine map_clear(this)
        class(ast_type_map_t), intent(inout) :: this
        
        this%count = 0
        ! Keep allocated memory for reuse
    end subroutine map_clear
    
    ! Internal: Grow the map capacity
    subroutine grow_map(map)
        type(ast_type_map_t), intent(inout) :: map
        type(type_entry_t), allocatable :: new_entries(:)
        integer :: new_capacity
        
        new_capacity = map%capacity * 2
        allocate(new_entries(new_capacity))
        
        ! Copy existing entries
        new_entries(1:map%count) = map%entries(1:map%count)
        
        ! Replace with new array
        call move_alloc(new_entries, map%entries)
        map%capacity = new_capacity
    end subroutine grow_map
    
    ! Internal: Check if two nodes are the same (by location)
    logical function same_node(n1, n2)
        class(ast_node), intent(in) :: n1, n2
        
        ! Compare by source location
        same_node = (n1%line == n2%line .and. n1%column == n2%column)
        
        ! Could add more sophisticated comparison if needed
    end function same_node
    
end module ast_extensions