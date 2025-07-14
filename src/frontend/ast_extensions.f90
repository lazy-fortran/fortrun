module ast_extensions
    ! Completely minimal extensions to avoid gfortran crashes
    use ast_core
    implicit none
    private
    
    public :: ast_type_map_t, create_type_map, set_node_type, get_node_type
    
    ! Minimal type map that just stores basic info
    type :: ast_type_map_t
        integer :: dummy = 0
    end type ast_type_map_t
    
contains

    ! Create a new type map (minimal)
    function create_type_map() result(map)
        type(ast_type_map_t) :: map
        map%dummy = 1
    end function create_type_map
    
    ! Set type for a node (minimal - just ignore)
    subroutine set_node_type(map, node, typ)
        use type_system_hm, only: mono_type_t
        type(ast_type_map_t), intent(inout) :: map
        class(ast_node), intent(in) :: node
        type(mono_type_t), intent(in) :: typ
        ! Do nothing - just a stub
    end subroutine set_node_type
    
    ! Get type for a node (minimal - return nothing)
    function get_node_type(map, node) result(typ)
        use type_system_hm, only: mono_type_t
        type(ast_type_map_t), intent(in) :: map
        class(ast_node), intent(in) :: node
        type(mono_type_t), allocatable :: typ
        
        ! Return nothing - caller should handle unallocated typ
    end function get_node_type
    
end module ast_extensions