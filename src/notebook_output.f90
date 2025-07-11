module notebook_output
    implicit none
    private
    
    ! Maximum number of output entries per cell
    integer, parameter :: MAX_OUTPUTS = 1000
    
    ! Output capture storage
    type, public :: output_entry_t
        character(len=:), allocatable :: content
        logical :: is_error = .false.
    end type output_entry_t
    
    type, public :: cell_output_t
        type(output_entry_t), allocatable :: entries(:)
        integer :: count = 0
        logical :: has_error = .false.
    end type cell_output_t
    
    ! Global output storage
    type(cell_output_t), allocatable :: cell_outputs(:)
    integer :: current_cell = 0
    integer :: max_cells = 100
    
    ! Public procedures
    public :: init_output_capture
    public :: start_cell_capture
    public :: notebook_print
    public :: notebook_error
    public :: get_cell_output
    public :: finalize_output_capture
    
contains

    subroutine init_output_capture(num_cells)
        integer, intent(in) :: num_cells
        integer :: i
        
        max_cells = num_cells
        if (allocated(cell_outputs)) deallocate(cell_outputs)
        allocate(cell_outputs(max_cells))
        
        do i = 1, max_cells
            allocate(cell_outputs(i)%entries(MAX_OUTPUTS))
            cell_outputs(i)%count = 0
            cell_outputs(i)%has_error = .false.
        end do
        
        current_cell = 0
        
    end subroutine init_output_capture
    
    subroutine start_cell_capture(cell_number)
        integer, intent(in) :: cell_number
        
        if (cell_number >= 1 .and. cell_number <= max_cells) then
            current_cell = cell_number
            cell_outputs(current_cell)%count = 0
            cell_outputs(current_cell)%has_error = .false.
        end if
        
    end subroutine start_cell_capture
    
    subroutine notebook_print(message)
        character(len=*), intent(in) :: message
        
        call add_output(message, .false.)
        
    end subroutine notebook_print
    
    subroutine notebook_print_int(label, value)
        character(len=*), intent(in) :: label
        integer, intent(in) :: value
        character(len=256) :: msg
        
        write(msg, '(a,i0)') trim(label) // ' ', value
        call add_output(trim(msg), .false.)
        
    end subroutine notebook_print_int
    
    subroutine notebook_print_real(label, value)
        character(len=*), intent(in) :: label
        real(8), intent(in) :: value
        character(len=256) :: msg
        
        write(msg, '(a,f0.6)') trim(label) // ' ', value
        call add_output(trim(msg), .false.)
        
    end subroutine notebook_print_real
    
    subroutine notebook_print_logical(label, value)
        character(len=*), intent(in) :: label
        logical, intent(in) :: value
        character(len=256) :: msg
        
        write(msg, '(a,l1)') trim(label) // ' ', value
        call add_output(trim(msg), .false.)
        
    end subroutine notebook_print_logical
    
    subroutine notebook_error(message)
        character(len=*), intent(in) :: message
        
        call add_output(message, .true.)
        
    end subroutine notebook_error
    
    subroutine add_output(message, is_error)
        character(len=*), intent(in) :: message
        logical, intent(in) :: is_error
        
        if (current_cell >= 1 .and. current_cell <= max_cells) then
            if (cell_outputs(current_cell)%count < MAX_OUTPUTS) then
                cell_outputs(current_cell)%count = cell_outputs(current_cell)%count + 1
                cell_outputs(current_cell)%entries(cell_outputs(current_cell)%count)%content = message
                cell_outputs(current_cell)%entries(cell_outputs(current_cell)%count)%is_error = is_error
                
                if (is_error) then
                    cell_outputs(current_cell)%has_error = .true.
                end if
            end if
        end if
        
    end subroutine add_output
    
    function get_cell_output(cell_number) result(output)
        integer, intent(in) :: cell_number
        type(cell_output_t) :: output
        integer :: i
        
        if (cell_number >= 1 .and. cell_number <= max_cells) then
            output = cell_outputs(cell_number)
        else
            ! Return empty output for invalid cell number
            allocate(output%entries(0))
            output%count = 0
            output%has_error = .false.
        end if
        
    end function get_cell_output
    
    subroutine finalize_output_capture()
        integer :: i, j
        
        if (allocated(cell_outputs)) then
            do i = 1, max_cells
                if (allocated(cell_outputs(i)%entries)) then
                    do j = 1, cell_outputs(i)%count
                        if (allocated(cell_outputs(i)%entries(j)%content)) then
                            deallocate(cell_outputs(i)%entries(j)%content)
                        end if
                    end do
                    deallocate(cell_outputs(i)%entries)
                end if
            end do
            deallocate(cell_outputs)
        end if
        
    end subroutine finalize_output_capture
    
end module notebook_output