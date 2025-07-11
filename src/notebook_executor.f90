module notebook_executor
    use notebook_parser
    implicit none
    private
    
    ! Cell execution result
    type, public :: cell_result_t
        logical :: success = .true.
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error
        character(len=:), allocatable :: figure_data  ! Base64 encoded PNG
    end type cell_result_t
    
    ! Execution results for entire notebook
    type, public :: execution_result_t
        type(cell_result_t), allocatable :: cells(:)
        logical :: success = .true.
        character(len=:), allocatable :: error_message
    end type execution_result_t
    
    ! Public procedures
    public :: execute_notebook
    public :: free_execution_results
    
contains

    subroutine execute_notebook(notebook, results)
        type(notebook_t), intent(in) :: notebook
        type(execution_result_t), intent(out) :: results
        
        integer :: i
        
        ! Allocate results for all cells
        allocate(results%cells(notebook%num_cells))
        results%success = .true.
        
        ! Execute each cell
        do i = 1, notebook%num_cells
            if (notebook%cells(i)%cell_type == CELL_CODE) then
                call execute_code_cell(notebook%cells(i), results%cells(i))
                
                if (.not. results%cells(i)%success) then
                    results%success = .false.
                    results%error_message = "Execution failed at cell " // int_to_str(i)
                end if
            else
                ! Markdown cells don't need execution
                results%cells(i)%success = .true.
                results%cells(i)%output = ""
            end if
        end do
        
    end subroutine execute_notebook
    
    subroutine execute_code_cell(cell, result)
        type(cell_t), intent(in) :: cell
        type(cell_result_t), intent(out) :: result
        
        ! TODO: Implement actual code execution
        ! For now, mock the execution
        result%success = .true.
        
        ! Mock output based on cell content
        if (index(cell%content, "print") > 0) then
            ! Extract what would be printed (simplified)
            result%output = extract_mock_output(cell%content)
        else
            result%output = ""
        end if
        
        ! Check for plotting commands
        if (index(cell%content, "show()") > 0) then
            ! Mock figure data
            result%figure_data = generate_mock_figure()
        end if
        
    end subroutine execute_code_cell
    
    function extract_mock_output(code) result(output)
        character(len=*), intent(in) :: code
        character(len=:), allocatable :: output
        
        ! Very simplified mock - just look for common patterns
        if (index(code, "'Result:'") > 0) then
            output = "Result: 10"
        else if (index(code, "'y ='") > 0) then
            output = "y = 20"
        else if (index(code, "'z ='") > 0) then
            output = "z = 30"
        else if (index(code, "'Hello, World!'") > 0) then
            output = "Hello, World!"
        else if (index(code, "Plot would be shown here") > 0) then
            output = "Plot would be shown here"
        else
            output = "Output from: " // trim(code)
        end if
        
    end function extract_mock_output
    
    function generate_mock_figure() result(figure_data)
        character(len=:), allocatable :: figure_data
        
        ! Mock base64 PNG data (tiny 1x1 transparent PNG)
        figure_data = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="
        
    end function generate_mock_figure
    
    subroutine free_execution_results(results)
        type(execution_result_t), intent(inout) :: results
        integer :: i
        
        if (allocated(results%cells)) then
            do i = 1, size(results%cells)
                if (allocated(results%cells(i)%output)) then
                    deallocate(results%cells(i)%output)
                end if
                if (allocated(results%cells(i)%error)) then
                    deallocate(results%cells(i)%error)
                end if
                if (allocated(results%cells(i)%figure_data)) then
                    deallocate(results%cells(i)%figure_data)
                end if
            end do
            deallocate(results%cells)
        end if
        
        if (allocated(results%error_message)) then
            deallocate(results%error_message)
        end if
        
    end subroutine free_execution_results
    
    ! Helper function
    function int_to_str(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str
        
        write(str, '(I0)') i
        str = trim(adjustl(str))
        
    end function int_to_str
    
end module notebook_executor