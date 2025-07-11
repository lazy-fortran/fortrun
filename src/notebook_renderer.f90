module notebook_renderer
    use notebook_parser
    use notebook_executor
    implicit none
    private
    
    ! Public procedures
    public :: render_notebook_markdown
    public :: save_notebook_markdown
    
contains

    subroutine render_notebook_markdown(notebook, results, output)
        type(notebook_t), intent(in) :: notebook
        type(execution_result_t), intent(in) :: results
        character(len=:), allocatable, intent(out) :: output
        
        integer :: i, total_length
        character(len=:), allocatable :: cell_output
        
        ! Calculate total output length (estimate)
        total_length = 0
        do i = 1, notebook%num_cells
            total_length = total_length + len(notebook%cells(i)%content) + 1000
        end do
        
        ! Allocate output
        allocate(character(len=total_length) :: output)
        output = ""
        
        ! Render each cell
        do i = 1, notebook%num_cells
            if (notebook%cells(i)%cell_type == CELL_MARKDOWN) then
                call render_markdown_cell(notebook%cells(i), cell_output)
            else
                call render_code_cell(notebook%cells(i), results%cells(i), cell_output)
            end if
            
            output = trim(output) // cell_output
            
            ! Add spacing between cells
            if (i < notebook%num_cells) then
                output = trim(output) // new_line('a') // new_line('a')
            end if
        end do
        
        ! Trim to actual size
        output = trim(output)
        
    end subroutine render_notebook_markdown
    
    subroutine render_markdown_cell(cell, output)
        type(cell_t), intent(in) :: cell
        character(len=:), allocatable, intent(out) :: output
        
        ! Markdown cells are rendered as-is
        output = trim(cell%content)
        
    end subroutine render_markdown_cell
    
    subroutine render_code_cell(cell, result, output)
        type(cell_t), intent(in) :: cell
        type(cell_result_t), intent(in) :: result
        character(len=:), allocatable, intent(out) :: output
        
        character(len=:), allocatable :: temp
        
        ! Start with code block
        output = "```fortran" // new_line('a')
        output = trim(output) // trim(cell%content) // new_line('a')
        output = trim(output) // "```"
        
        ! Add output if present
        if (allocated(result%output) .and. len_trim(result%output) > 0) then
            output = trim(output) // new_line('a') // new_line('a')
            output = trim(output) // "Output:" // new_line('a')
            output = trim(output) // "```" // new_line('a')
            output = trim(output) // trim(result%output) // new_line('a')
            output = trim(output) // "```"
        end if
        
        ! Add figure if present
        if (allocated(result%figure_data) .and. len_trim(result%figure_data) > 0) then
            output = trim(output) // new_line('a') // new_line('a')
            output = trim(output) // "![Figure](data:image/png;base64," // &
                     trim(result%figure_data) // ")"
        end if
        
        ! Add error if present
        if (allocated(result%error) .and. len_trim(result%error) > 0) then
            output = trim(output) // new_line('a') // new_line('a')
            output = trim(output) // "**Error:**" // new_line('a')
            output = trim(output) // "```" // new_line('a')
            output = trim(output) // trim(result%error) // new_line('a')
            output = trim(output) // "```"
        end if
        
    end subroutine render_code_cell
    
    subroutine save_notebook_markdown(notebook, results, filename)
        type(notebook_t), intent(in) :: notebook
        type(execution_result_t), intent(in) :: results
        character(len=*), intent(in) :: filename
        
        character(len=:), allocatable :: output
        integer :: unit, iostat
        
        ! Render to markdown
        call render_notebook_markdown(notebook, results, output)
        
        ! Write to file
        open(newunit=unit, file=filename, status='replace', &
             action='write', iostat=iostat)
        
        if (iostat == 0) then
            write(unit, '(A)') output
            close(unit)
        else
            print *, "Error: Could not write to file:", trim(filename)
        end if
        
    end subroutine save_notebook_markdown
    
end module notebook_renderer