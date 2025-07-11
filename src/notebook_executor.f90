module notebook_executor
    use notebook_parser
    use, intrinsic :: iso_c_binding
    implicit none
    private
    
    interface
        function c_getpid() bind(C, name="getpid")
            import :: c_int
            integer(c_int) :: c_getpid
        end function c_getpid
    end interface
    
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
        
        character(len=:), allocatable :: temp_dir, fpm_project_dir
        character(len=256) :: cache_dir
        integer :: i, code_cell_count
        
        ! Allocate results for all cells
        allocate(results%cells(notebook%num_cells))
        results%success = .true.
        
        ! Create temporary directory for notebook project
        call create_temp_notebook_dir(temp_dir)
        fpm_project_dir = trim(temp_dir) // '/notebook_project'
        
        ! Generate FPM project structure
        call generate_notebook_fpm_project(notebook, fpm_project_dir)
        
        ! Build all apps with FPM
        call build_notebook_apps(fpm_project_dir, results%success, results%error_message)
        
        if (.not. results%success) then
            return
        end if
        
        ! Execute each code cell's app and capture output
        code_cell_count = 0
        do i = 1, notebook%num_cells
            if (notebook%cells(i)%cell_type == CELL_CODE) then
                code_cell_count = code_cell_count + 1
                call execute_cell_app(fpm_project_dir, code_cell_count, results%cells(i))
            else
                ! Markdown cells don't need execution
                results%cells(i)%success = .true.
                results%cells(i)%output = ""
            end if
        end do
        
        ! Cleanup temporary directory
        call cleanup_temp_dir(temp_dir)
        
    end subroutine execute_notebook
    
    subroutine generate_notebook_fpm_project(notebook, project_dir)
        type(notebook_t), intent(in) :: notebook
        character(len=*), intent(in) :: project_dir
        
        character(len=512) :: command
        character(len=:), allocatable :: app_content, fpm_content
        integer :: unit, i, code_cell_count, iostat
        
        ! Create project directory
        command = 'mkdir -p ' // trim(project_dir) // '/app'
        call execute_command_line(command)
        
        ! Generate fpm.toml
        fpm_content = generate_notebook_fpm_toml()
        open(newunit=unit, file=trim(project_dir) // '/fpm.toml', status='replace')
        write(unit, '(a)') fpm_content
        close(unit)
        
        ! Generate an app for each code cell
        code_cell_count = 0
        do i = 1, notebook%num_cells
            if (notebook%cells(i)%cell_type == CELL_CODE) then
                code_cell_count = code_cell_count + 1
                app_content = generate_cell_app_code(notebook%cells(i)%content, code_cell_count)
                
                ! Write app file
                open(newunit=unit, file=trim(project_dir) // '/app/cell_' // &
                     trim(int_to_str(code_cell_count)) // '.f90', status='replace')
                write(unit, '(a)') app_content
                close(unit)
            end if
        end do
        
    end subroutine generate_notebook_fpm_project
    
    function generate_notebook_fpm_toml() result(content)
        character(len=:), allocatable :: content
        
        content = 'name = "notebook_exec"' // new_line('a') // &
                  'version = "0.1.0"' // new_line('a') // &
                  '' // new_line('a') // &
                  '[build]' // new_line('a') // &
                  'auto-executables = true' // new_line('a') // &
                  '' // new_line('a') // &
                  '[fortran]' // new_line('a') // &
                  'implicit-typing = false' // new_line('a') // &
                  'implicit-external = false' // new_line('a') // &
                  'source-form = "free"' // new_line('a')
        
    end function generate_notebook_fpm_toml
    
    function generate_cell_app_code(cell_content, cell_number) result(code)
        character(len=*), intent(in) :: cell_content
        integer, intent(in) :: cell_number
        character(len=:), allocatable :: code
        
        code = 'program cell_' // trim(int_to_str(cell_number)) // new_line('a') // &
               '    implicit none' // new_line('a') // &
               '    ' // new_line('a') // &
               adjustl(cell_content) // new_line('a') // &
               '    ' // new_line('a') // &
               'end program cell_' // trim(int_to_str(cell_number))
        
    end function generate_cell_app_code
    
    subroutine build_notebook_apps(project_dir, success, error_msg)
        character(len=*), intent(in) :: project_dir
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        character(len=512) :: command
        integer :: exit_code
        
        ! Change to project directory and build with FPM
        command = 'cd ' // trim(project_dir) // ' && fpm build 2>&1'
        call execute_and_capture(command, error_msg, exit_code)
        
        success = (exit_code == 0)
        
    end subroutine build_notebook_apps
    
    subroutine execute_cell_app(project_dir, cell_number, result)
        character(len=*), intent(in) :: project_dir
        integer, intent(in) :: cell_number
        type(cell_result_t), intent(out) :: result
        
        character(len=512) :: command
        integer :: exit_code
        
        ! Execute the cell app
        command = 'cd ' // trim(project_dir) // ' && fpm run --target cell_' // &
                  trim(int_to_str(cell_number)) // ' 2>&1'
        call execute_and_capture(command, result%output, exit_code)
        
        result%success = (exit_code == 0)
        if (.not. result%success .and. .not. allocated(result%error)) then
            result%error = result%output
        end if
        
    end subroutine execute_cell_app
    
    
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
    
    ! Helper functions
    subroutine create_temp_notebook_dir(temp_dir)
        character(len=:), allocatable, intent(out) :: temp_dir
        character(len=:), allocatable :: output
        integer :: exit_code
        
        ! Use mktemp to create unique directory
        call execute_and_capture('mktemp -d', output, exit_code)
        
        if (exit_code == 0) then
            ! Remove any trailing newlines or whitespace
            temp_dir = trim(adjustl(output))
            ! Remove newline character if present
            if (len(temp_dir) > 0 .and. temp_dir(len(temp_dir):len(temp_dir)) == char(10)) then
                temp_dir = temp_dir(1:len(temp_dir)-1)
            end if
        else
            ! Fallback to simple naming
            temp_dir = '/tmp/fortran_notebook_' // trim(int_to_str(get_process_id()))
            call execute_command_line('mkdir -p ' // trim(temp_dir))
        end if
        
    end subroutine create_temp_notebook_dir
    
    subroutine cleanup_temp_dir(temp_dir)
        character(len=*), intent(in) :: temp_dir
        character(len=512) :: command
        
        command = 'rm -rf ' // trim(temp_dir)
        call execute_command_line(command)
        
    end subroutine cleanup_temp_dir
    
    subroutine execute_and_capture(command, output, exit_code)
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: output
        integer, intent(out) :: exit_code
        
        character(len=256) :: temp_file
        character(len=512) :: full_command
        integer :: unit, iostat, file_size
        
        ! Create temp file for output
        temp_file = '/tmp/fortran_exec_' // trim(int_to_str(get_process_id())) // '.out'
        
        ! Execute command and redirect to file
        full_command = trim(command) // ' > ' // trim(temp_file) // ' 2>&1'
        call execute_command_line(full_command, exitstat=exit_code)
        
        ! Read output from file
        inquire(file=temp_file, size=file_size)
        
        if (file_size > 0) then
            open(newunit=unit, file=temp_file, status='old', &
                 access='stream', form='unformatted', iostat=iostat)
            
            if (iostat == 0) then
                allocate(character(len=file_size) :: output)
                read(unit, iostat=iostat) output
                close(unit)
            else
                output = ""
            end if
        else
            output = ""
        end if
        
        ! Clean up temp file
        call execute_command_line('rm -f ' // trim(temp_file))
        
    end subroutine execute_and_capture
    
    function int_to_str(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str
        
        write(str, '(I0)') i
        str = trim(adjustl(str))
        
    end function int_to_str
    
    function get_process_id() result(pid)
        integer :: pid
        
        pid = int(c_getpid())
        
    end function get_process_id
    
end module notebook_executor