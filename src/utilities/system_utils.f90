module system_utils
    !> Platform-agnostic system utilities module
    !> Provides cross-platform wrappers for common system operations
    use fpm_environment, only: get_os_type, OS_WINDOWS
    use temp_utils, only: get_temp_file_path, create_temp_dir, create_temp_file
    implicit none
    private

    public :: sys_copy_file, sys_remove_file, sys_remove_dir, sys_move_file
    public :: sys_list_files, sys_file_exists, sys_dir_exists
    public :: sys_get_absolute_path, sys_get_current_dir
    public :: sys_find_files, sys_create_dir, sys_create_symlink
    public :: sys_run_command, sys_get_path_separator
    public :: sys_count_files, sys_sleep, sys_kill_process
    public :: sys_process_exists, sys_get_temp_dir
    public :: sys_run_command_with_exit_code, get_stderr_redirect, escape_shell_arg
    public :: sys_copy_dir

contains

    !> Copy a file from source to destination
    subroutine sys_copy_file(source, dest, success, error_msg)
        character(len=*), intent(in) :: source, dest
        logical, intent(out) :: success
        character(len=*), intent(out), optional :: error_msg
        character(len=512) :: command
        integer :: exitstat

        if (get_os_type() == OS_WINDOWS) then
            ! Windows copy command - ensure proper path handling
            command = 'copy /Y "'//trim(escape_shell_arg(source))//'" "'//trim(escape_shell_arg(dest))//'" >nul 2>&1'
        else
            command = 'cp "'//trim(escape_shell_arg(source))//'" "'//trim(escape_shell_arg(dest))//'" 2>/dev/null'
        end if

        call execute_command_line(command, exitstat=exitstat)
        success = (exitstat == 0)

        if (present(error_msg)) then
            if (.not. success) then
                error_msg = "Failed to copy file"
            else
                error_msg = ""
            end if
        end if
    end subroutine sys_copy_file

    !> Copy a directory recursively from source to destination
    subroutine sys_copy_dir(source, dest, success, error_msg)
        character(len=*), intent(in) :: source, dest
        logical, intent(out) :: success
        character(len=*), intent(out), optional :: error_msg
        character(len=512) :: command
        integer :: exitstat

        if (get_os_type() == OS_WINDOWS) then
            command = 'xcopy /E /I /Y "'//trim(escape_shell_arg(source))//'" "'//trim(escape_shell_arg(dest))//'" >nul 2>&1'
        else
            command = 'cp -r "'//trim(escape_shell_arg(source))//'" "'//trim(escape_shell_arg(dest))//'" 2>/dev/null'
        end if

        call execute_command_line(command, exitstat=exitstat)
        success = (exitstat == 0)

        if (present(error_msg)) then
            if (.not. success) then
                error_msg = "Failed to copy directory"
            else
                error_msg = ""
            end if
        end if
    end subroutine sys_copy_dir

    !> Remove a file
    subroutine sys_remove_file(filepath, success)
        character(len=*), intent(in) :: filepath
        logical, intent(out), optional :: success
        character(len=512) :: command
        integer :: exitstat

        if (get_os_type() == OS_WINDOWS) then
            command = 'del /f /q "'//trim(escape_shell_arg(filepath))//'" 2>nul'
        else
            command = 'rm -f "'//trim(escape_shell_arg(filepath))//'" 2>/dev/null'
        end if

        call execute_command_line(command, exitstat=exitstat)
        if (present(success)) success = (exitstat == 0)
    end subroutine sys_remove_file

    !> Remove a directory and all its contents
    subroutine sys_remove_dir(dirpath, success)
        character(len=*), intent(in) :: dirpath
        logical, intent(out), optional :: success
        character(len=512) :: command
        integer :: exitstat

        if (get_os_type() == OS_WINDOWS) then
            command = 'rmdir /s /q "'//trim(escape_shell_arg(dirpath))//'" 2>nul'
        else
            command = 'rm -rf "'//trim(escape_shell_arg(dirpath))//'" 2>/dev/null'
        end if

        call execute_command_line(command, exitstat=exitstat)
        if (present(success)) success = (exitstat == 0)
    end subroutine sys_remove_dir

    !> Move/rename a file
    subroutine sys_move_file(source, dest, success)
        character(len=*), intent(in) :: source, dest
        logical, intent(out) :: success
        character(len=512) :: command
        integer :: exitstat

        if (get_os_type() == OS_WINDOWS) then
            command = 'move /Y "'//trim(escape_shell_arg(source))//'" "'//trim(escape_shell_arg(dest))//'" >nul 2>&1'
        else
            command = 'mv "'//trim(escape_shell_arg(source))//'" "'//trim(escape_shell_arg(dest))//'" 2>/dev/null'
        end if

        call execute_command_line(command, exitstat=exitstat)
        success = (exitstat == 0)
    end subroutine sys_move_file

    !> List files in a directory matching a pattern
    subroutine sys_list_files(directory, pattern, files, num_files)
        character(len=*), intent(in) :: directory, pattern
        character(len=*), intent(out) :: files(:)
        integer, intent(out) :: num_files
        character(len=512) :: command, temp_file
        integer :: unit, iostat
        character(len=512) :: line

        temp_file = create_temp_file('sys_list_files', '.tmp')

        if (get_os_type() == OS_WINDOWS) then
            ! For Windows cmd /c, we need to handle quotes specially
            ! Using ^ to escape quotes inside the cmd /c string
            command = 'cmd /c dir /b "'//trim(escape_shell_arg(directory))//sys_get_path_separator()// &
  trim(escape_shell_arg(pattern))//'" 2>nul > "'//trim(escape_shell_arg(temp_file))//'"'
        else
       command = 'ls "'//trim(escape_shell_arg(directory))//sys_get_path_separator()// &
                      trim(escape_shell_arg(pattern))//'" 2>/dev/null > "'//trim(escape_shell_arg(temp_file))//'"'
        end if

        call execute_command_line(command)

        num_files = 0
        open (newunit=unit, file=temp_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                if (num_files < size(files)) then
                    num_files = num_files + 1
                    ! Both dir /b and ls might return just filenames, so always prepend directory
                    if (index(line, '/') == 0 .and. index(line, '\') == 0) then
                        ! Line contains just filename, prepend directory
                        if (get_os_type() == OS_WINDOWS) then
                            files(num_files) = trim(directory)//'\'//trim(line)
                        else
                            files(num_files) = trim(directory)//'/'//trim(line)
                        end if
                    else
                        ! Line already contains full path
                        files(num_files) = trim(line)
                    end if
                end if
            end do
            close (unit)
        end if

        call sys_remove_file(temp_file)
    end subroutine sys_list_files

    !> Check if a file exists (handles both regular files and symlinks)
    function sys_file_exists(filepath) result(exists)
        character(len=*), intent(in) :: filepath
        logical :: exists
        integer :: unit, iostat

        ! First try standard inquire
        inquire (file=filepath, exist=exists)

        ! If not found, try opening it (handles symlinks better)
        if (.not. exists) then
          open (newunit=unit, file=filepath, status='old', action='read', iostat=iostat)
            if (iostat == 0) then
                exists = .true.
                close (unit)
            else
                exists = .false.
            end if
        end if
    end function sys_file_exists

    !> Check if a directory exists
    function sys_dir_exists(dirpath) result(exists)
        character(len=*), intent(in) :: dirpath
        logical :: exists
        character(len=512) :: command
        integer :: exitstat

        if (get_os_type() == OS_WINDOWS) then
     command = 'if exist "'//trim(escape_shell_arg(dirpath))//'" (exit 0) else (exit 1)'
        else
            command = 'test -d "'//trim(escape_shell_arg(dirpath))//'"'
        end if

        call execute_command_line(command, exitstat=exitstat)
        exists = (exitstat == 0)
    end function sys_dir_exists

    !> Get absolute path of a file
    subroutine sys_get_absolute_path(filepath, abs_path, success)
        character(len=*), intent(in) :: filepath
        character(len=*), intent(out) :: abs_path
        logical, intent(out), optional :: success
        character(len=512) :: command, temp_file
        integer :: unit, iostat

        temp_file = create_temp_file('sys_abspath_path', '.tmp')

        if (get_os_type() == OS_WINDOWS) then
            command = 'powershell -Command "(Resolve-Path -Path '''//trim(escape_shell_arg(filepath))// &
                      ''').Path" > "'//trim(escape_shell_arg(temp_file))//'"'
        else
       command = 'realpath "'//trim(escape_shell_arg(filepath))//'" > "'//trim(escape_shell_arg(temp_file))//'" 2>/dev/null'
        end if

        call execute_command_line(command, exitstat=iostat)

        if (iostat == 0) then
            open (newunit=unit, file=temp_file, status='old', iostat=iostat)
            if (iostat == 0) then
                read (unit, '(A)') abs_path
                close (unit)
                if (present(success)) success = .true.
            else
                abs_path = filepath
                if (present(success)) success = .false.
            end if
        else
            abs_path = filepath
            if (present(success)) success = .false.
        end if

        call sys_remove_file(temp_file)
    end subroutine sys_get_absolute_path

    !> Get current working directory
    subroutine sys_get_current_dir(cwd, success)
        character(len=*), intent(out) :: cwd
        logical, intent(out), optional :: success
        character(len=512) :: command, temp_file
        integer :: unit, iostat

        temp_file = create_temp_file('sys_cwd_cwd', '.tmp')

        if (get_os_type() == OS_WINDOWS) then
            command = 'cd > "'//trim(escape_shell_arg(temp_file))//'"'
        else
            command = 'pwd > "'//trim(escape_shell_arg(temp_file))//'"'
        end if

        call execute_command_line(command, exitstat=iostat)

        if (iostat == 0) then
            open (newunit=unit, file=temp_file, status='old', iostat=iostat)
            if (iostat == 0) then
                read (unit, '(A)') cwd
                close (unit)
                if (present(success)) success = .true.
            else
                cwd = '.'
                if (present(success)) success = .false.
            end if
        else
            cwd = '.'
            if (present(success)) success = .false.
        end if

        call sys_remove_file(temp_file)
    end subroutine sys_get_current_dir

    !> Find files matching a pattern (recursive or non-recursive)
   subroutine sys_find_files(directory, pattern, files, num_files, recursive, max_depth)
        character(len=*), intent(in) :: directory, pattern
        character(len=*), intent(out) :: files(:)
        integer, intent(out) :: num_files
        logical, intent(in), optional :: recursive
        integer, intent(in), optional :: max_depth
        character(len=1024) :: command, temp_file
        logical :: is_recursive
        integer :: depth, unit, iostat
        character(len=512) :: line

        is_recursive = .false.
        if (present(recursive)) is_recursive = recursive

        depth = 1
        if (present(max_depth)) depth = max_depth

        temp_file = create_temp_file('sys_find_found', '.tmp')

        if (get_os_type() == OS_WINDOWS) then
            if (is_recursive) then
              command = 'cmd /c dir /s /b "'//trim(escape_shell_arg(directory))//'\'// &
  trim(escape_shell_arg(pattern))//'" 2>nul > "'//trim(escape_shell_arg(temp_file))//'"'
            else
                command = 'cmd /c dir /b "'//trim(escape_shell_arg(directory))//'\'// &
  trim(escape_shell_arg(pattern))//'" 2>nul > "'//trim(escape_shell_arg(temp_file))//'"'
            end if
        else
            if (is_recursive) then
                command = 'find "'//trim(escape_shell_arg(directory))//'" -name "'//trim(escape_shell_arg(pattern))// &
                     '" -type f > "'//trim(escape_shell_arg(temp_file))//'" 2>/dev/null'
            else
                command = 'find "'//trim(escape_shell_arg(directory))//'" -maxdepth '
                write (command(len_trim(command) + 1:), '(I0)') depth
                command = trim(command)//' -name "'//trim(escape_shell_arg(pattern))// &
                     '" -type f > "'//trim(escape_shell_arg(temp_file))//'" 2>/dev/null'
            end if
        end if

        call execute_command_line(command)

        num_files = 0
        open (newunit=unit, file=temp_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                if (num_files < size(files)) then
                    num_files = num_files + 1
                    ! Both dir /b and ls might return just filenames, so always prepend directory
                    if (index(line, '/') == 0 .and. index(line, '\') == 0) then
                        ! Line contains just filename, prepend directory
                        if (get_os_type() == OS_WINDOWS) then
                            files(num_files) = trim(directory)//'\'//trim(line)
                        else
                            files(num_files) = trim(directory)//'/'//trim(line)
                        end if
                    else
                        ! Line already contains full path
                        files(num_files) = trim(line)
                    end if
                end if
            end do
            close (unit)
        end if

        call sys_remove_file(temp_file)
    end subroutine sys_find_files

    !> Create a directory (with parent directories if needed)
    subroutine sys_create_dir(dirpath, success, create_parents)
        character(len=*), intent(in) :: dirpath
        logical, intent(out), optional :: success
        logical, intent(in), optional :: create_parents
        character(len=512) :: command
        integer :: exitstat
        logical :: with_parents

        with_parents = .true.
        if (present(create_parents)) with_parents = create_parents

        if (get_os_type() == OS_WINDOWS) then
            if (with_parents) then
                command = 'mkdir "'//trim(escape_shell_arg(dirpath))//'" 2>nul'
            else
                command = 'mkdir "'//trim(escape_shell_arg(dirpath))//'" 2>nul'
            end if
        else
            if (with_parents) then
                command = 'mkdir -p "'//trim(escape_shell_arg(dirpath))//'" 2>/dev/null'
            else
                command = 'mkdir "'//trim(escape_shell_arg(dirpath))//'" 2>/dev/null'
            end if
        end if

        call execute_command_line(command, exitstat=exitstat)
        if (present(success)) success = (exitstat == 0)
    end subroutine sys_create_dir

    !> Create a symbolic link
    subroutine sys_create_symlink(target, link_name, success)
        character(len=*), intent(in) :: target, link_name
        logical, intent(out) :: success
        character(len=512) :: command
        integer :: exitstat

        if (get_os_type() == OS_WINDOWS) then
            ! Windows requires admin rights for symlinks, use junction for directories
            command = 'mklink "'//trim(escape_shell_arg(link_name))//'" "'//trim(escape_shell_arg(target))//'" 2>nul'
        else
            command = 'ln -s "'//trim(escape_shell_arg(target))//'" "'//trim(escape_shell_arg(link_name))//'" 2>/dev/null'
        end if

        call execute_command_line(command, exitstat=exitstat)
        success = (exitstat == 0)
    end subroutine sys_create_symlink

    !> Run a command and capture output
    subroutine sys_run_command(command, output, exit_code, timeout)
        character(len=*), intent(in) :: command
        character(len=*), intent(out) :: output
        integer, intent(out) :: exit_code
        integer, intent(in), optional :: timeout
        character(len=1024) :: full_command, temp_file
        integer :: unit, iostat

        ! Initialize output and exit_code
        output = ""
        exit_code = -1

        ! Check for empty or invalid command
        if (len_trim(command) == 0) then
            output = "Error: Empty command"
            exit_code = 127  ! Command not found
            return
        end if

        temp_file = create_temp_file('sys_cmd_output', '.tmp')

        ! Build the command carefully, wrapping in shell to handle edge cases
        if (get_os_type() == OS_WINDOWS) then
            if (present(timeout)) then
                ! Windows doesn't have a simple timeout command
                full_command = 'cmd /c "'//trim(command)//' > "'//trim(escape_shell_arg(temp_file))//'" 2>&1"'
            else
                full_command = 'cmd /c "'//trim(command)//' > "'//trim(escape_shell_arg(temp_file))//'" 2>&1"'
            end if
        else
            if (present(timeout)) then
    write (full_command, '(A,I0,A)') 'sh -c ''timeout ', timeout, ' '//trim(command)// &
                    ' > "'//trim(escape_shell_arg(temp_file))//'" 2>&1'''
            else
                full_command = 'sh -c '''//trim(command)//' > "'//trim(escape_shell_arg(temp_file))//'" 2>&1'''
            end if
        end if

        ! Use cmdstat to catch invalid command errors
        call execute_command_line(full_command, exitstat=exit_code, cmdstat=iostat)

        ! Handle command execution errors
        if (iostat /= 0) then
            exit_code = 127  ! Command not found
            output = "Command execution failed"
            call sys_remove_file(temp_file)
            return
        end if

        output = ""
        open (newunit=unit, file=temp_file, status='old', iostat=iostat)
        if (iostat == 0) then
            read (unit, '(A)', iostat=iostat) output
            close (unit)
        end if

        call sys_remove_file(temp_file)
    end subroutine sys_run_command

    !> Get platform-specific path separator
    function sys_get_path_separator() result(sep)
        character(len=1) :: sep

        if (get_os_type() == OS_WINDOWS) then
            sep = '\'
        else
            sep = '/'
        end if
    end function sys_get_path_separator

    !> Count files in a directory
    function sys_count_files(directory) result(count)
        character(len=*), intent(in) :: directory
        integer :: count
        character(len=512) :: command, temp_file, output
        integer :: unit, iostat

        temp_file = create_temp_file('sys_count_count', '.tmp')

        if (get_os_type() == OS_WINDOWS) then
            command = 'cmd /c dir /a-d /b "'//trim(escape_shell_arg(directory))// &
                 '" 2>nul | find /c /v """" > "'//trim(escape_shell_arg(temp_file))//'"'
        else
            command = 'find "'//trim(escape_shell_arg(directory))// &
             '" -type f 2>/dev/null | wc -l > "'//trim(escape_shell_arg(temp_file))//'"'
        end if

        call execute_command_line(command)

        count = 0
        open (newunit=unit, file=temp_file, status='old', iostat=iostat)
        if (iostat == 0) then
            read (unit, *, iostat=iostat) count
            close (unit)
        end if

        call sys_remove_file(temp_file)
    end function sys_count_files

    !> Sleep for specified seconds
    subroutine sys_sleep(seconds)
        integer, intent(in) :: seconds
        character(len=128) :: command

        if (get_os_type() == OS_WINDOWS) then
            write (command, '(A,I0,A)') 'ping -n ', seconds + 1, ' 127.0.0.1 >nul'
        else
            write (command, '(A,I0)') 'sleep ', seconds
        end if

        call execute_command_line(command)
    end subroutine sys_sleep

    !> Kill a process by PID
    subroutine sys_kill_process(pid, success, force)
        integer, intent(in) :: pid
        logical, intent(out) :: success
        logical, intent(in), optional :: force
        character(len=128) :: command
        integer :: exitstat
        logical :: force_kill

        force_kill = .false.
        if (present(force)) force_kill = force

        if (get_os_type() == OS_WINDOWS) then
            if (force_kill) then
                write (command, '(A,I0)') 'taskkill /F /PID ', pid
            else
                write (command, '(A,I0)') 'taskkill /PID ', pid
            end if
        else
            if (force_kill) then
                write (command, '(A,I0,A)') 'kill -9 ', pid, ' 2>/dev/null'
            else
                write (command, '(A,I0,A)') 'kill ', pid, ' 2>/dev/null'
            end if
        end if

        call execute_command_line(command, exitstat=exitstat)
        success = (exitstat == 0)
    end subroutine sys_kill_process

    !> Check if a process exists
    function sys_process_exists(pid) result(exists)
        integer, intent(in) :: pid
        logical :: exists
        character(len=128) :: command
        integer :: exitstat

        if (get_os_type() == OS_WINDOWS) then
            write(command, '(A,I0,A,I0,A)') 'tasklist /FI "PID eq ', pid, '" 2>nul | find "', pid, '" >nul'
        else
            write (command, '(A,I0,A)') 'kill -0 ', pid, ' 2>/dev/null'
        end if

        call execute_command_line(command, exitstat=exitstat)
        exists = (exitstat == 0)
    end function sys_process_exists

    !> Get system temporary directory
    function sys_get_temp_dir() result(temp_dir)
        character(len=:), allocatable :: temp_dir
        character(len=256) :: env_temp
        integer :: length

        if (get_os_type() == OS_WINDOWS) then
            call get_environment_variable('TEMP', env_temp, length)
            if (length > 0) then
                temp_dir = trim(env_temp)
            else
                call get_environment_variable('TMP', env_temp, length)
                if (length > 0) then
                    temp_dir = trim(env_temp)
                else
                    ! Fallback to common Windows temp location
                    temp_dir = 'C:\Windows\Temp'
                end if
            end if
        else
            temp_dir = '/tmp'
        end if
    end function sys_get_temp_dir

    !> Run a command and capture output and exit code to files
    !> This is a portable way to replace Unix "command > output 2>&1; echo $? > exit_file"
    subroutine sys_run_command_with_exit_code(command, output_file, exit_file)
        character(len=*), intent(in) :: command, output_file, exit_file
        character(len=2048) :: full_command
        integer :: exit_code, unit

        if (get_os_type() == OS_WINDOWS) then
            ! Windows: Run command first, then check ERRORLEVEL separately
            ! This ensures we capture the exit code of the command, not the echo
            call execute_command_line(trim(command)//' > "'//trim(escape_shell_arg(output_file))//'" 2>&1', exitstat=exit_code)
            ! Write the exit code manually
            open (newunit=unit, file=trim(exit_file), status='replace')
            write (unit, '(i0)') exit_code
            close (unit)
            return
        else
            ! Unix: Use shell to run command and capture exit code
       full_command = '('//trim(command)//') > "'//trim(escape_shell_arg(output_file)) &
                         //'" 2>&1; echo $? > "'//trim(escape_shell_arg(exit_file))//'"'
        end if

        call execute_command_line(full_command)
    end subroutine sys_run_command_with_exit_code

    !> Get platform-specific stderr redirection string
    function get_stderr_redirect() result(redirect)
        character(len=:), allocatable :: redirect

        if (get_os_type() == OS_WINDOWS) then
            redirect = ' 2>nul'
        else
            redirect = ' 2>/dev/null'
        end if
    end function get_stderr_redirect

    function escape_shell_arg(arg) result(escaped)
        character(len=*), intent(in) :: arg
        character(len=:), allocatable :: escaped
        integer :: i, n
        character(len=1) :: ch
        logical :: is_windows_path

        ! For use inside double quotes, we only need to escape:
        ! " (double quote), \ (backslash), $ (dollar), ` (backtick)
        ! We don't escape spaces since we're inside quotes

        ! On Windows, don't escape backslashes if this looks like a path
        is_windows_path = .false.
        if (get_os_type() == OS_WINDOWS) then
            ! Check if this looks like a Windows path (e.g., C:\ or \\network)
            if (len_trim(arg) >= 3) then
                if ((arg(2:2) == ':' .and. arg(3:3) == '\') .or. &
                    (arg(1:2) == '\\')) then
                    is_windows_path = .true.
                end if
            end if
        end if

        ! Count how many characters we need
        n = 0
        do i = 1, len_trim(arg)
            ch = arg(i:i)
            if (ch == '"' .or. (ch == '\' .and. .not. is_windows_path) .or. &
                ch == '$' .or. ch == '`') then
                n = n + 2  ! Need to escape these characters
            else
                n = n + 1
            end if
        end do

        ! Allocate result
        allocate (character(len=n) :: escaped)

        ! Build escaped string
        n = 0
        do i = 1, len_trim(arg)
            ch = arg(i:i)
            if (ch == '"' .or. (ch == '\' .and. .not. is_windows_path) .or. &
                ch == '$' .or. ch == '`') then
                n = n + 1
                escaped(n:n) = '\'
                n = n + 1
                escaped(n:n) = ch
            else
                n = n + 1
                escaped(n:n) = ch
            end if
        end do

        ! Trim to actual length
        escaped = escaped(1:n)
    end function escape_shell_arg

end module system_utils
