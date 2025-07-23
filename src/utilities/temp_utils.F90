module temp_utils
#ifdef _OPENMP
    use omp_lib, only: omp_get_thread_num
#endif
    use iso_fortran_env, only: error_unit
    use fpm_filesystem, only: join_path, get_temp_filename, exists
    use fpm_environment, only: get_env, get_os_type, OS_WINDOWS
    use iso_c_binding, only: c_int
    implicit none
    private
    public :: create_temp_dir, cleanup_temp_dir, get_temp_file_path, temp_dir_manager, &
              get_system_temp_dir, get_current_directory, get_project_root, path_join, &
              mkdir, create_test_cache_dir

    ! Interface for getpid
    interface
        function getpid() bind(c, name="getpid")
            import :: c_int
            integer(c_int) :: getpid
        end function getpid
    end interface

    ! Type for managing temporary directories with automatic cleanup
    type :: temp_dir_manager
        character(len=:), allocatable :: path
        logical :: cleanup_on_destroy = .true.
    contains
        procedure :: create => temp_dir_create
        procedure :: get_path => temp_dir_get_path
        procedure :: get_file_path => temp_dir_get_file_path
        procedure :: cleanup => temp_dir_cleanup
        final :: temp_dir_destroy
    end type temp_dir_manager

contains

    ! Simple inline escape function to avoid circular dependency
    function escape_quotes(str) result(escaped)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: escaped
        integer :: i, n, len_str
        character(len=1) :: ch
        
        len_str = len_trim(str)
        ! Count characters needed
        n = 0
        do i = 1, len_str
            ch = str(i:i)
            if (ch == '"' .or. ch == '\' .or. ch == '$' .or. ch == '`') then
                n = n + 2
            else
                n = n + 1
            end if
        end do
        
        ! Allocate and build escaped string
        allocate(character(len=n) :: escaped)
        n = 0
        do i = 1, len_str
            ch = str(i:i)
            if (ch == '"' .or. ch == '\' .or. ch == '$' .or. ch == '`') then
                n = n + 1
                escaped(n:n) = '\'
                n = n + 1
                escaped(n:n) = ch
            else
                n = n + 1
                escaped(n:n) = ch
            end if
        end do
    end function escape_quotes

    function create_temp_dir(prefix) result(temp_dir)
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable :: temp_dir
        character(len=32) :: random_suffix
        character(len=256) :: base_temp_dir
        integer :: ios

        ! Get system temp directory using FPM's get_env
        base_temp_dir = get_env('TMPDIR', '')
        if (len_trim(base_temp_dir) == 0) then
            base_temp_dir = get_env('TMP', '')
            if (len_trim(base_temp_dir) == 0) then
                base_temp_dir = get_env('TEMP', '')
                if (len_trim(base_temp_dir) == 0) then
                    base_temp_dir = get_system_temp_dir()
                end if
            end if
        end if

        ! Generate random suffix using system time and process ID
        call generate_random_suffix(random_suffix)

        ! Create unique temp directory path using FPM's join_path
        temp_dir = join_path(trim(base_temp_dir), &
                             trim(prefix)//'_'//trim(random_suffix))

        ! Create the directory
        call mkdir(temp_dir)

    end function create_temp_dir

    subroutine cleanup_temp_dir(temp_dir)
        character(len=*), intent(in) :: temp_dir
        integer :: ios

        if (len_trim(temp_dir) > 0) then
            if (get_os_type() == OS_WINDOWS) then
                ! Windows system - use rmdir command
                call execute_command_line('rmdir /s /q "'//trim(escape_quotes(temp_dir))// &
                                          '" 2>nul', exitstat=ios)
            else
                ! Unix/Linux system - use rm command
                call execute_command_line('rm -rf "'//trim(escape_quotes(temp_dir))//'"', exitstat=ios)
            end if
            ! Don't error on cleanup failure - just warn
            if (ios /= 0) then
                print *, 'Warning: Failed to cleanup temporary directory: '// &
                    trim(temp_dir)
            end if
        end if

    end subroutine cleanup_temp_dir

    function get_temp_file_path(temp_dir, filename) result(file_path)
        character(len=*), intent(in) :: temp_dir, filename
        character(len=:), allocatable :: file_path

        ! Use FPM's cross-platform join_path
        file_path = join_path(trim(temp_dir), trim(filename))

    end function get_temp_file_path

    subroutine generate_random_suffix(suffix)
        character(len=*), intent(out) :: suffix
        integer :: time_vals(8)
        integer :: pid_estimate, thread_id
        integer :: random_num
        integer :: counter
        real :: rand_val
        integer :: rand_int
        save :: counter
        data counter/0/

        ! Get current time with microseconds
        call date_and_time(values=time_vals)

        ! Add thread-specific component
#ifdef _OPENMP
        ! Use OpenMP thread number if available
        thread_id = omp_get_thread_num()
#else
        thread_id = 0
#endif

        ! Thread-safe counter increment
#ifdef _OPENMP
        !$omp atomic
#endif
        counter = counter + 1

        ! Add true randomness to avoid collisions when multiple processes start simultaneously
        ! Initialize random seed with time-based values for better randomness
        block
            integer :: seed_size, i
            integer, allocatable :: seed(:)
            integer :: pid_val, clock_val

            call random_seed(size=seed_size)
            allocate (seed(seed_size))

            ! Get additional entropy sources
            call system_clock(clock_val)
            ! Use getpid if available (runtime check for Windows)
            if (get_os_type() == OS_WINDOWS) then
                ! Windows: use time-based pseudo-PID
                pid_val = time_vals(8)*1000 + time_vals(7)*60 + time_vals(6) + clock_val
            else
                ! Unix/Linux: use real PID
                pid_val = getpid()
            end if

            ! Initialize seed with multiple entropy sources
            do i = 1, seed_size
                seed(i) = time_vals(8) + time_vals(7)*60 + time_vals(6)*3600 + &
                          counter*7919 + i*13 + thread_id*31 + &
                          clock_val*17 + pid_val*23 + &
                          mod(loc(suffix), 65536)*29  ! Memory address for more entropy
            end do

            call random_seed(put=seed)
        end block

        call random_number(rand_val)
        rand_int = int(rand_val*999999)

        ! Create unique random number combining time, thread, counter, and random value
        pid_estimate = time_vals(8)*1000 + time_vals(7)*100 + time_vals(6)
        random_num = time_vals(6)*1000000 + time_vals(7)*10000 + time_vals(8)*100
   random_num = random_num + thread_id*1000000 + counter*10000 + pid_estimate + rand_int

        ! Convert to hex string for shorter but unique suffix
        write (suffix, '(z0)') abs(random_num)

    end subroutine generate_random_suffix

    ! Type-bound procedures for temp_dir_manager

    subroutine temp_dir_create(this, prefix)
        class(temp_dir_manager), intent(inout) :: this
        character(len=*), intent(in) :: prefix

        this%path = create_temp_dir(prefix)

    end subroutine temp_dir_create

    function temp_dir_get_path(this) result(path)
        class(temp_dir_manager), intent(in) :: this
        character(len=:), allocatable :: path

        if (allocated(this%path)) then
            path = this%path
        else
            error stop 'temp_dir_manager: directory not created'
        end if

    end function temp_dir_get_path

    function temp_dir_get_file_path(this, filename) result(file_path)
        class(temp_dir_manager), intent(in) :: this
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: file_path

        if (allocated(this%path)) then
            file_path = get_temp_file_path(this%path, filename)
        else
            error stop 'temp_dir_manager: directory not created'
        end if

    end function temp_dir_get_file_path

    subroutine temp_dir_cleanup(this)
        class(temp_dir_manager), intent(inout) :: this

        if (allocated(this%path)) then
            call cleanup_temp_dir(this%path)
            deallocate (this%path)
        end if

    end subroutine temp_dir_cleanup

    subroutine temp_dir_destroy(this)
        type(temp_dir_manager), intent(inout) :: this

        if (this%cleanup_on_destroy) then
            call this%cleanup()
        end if

    end subroutine temp_dir_destroy

    function get_system_temp_dir() result(temp_dir)
        character(len=:), allocatable :: temp_dir

        ! Use runtime OS detection instead of compile-time
        if (get_os_type() == OS_WINDOWS) then
            ! Windows system - try environment variables
            block
                character(len=256) :: temp_env
                integer :: env_status

                ! Try Windows environment variables first
                call get_environment_variable('TEMP', temp_env, status=env_status)
                if (env_status == 0 .and. len_trim(temp_env) > 0) then
                    temp_dir = trim(temp_env)
                else
                    call get_environment_variable('TMP', temp_env, status=env_status)
                    if (env_status == 0 .and. len_trim(temp_env) > 0) then
                        temp_dir = trim(temp_env)
                    else
                        ! Fallback to Windows default
                        temp_dir = 'C:\Windows\Temp'
                    end if
                end if
            end block
        else
            ! Unix/Linux system
            temp_dir = '/tmp'
        end if

    end function get_system_temp_dir

    function get_current_directory() result(cwd)
        character(len=:), allocatable :: cwd
        character(len=512) :: pwd_env
        integer :: status, unit, iostat

        ! First try environment variable
        call get_environment_variable('PWD', pwd_env, status=status)
        if (status == 0) then
            cwd = trim(pwd_env)
            return
        end if

        ! Try getting current directory via system command
        block
            character(len=:), allocatable :: temp_file, pwd_cmd, rm_cmd
            temp_file = join_path(get_system_temp_dir(), 'fortran_pwd.tmp')
            if (get_os_type() == OS_WINDOWS) then
                pwd_cmd = 'cd > "'//escape_quotes(temp_file)//'"'
                rm_cmd = 'del /f "'//escape_quotes(temp_file)//'"'
            else
                pwd_cmd = 'pwd > "'//escape_quotes(temp_file)//'"'
                rm_cmd = 'rm -f "'//escape_quotes(temp_file)//'"'
            end if
            call execute_command_line(pwd_cmd, wait=.true.)
            open (newunit=unit, file=temp_file, status='old', iostat=iostat)
            if (iostat == 0) then
                read (unit, '(A)', iostat=iostat) pwd_env
                close (unit)
                call execute_command_line(rm_cmd, wait=.true.)
                if (iostat == 0) then
                    cwd = trim(pwd_env)
                    return
                end if
            end if
        end block

        ! Ultimate fallback
        cwd = '.'

    end function get_current_directory

    function get_project_root() result(root_dir)
        character(len=:), allocatable :: root_dir
        character(len=:), allocatable :: current_dir
        character(len=512) :: test_path
        logical :: exists
        integer :: i, last_slash

        ! Get current directory
        current_dir = get_current_directory()

        ! Search upward for project markers (fpm.toml or .git)
        root_dir = current_dir
        do i = 1, 10  ! Limit search depth
            ! Check for fpm.toml
            test_path = trim(root_dir)//'/fpm.toml'
            inquire (file=test_path, exist=exists)
            if (exists) return

            ! Check for .git directory
            test_path = trim(root_dir)//'/.git'
            inquire (file=test_path, exist=exists)
            if (exists) return

            ! Move up one directory
            last_slash = 0
            do last_slash = len_trim(root_dir), 1, -1
                if (get_os_type() == OS_WINDOWS) then
                    if (root_dir(last_slash:last_slash) == '/' .or. &
                        root_dir(last_slash:last_slash) == '\') exit
                else
                    if (root_dir(last_slash:last_slash) == '/') exit
                end if
            end do

            if (last_slash <= 1) then
                ! Reached root directory, use original current directory
                root_dir = current_dir
                return
            end if

            root_dir = root_dir(1:last_slash - 1)
        end do

        ! If not found, use current directory
        root_dir = current_dir

    end function get_project_root

    function path_join(path1, path2) result(joined_path)
        character(len=*), intent(in) :: path1, path2
        character(len=:), allocatable :: joined_path

        if (len_trim(path1) == 0) then
            joined_path = trim(path2)
        else if (len_trim(path2) == 0) then
            joined_path = trim(path1)
        else if (path2(1:1) == '/' .or. (len(path2) >= 2 .and. path2(2:2) == ':')) then
            ! path2 is absolute (Unix or Windows C:\...)
            joined_path = trim(path2)
        else
            ! Join with separator
            if (get_os_type() == OS_WINDOWS) then
                if (path1(len_trim(path1):len_trim(path1)) == '/' .or. &
                    path1(len_trim(path1):len_trim(path1)) == '\') then
                    joined_path = trim(path1)//trim(path2)
                else
                    joined_path = join_path(trim(path1), trim(path2))
                end if
            else
                if (path1(len_trim(path1):len_trim(path1)) == '/') then
                    joined_path = trim(path1)//trim(path2)
                else
                    joined_path = join_path(trim(path1), trim(path2))
                end if
            end if
        end if

    end function path_join

    !> Simple wrapper around FPM's join_path for path construction
    function path_join_simple(path1, path2) result(joined_path)
        character(len=*), intent(in) :: path1, path2
        character(len=:), allocatable :: joined_path

        joined_path = join_path(trim(path1), trim(path2))

    end function path_join_simple

    !> Safe mkdir that creates parent directories and doesn't terminate on failure (unlike FPM's mkdir)
    subroutine mkdir(dir_path)
        character(len=*), intent(in) :: dir_path
        character(len=512) :: command
        integer :: exitstat, cmdstat

        ! Skip if directory already exists
        if (exists(dir_path)) then
            ! Check if it's actually a directory or a file
            block
                logical :: is_dir, path_exists
                integer :: ios
                ! Use inquire with directory attribute for reliable detection
                inquire(file=trim(dir_path), exist=path_exists, iostat=ios)
                if (ios == 0 .and. path_exists) then
                    ! Check if it's a directory by trying to list it
                    if (get_os_type() == OS_WINDOWS) then
                        call execute_command_line('dir "'//trim(escape_quotes(dir_path))//'" >nul 2>&1', exitstat=ios)
                    else
                        call execute_command_line('test -d "'//trim(escape_quotes(dir_path))//'"', exitstat=ios)
                    end if
                    is_dir = (ios == 0)
                    if (.not. is_dir) then
                        print '(a,a,a)', 'ERROR: Path exists as file, not directory: ', trim(dir_path)
                        ! Try to remove the file and create directory
                        if (get_os_type() == OS_WINDOWS) then
                            ! Use attrib to remove any attributes that might prevent deletion
                            call execute_command_line('attrib -R -H -S "'//trim(escape_quotes(dir_path))//'" 2>nul', exitstat=ios)
                            call execute_command_line('del /f /q "'//trim(escape_quotes(dir_path))//'" 2>nul', exitstat=ios)
                        else
                            call execute_command_line('rm -f "'//trim(escape_quotes(dir_path))//'" 2>/dev/null', exitstat=ios)
                        end if
                        if (ios == 0) then
                            print *, 'Removed file, will create directory instead'
                            ! Don't return - continue to create directory below
                        else
                            print *, 'Failed to remove file, cannot create directory'
                            return
                        end if
                    else
                        ! It's already a directory, nothing to do
                        return
                    end if
                end if
            end block
            ! If we removed the file, continue to create directory
        end if

        ! Skip invalid paths that would cause problems
        if (len_trim(dir_path) == 0) return
        if (index(dir_path, '/dev/null') > 0) return

        ! Use runtime OS detection instead of preprocessor
        if (get_os_type() == OS_WINDOWS) then
            ! Force removal of any existing file at this path first 
            call execute_command_line('if exist "'//trim(escape_quotes(dir_path))//'" attrib -R -H -S "'// &
                                     trim(escape_quotes(dir_path))//'" 2>nul', exitstat=exitstat)
            call execute_command_line('if exist "'//trim(escape_quotes(dir_path))//'" del /f /q "'// &
                                     trim(escape_quotes(dir_path))//'" 2>nul', exitstat=exitstat)
            ! Now create directory
            command = 'mkdir "'//trim(escape_quotes(dir_path))//'" 2>nul'
        else
            command = 'mkdir -p "'//trim(escape_quotes(dir_path))//'" 2>/dev/null'
        end if

        call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    end subroutine mkdir

    !> Create a unique test cache directory to avoid race conditions in parallel tests
    function create_test_cache_dir(test_name) result(cache_dir)
        character(len=*), intent(in) :: test_name
        character(len=:), allocatable :: cache_dir
        character(len=32) :: random_suffix
        character(len=:), allocatable :: temp_base

        ! Get temp directory base
        temp_base = create_temp_dir('test_cache_'//trim(test_name))

        ! Use this as the cache directory
        cache_dir = temp_base

        ! Don't call mkdir again - create_temp_dir already creates the directory

    end function create_test_cache_dir

end module temp_utils
