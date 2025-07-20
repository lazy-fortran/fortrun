module temp_utils
#ifdef _OPENMP
    use omp_lib, only: omp_get_thread_num
#endif
    use fpm_filesystem, only: join_path, mkdir, get_temp_filename
    use fpm_environment, only: get_env, get_os_type, OS_WINDOWS
    implicit none
    private
    public :: create_temp_dir, cleanup_temp_dir, get_temp_file_path, temp_dir_manager, &
              get_system_temp_dir, get_current_directory, get_project_root, path_join

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
       temp_dir = join_path(trim(base_temp_dir), trim(prefix)//'_'//trim(random_suffix))

        ! Create the directory using FPM's mkdir (handles cross-platform)
        call mkdir(temp_dir)

    end function create_temp_dir

    subroutine cleanup_temp_dir(temp_dir)
        character(len=*), intent(in) :: temp_dir
        integer :: ios

        if (len_trim(temp_dir) > 0) then
#ifdef _WIN32
            ! Check if running under MSYS2
            block
                character(len=256) :: msystem
                integer :: status
                call get_environment_variable('MSYSTEM', msystem, status=status)
                if (status == 0 .and. len_trim(msystem) > 0) then
                    ! MSYS2 environment
                    call execute_command_line('rm -rf "'//trim(temp_dir)//'"', &
                                              exitstat=ios)
                else
                    ! Native Windows
                    call execute_command_line('rmdir /s /q "'//trim(temp_dir)// &
                                              '" 2>nul', exitstat=ios)
                end if
            end block
#else
            call execute_command_line('rm -rf "'//trim(temp_dir)//'"', exitstat=ios)
#endif
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

        ! Create unique random number combining time, thread, and counter
        pid_estimate = time_vals(8)*1000 + time_vals(7)*100 + time_vals(6)
        random_num = time_vals(6)*1000000 + time_vals(7)*10000 + time_vals(8)*100
        random_num = random_num + thread_id*1000000 + counter*10000 + pid_estimate

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
        character(len=256) :: msystem
        integer :: status

#ifdef _WIN32
        ! Always use Windows temp directory on Windows, even under MSYS2
        ! This avoids path separator issues
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
#else
        temp_dir = '/tmp'
#endif

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
            temp_file = trim(get_system_temp_dir())//'/fortran_pwd.tmp'
#ifdef _WIN32
            pwd_cmd = 'cd > "'//temp_file//'"'
            rm_cmd = 'del /f "'//temp_file//'"'
#else
            pwd_cmd = 'pwd > "'//temp_file//'"'
            rm_cmd = 'rm -f "'//temp_file//'"'
#endif
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
#ifdef _WIN32
                if (root_dir(last_slash:last_slash) == '/' .or. &
                    root_dir(last_slash:last_slash) == '\') exit
#else
                if (root_dir(last_slash:last_slash) == '/') exit
#endif
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
#ifdef _WIN32
            if (path1(len_trim(path1):len_trim(path1)) == '/' .or. &
                path1(len_trim(path1):len_trim(path1)) == '\') then
                joined_path = trim(path1)//trim(path2)
            else
                joined_path = trim(path1)//'\'//trim(path2)
            end if
#else
            if (path1(len_trim(path1):len_trim(path1)) == '/') then
                joined_path = trim(path1)//trim(path2)
            else
                joined_path = trim(path1)//'/'//trim(path2)
            end if
#endif
        end if

    end function path_join

    !> Simple wrapper around FPM's join_path for path construction
    function path_join_simple(path1, path2) result(joined_path)
        character(len=*), intent(in) :: path1, path2
        character(len=:), allocatable :: joined_path

        joined_path = join_path(trim(path1), trim(path2))

    end function path_join_simple

end module temp_utils
