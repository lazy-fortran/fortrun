module temp_utils
    implicit none
    private
    public :: create_temp_dir, cleanup_temp_dir, get_temp_file_path, temp_dir_manager

    ! Type for managing temporary directories with automatic cleanup
    type :: temp_dir_manager
        character(len=:), allocatable :: path
        logical :: cleanup_on_destroy = .true.
    contains
        procedure :: create => temp_dir_create
        procedure :: get_file_path => temp_dir_get_file_path
        procedure :: cleanup => temp_dir_cleanup
        procedure :: deep_copy => temp_dir_deep_copy
        procedure :: assign => temp_dir_assign
        generic :: assignment(=) => assign
        final :: temp_dir_destroy
    end type temp_dir_manager

contains

    subroutine create_temp_dir(prefix, temp_dir)
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable, intent(out) :: temp_dir
        character(len=32) :: random_suffix
        character(len=256) :: base_temp_dir
        integer :: ios

        ! Get system temp directory
        call get_environment_variable('TMPDIR', base_temp_dir, status=ios)
        if (ios /= 0) then
            base_temp_dir = '/tmp'
        end if

        ! Generate random suffix using system time and process ID
        call generate_random_suffix(random_suffix)

        ! Create unique temp directory path
        temp_dir = trim(base_temp_dir)//'/'//trim(prefix)//'_'//trim(random_suffix)

        ! Create the directory
        call execute_command_line('mkdir -p "'//temp_dir//'"', exitstat=ios)
        if (ios /= 0) then
            error stop 'Failed to create temporary directory: '//temp_dir
        end if

    end subroutine create_temp_dir

    subroutine cleanup_temp_dir(temp_dir)
        character(len=*), intent(in) :: temp_dir
        integer :: ios

        if (len_trim(temp_dir) > 0) then
            call execute_command_line('rm -rf "'//trim(temp_dir)//'"', exitstat=ios)
            ! Don't error on cleanup failure - just warn
            if (ios /= 0) then
             print *, 'Warning: Failed to cleanup temporary directory: '//trim(temp_dir)
            end if
        end if

    end subroutine cleanup_temp_dir

    subroutine get_temp_file_path(temp_dir, filename, file_path)
        character(len=*), intent(in) :: temp_dir, filename
        character(len=:), allocatable, intent(out) :: file_path

        file_path = trim(temp_dir)//'/'//trim(filename)

    end subroutine get_temp_file_path

    subroutine generate_random_suffix(suffix)
        character(len=*), intent(out) :: suffix
        integer :: time_vals(8)
        integer :: pid_estimate
        integer :: random_num

        ! Get current time
        call date_and_time(values=time_vals)

        ! Create pseudo-random number from time and a simple PID estimate
        pid_estimate = time_vals(8)*1000 + time_vals(7)*100 + time_vals(6)
        random_num = time_vals(6)*1000000 + time_vals(7)*1000 + time_vals(8)
        random_num = random_num + pid_estimate

        ! Convert to hex string for shorter suffix
        write (suffix, '(z0)') abs(random_num)

    end subroutine generate_random_suffix

    ! Type-bound procedures for temp_dir_manager

    subroutine temp_dir_create(this, prefix)
        class(temp_dir_manager), intent(inout) :: this
        character(len=*), intent(in) :: prefix

        call create_temp_dir(prefix, this%path)

    end subroutine temp_dir_create

    subroutine temp_dir_get_file_path(this, filename, file_path)
        class(temp_dir_manager), intent(in) :: this
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: file_path

        if (allocated(this%path)) then
            call get_temp_file_path(this%path, filename, file_path)
        else
            error stop 'temp_dir_manager: directory not created'
        end if

    end subroutine temp_dir_get_file_path

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

    ! Deep copy procedures for temp_dir_manager
    function temp_dir_deep_copy(this) result(copy)
        class(temp_dir_manager), intent(in) :: this
        type(temp_dir_manager) :: copy

        copy%cleanup_on_destroy = this%cleanup_on_destroy
        if (allocated(this%path)) then
            copy%path = this%path
        end if
    end function temp_dir_deep_copy

    subroutine temp_dir_assign(lhs, rhs)
        class(temp_dir_manager), intent(out) :: lhs
        type(temp_dir_manager), intent(in) :: rhs

        lhs%cleanup_on_destroy = rhs%cleanup_on_destroy
        if (allocated(rhs%path)) then
            lhs%path = rhs%path
        end if
    end subroutine temp_dir_assign

end module temp_utils
