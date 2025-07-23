program test_os_detection
    use fpm_environment, only: get_os_type, OS_WINDOWS, OS_LINUX, OS_MACOS, OS_NAME
    implicit none
    
    integer :: os_type
    character(len=255) :: os_env, ostype_env
    integer :: length, rc
    
    ! Get the OS type
    os_type = get_os_type()
    
    write(*,'(A,I0)') "Detected OS type: ", os_type
    write(*,'(A,A)') "OS name: ", OS_NAME(os_type)
    write(*,'(A,L1)') "Is Windows: ", os_type == OS_WINDOWS
    
    ! Check environment variables that the detection function uses
    call get_environment_variable('OS', os_env, length, rc)
    if (rc == 0 .and. length > 0) then
        write(*,'(A,A)') "OS environment variable: '", trim(os_env), "'"
    else
        write(*,'(A)') "OS environment variable not set or empty"
    end if
    
    call get_environment_variable('OSTYPE', ostype_env, length, rc)
    if (rc == 0 .and. length > 0) then
        write(*,'(A,A)') "OSTYPE environment variable: '", trim(ostype_env), "'"
    else
        write(*,'(A)') "OSTYPE environment variable not set or empty"
    end if
    
    ! Check other relevant environment variables
    call get_environment_variable('RUNNER_OS', os_env, length, rc)
    if (rc == 0 .and. length > 0) then
        write(*,'(A,A)') "RUNNER_OS: '", trim(os_env), "'"
    end if
    
    call get_environment_variable('GITHUB_ACTIONS', os_env, length, rc)
    if (rc == 0 .and. length > 0) then
        write(*,'(A,A)') "GITHUB_ACTIONS: '", trim(os_env), "'"
    end if
    
    call get_environment_variable('MSYSTEM', os_env, length, rc)
    if (rc == 0 .and. length > 0) then
        write(*,'(A,A)') "MSYSTEM: '", trim(os_env), "'"
    end if
    
end program test_os_detection