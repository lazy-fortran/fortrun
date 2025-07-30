program test_cli_output_clean
    !> Test that fortran executable output is clean in quiet mode
    use temp_utils, only: temp_dir_manager, create_temp_file, create_temp_dir, path_join
    use system_utils, only: sys_run_command_with_exit_code, sys_remove_file
    use fpm_environment, only: get_os_type, OS_WINDOWS, get_env
    implicit none

    character(len=512) :: command, test_file, output_file, exit_file
    character(len=1024) :: output_content, line
    character(len=:), allocatable :: temp_dir, exe_path_file
    integer :: exit_code, unit, iostat
    logical :: test_passed, found_unwanted
    type(temp_dir_manager) :: temp_mgr
    character(len=:), allocatable :: fortran_exe
    character(len=512) :: find_command
    integer :: find_unit, find_iostat

    print *, "=== Output Behavior Tests ==="
    print *

    ! Create temp directory for this test
    temp_dir = create_temp_dir('cli_output_test')
    exe_path_file = path_join(temp_dir, 'fortran_exe_path.txt')

    ! Get fortran executable path by finding it
    if (get_os_type() == OS_WINDOWS) then
        find_command = 'dir /s /b build\*.exe 2>nul | findstr "app\\fortrun.exe" | findstr /v "test" > "' // exe_path_file // '"'
    else
        find_command = 'find build -name fortrun -type f | grep -v test | head -1 > "' // exe_path_file // '"'
    end if

    call execute_command_line(trim(find_command), wait=.true.)

    ! Read the executable path
    fortran_exe = ''
    open (newunit=find_unit, file=exe_path_file, status='old', iostat=find_iostat)
    if (find_iostat == 0) then
        read (find_unit, '(a)') line
        close (find_unit)
        fortran_exe = trim(line)
    else
        print *, "ERROR: Could not find fortrun executable"
        stop 1
    end if

    ! Create temp directory
    call temp_mgr%create('output_behavior_test')

    ! Test 1: Cache miss output should show only "Compiling... X.Xs" and program output
    print *, "Test 1: Cache miss output (quiet mode)"

    ! Create a test file
    test_file = temp_mgr%get_file_path('cache_miss_test.f90')
    open (newunit=unit, file=test_file, status='replace')
    write (unit, '(a)') 'program test'
    write (unit, '(a)') '  print *, "Hello from cache miss"'
    write (unit, '(a)') 'end program test'
    close (unit)

    ! Run fortran in quiet mode with custom cache directory
    output_file = temp_mgr%get_file_path('output1.txt')
    exit_file = temp_mgr%get_file_path('exit1.txt')
    command = trim(fortran_exe) // ' --cache-dir "' // trim(temp_mgr%get_path()) // '/test_cache" "' // trim(test_file) // '"'
    call sys_run_command_with_exit_code(command, output_file, exit_file)

    ! Read exit code
    open (newunit=unit, file=exit_file, status='old')
    read (unit, *) exit_code
    close (unit)

    ! Read output
    output_content = ''
    open (newunit=unit, file=output_file, status='old', iostat=iostat)
    if (iostat == 0) then
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            output_content = trim(output_content)//trim(line)//char(10)
        end do
        close (unit)
    end if

    if (exit_code == 0) then
        ! Check output format
        found_unwanted = .false.

        ! Should not contain FPM build progress
        if (index(output_content, '[') > 0 .and. index(output_content, '%]') > 0) then
            print *, "  FAIL: Found FPM build progress in output"
            found_unwanted = .true.
        end if

        ! Should not contain "Project is up to date"
        if (index(output_content, 'Project is up to date') > 0) then
            print *, "  FAIL: Found 'Project is up to date' in output"
            found_unwanted = .true.
        end if

        ! Should not contain "Project compiled successfully"
        if (index(output_content, 'Project compiled successfully') > 0) then
            print *, "  FAIL: Found 'Project compiled successfully' in output"
            found_unwanted = .true.
        end if

        ! Should contain "Compiling..."
        if (index(output_content, 'Compiling...') == 0) then
            print *, "  FAIL: Missing 'Compiling...' indicator"
            found_unwanted = .true.
        end if

        ! Should contain program output
        if (index(output_content, 'Hello from cache miss') == 0) then
            print *, "  FAIL: Missing program output"
            found_unwanted = .true.
        end if

        if (.not. found_unwanted) then
            print *, "  PASS: Cache miss output is clean"
        end if
    else
        print *, "  FAIL: Command failed with exit code:", exit_code
    end if

    ! Test 2: Cache hit output should show only program output
    print *
    print *, "Test 2: Cache hit output (quiet mode)"

    ! Run again (should hit cache)
    output_file = temp_mgr%get_file_path('output2.txt')
    exit_file = temp_mgr%get_file_path('exit2.txt')
    call sys_run_command_with_exit_code(command, output_file, exit_file)

    ! Read exit code
    open (newunit=unit, file=exit_file, status='old')
    read (unit, *) exit_code
    close (unit)

    ! Read output
    output_content = ''
    open (newunit=unit, file=output_file, status='old', iostat=iostat)
    if (iostat == 0) then
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            output_content = trim(output_content)//trim(line)//char(10)
        end do
        close (unit)
    end if

    if (exit_code == 0) then
        ! Check output format
        found_unwanted = .false.

        ! Should not contain "Compiling..."
        if (index(output_content, 'Compiling...') > 0) then
            print *, "  FAIL: Found 'Compiling...' in cache hit"
            found_unwanted = .true.
        end if

        ! Should not contain FPM messages
      if (index(output_content, 'Project') > 0 .or. index(output_content, '[') > 0) then
            print *, "  FAIL: Found FPM output in cache hit"
            found_unwanted = .true.
        end if

        ! Should contain only program output
 if (index(output_content, 'Hello from cache miss') > 0 .and. .not. found_unwanted) then
            print *, "  PASS: Cache hit output contains only program output"
        else
            print *, "  FAIL: Unexpected cache hit output"
        end if
    else
        print *, "  FAIL: Command failed with exit code:", exit_code
    end if

    ! Test 3: Verbose mode should show FPM output
    print *
    print *, "Test 3: Verbose mode output"

    output_file = temp_mgr%get_file_path('output3.txt')
    exit_file = temp_mgr%get_file_path('exit3.txt')
    command = trim(fortran_exe) // ' -v --cache-dir "' // trim(temp_mgr%get_path()) // '/test_cache" "' // trim(test_file) // '"'
    call sys_run_command_with_exit_code(command, output_file, exit_file)

    ! Read exit code
    open (newunit=unit, file=exit_file, status='old')
    read (unit, *) exit_code
    close (unit)

    ! Read output
    output_content = ''
    open (newunit=unit, file=output_file, status='old', iostat=iostat)
    if (iostat == 0) then
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            output_content = trim(output_content)//trim(line)//char(10)
        end do
        close (unit)
    end if

    if (exit_code == 0) then
        ! Should contain FPM messages in verbose mode
        if (index(output_content, 'Cache hit:') > 0 .or. index(output_content, 'Cache miss:') > 0) then
            print *, "  PASS: Verbose mode shows cache status"
        else
            print *, "  FAIL: Verbose mode missing cache status"
        end if
    else
        print *, "  FAIL: Command failed with exit code:", exit_code
    end if

    ! Clean up
    call sys_remove_file(test_file)

    print *
    print *, "All output behavior tests completed"

end program test_cli_output_clean
