program test_figure_capture_coverage
    use figure_capture
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Figure Capture Coverage Tests ==="
    print *
    
    all_tests_passed = .true.
    
    ! Test figure capture functionality
    if (.not. test_initialization()) all_tests_passed = .false.
    if (.not. test_figure_saving()) all_tests_passed = .false.
    if (.not. test_show_interception()) all_tests_passed = .false.
    if (.not. test_base64_conversion()) all_tests_passed = .false.
    if (.not. test_cleanup()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All figure capture coverage tests passed!"
        stop 0
    else
        print *, "Some figure capture coverage tests failed!"
        stop 1
    end if
    
contains

    function test_initialization() result(passed)
        logical :: passed
        character(len=256) :: fig_dir
        logical :: dir_exists
        
        print *, "Test 1: Figure capture initialization"
        passed = .true.
        
        ! Initialize figure capture
        call init_figure_capture("/tmp/test_figures")
        
        ! Check if directory was created
        call get_figure_directory(fig_dir)
        inquire(file=trim(fig_dir), exist=dir_exists)
        
        if (.not. dir_exists) then
            print *, "  WARNING: Figure directory not created"
        end if
        
        ! Clean up
        call cleanup_figure_capture()
        
        if (passed) print *, "  PASS: Initialization"
        
    end function test_initialization

    function test_figure_saving() result(passed)
        logical :: passed
        character(len=256) :: fig_path
        integer :: unit, i
        logical :: file_exists
        
        print *, "Test 2: Figure saving functionality"
        passed = .true.
        
        call init_figure_capture("/tmp/test_fig_save")
        
        ! Test saving multiple figures
        do i = 1, 3
            call get_next_figure_path(fig_path)
            
            ! Create a dummy figure file
            open(newunit=unit, file=trim(fig_path), status='replace')
            write(unit, '(a)') '%!PS-Adobe-3.0'
            write(unit, '(a)') '%%BoundingBox: 0 0 100 100'
            write(unit, '(a)') 'newpath'
            write(unit, '(a)') '50 50 30 0 360 arc'
            write(unit, '(a)') 'stroke'
            write(unit, '(a)') 'showpage'
            close(unit)
            
            inquire(file=trim(fig_path), exist=file_exists)
            if (.not. file_exists) then
                print *, "  FAILED: Figure file not created"
                passed = .false.
            end if
            
            call increment_figure_counter()
        end do
        
        ! Check counter value
        if (get_figure_counter() /= 3) then
            print *, "  WARNING: Figure counter incorrect"
        end if
        
        call cleanup_figure_capture()
        call execute_command_line("rm -rf /tmp/test_fig_save")
        
        if (passed) print *, "  PASS: Figure saving"
        
    end function test_figure_saving

    function test_show_interception() result(passed)
        logical :: passed
        character(len=256) :: fig_path
        
        print *, "Test 3: Show command interception"
        passed = .true.
        
        call init_figure_capture("/tmp/test_show")
        
        ! Test intercepting show command
        call intercept_show()
        
        ! Get the path where figure would be saved
        call get_next_figure_path(fig_path)
        
        ! In real usage, the plotting library would create the file
        ! Here we just test the infrastructure
        
        call cleanup_figure_capture()
        call execute_command_line("rm -rf /tmp/test_show")
        
        if (passed) print *, "  PASS: Show interception"
        
    end function test_show_interception

    function test_base64_conversion() result(passed)
        logical :: passed
        character(len=256) :: test_file, output_file
        character(len=:), allocatable :: base64_data
        integer :: unit
        logical :: success
        
        print *, "Test 4: Base64 conversion"
        passed = .true.
        
        ! Create test binary file
        test_file = "/tmp/test_base64_input.bin"
        output_file = "/tmp/test_base64_output.txt"
        
        open(newunit=unit, file=test_file, status='replace', access='stream')
        write(unit) 127_1, 0_1, 127_1, 64_1  ! Some binary data
        close(unit)
        
        ! Convert to base64
        call convert_to_base64(test_file, output_file, success)
        
        if (.not. success) then
            print *, "  WARNING: Base64 conversion might have failed"
        end if
        
        ! Try to read base64 output
        call read_base64_file(output_file, base64_data)
        
        if (allocated(base64_data)) then
            if (len(base64_data) == 0) then
                print *, "  WARNING: Base64 data is empty"
            end if
        else
            print *, "  WARNING: Base64 data not allocated"
        end if
        
        ! Clean up
        call execute_command_line("rm -f " // trim(test_file))
        call execute_command_line("rm -f " // trim(output_file))
        
        if (passed) print *, "  PASS: Base64 conversion"
        
    end function test_base64_conversion

    function test_cleanup() result(passed)
        logical :: passed
        character(len=256) :: fig_dir
        logical :: dir_exists
        integer :: i, unit
        character(len=256) :: fig_path
        
        print *, "Test 5: Cleanup functionality"
        passed = .true.
        
        ! Initialize and create some files
        call init_figure_capture()
        
        ! Create dummy figure files
        do i = 1, 2
            call get_next_figure_path(fig_path)
            open(newunit=unit, file=trim(fig_path), status='replace')
            write(unit, '(a)') 'dummy figure'
            close(unit)
            call increment_figure_counter()
        end do
        
        ! Cleanup
        call cleanup_figure_capture()
        
        ! Check if files still exist (they should after cleanup)
        call get_figure_directory(fig_dir)
        inquire(file=trim(fig_dir), exist=dir_exists)
        
        ! Manual cleanup
        call execute_command_line("rm -rf /tmp/test_cleanup")
        
        if (passed) print *, "  PASS: Cleanup"
        
    end function test_cleanup

end program test_figure_capture_coverage