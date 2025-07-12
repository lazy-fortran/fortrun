program test_figure_capture_extended
    use figure_capture
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Extended Figure Capture Tests ==="
    print *
    
    all_tests_passed = .true.
    
    ! Test extended functionality
    if (.not. test_figure_directory_operations()) all_tests_passed = .false.
    if (.not. test_base64_edge_cases()) all_tests_passed = .false.
    if (.not. test_figure_counter_behavior()) all_tests_passed = .false.
    if (.not. test_show_interceptor_edge_cases()) all_tests_passed = .false.
    if (.not. test_error_conditions()) all_tests_passed = .false.
    
    print *
    if (all_tests_passed) then
        print *, "All extended figure capture tests passed!"
        stop 0
    else
        print *, "Some extended figure capture tests failed!"
        stop 1
    end if
    
contains

    function test_figure_directory_operations() result(passed)
        logical :: passed
        character(len=:), allocatable :: figure_data
        
        print *, "Test 1: Figure directory operations"
        passed = .true.
        
        ! Test multiple init/finalize cycles
        call init_figure_capture()
        call finalize_figure_capture()
        call init_figure_capture()
        call finalize_figure_capture()
        
        ! Test operations after finalize
        call finalize_figure_capture()
        call disable_figure_capture()
        call enable_figure_capture()
        
        print *, "  PASS: Directory operations"
        
    end function test_figure_directory_operations

    function test_base64_edge_cases() result(passed)
        logical :: passed
        character(len=:), allocatable :: figure_data
        
        print *, "Test 2: Base64 encoding edge cases"
        passed = .true.
        
        call init_figure_capture()
        
        ! Try to get figure data with no figures (figure number 1)
        figure_data = get_figure_data(1)
        
        ! Test with disabled capture
        call disable_figure_capture()
        figure_data = get_figure_data(1)
        
        ! Re-enable and test
        call enable_figure_capture()
        figure_data = get_figure_data(1)
        
        call finalize_figure_capture()
        
        print *, "  PASS: Base64 edge cases"
        
    end function test_base64_edge_cases

    function test_figure_counter_behavior() result(passed)
        logical :: passed
        character(len=:), allocatable :: figure_data
        
        print *, "Test 3: Figure counter behavior"
        passed = .true.
        
        call init_figure_capture()
        
        ! Test multiple figure captures
        figure_data = get_figure_data(1)
        figure_data = get_figure_data(2)
        figure_data = get_figure_data(3)
        
        ! Test after reinit
        call finalize_figure_capture()
        call init_figure_capture()
        figure_data = get_figure_data(1)
        
        call finalize_figure_capture()
        
        print *, "  PASS: Counter behavior"
        
    end function test_figure_counter_behavior

    function test_show_interceptor_edge_cases() result(passed)
        logical :: passed
        
        print *, "Test 4: Show interceptor edge cases"
        passed = .true.
        
        call init_figure_capture()
        
        ! Test interceptor with disabled capture
        call disable_figure_capture()
        call fortplot_show_interceptor()
        
        ! Test interceptor with enabled capture
        call enable_figure_capture()
        call fortplot_show_interceptor()
        
        ! Test multiple interceptor calls
        call fortplot_show_interceptor()
        call fortplot_show_interceptor()
        
        call finalize_figure_capture()
        
        print *, "  PASS: Interceptor edge cases"
        
    end function test_show_interceptor_edge_cases

    function test_error_conditions() result(passed)
        logical :: passed
        character(len=:), allocatable :: figure_data
        
        print *, "Test 5: Error conditions and boundary cases"
        passed = .true.
        
        ! Test operations without init
        figure_data = get_figure_data(1)
        call enable_figure_capture()
        call disable_figure_capture()
        call fortplot_show_interceptor()
        
        ! Test double finalize
        call finalize_figure_capture()
        call finalize_figure_capture()
        
        ! Test init after finalize
        call init_figure_capture()
        call finalize_figure_capture()
        
        print *, "  PASS: Error conditions"
        
    end function test_error_conditions

end program test_figure_capture_extended