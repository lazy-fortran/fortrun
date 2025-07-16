program test_logger
    use logger
    implicit none
    
    integer :: test_count = 0, pass_count = 0
    
    ! Test verbose level mapping
    call test_verbose_level_mapping()
    
    ! Test log level filtering
    call test_log_filtering()
    
    ! Test debug categories
    call test_debug_categories()
    
    call print_results()
    
contains

    subroutine test_verbose_level_mapping()
        character(len=*), parameter :: test_name = "Verbose level mapping"
        logical :: success
        
        test_count = test_count + 1
        success = .true.
        
        ! Test level 0 (quiet) - only errors
        call set_verbose_level(0)
        if (get_current_log_level() /= 0) success = .false.
        
        ! Test level 1 (verbose) - info and above
        call set_verbose_level(1) 
        if (get_current_log_level() /= 1) success = .false.
        
        ! Test level 2 (very verbose) - debug and above
        call set_verbose_level(2)
        if (get_current_log_level() /= 2) success = .false.
        
        ! Test level 3 (ultra verbose) - verbose debug
        call set_verbose_level(3)
        if (get_current_log_level() /= 3) success = .false.
        
        if (success) then
            pass_count = pass_count + 1
            print *, "PASS: " // test_name
        else
            print *, "FAIL: " // test_name
        end if
    end subroutine
    
    subroutine test_log_filtering()
        character(len=*), parameter :: test_name = "Log level filtering"
        logical :: success
        
        test_count = test_count + 1
        success = .true.
        
        ! Test that debug messages are filtered at level 1
        call set_verbose_level(1)
        if (should_log(2)) success = .false.     ! Level 2 (info) should not show at verbose 1
        if (should_log(3)) success = .false.     ! Level 3 (debug) should not show at verbose 1
        if (.not. should_log(0)) success = .false.  ! Level 0 (error) should always show
        if (.not. should_log(1)) success = .false.  ! Level 1 (warn) should show at verbose 1
        
        ! Test that info messages show at level 2
        call set_verbose_level(2)
        if (should_log(3)) success = .false.     ! Level 3 (debug) should not show at verbose 2
        if (.not. should_log(2)) success = .false.  ! Level 2 (info) should show at verbose 2
        if (.not. should_log(1)) success = .false.  ! Level 1 (warn) should show at verbose 2
        if (.not. should_log(0)) success = .false.  ! Level 0 (error) should always show
        
        ! Test that all messages are shown at level 3
        call set_verbose_level(3)
        if (.not. should_log(3)) success = .false.  ! All levels should show at verbose 3
        if (.not. should_log(2)) success = .false.
        if (.not. should_log(1)) success = .false.
        if (.not. should_log(0)) success = .false.
        
        if (success) then
            pass_count = pass_count + 1
            print *, "PASS: " // test_name
        else
            print *, "FAIL: " // test_name
        end if
    end subroutine
    
    subroutine test_debug_categories()
        character(len=*), parameter :: test_name = "Debug categories enabled correctly"
        logical :: success
        
        test_count = test_count + 1
        success = .true.
        
        ! Test that debug categories are enabled at level 3
        call set_verbose_level(3)
        if (.not. is_debug_category_enabled("parsing")) success = .false.
        if (.not. is_debug_category_enabled("lexing")) success = .false.
        if (.not. is_debug_category_enabled("semantic")) success = .false.
        if (.not. is_debug_category_enabled("codegen")) success = .false.
        
        ! Test that only some categories are enabled at level 2
        call set_verbose_level(2)
        if (.not. is_debug_category_enabled("parsing")) success = .false.
        if (is_debug_category_enabled("lexing")) success = .false.  ! Should be disabled
        
        if (success) then
            pass_count = pass_count + 1
            print *, "PASS: " // test_name
        else
            print *, "FAIL: " // test_name
        end if
    end subroutine
    
    subroutine print_results()
        print *, ""
        write(*, '(A, I0, A, I0, A)') 'Tests: ', pass_count, '/', test_count, ' passed'
        if (pass_count /= test_count) then
            write(*, '(A, I0, A)') 'FAILED: ', test_count - pass_count, ' tests failed'
            stop 1
        end if
    end subroutine

end program test_logger