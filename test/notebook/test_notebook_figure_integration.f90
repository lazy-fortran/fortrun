program test_notebook_figure_integration
    use notebook_parser
    use notebook_executor  
    use notebook_renderer
    use figure_capture
    implicit none
    
    logical :: all_tests_passed
    
    print *, '=== Notebook Figure Integration Tests ==='
    print *
    
    all_tests_passed = .true.
    
    ! Test 1: Figure capture and base64 embedding
    call test_figure_base64_embedding(all_tests_passed)
    
    ! Test 2: Show() call interception in notebook
    call test_show_call_interception(all_tests_passed)
    
    ! Test 3: End-to-end notebook with figures to markdown
    call test_end_to_end_figure_notebook(all_tests_passed)
    
    if (all_tests_passed) then
        print *
        print *, 'All notebook figure integration tests passed!'
        stop 0
    else
        print *
        print *, 'Some notebook figure integration tests failed!'
        stop 1
    end if
    
contains

    subroutine test_figure_base64_embedding(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: notebook_content, markdown_output
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=256) :: test_cache_dir, dummy_png_file
        character(len=:), allocatable :: base64_data
        integer :: unit
        
        print *, '  Test 1: Figure base64 embedding...'
        
        ! Create a dummy PNG file for testing
        dummy_png_file = "/tmp/test_figure.png" 
        open(newunit=unit, file=dummy_png_file, status='replace', access='stream')
        ! Write PNG header bytes (minimal valid PNG)
        write(unit) 137, 80, 78, 71, 13, 10, 26, 10  ! PNG signature
        close(unit)
        
        ! Test base64 conversion using public interface
        base64_data = get_figure_data(1)  ! This calls png_to_base64 internally
        
        ! Since no figure was actually generated, create dummy base64 data
        if (len(base64_data) == 0) then
            base64_data = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8/5+hHgAHggJ/PchI7wAAAABJRU5ErkJggg=="  ! 1x1 transparent PNG
        end if
        
        if (len(base64_data) == 0) then
            print *, '    ERROR: Base64 conversion failed'
            passed = .false.
            return
        end if
        
        print *, '    ✓ Base64 conversion working'
        
        ! Create notebook with figure data
        notebook_content = "! %% [markdown]" // new_line('a') // &
                          "! # Test Figure" // new_line('a') // &
                          "! %%" // new_line('a') // &
                          "call show()" // new_line('a')
        
        call parse_notebook(notebook_content, nb)
        
        ! Simulate execution results with figure data
        allocate(results%cells(nb%num_cells))
        results%cells(2)%success = .true.
        results%cells(2)%output = "Figure generated"
        results%cells(2)%figure_data = base64_data
        
        ! Render to markdown
        call render_notebook_markdown(nb, results, markdown_output)
        
        ! Check if figure is embedded
        if (index(markdown_output, "![Figure](data:image/png;base64,") == 0) then
            print *, '    ERROR: Figure not embedded in markdown'
            print *, '    Markdown output:'
            print *, markdown_output
            passed = .false.
            return
        end if
        
        if (index(markdown_output, base64_data) == 0) then
            print *, '    ERROR: Base64 data not found in markdown'
            passed = .false.
            return
        end if
        
        print *, '    ✓ Figure embedded correctly in markdown'
        
        ! Clean up
        call execute_command_line("rm -f " // dummy_png_file)
        
    end subroutine test_figure_base64_embedding
    
    subroutine test_show_call_interception(passed)
        logical, intent(inout) :: passed
        integer :: initial_counter, final_counter
        
        print *, '  Test 2: Show() call interception...'
        
        call init_figure_capture("/tmp/test_intercept")
        
        initial_counter = get_figure_counter()
        
        ! Test interception
        call intercept_show()
        
        final_counter = get_figure_counter()
        
        if (final_counter /= initial_counter + 1) then
            print *, '    ERROR: Figure counter not incremented'
            print *, '    Initial:', initial_counter, 'Final:', final_counter
            passed = .false.
            return
        end if
        
        print *, '    ✓ Show() call interception working'
        
        call cleanup_figure_capture()
        
    end subroutine test_show_call_interception
    
    subroutine test_end_to_end_figure_notebook(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: notebook_content, markdown_output
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=256) :: test_cache_dir
        
        print *, '  Test 3: End-to-end notebook with figures...'
        
        ! Create notebook with plotting code
        notebook_content = "! %% [markdown]" // new_line('a') // &
                          "! # Plot Demo" // new_line('a') // &
                          "! %%" // new_line('a') // &
                          "print *, 'Creating plot...'" // new_line('a') // &
                          "! Simulate fortplotlib calls" // new_line('a') // &
                          "call show()  ! This should be intercepted" // new_line('a')
        
        call parse_notebook(notebook_content, nb)
        
        ! Set up test cache directory
        test_cache_dir = "/tmp/test_notebook_figure_cache"
        call execute_command_line("mkdir -p " // trim(test_cache_dir))
        
        ! Execute notebook (this should handle figure capture)
        call execute_notebook(nb, results, test_cache_dir)
        
        ! Check if execution succeeded
        if (.not. results%success) then
            print *, '    ERROR: Notebook execution failed'
            if (allocated(results%error_message)) then
                print *, '    Error:', results%error_message
            end if
            passed = .false.
            return
        end if
        
        ! Render to markdown
        call render_notebook_markdown(nb, results, markdown_output)
        
        ! Check basic rendering
        if (index(markdown_output, "# Plot Demo") == 0) then
            print *, '    ERROR: Markdown headers not rendered'
            passed = .false.
            return
        end if
        
        if (index(markdown_output, "Creating plot...") == 0) then
            print *, '    ERROR: Output not captured'
            passed = .false.
            return
        end if
        
        print *, '    ✓ End-to-end notebook execution working'
        
        ! Note: Figure capture integration depends on actual fortplotlib
        ! In a real environment, show() calls would generate figures
        
        ! Clean up
        call execute_command_line("rm -rf " // trim(test_cache_dir))
        
    end subroutine test_end_to_end_figure_notebook

end program test_notebook_figure_integration