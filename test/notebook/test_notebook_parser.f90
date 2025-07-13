program test_notebook_parser
    use notebook_parser
    implicit none
    
    character(len=1024) :: test_content
    type(notebook_t) :: notebook
    integer :: i
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing notebook parser..."
    
    ! Test 1: Parse simple notebook with markdown and code cells
    call test_simple_notebook()
    
    ! Test 2: Parse notebook with no initial marker (defaults to code)
    call test_default_code_cell()
    
    ! Test 3: Parse notebook with multiple cells
    call test_multiple_cells()
    
    ! Test 4: Parse notebook with empty cells
    call test_empty_cells()
    
    ! Test 5: Parse notebook with nested code
    call test_nested_code()
    
    if (all_tests_passed) then
        print *, "All notebook parser tests passed!"
        stop 0
    else
        print *, "Some notebook parser tests failed!"
        stop 1
    end if
    
contains

    subroutine test_simple_notebook()
        character(len=:), allocatable :: content
        type(notebook_t) :: nb
        
        print *, "  Test 1: Simple notebook..."
        
        content = "! %% [markdown]" // new_line('a') // &
                  "! # Title" // new_line('a') // &
                  "! %%" // new_line('a') // &
                  "x = 1" // new_line('a') // &
                  "print *, x"
        
        call parse_notebook(content, nb)
        
        if (nb%num_cells /= 2) then
            print *, "    FAILED: Expected 2 cells, got", nb%num_cells
            all_tests_passed = .false.
            return
        end if
        
        if (nb%cells(1)%cell_type /= CELL_MARKDOWN) then
            print *, "    FAILED: First cell should be markdown"
            all_tests_passed = .false.
            return
        end if
        
        if (nb%cells(2)%cell_type /= CELL_CODE) then
            print *, "    FAILED: Second cell should be code"
            all_tests_passed = .false.
            return
        end if
        
        if (trim(nb%cells(1)%content) /= "# Title") then
            print *, "    FAILED: Markdown content mismatch"
            all_tests_passed = .false.
            return
        end if
        
        print *, "    PASSED"
    end subroutine test_simple_notebook
    
    subroutine test_default_code_cell()
        character(len=:), allocatable :: content
        type(notebook_t) :: nb
        
        print *, "  Test 2: Default code cell..."
        
        content = "program test" // new_line('a') // &
                  "print *, 'hello'" // new_line('a') // &
                  "end program"
        
        call parse_notebook(content, nb)
        
        if (nb%num_cells /= 1) then
            print *, "    FAILED: Expected 1 cell, got", nb%num_cells
            all_tests_passed = .false.
            return
        end if
        
        if (nb%cells(1)%cell_type /= CELL_CODE) then
            print *, "    FAILED: Default cell should be code"
            all_tests_passed = .false.
            return
        end if
        
        print *, "    PASSED"
    end subroutine test_default_code_cell
    
    subroutine test_multiple_cells()
        character(len=:), allocatable :: content
        type(notebook_t) :: nb
        
        print *, "  Test 3: Multiple cells..."
        
        content = "! %% [markdown]" // new_line('a') // &
                  "! # Section 1" // new_line('a') // &
                  "! %%" // new_line('a') // &
                  "x = 1" // new_line('a') // &
                  "! %% [markdown]" // new_line('a') // &
                  "! # Section 2" // new_line('a') // &
                  "! %%" // new_line('a') // &
                  "y = 2"
        
        call parse_notebook(content, nb)
        
        if (nb%num_cells /= 4) then
            print *, "    FAILED: Expected 4 cells, got", nb%num_cells
            all_tests_passed = .false.
            return
        end if
        
        if (nb%cells(1)%cell_type /= CELL_MARKDOWN .or. &
            nb%cells(2)%cell_type /= CELL_CODE .or. &
            nb%cells(3)%cell_type /= CELL_MARKDOWN .or. &
            nb%cells(4)%cell_type /= CELL_CODE) then
            print *, "    FAILED: Cell type sequence mismatch"
            all_tests_passed = .false.
            return
        end if
        
        print *, "    PASSED"
    end subroutine test_multiple_cells
    
    subroutine test_empty_cells()
        character(len=:), allocatable :: content
        type(notebook_t) :: nb
        
        print *, "  Test 4: Empty cells..."
        
        content = "! %%" // new_line('a') // &
                  "! %% [markdown]" // new_line('a') // &
                  "! %%" // new_line('a')
        
        call parse_notebook(content, nb)
        
        if (nb%num_cells /= 3) then
            print *, "    FAILED: Expected 3 cells, got", nb%num_cells
            all_tests_passed = .false.
            return
        end if
        
        ! Check that empty cells are preserved
        if (len_trim(nb%cells(1)%content) /= 0) then
            print *, "    FAILED: First cell should be empty"
            all_tests_passed = .false.
            return
        end if
        
        print *, "    PASSED"
    end subroutine test_empty_cells
    
    subroutine test_nested_code()
        character(len=:), allocatable :: content
        type(notebook_t) :: nb
        
        print *, "  Test 5: Nested code structures..."
        
        content = "! %%" // new_line('a') // &
                  "program main" // new_line('a') // &
                  "  ! This is a comment, not a cell marker" // new_line('a') // &
                  "  x = 1  ! %% This should not be parsed as marker" // new_line('a') // &
                  "end program"
        
        call parse_notebook(content, nb)
        
        if (nb%num_cells /= 1) then
            print *, "    FAILED: Expected 1 cell, got", nb%num_cells
            all_tests_passed = .false.
            return
        end if
        
        ! Check that the content includes all lines
        if (index(nb%cells(1)%content, "This is a comment") == 0) then
            print *, "    FAILED: Comment line missing from content"
            all_tests_passed = .false.
            return
        end if
        
        print *, "    PASSED"
    end subroutine test_nested_code
    
end program test_notebook_parser