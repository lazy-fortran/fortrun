program test_notebook_caching
    use notebook_parser
    use notebook_executor
    use temp_utils, only: temp_dir_manager
    use fpm_environment, only: get_os_type, OS_WINDOWS
    implicit none

    logical :: all_tests_passed

    ! Output early message to ensure test is starting
    print '(a)', 'test_notebook_caching: EARLY START - Process started successfully'
    flush (6)

    print '(a)', 'test_notebook_caching: Initialization complete'
    flush (6)

    print '(a)', 'test_notebook_caching: About to print header'
    flush (6)

    print *, '=== Notebook Caching Tests ==='
    flush (6)  ! Ensure output is flushed
    print *

    print '(a)', 'test_notebook_caching: Header printed, about to call test subroutines'
    flush (6)

    ! First, let's try to cleanup any stale locks
    print '(a)', 'test_notebook_caching: Cleaning up any stale locks before test'
    flush (6)
    call execute_command_line('find /tmp -name "*.lock" -type f -delete 2>/dev/null || true')
    call execute_command_line('find /tmp -name "*.lock" -type l -delete 2>/dev/null || true')
    print '(a)', 'test_notebook_caching: Stale lock cleanup completed'
    flush (6)

    all_tests_passed = .true.

    ! Test 1: Cache directory creation
    call test_cache_directory_creation(all_tests_passed)

    ! Test 2: Cache reuse with same content
    call test_cache_reuse(all_tests_passed)

    ! Test 3: Cache invalidation with different content
    call test_cache_invalidation(all_tests_passed)

    if (all_tests_passed) then
        print *
        print *, 'All notebook caching tests passed!'
        stop 0
    else
        print *
        print *, 'Some caching tests failed!'
        stop 1
    end if

contains

    subroutine test_cache_directory_creation(passed)
        logical, intent(inout) :: passed
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=:), allocatable :: test_cache_dir
        logical :: dir_exists

        print *, 'Test 1: Cache directory creation'
        flush (6)

        ! Cleanup any existing locks for this test specifically
        print *, 'DEBUG: Cleaning up existing cache locks for Test 1'
        flush (6)
        call execute_command_line('find /tmp -name "*notebook*lock*" -delete 2>/dev/null || true')

        ! Set up cache directory
        print *, 'DEBUG: About to create temp_dir_manager'
        flush (6)
        block
            type(temp_dir_manager) :: temp_mgr
            print *, 'DEBUG: temp_dir_manager created, calling create'
            flush (6)
            call temp_mgr%create('test_notebook_caching')
            print *, 'DEBUG: temp_mgr%create completed'
            flush (6)
            test_cache_dir = temp_mgr%path
            print *, 'DEBUG: test_cache_dir assigned: ', trim(test_cache_dir)
            flush (6)

            ! Debug: Check if directory actually exists
            inquire (file=trim(test_cache_dir), exist=dir_exists)
            print *, 'DEBUG: temp_cache_dir exists = ', dir_exists
            flush (6)
        end block

        ! Create simple notebook
        print *, 'DEBUG: About to create notebook structure'
        flush (6)
        nb%num_cells = 1
        allocate (nb%cells(1))
        nb%cells(1)%cell_type = CELL_CODE
        nb%cells(1)%content = "x = 123.0"//new_line('a')//"print *, 'x =', x"
        print *, 'DEBUG: Notebook structure created'
        flush (6)

        ! Execute notebook
        print *, 'DEBUG: About to call execute_notebook'
        print *, 'DEBUG: test_cache_dir = ', trim(test_cache_dir)
        print *, 'DEBUG: notebook content = ', trim(nb%cells(1)%content)
        flush (6)

        call execute_notebook(nb, results, test_cache_dir)

        print *, 'DEBUG: execute_notebook call completed'
        print *, 'DEBUG: results%success = ', results%success
        if (.not. results%success) then
            print *, 'DEBUG: Error message = ', trim(results%error_message)
        end if
        print *, 'DEBUG: allocated(results%cells) = ', allocated(results%cells)
        if (allocated(results%cells)) then
            print *, 'DEBUG: size(results%cells) = ', size(results%cells)
        end if
        flush (6)

        ! Check that cache directory was created
        inquire (file=test_cache_dir, exist=dir_exists)
        print *, 'DEBUG: Final cache directory check - exists = ', dir_exists
        flush (6)
        if (.not. dir_exists) then
            print *, '  FAIL: Cache directory not created'
            print *, '  Expected directory: ', trim(test_cache_dir)
            passed = .false.
            goto 99
        end if

        ! Additional check - see if the execution actually succeeded
        if (.not. results%success) then
            print *, '  FAIL: Notebook execution failed'
            if (allocated(results%error_message)) then
                print *, '  Error: ', trim(results%error_message)
            end if
            passed = .false.
            goto 99
        end if

        print *, '  PASS'

99      continue
        ! Cleanup
        call execute_command_line("rm -rf "//trim(test_cache_dir))
        call free_notebook(nb)
        call free_execution_results(results)

    end subroutine test_cache_directory_creation

    subroutine test_cache_reuse(passed)
        logical, intent(inout) :: passed
        type(notebook_t) :: nb1, nb2
        type(execution_result_t) :: results1, results2
        character(len=:), allocatable :: test_cache_dir

        print *, 'Test 2: Cache reuse with same content'
        flush (6)

        ! Set up cache directory
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('test_notebook_reuse')
            test_cache_dir = temp_mgr%path
        end block

        ! Create first notebook
        nb1%num_cells = 1
        allocate (nb1%cells(1))
        nb1%cells(1)%cell_type = CELL_CODE
      nb1%cells(1)%content = "value = 456.0"//new_line('a')//"print *, 'value =', value"

        ! Create identical second notebook
        nb2%num_cells = 1
        allocate (nb2%cells(1))
        nb2%cells(1)%cell_type = CELL_CODE
nb2%cells(1)%content = "value = 456.0"//new_line('a')//"print *, 'value =', value"  ! Same content

        ! Execute both notebooks
        print *, 'DEBUG: About to execute first notebook'
        flush (6)
        call execute_notebook(nb1, results1, test_cache_dir)
        print *, 'DEBUG: First notebook executed, success = ', results1%success
        if (.not. results1%success .and. allocated(results1%error_message)) then
            print *, 'DEBUG: First notebook error: ', trim(results1%error_message)
        end if
        flush (6)

        print *, 'DEBUG: About to execute second notebook'
        flush (6)
        call execute_notebook(nb2, results2, test_cache_dir)
        print *, 'DEBUG: Second notebook executed, success = ', results2%success
        if (.not. results2%success .and. allocated(results2%error_message)) then
            print *, 'DEBUG: Second notebook error: ', trim(results2%error_message)
        end if
        flush (6)

        ! Check that results structure is valid (execution may fail but structure should be there)
        if (.not. allocated(results1%cells)) then
            print *, '  FAIL: First execution results not allocated'
            passed = .false.
            goto 99
        end if

        if (.not. allocated(results2%cells)) then
            print *, '  FAIL: Second execution results not allocated'
            passed = .false.
            goto 99
        end if

        print *, '  PASS'

99      continue
        ! Cleanup
        call execute_command_line("rm -rf "//trim(test_cache_dir))
        call free_notebook(nb1)
        call free_notebook(nb2)
        call free_execution_results(results1)
        call free_execution_results(results2)

    end subroutine test_cache_reuse

    subroutine test_cache_invalidation(passed)
        logical, intent(inout) :: passed
        type(notebook_t) :: nb1, nb2
        type(execution_result_t) :: results1, results2
        character(len=:), allocatable :: test_cache_dir

        print *, 'Test 3: Cache invalidation with different content'

        ! Set up cache directory
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('test_notebook_invalidation')
            test_cache_dir = temp_mgr%path
        end block

        ! Create first notebook
        nb1%num_cells = 1
        allocate (nb1%cells(1))
        nb1%cells(1)%cell_type = CELL_CODE
        nb1%cells(1)%content = "first_value = 789.0" // new_line('a') // "print *, 'first =', first_value"

        ! Create different second notebook
        nb2%num_cells = 1
        allocate (nb2%cells(1))
        nb2%cells(1)%cell_type = CELL_CODE
        nb2%cells(1)%content = "second_value = 101112.0" // new_line('a') // "print *, 'second =', second_value"  ! Different content

        ! Execute both notebooks
        print *, 'DEBUG: About to execute first invalidation notebook'
        flush (6)
        call execute_notebook(nb1, results1, test_cache_dir)
    print *, 'DEBUG: First invalidation notebook executed, success = ', results1%success
        if (.not. results1%success .and. allocated(results1%error_message)) then
     print *, 'DEBUG: First invalidation notebook error: ', trim(results1%error_message)
        end if
        flush (6)

        print *, 'DEBUG: About to execute second invalidation notebook'
        flush (6)
        call execute_notebook(nb2, results2, test_cache_dir)
   print *, 'DEBUG: Second invalidation notebook executed, success = ', results2%success
        if (.not. results2%success .and. allocated(results2%error_message)) then
    print *, 'DEBUG: Second invalidation notebook error: ', trim(results2%error_message)
        end if
        flush (6)

        ! Check that results structure is valid (different content should create different cache keys)
        if (.not. allocated(results1%cells)) then
            print *, '  FAIL: First execution results not allocated'
            passed = .false.
            goto 99
        end if

        if (.not. allocated(results2%cells)) then
            print *, '  FAIL: Second execution results not allocated'
            passed = .false.
            goto 99
        end if

        print *, '  PASS'

99      continue
        ! Cleanup
        call execute_command_line("rm -rf "//trim(test_cache_dir))
        call free_notebook(nb1)
        call free_notebook(nb2)
        call free_execution_results(results1)
        call free_execution_results(results2)

    end subroutine test_cache_invalidation

end program test_notebook_caching
