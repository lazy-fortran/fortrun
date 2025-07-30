program test_content_hashing_edge_cases
    use cache, only: get_content_hash, get_single_file_content_hash
    use temp_utils, only: temp_dir_manager, fortran_with_isolated_cache
    use system_utils, only: sys_create_symlink
    implicit none

    type(temp_dir_manager) :: temp_mgr
    character(len=256) :: test_file1, test_file2, symlink_file
    character(len=64) :: hash1, hash2
    character(len=1024) :: command
    integer :: test_count, pass_count, exit_code, unit, ios, i
    logical :: success
    character(len=1024) :: line

    test_count = 0
    pass_count = 0

    print *, "=== Content Hashing Edge Cases Tests ==="
    print *, ""

    call temp_mgr%create('hashing_edge_cases')

    ! Test 1: Large file hashing performance
    call test_large_file_hashing()

    ! Test 2: Binary file detection
    call test_binary_file_handling()

    ! Test 3: Symbolic link handling
    call test_symbolic_link_hashing()

    ! Test 4: Character encoding variations
    call test_character_encoding_hashing()

    ! Test 5: Hash consistency across platforms
    call test_hash_consistency()

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All content hashing edge case tests passed!"
        stop 0
    else
        print *, "Some content hashing edge case tests failed!"
        stop 1
    end if

contains

    subroutine test_large_file_hashing()
        real :: start_time, end_time
        character(len=256) :: large_file

        call test_start("Large file hashing performance")

        ! Create a large file (10MB equivalent in text)
        large_file = temp_mgr%get_file_path('large_hash_test.f90')
        open (newunit=unit, file=large_file, status='replace', iostat=ios)

        write (unit, '(A)') 'program large_hash_test'
        write (unit, '(A)') '    implicit none'

        ! Generate ~10,000 lines to simulate large file
        do i = 1, 10000
            write (unit, '(A,I0,A)') '    ! Comment line ', i, &
                ' with some padding to make it longer xxxxxxxxxxxxxxxxxxxxxxxxxx'
        end do

        write (unit, '(A)') '    print *, "Large file test"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Time the hashing operation
        call cpu_time(start_time)
        hash1 = get_single_file_content_hash(large_file)
        call cpu_time(end_time)

        ! Should complete in reasonable time (< 1 second)
        success = (end_time - start_time < 1.0) .and. (len_trim(hash1) > 0)

        call test_result(success)

        if (.not. success) then
            print *, "  Large file hashing failed or too slow"
            print *, "  Time taken: ", end_time - start_time, " seconds"
        end if
    end subroutine test_large_file_hashing

    subroutine test_binary_file_handling()
        character(len=256) :: binary_file, text_file

        call test_start("Binary file detection and handling")

        ! Create a binary file
        binary_file = temp_mgr%get_file_path('binary_test.dat')
        open (newunit=unit, file=binary_file, status='replace', &
              access='stream', form='unformatted', iostat=ios)
        ! Write some binary data
        write (unit) 0, 255, 127, 64, 32, 16, 8, 4, 2, 1
        close (unit)

        ! Create equivalent text file
        text_file = temp_mgr%get_file_path('text_test.f90')
        open (newunit=unit, file=text_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program text_test'
        write (unit, '(A)') '    print *, "Text"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Hash both files - binary should be handled differently
        hash1 = get_single_file_content_hash(binary_file)
        hash2 = get_single_file_content_hash(text_file)

        ! Both should produce valid hashes
        success = (len_trim(hash1) > 0) .and. (len_trim(hash2) > 0)

        call test_result(success)

        if (.not. success) then
            print *, "  Binary file handling failed"
        end if
    end subroutine test_binary_file_handling

    subroutine test_symbolic_link_hashing()
        character(len=256) :: original_file, link_target
        logical :: link_success

        call test_start("Symbolic link hashing")

        ! Create original file
        original_file = temp_mgr%get_file_path('original.f90')
        open (newunit=unit, file=original_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program symlink_test'
        write (unit, '(A)') '    print *, "Original file"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Create symbolic link
        symlink_file = temp_mgr%get_file_path('symlink.f90')
        call sys_create_symlink(original_file, symlink_file, link_success)

        if (link_success) then
            ! Hash both - should produce same hash
            hash1 = get_single_file_content_hash(original_file)
            hash2 = get_single_file_content_hash(symlink_file)

            success = (hash1 == hash2) .and. (len_trim(hash1) > 0)
        else
            ! If symlinks not supported, test passes
            success = .true.
        end if

        call test_result(success)

        if (.not. success) then
            print *, "  Symbolic link hashing failed"
            print *, "  Original hash: ", trim(hash1)
            print *, "  Symlink hash: ", trim(hash2)
        end if
    end subroutine test_symbolic_link_hashing

    subroutine test_character_encoding_hashing()
        character(len=256) :: utf8_file, ascii_file

        call test_start("Character encoding variations")

        ! Create file with UTF-8 characters in comments
        utf8_file = temp_mgr%get_file_path('utf8_test.f90')
        open (newunit=unit, file=utf8_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program utf8_test'
        write (unit, '(A)') '    ! Comment with special chars: café, naïve'
        write (unit, '(A)') '    print *, "UTF-8 test"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Create ASCII-only version
        ascii_file = temp_mgr%get_file_path('ascii_test.f90')
        open (newunit=unit, file=ascii_file, status='replace', iostat=ios)
        write (unit, '(A)') 'program ascii_test'
        write (unit, '(A)') '    ! Comment with only ASCII chars'
        write (unit, '(A)') '    print *, "ASCII test"'
        write (unit, '(A)') 'end program'
        close (unit)

        ! Both should produce valid but different hashes
        hash1 = get_single_file_content_hash(utf8_file)
        hash2 = get_single_file_content_hash(ascii_file)

        success = (len_trim(hash1) > 0) .and. (len_trim(hash2) > 0) .and. &
                  (hash1 /= hash2)

        call test_result(success)

        if (.not. success) then
            print *, "  Character encoding hashing failed"
        end if
    end subroutine test_character_encoding_hashing

    subroutine test_hash_consistency()
        character(len=256) :: consistent_file
        character(len=64) :: hash_run1, hash_run2, hash_run3

        call test_start("Hash consistency across multiple runs")

        ! Create a file with mixed content
        consistent_file = temp_mgr%get_file_path('consistent.f90')
        open (newunit=unit, file=consistent_file, status='replace', iostat=ios)
        write (unit, '(A)') 'module hash_consistency_test'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') '    integer, parameter :: MAGIC = 42'
        write (unit, '(A)') '    real, parameter :: PI = 3.14159265359'
        write (unit, '(A)') 'contains'
        write (unit, '(A)') '    subroutine test_sub()'
        write (unit, '(A)') '        print *, "Consistency test"'
        write (unit, '(A)') '    end subroutine test_sub'
        write (unit, '(A)') 'end module hash_consistency_test'
        close (unit)

        ! Hash the same file multiple times
        hash_run1 = get_single_file_content_hash(consistent_file)
        hash_run2 = get_single_file_content_hash(consistent_file)
        hash_run3 = get_single_file_content_hash(consistent_file)

        ! All hashes should be identical
        success = (hash_run1 == hash_run2) .and. &
                  (hash_run2 == hash_run3) .and. &
                  (len_trim(hash_run1) > 0)

        call test_result(success)

        if (.not. success) then
            print *, "  Hash consistency failed"
            print *, "  Run 1: ", trim(hash_run1)
            print *, "  Run 2: ", trim(hash_run2)
            print *, "  Run 3: ", trim(hash_run3)
        end if
    end subroutine test_hash_consistency

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write (*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_result(test_success)
        logical, intent(in) :: test_success
        if (test_success) then
            print *, " ... PASSED"
            pass_count = pass_count + 1
        else
            print *, " ... FAILED"
        end if
    end subroutine test_result

end program test_content_hashing_edge_cases
