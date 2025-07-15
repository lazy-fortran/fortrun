program test_file_isolation
    implicit none

    integer :: unit

    ! Test 1: Write to file1
    open (newunit=unit, file='/tmp/file1.txt', status='replace')
    write (unit, '(a)') 'Content from test 1'
    close (unit)

    ! Test 2: Write to file2
    open (newunit=unit, file='/tmp/file2.txt', status='replace')
    write (unit, '(a)') 'Content from test 2'
    close (unit)

    ! Test 3: Write to file3
    open (newunit=unit, file='/tmp/file3.txt', status='replace')
    write (unit, '(a)') 'Content from test 3'
    close (unit)

    ! Check contents
    print '(a)', '/tmp/file1.txt:'
    call execute_command_line('cat /tmp/file1.txt')

    print '(a)', '/tmp/file2.txt:'
    call execute_command_line('cat /tmp/file2.txt')

    print '(a)', '/tmp/file3.txt:'
    call execute_command_line('cat /tmp/file3.txt')

    ! Cleanup
    call execute_command_line('rm -f /tmp/file1.txt /tmp/file2.txt /tmp/file3.txt')

end program test_file_isolation
