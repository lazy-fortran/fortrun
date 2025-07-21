program test_date_time
    implicit none
    integer :: values(8)

    print '(a)', 'Testing date_and_time...'
    call date_and_time(values=values)
    print '(a)', 'Date and time obtained successfully'
    print '(8i6)', values

end program test_date_time
