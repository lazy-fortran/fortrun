day = 3
select case (day)
case (1)
    print *, "Monday"
case (2:5)
    print *, "Weekday"
case default
    print *, "Other"
end select