program test_unknown
    use nonexistent_module
    implicit none
    print *, "This should fail"
end program test_unknown
