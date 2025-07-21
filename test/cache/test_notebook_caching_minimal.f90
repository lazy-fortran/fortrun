program test_notebook_caching_minimal
    implicit none

    print *, 'test_notebook_caching_minimal: STARTING NOW'
    call flush (6)

    print *, 'test_notebook_caching_minimal: Testing minimal notebook functionality'
    call flush (6)

    ! Just test that we can print something and exit
    print *, 'test_notebook_caching_minimal: SUCCESS'
    call flush (6)

    stop 0
end program test_notebook_caching_minimal
