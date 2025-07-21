program test_notebook_imports
    implicit none

    print *, 'test_notebook_imports: Starting import test'
    call flush (6)

    ! Test import of each module individually
    print *, 'test_notebook_imports: Testing notebook_parser import'
    call flush (6)
    block
        use notebook_parser
        print *, 'test_notebook_imports: notebook_parser loaded successfully'
        call flush (6)
    end block

    print *, 'test_notebook_imports: Testing notebook_executor import'
    call flush (6)
    block
        use notebook_executor
        print *, 'test_notebook_imports: notebook_executor loaded successfully'
        call flush (6)
    end block

    print *, 'test_notebook_imports: Testing temp_utils import'
    call flush (6)
    block
        use temp_utils, only: temp_dir_manager
        print *, 'test_notebook_imports: temp_utils loaded successfully'
        call flush (6)
    end block

    print *, 'test_notebook_imports: All imports successful'
    call flush (6)

    stop 0
end program test_notebook_imports
