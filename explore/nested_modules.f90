! Test deeply nested module dependencies
program nested_test
    use level1_mod
    implicit none
    
    call level1_sub()
end program

module level1_mod
    use level2_mod
    implicit none
    
contains
    subroutine level1_sub()
        print *, "Level 1"
        call level2_sub()
    end subroutine
end module

module level2_mod
    use level3_mod
    implicit none
    
contains
    subroutine level2_sub()
        print *, "Level 2"
        call level3_sub()
    end subroutine
end module

module level3_mod
    implicit none
    
contains
    subroutine level3_sub()
        print *, "Level 3 - deepest level"
    end subroutine
end module