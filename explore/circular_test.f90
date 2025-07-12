! Test circular dependency handling
program circular_test
    use mod_a
    implicit none
    
    call routine_a()
end program

module mod_a
    use mod_b  ! This creates circular dependency if mod_b uses mod_a
    implicit none
    
contains
    subroutine routine_a()
        print *, "In routine A"
        call routine_b()
    end subroutine
end module

module mod_b
    ! Commented out to avoid actual circular dependency
    ! use mod_a  
    implicit none
    
contains
    subroutine routine_b()
        print *, "In routine B"
        ! call routine_a()  ! This would create actual circular dependency
    end subroutine
end module