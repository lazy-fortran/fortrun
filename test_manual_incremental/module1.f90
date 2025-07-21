module module1
    implicit none
    integer, parameter :: MOD1_VALUE = 42
contains
    function mod1_func(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x + MOD1_VALUE
    end function mod1_func
end module module1
EOF < /dev/null
