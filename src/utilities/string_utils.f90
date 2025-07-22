module string_utils
    implicit none
    private
    public :: int_to_char, logical_to_char

contains

    function int_to_char(i) result(str)
        integer, intent(in) :: i
        character(len=32) :: str
        integer :: iostat
        
        write(str, '(i0)', iostat=iostat) i
        if (iostat /= 0) then
            str = 'ERROR'
        else
            str = trim(str)
        end if
    end function int_to_char

    function logical_to_char(l) result(str)
        logical, intent(in) :: l
        character(len=1) :: str
        if (l) then
            str = 'T'
        else
            str = 'F'
        end if
    end function logical_to_char

end module string_utils