module use_statement_collector
  implicit none
  private
  
  public :: collect_use_statements, is_use_statement
  
  ! Maximum number of USE statements supported
  integer, parameter :: MAX_USE_STATEMENTS = 50
  
contains

  !> Collect all USE statements from a file
  subroutine collect_use_statements(unit_in, use_statements, use_count)
    integer, intent(in) :: unit_in
    character(len=256), dimension(:), intent(out) :: use_statements
    integer, intent(out) :: use_count
    
    character(len=1024) :: line
    integer :: ios
    
    use_count = 0
    
    ! Read through file and collect USE statements
    do
      read(unit_in, '(A)', iostat=ios) line
      if (ios /= 0) exit
      
      if (is_use_statement(line)) then
        use_count = use_count + 1
        if (use_count <= size(use_statements)) then
          use_statements(use_count) = trim(line)
        end if
      end if
    end do
    
  end subroutine collect_use_statements

  !> Check if a line contains a USE statement
  function is_use_statement(line) result(is_use)
    character(len=*), intent(in) :: line
    logical :: is_use
    character(len=256) :: trimmed
    
    trimmed = adjustl(line)
    is_use = index(trimmed, 'use ') == 1
  end function is_use_statement

end module use_statement_collector