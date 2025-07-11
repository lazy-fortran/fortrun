module runner
  implicit none
  private
  public :: run_fortran_file
  
contains

  subroutine run_fortran_file(filename, exit_code)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: exit_code
    
    logical :: file_exists
    integer :: unit, iostat
    character(len=256) :: temp_dir
    character(len=512) :: command
    
    exit_code = 0
    
    ! Check if file exists
    inquire(file=filename, exist=file_exists)
    if (.not. file_exists) then
      print '(a,a)', 'Error: File not found: ', trim(filename)
      exit_code = 1
      return
    end if
    
    ! Check file extension
    if (index(filename, '.f90') == 0 .and. index(filename, '.F90') == 0) then
      print '(a)', 'Error: Input file must have .f90 or .F90 extension'
      exit_code = 1
      return
    end if
    
    ! For now, just print what we would do
    print '(a,a)', 'Would run: ', trim(filename)
    print '(a)', 'TODO: Implement actual execution with FPM'
    
  end subroutine run_fortran_file
  
end module runner