program debug_scope_tracking
  ! Debug the scope tracking issue for add function
  use preprocessor
  implicit none
  
  character(len=*), parameter :: input_file = '/home/ert/code/fortran/example/preprocessor/math.f'
  character(len=256) :: output_file, error_msg
  integer :: unit
  character(len=1024) :: line
  integer :: ios, line_num
  
  ! Create a temporary output file
  output_file = '/tmp/debug_scope_math.f90'
  
  ! Preprocess the file
  call preprocess_file(input_file, output_file, error_msg)
  
  if (len_trim(error_msg) > 0) then
    print *, 'Error:', trim(error_msg)
    stop 1
  end if
  
  print *, 'Preprocessed file created at:', trim(output_file)
  print *, 'Contents:'
  print *, '========================================='
  
  ! Show the contents
  open(newunit=unit, file=output_file, status='old', action='read')
  line_num = 0
  do
    read(unit, '(A)', iostat=ios) line
    if (ios /= 0) exit
    line_num = line_num + 1
    print '(I3,A,A)', line_num, '→', trim(line)
  end do
  close(unit)
  
end program debug_scope_tracking