program test_existing_program_check
  use preprocessor
  implicit none
  
  character(len=256) :: temp_file, output_file, error_msg
  character(len=1024) :: line
  integer :: unit, ios, program_count
  
  ! Create the same test file as in test_existing_program
  temp_file = 'test_existing.f'
  output_file = 'test_existing.f90'
  open(newunit=unit, file=temp_file, status='replace')
  write(unit, '(a)') "program mytest"
  write(unit, '(a)') "  implicit none"
  write(unit, '(a)') "  print *, 'Already has program'"
  write(unit, '(a)') "end program mytest"
  close(unit)
  
  ! Preprocess
  call preprocess_file(temp_file, output_file, error_msg)
  
  if (len_trim(error_msg) == 0) then
    print '(a)', 'Preprocessing completed successfully'
    
    ! Count program statements
    program_count = 0
    open(newunit=unit, file=output_file, status='old', action='read')
    do
      read(unit, '(a)', iostat=ios) line
      if (ios /= 0) exit
      
      print '(a,a)', 'Line: ', trim(line)
      
      if (index(adjustl(line), 'program ') == 1 .and. &
          index(line, 'end program') == 0) then
        program_count = program_count + 1
        print '(a,i0)', '  Found program statement #', program_count
      end if
    end do
    close(unit)
    
    print '(a,i0)', 'Total program statements found: ', program_count
  else
    print '(a,a)', 'Error: ', trim(error_msg)
  end if
  
  ! Cleanup
  call execute_command_line('rm -f ' // trim(temp_file) // ' ' // trim(output_file))
  
end program test_existing_program_check