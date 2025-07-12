program test_preprocessor_isolated
  use preprocessor
  implicit none
  
  integer :: test_count, pass_count
  
  test_count = 0
  pass_count = 0
  
  call test_existing_program(test_count, pass_count)
  
  print '(a)', ''
  print '(a,i0,a,i0,a)', 'Preprocessor tests: ', pass_count, '/', test_count, ' passed'
  
  if (pass_count /= test_count) then
    error stop 'Some tests failed!'
  end if
  
contains

  subroutine test_existing_program(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    character(len=256) :: temp_file, output_file, error_msg
    character(len=1024) :: line
    integer :: unit, ios, program_count
    
    ! Create temporary test file with existing program
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
    
    test_count = test_count + 1
    if (len_trim(error_msg) == 0) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Existing program preprocessing completed'
    else
      print '(a,a)', 'FAIL: Existing program preprocessing error: ', trim(error_msg)
      return
    end if
    
    ! Check output doesn't have double program statements
    program_count = 0
    
    print '(a)', ''
    print '(a)', 'Checking output file for program statements:'
    open(newunit=unit, file=output_file, status='old', action='read')
    do
      read(unit, '(a)', iostat=ios) line
      if (ios /= 0) exit
      
      print '(a,a)', '  Line: ', trim(line)
      
      if (index(adjustl(line), 'program ') == 1 .and. &
          index(line, 'end program') == 0) then
        program_count = program_count + 1
        print '(a,i0)', '    -> Found program statement #', program_count
      end if
    end do
    close(unit)
    
    test_count = test_count + 1
    if (program_count == 1) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Existing program not wrapped again'
    else
      print '(a,i0)', 'FAIL: Wrong number of program statements: ', program_count
    end if
    
    ! Cleanup
    call execute_command_line('rm -f ' // trim(temp_file) // ' ' // trim(output_file))
  end subroutine test_existing_program

end program test_preprocessor_isolated