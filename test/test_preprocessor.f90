program test_preprocessor
  use preprocessor
  implicit none
  
  integer :: test_count, pass_count
  
  test_count = 0
  pass_count = 0
  
  call test_is_preprocessor_file(test_count, pass_count)
  call test_simple_preprocessing(test_count, pass_count)
  call test_function_preprocessing(test_count, pass_count)
  call test_subroutine_preprocessing(test_count, pass_count)
  call test_existing_program(test_count, pass_count)
  call test_error_handling(test_count, pass_count)
  
  print '(a)', ''
  print '(a,i0,a,i0,a)', 'Preprocessor tests: ', pass_count, '/', test_count, ' passed'
  
  if (pass_count /= test_count) then
    error stop 'Some tests failed!'
  end if
  
contains

  subroutine test_is_preprocessor_file(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    
    test_count = test_count + 1
    if (is_preprocessor_file('test.f')) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: is_preprocessor_file recognizes .f files'
    else
      print '(a)', 'FAIL: is_preprocessor_file should recognize .f files'
    end if
    
    test_count = test_count + 1
    if (.not. is_preprocessor_file('test.f90')) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: is_preprocessor_file rejects .f90 files'
    else
      print '(a)', 'FAIL: is_preprocessor_file should reject .f90 files'
    end if
    
    test_count = test_count + 1
    if (is_preprocessor_file('path/to/file.f')) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: is_preprocessor_file works with paths'
    else
      print '(a)', 'FAIL: is_preprocessor_file should work with paths'
    end if
  end subroutine test_is_preprocessor_file
  
  subroutine test_simple_preprocessing(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    character(len=256) :: temp_file, output_file, error_msg
    character(len=1024) :: line
    integer :: unit, ios
    logical :: found_program, found_implicit, found_print, found_end
    
    ! Create temporary test file
    temp_file = 'test_simple.f'
    open(newunit=unit, file=temp_file, status='replace')
    write(unit, '(a)') "print *, 'Hello World'"
    close(unit)
    
    ! Preprocess
    call preprocess_file(temp_file, output_file, error_msg)
    
    test_count = test_count + 1
    if (len_trim(error_msg) == 0) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Simple preprocessing completed without errors'
    else
      print '(a,a)', 'FAIL: Simple preprocessing error: ', trim(error_msg)
      return
    end if
    
    ! Check output
    found_program = .false.
    found_implicit = .false.
    found_print = .false.
    found_end = .false.
    
    open(newunit=unit, file=output_file, status='old', action='read')
    do
      read(unit, '(a)', iostat=ios) line
      if (ios /= 0) exit
      
      if (index(line, 'program main') > 0) found_program = .true.
      if (index(line, 'implicit none') > 0) found_implicit = .true.
      if (index(line, 'Hello World') > 0) found_print = .true.
      if (index(line, 'end program main') > 0) found_end = .true.
    end do
    close(unit)
    
    test_count = test_count + 1
    if (found_program .and. found_implicit .and. found_print .and. found_end) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Simple preprocessing generates correct structure'
    else
      print '(a)', 'FAIL: Simple preprocessing missing required elements'
    end if
    
    ! Cleanup
    call execute_command_line('rm -f ' // trim(temp_file) // ' ' // trim(output_file))
  end subroutine test_simple_preprocessing
  
  subroutine test_function_preprocessing(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    character(len=256) :: temp_file, output_file, error_msg
    character(len=1024) :: line
    integer :: unit, ios
    logical :: found_contains, found_function
    
    ! Create temporary test file with function
    temp_file = 'test_function.f'
    open(newunit=unit, file=temp_file, status='replace')
    write(unit, '(a)') "x = add(1.0, 2.0)"
    write(unit, '(a)') "print *, x"
    write(unit, '(a)') ""
    write(unit, '(a)') "real function add(a, b)"
    write(unit, '(a)') "  real :: a, b"
    write(unit, '(a)') "  add = a + b"
    write(unit, '(a)') "end function add"
    close(unit)
    
    ! Preprocess
    call preprocess_file(temp_file, output_file, error_msg)
    
    test_count = test_count + 1
    if (len_trim(error_msg) == 0) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Function preprocessing completed without errors'
    else
      print '(a,a)', 'FAIL: Function preprocessing error: ', trim(error_msg)
      return
    end if
    
    ! Check output has contains statement
    found_contains = .false.
    found_function = .false.
    
    open(newunit=unit, file=output_file, status='old', action='read')
    do
      read(unit, '(a)', iostat=ios) line
      if (ios /= 0) exit
      
      if (trim(adjustl(line)) == 'contains') found_contains = .true.
      if (index(line, 'real function add') > 0) found_function = .true.
    end do
    close(unit)
    
    test_count = test_count + 1
    if (found_contains .and. found_function) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Function preprocessing adds contains statement'
    else
      print '(a)', 'FAIL: Function preprocessing missing contains or function'
    end if
    
    ! Cleanup
    call execute_command_line('rm -f ' // trim(temp_file) // ' ' // trim(output_file))
  end subroutine test_function_preprocessing
  
  subroutine test_subroutine_preprocessing(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    character(len=256) :: temp_file, output_file, error_msg
    character(len=1024) :: line
    integer :: unit, ios
    logical :: found_contains, found_subroutine
    
    ! Create temporary test file with subroutine
    temp_file = 'test_subroutine.f'
    open(newunit=unit, file=temp_file, status='replace')
    write(unit, '(a)') "call greet('World')"
    write(unit, '(a)') ""
    write(unit, '(a)') "subroutine greet(name)"
    write(unit, '(a)') "  character(len=*) :: name"
    write(unit, '(a)') "  print *, 'Hello, ', name"
    write(unit, '(a)') "end subroutine greet"
    close(unit)
    
    ! Preprocess
    call preprocess_file(temp_file, output_file, error_msg)
    
    test_count = test_count + 1
    if (len_trim(error_msg) == 0) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Subroutine preprocessing completed without errors'
    else
      print '(a,a)', 'FAIL: Subroutine preprocessing error: ', trim(error_msg)
      return
    end if
    
    ! Check output has contains statement
    found_contains = .false.
    found_subroutine = .false.
    
    open(newunit=unit, file=output_file, status='old', action='read')
    do
      read(unit, '(a)', iostat=ios) line
      if (ios /= 0) exit
      
      if (trim(adjustl(line)) == 'contains') found_contains = .true.
      if (index(line, 'subroutine greet') > 0) found_subroutine = .true.
    end do
    close(unit)
    
    test_count = test_count + 1
    if (found_contains .and. found_subroutine) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Subroutine preprocessing adds contains statement'
    else
      print '(a)', 'FAIL: Subroutine preprocessing missing contains or subroutine'
    end if
    
    ! Cleanup
    call execute_command_line('rm -f ' // trim(temp_file) // ' ' // trim(output_file))
  end subroutine test_subroutine_preprocessing
  
  subroutine test_existing_program(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    character(len=256) :: temp_file, output_file, error_msg
    character(len=1024) :: line
    integer :: unit, ios, program_count
    
    ! Create temporary test file with existing program
    temp_file = 'test_existing.f'
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
    
    open(newunit=unit, file=output_file, status='old', action='read')
    do
      read(unit, '(a)', iostat=ios) line
      if (ios /= 0) exit
      
      if (index(adjustl(line), 'program ') == 1 .and. &
          index(line, 'end program') == 0) then
        program_count = program_count + 1
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
  
  subroutine test_error_handling(test_count, pass_count)
    integer, intent(inout) :: test_count, pass_count
    character(len=256) :: output_file, error_msg
    
    ! Test with non-existent file
    call preprocess_file('nonexistent.f', output_file, error_msg)
    
    test_count = test_count + 1
    if (len_trim(error_msg) > 0) then
      pass_count = pass_count + 1
      print '(a)', 'PASS: Error handling for missing file'
    else
      print '(a)', 'FAIL: Should report error for missing file'
    end if
  end subroutine test_error_handling

end program test_preprocessor