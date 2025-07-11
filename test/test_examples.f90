program test_examples
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  
  character(len=256), dimension(:), allocatable :: example_files
  character(len=1024) :: output
  integer :: n_examples, i, exit_code
  integer :: n_passed, n_failed
  logical :: file_exists
  
  ! List of example files to test
  n_examples = 5
  allocate(example_files(n_examples))
  example_files(1) = 'example/hello.f90'
  example_files(2) = 'example/calculator.f90'
  example_files(3) = 'example/precision_test.f90'
  example_files(4) = 'example/precision_compare.f90'
  example_files(5) = 'example/real_default_test.f90'
  
  n_passed = 0
  n_failed = 0
  
  print '(a)', '='//repeat('=', 60)
  print '(a)', 'Running Fortran CLI Example Tests'
  print '(a)', '='//repeat('=', 60)
  print *
  
  do i = 1, n_examples
    ! Check if file exists
    inquire(file=example_files(i), exist=file_exists)
    
    if (.not. file_exists) then
      print '(a,a,a)', 'SKIP: ', trim(example_files(i)), ' (file not found)'
      cycle
    end if
    
    ! Run the example
    print '(a,a,a)', 'Running: ', trim(example_files(i)), '...'
    call run_example(example_files(i), output, exit_code)
    
    if (exit_code == 0) then
      print '(a,a,a)', '  ✓ PASS: ', trim(example_files(i)), ' (exit code 0)'
      n_passed = n_passed + 1
      
      ! Show output for specific examples
      select case (trim(example_files(i)))
      case ('example/hello.f90')
        if (index(output, 'Hello from fortran CLI!') == 0) then
          print '(a)', '    WARNING: Expected output not found'
        end if
      case ('example/real_default_test.f90')
        if (index(output, 'sizeof(real) =                     8  bytes') > 0) then
          print '(a)', '    ✓ Double precision default confirmed'
        else
          print '(a)', '    WARNING: Double precision not working as expected'
        end if
      end select
    else
      print '(a,a,a,i0,a)', '  ✗ FAIL: ', trim(example_files(i)), &
                             ' (exit code ', exit_code, ')'
      n_failed = n_failed + 1
      ! Show error output
      if (len_trim(output) > 0) then
        print '(a)', '    Error output:'
        print '(a,a)', '    ', trim(output)
      end if
    end if
    print *
  end do
  
  ! Summary
  print '(a)', '='//repeat('=', 60)
  print '(a)', 'Test Summary'
  print '(a)', '='//repeat('=', 60)
  print '(a,i0)', 'Total examples: ', n_examples
  print '(a,i0)', 'Passed: ', n_passed
  print '(a,i0)', 'Failed: ', n_failed
  print '(a,i0)', 'Skipped: ', n_examples - n_passed - n_failed
  
  if (n_failed > 0) then
    print *
    print '(a)', 'OVERALL: FAILED'
    stop 1
  else
    print *
    print '(a)', 'OVERALL: PASSED'
  end if
  
contains

  subroutine run_example(filename, output, exit_code)
    character(len=*), intent(in) :: filename
    character(len=*), intent(out) :: output
    integer, intent(out) :: exit_code
    
    character(len=512) :: command
    character(len=256) :: cache_pattern
    integer :: unit, iostat, j
    character(len=1024) :: line
    
    ! Extract base name for cache cleanup
    j = index(filename, '/', back=.true.)
    if (j > 0) then
      cache_pattern = filename(j+1:)
    else
      cache_pattern = filename
    end if
    j = index(cache_pattern, '.f90')
    if (j > 0) then
      cache_pattern = cache_pattern(1:j-1)
    end if
    
    ! Clean cache for this example
    command = 'rm -rf ~/.cache/fortran/' // trim(cache_pattern) // '_*'
    call execute_command_line(trim(command))
    
    ! Run the example
    command = './build/gfortran_*/app/fortran ' // trim(filename) // &
              ' > test_output.tmp 2>&1'
    call execute_command_line(trim(command), exitstat=exit_code)
    
    ! Read output
    output = ''
    open(newunit=unit, file='test_output.tmp', status='old', iostat=iostat)
    if (iostat == 0) then
      do
        read(unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit
        if (len_trim(output) > 0) then
          output = trim(output) // ' | ' // trim(adjustl(line))
        else
          output = trim(adjustl(line))
        end if
      end do
      close(unit)
    end if
    
    ! Extract just the program output (after FPM messages)
    j = index(output, '[100%] Project compiled successfully.')
    if (j > 0) then
      output = output(j+37:)  ! Skip the FPM message
    end if
    
    ! Clean up
    call execute_command_line('rm -f test_output.tmp')
    
  end subroutine run_example
  
end program test_examples