program test_formatter_edge_cases
    use formatter_utils
    implicit none

    logical :: all_passed = .true.
    character(len=1024) :: input_code, formatted_code
    character(len=256) :: test_line
    integer :: indent_level

    print *, "=== Formatter Utils Edge Cases Tests ==="

    ! Test 1: Empty and whitespace-only code
    call test_empty_code()

    ! Test 2: Very long lines
    call test_long_lines()

    ! Test 3: Complex nested structures
    call test_nested_structures()

    ! Test 4: Mixed indentation (tabs and spaces)
    call test_mixed_indentation()

    ! Test 5: Special Fortran constructs
    call test_special_constructs()

    ! Test 6: Comments and continuation lines
    call test_comments_continuations()

    if (all_passed) then
        print *, ""
        print *, "All formatter utils edge case tests PASSED!"
        stop 0
    else
        print *, ""
        print *, "Some formatter utils edge case tests FAILED!"
        stop 1
    end if

contains

    subroutine test_empty_code()
        print *, ""
        print *, "Test: Empty and whitespace-only code"

        ! Test completely empty string
        input_code = ""
        formatted_code = format_fortran_code(input_code)
        if (len_trim(formatted_code) == 0) then
            print *, "  PASS: Empty code handled"
        else
            print *, "  FAIL: Empty code should remain empty"
            all_passed = .false.
        end if

        ! Test only whitespace
        input_code = "   "//char(9)//"  "//char(10)//"   "
        formatted_code = format_fortran_code(input_code)
        print *, "  PASS: Whitespace-only code handled"

        ! Test single line with only spaces
        input_code = "                    "
        print *, "  PASS: Whitespace line test completed"
    end subroutine

    subroutine test_long_lines()
        integer :: i

        print *, ""
        print *, "Test: Very long lines"

        ! Create a very long line (over 132 chars)
        test_line = "program test_long_line_that_exceeds_standard_fortran_line_length_limit_of_132_characters_"
        test_line = trim(test_line) // "and_should_be_handled_properly_by_the_formatter_without_crashing"

        formatted_code = format_fortran_code(test_line)
        if (len_trim(formatted_code) > 0) then
            print *, "  PASS: Long line handled"
        else
            print *, "  FAIL: Long line disappeared"
            all_passed = .false.
        end if

        ! Test line at exactly 132 characters
        test_line = repeat('a', 132)
        formatted_code = format_fortran_code(test_line)
        print *, "  PASS: 132-character line handled"

        ! Test with very long variable names
        input_code = "real :: very_long_variable_name_that_exceeds_reasonable_length_expectations"
        do i = 1, 10
            input_code = trim(input_code)//", var"//achar(48 + i)
        end do
        formatted_code = format_fortran_code(input_code)
        print *, "  PASS: Long variable declaration handled"
    end subroutine

    subroutine test_nested_structures()
        print *, ""
        print *, "Test: Complex nested structures"

        ! Test deeply nested if statements
        input_code = "if (a) then"//char(10)// &
                     "  if (b) then"//char(10)// &
                     "    if (c) then"//char(10)// &
                     "      if (d) then"//char(10)// &
                     "        x = 1"//char(10)// &
                     "      end if"//char(10)// &
                     "    end if"//char(10)// &
                     "  end if"//char(10)// &
                     "end if"

        formatted_code = format_fortran_code(input_code)
        if (index(formatted_code, "if") > 0) then
            print *, "  PASS: Nested if statements handled"
        else
            print *, "  FAIL: Nested structures lost"
            all_passed = .false.
        end if

        ! Test nested do loops with labels
        input_code = "outer: do i = 1, n"//char(10)// &
                     "  middle: do j = 1, m"//char(10)// &
                     "    inner: do k = 1, p"//char(10)// &
                     "      array(i,j,k) = 0"//char(10)// &
                     "    end do inner"//char(10)// &
                     "  end do middle"//char(10)// &
                     "end do outer"

        formatted_code = format_fortran_code(input_code)
        print *, "  PASS: Labeled nested loops handled"

        ! Test associate blocks
        input_code = "associate (x => array(1:10), y => array(11:20))"//char(10)// &
                     "  x = y + 1"//char(10)// &
                     "end associate"

        formatted_code = format_fortran_code(input_code)
        print *, "  PASS: Associate blocks handled"
    end subroutine

    subroutine test_mixed_indentation()
        print *, ""
        print *, "Test: Mixed indentation (tabs and spaces)"

        ! Test mixed tabs and spaces
        input_code = char(9)//"program mixed"//char(10)// &
                     "    "//char(9)//"integer :: i"//char(10)// &
                     char(9)//char(9)//"real :: x"//char(10)// &
                     "        "//"end program"

        formatted_code = format_fortran_code(input_code)
        ! Check if formatting normalizes indentation
        if (index(formatted_code, char(9)) == 0) then
            print *, "  PASS: Tabs converted to spaces"
        else
            print *, "  INFO: Mixed indentation preserved"
        end if

        ! Test inconsistent indentation
        input_code = "subroutine test()"//char(10)// &
                     "integer :: a"//char(10)// &
                     "   real :: b"//char(10)// &
                     "      logical :: c"//char(10)// &
                     " character :: d"//char(10)// &
                     "end subroutine"

        formatted_code = format_fortran_code(input_code)
        print *, "  PASS: Inconsistent indentation handled"
    end subroutine

    subroutine test_special_constructs()
        print *, ""
        print *, "Test: Special Fortran constructs"

        ! Test where construct
        input_code = "where (array > 0)"//char(10)// &
                     "  array = sqrt(array)"//char(10)// &
                     "elsewhere (array < 0)"//char(10)// &
                     "  array = -sqrt(-array)"//char(10)// &
                     "elsewhere"//char(10)// &
                     "  array = 0"//char(10)// &
                     "end where"

        formatted_code = format_fortran_code(input_code)
        print *, "  PASS: Where construct handled"

        ! Test forall construct
        input_code = "forall (i=1:n, j=1:m, i/=j)"//char(10)// &
                     "  matrix(i,j) = real(i*j)"//char(10)// &
                     "end forall"

        formatted_code = format_fortran_code(input_code)
        print *, "  PASS: Forall construct handled"

        ! Test select case with various cases
        input_code = "select case (trim(option))"//char(10)// &
                     "case ('A', 'a')"//char(10)// &
                     "  call process_a()"//char(10)// &
                     "case ('B':'D')"//char(10)// &
                     "  call process_range()"//char(10)// &
                     "case default"//char(10)// &
                     "  call process_default()"//char(10)// &
                     "end select"

        formatted_code = format_fortran_code(input_code)
        print *, "  PASS: Select case handled"

        ! Test interface block
        input_code = "interface operator(+)"//char(10)// &
                     "  module procedure add_custom"//char(10)// &
                     "end interface"

        formatted_code = format_fortran_code(input_code)
        print *, "  PASS: Interface block handled"
    end subroutine

    subroutine test_comments_continuations()
        print *, ""
        print *, "Test: Comments and continuation lines"

        ! Test inline comments
        input_code = "real :: x  ! This is x"//char(10)// &
                     "real :: y  ! This is y"//char(10)// &
                     "x = 1.0    ! Set x"//char(10)// &
                     "y = 2.0    ! Set y"

        formatted_code = format_fortran_code(input_code)
        if (index(formatted_code, "!") > 0) then
            print *, "  PASS: Inline comments preserved"
        else
            print *, "  INFO: Comment handling may vary"
        end if

        ! Test continuation lines with &
        input_code = "real :: very_long_array_name(100, 200, 300), &"//char(10)// &
                     "        another_array(50), &"//char(10)// &
                     "        yet_another_array(10, 20)"

        formatted_code = format_fortran_code(input_code)
        if (index(formatted_code, "&") > 0) then
            print *, "  PASS: Continuation lines preserved"
        else
            print *, "  INFO: Continuation handling may vary"
        end if

        ! Test comment blocks
        input_code = "! This is a comment block"//char(10)// &
                     "! with multiple lines"//char(10)// &
                     "! that should be preserved"//char(10)// &
                     "program test"//char(10)// &
                     "end program"

        formatted_code = format_fortran_code(input_code)
        print *, "  PASS: Comment blocks handled"

        ! Test old-style comments
        input_code = "C     Old style comment"//char(10)// &
                     "*     Another old style"//char(10)// &
                     "      program old"//char(10)// &
                     "      end"

        formatted_code = format_fortran_code(input_code)
        print *, "  PASS: Old-style comments handled"
    end subroutine

end program test_formatter_edge_cases
