program test_lexer_serialization
    use lexer
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Run tests
    if (.not. test_json_serialization()) all_passed = .false.
    if (.not. test_json_file_output()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All serialization tests passed"
        stop 0
    else
        print '(a)', "Some serialization tests failed"
        stop 1
    end if

contains

    logical function test_json_serialization()
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: json_str
        
        test_json_serialization = .true.
        
        print '(a)', "Testing JSON string serialization..."
        
        ! Tokenize simple expression
        call tokenize("x = 5", tokens)
        
        ! Convert to JSON string
        json_str = json_write_tokens_to_string(tokens)
        
        ! Basic validation - should contain expected tokens
        if (index(json_str, '"type": "identifier"') == 0) then
            print '(a)', "FAIL: JSON missing identifier token"
            test_json_serialization = .false.
        end if
        
        if (index(json_str, '"text": "x"') == 0) then
            print '(a)', "FAIL: JSON missing x text"
            test_json_serialization = .false.
        end if
        
        if (index(json_str, '"type": "operator"') == 0) then
            print '(a)', "FAIL: JSON missing operator token"
            test_json_serialization = .false.
        end if
        
        if (index(json_str, '"text": "="') == 0) then
            print '(a)', "FAIL: JSON missing = text"
            test_json_serialization = .false.
        end if
        
        if (index(json_str, '"type": "number"') == 0) then
            print '(a)', "FAIL: JSON missing number token"
            test_json_serialization = .false.
        end if
        
        if (index(json_str, '"text": "5"') == 0) then
            print '(a)', "FAIL: JSON missing 5 text"
            test_json_serialization = .false.
        end if
        
        if (test_json_serialization) then
            print '(a)', "PASS: JSON string serialization"
        end if
        
    end function test_json_serialization

    logical function test_json_file_output()
        type(token_t), allocatable :: tokens(:)
        character(len=*), parameter :: test_file = "test_tokens.json"
        integer :: unit, iostat, file_size
        character(len=1000) :: buffer
        character(len=100) :: line
        
        test_json_file_output = .true.
        
        print '(a)', "Testing JSON file serialization..."
        
        ! Tokenize simple expression
        call tokenize("y = 3.14", tokens)
        
        ! Write to file
        call json_write_tokens_to_file(tokens, test_file)
        
        ! Check if file was created and has content
        inquire(file=test_file, size=file_size, iostat=iostat)
        
        if (iostat /= 0) then
            print '(a)', "FAIL: Could not inquire about test file"
            test_json_file_output = .false.
            return
        end if
        
        if (file_size <= 0) then
            print '(a)', "FAIL: Test file is empty"
            test_json_file_output = .false.
            return
        end if
        
        ! Read entire file content
        open(newunit=unit, file=test_file, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print '(a)', "FAIL: Could not open test file for reading"
            test_json_file_output = .false.
            return
        end if
        
        ! Read entire file as one string
        buffer = ""
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            buffer = trim(buffer) // " " // trim(line)
        end do
        close(unit)
        
        ! Basic validation
        if (index(buffer, '"tokens"') == 0) then
            print '(a)', "FAIL: File missing tokens array"
            test_json_file_output = .false.
        end if
        
        if (index(buffer, '"text": "y"') == 0) then
            print '(a)', "FAIL: File missing y text"
            test_json_file_output = .false.
        end if
        
        if (index(buffer, '"text": "3.14"') == 0) then
            print '(a)', "FAIL: File missing 3.14 text"
            test_json_file_output = .false.
        end if
        
        ! Clean up
        open(newunit=unit, file=test_file, status='old')
        close(unit, status='delete')
        
        if (test_json_file_output) then
            print '(a)', "PASS: JSON file serialization"
        end if
        
    end function test_json_file_output

end program test_lexer_serialization