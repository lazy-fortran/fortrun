program debug_multi_function_parse
    use frontend, only: compile_source, compilation_options_t
    implicit none
    
    type(compilation_options_t) :: options
    character(len=256) :: error_msg
    
    ! Set up debug options
    options%debug_tokens = .true.
    options%debug_ast = .true.
    options%debug_semantic = .false.
    options%debug_codegen = .false.
    
    ! Try to compile the test file
    call compile_source("example/frontend_test_cases/multiple_functions/multiple_functions.f", options, error_msg)
    
    if (error_msg /= "") then
        print *, "Error: ", trim(error_msg)
    else
        print *, "Compilation completed"
    end if
    
end program debug_multi_function_parse