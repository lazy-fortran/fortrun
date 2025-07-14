module preprocessor
    ! Compatibility module - redirects to the new compiler frontend
    ! This module exists only for backward compatibility and will be removed in future versions
    
    use frontend_integration, only: compile_with_frontend, compile_with_frontend_debug, is_simple_fortran_file
    
    implicit none
    private
    
    public :: preprocess_file, is_preprocessor_file, preprocess_file_debug
    
contains

    ! Compatibility wrapper
    subroutine preprocess_file(input_file, output_file, error_msg)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg
        
        call compile_with_frontend(input_file, output_file, error_msg)
    end subroutine preprocess_file
    
    ! Compatibility wrapper
    subroutine preprocess_file_debug(input_file, output_file, error_msg, debug_tokens, debug_ast, debug_semantic, debug_codegen)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg
        logical, intent(in) :: debug_tokens, debug_ast, debug_semantic, debug_codegen
        
        call compile_with_frontend_debug(input_file, output_file, error_msg, debug_tokens, debug_ast, debug_semantic, debug_codegen)
    end subroutine preprocess_file_debug
    
    ! Compatibility wrapper
    function is_preprocessor_file(filename) result(is_sf)
        character(len=*), intent(in) :: filename
        logical :: is_sf
        
        is_sf = is_simple_fortran_file(filename)
    end function is_preprocessor_file
    
end module preprocessor