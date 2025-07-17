module standardizer
    ! Lazy fortran standardization module - converts .f files to standard F90
    ! Transforms lazy fortran dialect to standard Fortran with type inference and program structure

    use frontend_integration, only: compile_with_frontend, compile_with_frontend_debug, is_simple_fortran_file

    implicit none
    private

    public :: standardize_file, is_standardizable_file, standardize_file_debug

contains

    ! Standardize lazy fortran file to standard F90
    subroutine standardize_file(input_file, output_file, error_msg)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg

        call compile_with_frontend(input_file, output_file, error_msg)
    end subroutine standardize_file

    ! Standardize with debug output
    subroutine standardize_file_debug(input_file, output_file, error_msg, debug_tokens, debug_ast, debug_semantic, debug_codegen)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg
        logical, intent(in) :: debug_tokens, debug_ast, debug_semantic, debug_codegen

        call compile_with_frontend_debug(input_file, output_file, error_msg, debug_tokens, debug_ast, debug_semantic, debug_codegen)
    end subroutine standardize_file_debug

    ! Check if file can be standardized (lazy fortran .f file)
    function is_standardizable_file(filename) result(is_sf)
        character(len=*), intent(in) :: filename
        logical :: is_sf

        is_sf = is_simple_fortran_file(filename)
    end function is_standardizable_file

end module standardizer
