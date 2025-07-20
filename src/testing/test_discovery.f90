module test_discovery
    use iso_fortran_env, only: error_unit
    use fpm, only: build_model
    use fpm_model, only: fpm_model_t, FPM_SCOPE_TEST
    use fpm_command_line, only: fpm_build_settings
    use fpm_manifest, only: get_package_data, package_config_t
    use fpm_targets, only: targets_from_sources, build_target_t, build_target_ptr, FPM_TARGET_EXECUTABLE
    use fpm_filesystem, only: exists
    use fpm_error, only: error_t
    use fpm_strings, only: string_t
    implicit none
    private

    public :: discover_fpm_tests, test_case_t

    integer, parameter :: MAX_TESTS = 200
    integer, parameter :: MAX_PATH_LEN = 512

    type :: test_case_t
        character(len=MAX_PATH_LEN) :: name = ""
        character(len=MAX_PATH_LEN) :: executable = ""
    end type test_case_t

contains

    subroutine discover_fpm_tests(tests, num_tests, filter, quiet, success)
        type(test_case_t), intent(out) :: tests(:)
        integer, intent(out) :: num_tests
        character(len=*), intent(in) :: filter
        logical, intent(in) :: quiet
        logical, intent(out) :: success

        type(fpm_model_t) :: model
        type(fpm_build_settings) :: settings
        type(package_config_t) :: package
        type(error_t), allocatable :: error
        type(build_target_ptr), allocatable :: targets(:)
        integer :: i
        character(len=MAX_PATH_LEN) :: test_name

        success = .false.
        num_tests = 0

        ! Initialize build settings
        settings%profile = "release"
        settings%prune = .true.
        settings%build_tests = .true.

        ! Get package manifest
        call get_package_data(package, ".", error, apply_defaults=.true.)
        if (allocated(error)) then
     if (.not. quiet) write (error_unit, '(A)') "ERROR: Failed to read package manifest"
            return
        end if

        ! Build model
        call build_model(model, settings, package, error)
        if (allocated(error)) then
            if (.not. quiet) write (error_unit, '(A)') "ERROR: Failed to build model"
            return
        end if

        ! Generate targets
       call targets_from_sources(targets, model, settings%prune, package%library, error)
        if (allocated(error)) then
          if (.not. quiet) write (error_unit, '(A)') "ERROR: Failed to generate targets"
            return
        end if

        ! Extract test executables
        if (.not. quiet) write (*, '(A)') "Discovering tests..."

        do i = 1, size(targets)
            ! Check if this is a test executable
            if (targets(i)%ptr%target_type == FPM_TARGET_EXECUTABLE .and. &
                allocated(targets(i)%ptr%source) .and. &
                targets(i)%ptr%source%unit_scope == FPM_SCOPE_TEST) then

                ! Extract test name from output file path
                test_name = extract_test_name(targets(i)%ptr%output_file)

                ! Apply filter if specified
                if (len_trim(filter) > 0) then
                    if (index(test_name, trim(filter)) == 0) cycle
                end if

                ! Check if executable exists
                if (.not. exists(targets(i)%ptr%output_file)) cycle

                ! Add to test list
                if (num_tests < size(tests)) then
                    num_tests = num_tests + 1
                    tests(num_tests)%name = trim(test_name)
                    tests(num_tests)%executable = trim(targets(i)%ptr%output_file)
                end if
            end if
        end do

        success = .true.
    end subroutine discover_fpm_tests

    function extract_test_name(output_file) result(test_name)
        character(len=*), intent(in) :: output_file
        character(len=MAX_PATH_LEN) :: test_name
        integer :: idx

        ! Extract just the filename from the path
        idx = index(output_file, '/', back=.true.)
        if (idx > 0) then
            test_name = output_file(idx + 1:)
        else
            test_name = trim(output_file)
        end if
    end function extract_test_name

end module test_discovery
