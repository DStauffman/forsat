! Builds the unit test into a program and runs it
! Autobuilt by dstauffman Fortran code

program run_test_utils_unit_vec
    use fruit
    use test_utils_unit_vec
    call init_fruit
    call setup
    call test_nominal
    call setup
    call test_bad_axis
    call setup
    call test_single_vector
    call setup
    call test_single_vector_axis1
    call fruit_summary
    call fruit_finalize
end program run_test_utils_unit_vec
