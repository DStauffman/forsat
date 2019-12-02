! Builds the unit test into a program and runs it
! Autobuilt by dstauffman Fortran code

program run_test_utils_histcounts
    use fruit
    use test_utils_histcounts
    call init_fruit
    call setup
    call test_nominal
    call setup
    call test_right
    call setup
    call test_out_of_bounds
    call fruit_summary
    call fruit_finalize
end program run_test_utils_histcounts
