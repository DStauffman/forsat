! Builds the unit test into a program and runs it
! Autobuilt by dstauffman Fortran code

program run_test_utils_find_those_alive
    use fruit
    use test_utils_find_those_alive
    call init_fruit
    call setup
    call test_normal_use
    call setup
    call test_nan_use
    call fruit_summary
    call fruit_finalize
end program run_test_utils_find_those_alive
