! Builds the unit test into a program and runs it
! Autobuilt by dstauffman Fortran code

program run_test_stats_prob_to_rate
    use fruit
    use test_stats_prob_to_rate
    call init_fruit
    call setup
    call test_conversion
    call setup
    call test_scalar
    call setup
    call test_lt_zero
    call setup
    call test_gt_one
    call setup
    call test_circular
    call fruit_summary
    call fruit_finalize
end program run_test_stats_prob_to_rate
