! Builds all the unit tests into a single program and runs it
! Autobuilt by dstauffman Fortran code

program run_all_tests
    !! imports
    use fruit

    use test_stats_prob_to_rate, only: t1_setup=>setup, t1_test_conversion=>test_conversion, t1_test_scalar=>test_scalar, &
                                       t1_test_lt_zero=>test_lt_zero, t1_test_gt_one=>test_gt_one, t1_test_circular=>test_circular
    use test_text_input_str2num, only: t2_test_bool=>test_bool, t2_test_integer=>test_integer, &
                                       t2_test_integer_vector=>test_integer_vector, t2_test_real=>test_real, &
                                       t2_test_real_vector=>test_real_vector, t2_test_string=>test_string
    use test_utils_find_those_alive, only: t3_setup=>setup, t3_test_normal_use=>test_normal_use, t3_test_nan_use=>test_nan_use
    use test_utils_histcounts, only: t4_setup=>setup, t4_test_nominal=>test_nominal, t4_test_right=>test_right, &
                                     t4_test_out_of_bounds=>test_out_of_bounds
    use test_utils_unit_vec, only: t5_setup=>setup, t5_test_nominal=>test_nominal, t5_test_bad_axis=>test_bad_axis, &
                                   t5_test_single_vector=>test_single_vector, t5_test_single_vector_axis1=>test_single_vector_axis1
    !! fruit initialization
    call init_fruit

    !! tests
    ! test_stats_prob_to_rate
    call t1_setup
    call t1_test_conversion
    call t1_setup
    call t1_test_scalar
    call t1_setup
    call t1_test_lt_zero
    call t1_setup
    call t1_test_gt_one
    call t1_setup
    call t1_test_circular
    ! test_text_input_str2num
    call t2_test_bool
    call t2_test_integer
    call t2_test_integer_vector
    call t2_test_real
    call t2_test_real_vector
    call t2_test_string
    ! test_utils_find_those_alive
    call t3_setup
    call t3_test_normal_use
    call t3_setup
    call t3_test_nan_use
    ! test_utils_histcounts
    call t4_setup
    call t4_test_nominal
    call t4_setup
    call t4_test_right
    call t4_setup
    call t4_test_out_of_bounds
    ! test_utils_unit_vec
    call t5_setup
    call t5_test_nominal
    call t5_setup
    call t5_test_bad_axis
    call t5_setup
    call t5_test_single_vector
    call t5_setup
    call t5_test_single_vector_axis1

    !! Fruit finalization
    call fruit_summary
    call fruit_finalize
end program run_all_tests
