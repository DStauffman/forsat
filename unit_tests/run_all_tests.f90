! Builds all the unit tests into a single program and runs it
! Autobuilt by dstauffman Fortran code

program run_all_tests
    !! imports
    use fruit

    use test_text_input_str2num, only: t1_test_bool=>test_bool, t1_test_integer=>test_integer, &
                                       t1_test_integer_vector=>test_integer_vector, t1_test_real=>test_real, &
                                       t1_test_real_vector=>test_real_vector, t1_test_string=>test_string
    use test_utils_find_those_alive, only: t2_setup=>setup, t2_test_normal_use=>test_normal_use, t2_test_nan_use=>test_nan_use
    use test_utils_histcounts, only: t3_setup=>setup, t3_test_nominal=>test_nominal, t3_test_right=>test_right, &
                                     t3_test_out_of_bounds=>test_out_of_bounds
    use test_utils_unit_vec, only: t4_setup=>setup, t4_test_nominal=>test_nominal, t4_test_bad_axis=>test_bad_axis, &
                                   t4_test_single_vector=>test_single_vector, t4_test_single_vector_axis1=>test_single_vector_axis1
    !! fruit initialization
    call init_fruit

    !! tests
    ! test_text_input_str2num
    call t1_test_bool
    call t1_test_integer
    call t1_test_integer_vector
    call t1_test_real
    call t1_test_real_vector
    call t1_test_string
    ! test_utils_find_those_alive
    call t2_setup
    call t2_test_normal_use
    call t2_setup
    call t2_test_nan_use
    ! test_utils_histcounts
    call t3_setup
    call t3_test_nominal
    call t3_setup
    call t3_test_right
    call t3_setup
    call t3_test_out_of_bounds
    ! test_utils_unit_vec
    call t4_setup
    call t4_test_nominal
    call t4_setup
    call t4_test_bad_axis
    call t4_setup
    call t4_test_single_vector
    call t4_setup
    call t4_test_single_vector_axis1

    !! Fruit finalization
    call fruit_summary
    call fruit_finalize
end program run_all_tests
