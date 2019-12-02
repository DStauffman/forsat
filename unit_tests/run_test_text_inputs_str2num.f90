! Builds the unit test into a program and runs it
! Autobuilt by dstauffman Fortran code

program run_test_text_input_str2num
    use fruit
    use test_text_input_str2num
    call init_fruit
    call test_bool
    call test_integer
    call test_integer_vector
    call test_real
    call test_real_vector
    call test_string
    call fruit_summary
    call fruit_finalize
end program run_test_text_input_str2num
