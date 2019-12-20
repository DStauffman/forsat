module test_text_input_str2num

    use fruit,      only: assert_equals, assert_true

    use constants,  only: LARGE, LARGE_INT, NEG_LARGE, NEG_LARGE_INT, RK, SMALL
    use text_input, only: str2num

    implicit none

contains

    subroutine test_bool
        logical :: num
        call str2num('T', num)
        call assert_equals(.true., num)
        call str2num('.true.', num)
        call assert_equals(.true., num)
        call str2num('.TRUE.', num)
        call assert_equals(.true., num)
        call str2num('TRUE', num)
        call assert_equals(.true., num)
        call str2num('F', num)
        call assert_equals(.false., num)
        call str2num('.false.', num)
        call assert_equals(.false., num)
        call str2num('.FALSE.', num)
        call assert_equals(.false., num)
        call str2num('FALSE', num)
        call assert_equals(.false., num)
    end subroutine

    subroutine test_integer
        integer :: num
        call str2num('101', num)
        call assert_equals(101, num)
        call str2num('000005', num)
        call assert_equals(5, num)
        call str2num('    6   ', num)
        call assert_equals(6, num)
        call str2num('-30010', num)
        call assert_equals(-30010, num)
        call str2num('LARGE_INT', num)
        call assert_equals(LARGE_INT, num)
        call str2num('NEG_LARGE_INT', num)
        call assert_equals(NEG_LARGE_INT, num)
    end subroutine

    subroutine test_integer_vector
        integer, dimension(:), allocatable :: num, exp
        call str2num('[1, 3, 5]', num)
        exp = [1, 3, 5]
        call assert_equals(exp, num, size(exp))
        call str2num('[12030]', num)
        exp = [12030]
        call assert_equals(exp, num, size(exp))
        call str2num('[004, LARGE_INT, 050, NEG_LARGE_INT]', num)
        exp = [4, LARGE_INT, 50, NEG_LARGE_INT]
        call assert_equals(exp, num, size(exp))
    end subroutine

    subroutine test_real
        real(RK) :: num
        call str2num('101.', num)
        call assert_equals(101._RK, num)
        call str2num('000005.', num)
        call assert_equals(5._RK, num)
        call str2num('    6.3   ', num)
        call assert_equals(6.3_RK, num)
        call str2num('-30.010', num)
        call assert_equals(-30.010_RK, num)
        call str2num('5.02e6', num)
        call assert_equals(5.02e6_RK, num)
        call str2num('-3.06e-8', num)
        call assert_equals(-3.06e-8_RK, num)
        call str2num('   LARGE', num)
        call assert_equals(LARGE, num)
        call str2num('NEG_LARGE   ', num)
        call assert_equals(NEG_LARGE, num)
        call str2num('SMALL', num)
        call assert_equals(SMALL, num)
    end subroutine

    subroutine test_real_vector
        real(RK), dimension(:), allocatable :: num, exp
        call str2num('[1.5, 3.14159, 005.]', num)
        exp = [1.5_RK, 3.14159_RK, 5._RK]
        call assert_equals(exp, num, size(exp))
        call str2num('[-11.3240]', num)
        exp = [-11.324_RK]
        call assert_equals(exp, num, size(exp))
        call str2num('[0.404, LARGE, 0.50e-3, NEG_LARGE, SMALL]', num)
        exp = [0.404_RK, LARGE, 0.0005_RK, NEG_LARGE, SMALL]
        call assert_equals(exp, num, size(exp))
    end subroutine

    subroutine test_string
        character(len=:), allocatable :: str
        call str2num('''A string to test''', str)
        call assert_equals('A string to test', str)
        call str2num("''", str)
        call assert_equals('', str)
    end subroutine

end module test_text_input_str2num
