module test_stats_prob_to_rate

    use fruit, only: assert_equals

    use constants, only: ONE, RK, TWO, ZERO
    use stats,     only: prob_to_rate, rate_to_prob

    implicit none
    
    ! constants
    real(RK), parameter :: TOLERANCE = 1e-10_RK

    ! Global variables for use in all tests
    integer                             :: i
    real(RK)                            :: time, rate_0d
    real(RK), dimension(:), allocatable :: prob, prob2, rate, exp_rate

contains

    subroutine setup
        prob     = [(0.01_RK*i, i=1, 100)]
        time     = 5._RK
        exp_rate = -log(1 - prob) / time
    end subroutine setup

    subroutine test_conversion
        rate = prob_to_rate(prob, time)
        call assert_equals(exp_rate, rate, size(exp_rate), tolerance)
    end subroutine

    subroutine test_scalar
        rate_0d = prob_to_rate(ZERO)
        call assert_equals(ZERO, rate_0d)
    end subroutine

    subroutine test_lt_zero
        !try:
        !    prob_to_rate([ZERO, 0.5_RK, -1._RK])
        !catch:
        !    ! I wish I could
    end subroutine

    subroutine test_gt_one
        ! Should error:
        ! prob_to_rate([0._RK, 0.5_RK, 1.5_RK])
    end subroutine

    subroutine test_circular
        rate = prob_to_rate(prob, time)
        call assert_equals(exp_rate, rate, size(exp_rate), tolerance)
        prob2 = rate_to_prob(rate, time)
        call assert_equals(prob, prob2, size(prob), tolerance)
    end subroutine

end module test_stats_prob_to_rate
