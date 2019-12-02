module test_utils_histcounts

    use fruit, only: assert_equals

    use constants, only: RK
    use utils,     only: histcounts

    implicit none

    ! Global variables for use in all tests
    real(RK), dimension(:), allocatable :: x, bins
    integer,  dimension(:), allocatable :: expected, hist

contains

    subroutine setup
        x        = [0.2_RK, 6.4_RK, 3.0_RK, 1.6_RK, 0.5_RK]
        bins     = [0.0_RK, 1.0_RK, 2.5_RK, 4.0_RK, 10.0_RK]
        expected = [2, 1, 1, 1]
    end subroutine setup

    subroutine test_nominal
        hist = histcounts(x, bins)
        call assert_equals(expected, hist, size(expected))
    end subroutine

    subroutine test_right
        x    = [1._RK, 1._RK, 2._RK, 2._RK, 2._RK]
        bins = [0._RK, 1._RK, 2._RK, 3._RK]
        hist = histcounts(x, bins, right=.false.)
        call assert_equals([0, 2, 3], hist, 3, 'right is bad')

        hist = histcounts(x, bins, right=.true.)
        call assert_equals([2, 3, 0], hist, 3, 'left is bad')
    end subroutine

    subroutine test_out_of_bounds
        hist = histcounts(x, [100._RK, 1000._RK], check_bounds=.false.)
        call assert_equals([0], hist, 1, 'should have errored')

        ! TODO: this should error
        !hist = histcounts(x, [100._RK, 1000._RK], check_bounds=.true.)
    end subroutine

end module test_utils_histcounts
