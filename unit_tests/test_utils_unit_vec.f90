module test_utils_unit_vec

    use fruit, only: assert_equals

    use constants, only: ONE, RK, TWO, ZERO
    use utils,     only: unit_vec

    implicit none
    
    ! constants
    real(RK), parameter :: HR2 = sqrt(TWO) / TWO
    real(RK), parameter :: tolerance = 1e-14_RK

    ! Global variables for use in all tests
    real(RK), dimension(:),    allocatable :: norm_1d
    real(RK), dimension(:, :), allocatable :: data, exp_data, norm_2d

contains

    subroutine setup
        data     = reshape([ONE, ZERO, ZERO, ZERO, ZERO, ZERO, -ONE, ZERO, ONE], [3, 3])
        exp_data = reshape([ONE, ZERO, ZERO, ZERO, ZERO, ZERO, -HR2, ZERO, HR2], [3, 3])
    end subroutine setup

    subroutine test_nominal
        norm_2d = unit_vec(data, dim=1)
        call assert_equals(exp_data, norm_2d, size(exp_data,1), size(exp_data,2), tolerance)
    end subroutine

    subroutine test_bad_axis
        ! TODO: expect error
!        norm_data = unit_vec(data, dim=3)
    end subroutine

    subroutine test_single_vector
    integer :: i
        do i=1, 3
            norm_1d = unit_vec(data(:, i))
            call assert_equals(exp_data(:, i), norm_1d, size(exp_data(:, i)), tolerance)
        end do
    end subroutine

    subroutine test_single_vector_axis1
        integer :: i
        do i=1, 3
            norm_2d = unit_vec(spread(data(:,i), dim=2, ncopies=1), dim=1)
            call assert_equals(exp_data(:,i), pack(norm_2d, mask=.true.), size(exp_data(:,i)), tolerance)
        end do
    end subroutine

end module test_utils_unit_vec
