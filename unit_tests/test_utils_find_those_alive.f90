module test_utils_find_those_alive

    use fruit, only: assert_equals

    use constants, only: NAN, RK
    use utils,     only: find_those_alive

    implicit none

    ! Global variables for use in all tests
    real(RK), dimension(:), allocatable :: age, age_nan
    logical,  dimension(:), allocatable :: exp_alive, alive

contains

    subroutine setup
        age        = [-1._RK, -2._RK, 0._RK, 15._RK, 34.2_RK]
        exp_alive  = [.false., .false., .false., .true., .true.]
        age_nan    = age(:)
        age_nan(2) = NAN
    end subroutine setup

    subroutine test_normal_use
        alive = find_those_alive(age)
        call assert_equals(exp_alive, alive, size(exp_alive))
    end subroutine test_normal_use

    subroutine test_nan_use
        alive = find_those_alive(age_nan)
        call assert_equals(exp_alive, alive, size(exp_alive))
    end subroutine test_nan_use


end module test_utils_find_those_alive
