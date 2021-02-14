module kalman

    use constants, only: ONE, RK, ZERO
    !use la_precision, only: WP => RK
    !use f95_lapack, only: la_gesv

    implicit none (type, external)

    external :: dgelsd

    private

    public :: calculate_kalman_gain, calculate_prediction, calculate_innovation, calculate_normalized_innovation, &
        calculate_delta_state, mat_divide, propagate_covariance, update_covariance

    interface mat_divide
        module procedure mat_divide_2d_2d
        module procedure mat_divide_2d_1d
    end interface mat_divide

    interface calculate_prediction
        module procedure calculate_prediction_no_const
        module procedure calculate_prediction_with_const
    end interface calculate_prediction

    interface propagate_covariance
        module procedure propagate_covariance_gamma
        module procedure propagate_covariance_no_gamma
    end interface propagate_covariance

    interface propagate_covariance_inplace
        module procedure propagate_covariance_inplace_gamma
        module procedure propagate_covariance_inplace_no_gamma
    end interface propagate_covariance_inplace

contains

    pure function eye(n) result(res)
        ! inputs and outputs
        integer, intent(in) :: n
        real(RK), dimension(n, n) :: res
        ! local variables
        integer :: i
        res = ZERO
        do i = 1, n
            res(i, i) = ONE
        end do
    end function eye

    function mat_divide_2d_2d(a, b) result(res)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(in)  :: a
        real(RK), dimension(:, :), intent(in)  :: b
        real(RK), dimension(:, :), allocatable :: res
        ! local variables
        integer, parameter                     :: lwmax = 1000
        integer                                :: info, m, n, lda, ldb, lwork, nrhs, rank
        integer, dimension(:),     allocatable :: iwork
        real(RK)                               :: rcond
        real(RK), dimension(:),    allocatable :: s, work
        real(RK), dimension(:, :), allocatable :: wa, wb
        ! create working copies as the lapack routine destroys them inplace
        allocate(wa(size(a, 1), size(a, 2)), wb(size(b, 1), size(b, 2)))
        wa = a
        wb = b
        ! call the least squares solver using modern fortran the way that it should work, but that I can't install
        !call la_gels(wa, wb)
        ! call the least squares solver using code that is older than I am
        m = size(wa, 1)
        n = size(wa, 2)
        nrhs = size(wb, 2)
        lda = max(1, m)
        ldb = max(1, max(m, n))
        allocate(s(min(m, n)))
        allocate(work(lwmax))
        allocate(iwork(lwmax))
        rcond = -1._RK
        ! query the optimal workspace
        lwork = -1
        call dgelsd(m, n, nrhs, wa, lda, wb, ldb, s, rcond, rank, work, lwork, iwork, info)
        lwork = min(lwmax, int(work(1)))
        if (lwork /= lwmax) then
            deallocate(work)
            deallocate(iwork)
            allocate(work(lwork))
            allocate(iwork(lwork))
        end if
        ! solve equation
        call dgelsd(m, n, nrhs, wa, lda, wb, ldb, s, rcond, rank, work, lwork, iwork, info)
        ! check for convergence
        if (info > 0) then
            error stop 'The algorithm computing SVD failed to converge.'
        end if
        ! store the solution
        res = wb(:, 1:nrhs)
    end function mat_divide_2d_2d

    function mat_divide_2d_1d(a, b) result(res)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(in) :: a
        real(RK), dimension(:),    intent(in) :: b
        real(RK), dimension(:), allocatable   :: res
        ! local variables
        integer, parameter                     :: lwmax = 1000
        integer                                :: info, m, n, lda, ldb, lwork, nrhs, rank
        integer, dimension(:),     allocatable :: iwork
        real(RK)                               :: rcond
        real(RK), dimension(:),    allocatable :: s, work
        real(RK), dimension(:, :), allocatable :: wa, wb
        ! create working copies as the lapack routine destroys them inplace
        allocate(wa(size(a, 1), size(a, 2)), wb(size(b), 1))
        wa = a
        wb(:, 1) = b
        ! call the least squares solver
        !call la_gels(wa, wb)
        ! call the least squares solver using code that is older than I am
        m = size(wa, 1)
        n = size(wa, 2)
        nrhs = size(wb, 2) ! should always be 1
        lda = max(1, m)
        ldb = max(1, max(m, n))
        allocate(s(min(m, n)))
        allocate(work(lwmax))
        allocate(iwork(lwmax))
        rcond = -1._RK
        ! query the optimal workspace
        lwork = -1
        call dgelsd(m, n, nrhs, wa, lda, wb, ldb, s, rcond, rank, work, lwork, iwork, info)
        lwork = min(lwmax, int(work(1)))
        if (lwork /= lwmax) then
            deallocate(work)
            deallocate(iwork)
            allocate(work(lwork))
            allocate(iwork(lwork))
        end if
        ! solve equation
        call dgelsd(m, n, nrhs, wa, lda, wb, ldb, s, rcond, rank, work, lwork, iwork, info)
        ! check for convergence
        if (info > 0) then
            error stop 'The algorithm computing SVD failed to converge.'
        end if
        ! store the solution
        res = wb(:, 1)
    end function mat_divide_2d_1d

    function calculate_kalman_gain(P, H, R) result(K)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(in)  :: P
        real(RK), dimension(:, :), intent(in)  :: H
        real(RK), dimension(:, :), intent(in)  :: R
        real(RK), dimension(:, :), allocatable :: K
        ! local variables
        real(RK), dimension(:, :), allocatable :: Pz
        ! calculate the innovation covariance
        Pz = matmul(H, matmul(P, transpose(H))) + R
        ! implicit solver
        K = transpose(mat_divide(transpose(Pz), transpose(matmul(P, transpose(H)))))
    end function calculate_kalman_gain

    subroutine calculate_kalman_gain_and_innov_cov(P, H, R, K, Pz)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(in)    :: P
        real(RK), dimension(:, :), intent(in)    :: H
        real(RK), dimension(:, :), intent(in)    :: R
        real(RK), dimension(:, :), intent(inout) :: K
        real(RK), dimension(:, :), intent(inout) :: Pz
        ! calculate the innovation covariance
        Pz = matmul(H, matmul(P, transpose(H))) + R
        ! implicit solver
        K = transpose(mat_divide(transpose(Pz), transpose(matmul(P, transpose(H)))))
    end subroutine calculate_kalman_gain_and_innov_cov

    pure function calculate_prediction_no_const(H, state) result(u)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(in) :: H
        real(RK), dimension(:),    intent(in) :: state
        real(RK), dimension(:), allocatable   :: u
        ! calculations
        u = matmul(H, state)
    end function calculate_prediction_no_const

    pure function calculate_prediction_with_const(H, state, const) result(u)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(in)  :: H
        real(RK), dimension(:),    intent(in)  :: state
        real(RK), dimension(:),    intent(in)  :: const
        real(RK), dimension(:), allocatable    :: u
        u = matmul(H, state + const)
    end function calculate_prediction_with_const

    pure function calculate_innovation(u_meas, u_pred) result(innov)
        ! inputs and outputs
        real(RK), dimension(:), intent(in)  :: u_meas
        real(RK), dimension(:), intent(in)  :: u_pred
        real(RK), dimension(:), allocatable :: innov
        innov = u_meas - u_pred
    end function calculate_innovation

    function calculate_normalized_innovation(z, Pz) result(n_innov)
        ! inputs and outputs
        real(RK), dimension(:),    intent(in) :: z
        real(RK), dimension(:, :), intent(in) :: Pz
        real(RK), dimension(:), allocatable   :: n_innov
        n_innov = mat_divide(Pz, z)
    end function calculate_normalized_innovation

    pure function calculate_delta_state(K, z) result(dx)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(in) :: K
        real(RK), dimension(:),    intent(in) :: z
        real(RK), dimension(:), allocatable   :: dx
        ! calculations
        dx = matmul(K, z)
    end function calculate_delta_state

    pure function propagate_covariance_no_gamma(P, phi, Q) result(cov)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(in)  :: P
        real(RK), dimension(:, :), intent(in)  :: phi
        real(RK), dimension(:, :), intent(in)  :: Q
        real(RK), dimension(:, :), allocatable :: cov
        ! calculations
        cov = matmul(phi, matmul(P, transpose(phi))) + Q
    end function propagate_covariance_no_gamma

    pure function propagate_covariance_gamma(P, phi, Q, gamma) result(cov)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(in)  :: P
        real(RK), dimension(:, :), intent(in)  :: phi
        real(RK), dimension(:, :), intent(in)  :: Q
        real(RK), dimension(:, :), intent(in)  :: gamma
        real(RK), dimension(:, :), allocatable :: cov
        ! calculations
        cov = matmul(phi, matmul(P, transpose(phi))) + matmul(gamma, matmul(Q, transpose(gamma)))
    end function propagate_covariance_gamma

    subroutine propagate_covariance_inplace_no_gamma(P, phi, Q)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(inout) :: P
        real(RK), dimension(:, :), intent(in)    :: phi
        real(RK), dimension(:, :), intent(in)    :: Q
        ! calculations
        P(:, :) = matmul(phi, matmul(P, transpose(phi))) + Q
    end subroutine propagate_covariance_inplace_no_gamma

    subroutine propagate_covariance_inplace_gamma(P, phi, Q, gamma)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(inout) :: P
        real(RK), dimension(:, :), intent(in)    :: phi
        real(RK), dimension(:, :), intent(in)    :: Q
        real(RK), dimension(:, :), intent(in)    :: gamma
        ! calculations
        P(:, :) = matmul(phi, matmul(P, transpose(phi))) + matmul(gamma, matmul(Q, transpose(gamma)))
    end subroutine propagate_covariance_inplace_gamma

    pure function update_covariance(P, K, H) result(cov)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(in)  :: P
        real(RK), dimension(:, :), intent(in)  :: K
        real(RK), dimension(:, :), intent(in)  :: H
        real(RK), dimension(:, :), allocatable :: cov
        ! local variables
        integer n
        ! calculations
        n = size(P, 1)
        cov = matmul(eye(n) - matmul(K, H), P)
    end function update_covariance

    subroutine update_covariance_inplace(P, K, H, cov)
        ! inputs and outputs
        real(RK), dimension(:, :), intent(inout) :: P
        real(RK), dimension(:, :), intent(in)    :: K
        real(RK), dimension(:, :), intent(in)    :: H
        real(RK), dimension(:, :), intent(inout) :: cov
        ! local variables
        integer n
        ! calculations
        n = size(P, 1)
        cov(:, :) = matmul(eye(n) - matmul(K, H), P)
    end subroutine update_covariance_inplace

end module kalman
