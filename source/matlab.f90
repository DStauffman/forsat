module matlab

    use ieee_arithmetic, only: ieee_is_nan

    use constants, only: NAN, ONE, RK, ZERO

    implicit none

    private

    public :: cumsum, diff, discretize, find, is_nan, logical_to_num, maximum, mean, minimum

    interface cumsum
        module procedure cumsum_1d
        module procedure cumsum_2d
    end interface cumsum
    interface diff
        module procedure diff_1d
    end interface diff
    interface discretize
        module procedure discretize_real
    end interface discretize
contains

    !! is_nan
    pure elemental function is_nan(x) result(y)
        real(RK), intent(in) :: x
        logical              :: y
        if (ieee_is_nan(x)) then
            y = .true.
        else
            y = .false.
        end if
    end function is_nan

    !! find
    pure function find(ix_logic) result(ix_int)
        logical, intent(in), dimension(:)  :: ix_logic
        integer, dimension(:), allocatable :: ix_int
        ! local variables
        integer :: i, c
        integer, dimension(1:size(ix_logic)) :: temp
        ! find the indices where the ix_logic is true
        c = 0
        do i = lbound(ix_logic, dim=1), ubound(ix_logic, dim=1)
            if (ix_logic(i)) then
                c = c + 1
                temp(c) = i
            end if
        end do
        ix_int = temp(1:c)
    end function find

    !! cumsum
    pure function cumsum_1d(array, axis) result(output)
        real(RK), intent(in), dimension(:)  :: array
        integer, intent(in), optional       :: axis
        real(RK), dimension(:), allocatable :: output
        ! local variables
        integer  :: this_axis, i
        real(RK) :: temp
        if (present(axis)) then
            this_axis = axis
        else
            this_axis = 1
        end if
        ! TODO: add check that axis must be 1
        allocate(output, mold=array)
        temp = 0._RK
        do i = lbound(array, dim=this_axis), ubound(array, dim=this_axis)
            temp = temp + array(i)
            output(i) = temp
        end do
    end function cumsum_1d

    pure function cumsum_2d(array, axis) result(output)
        real(RK), intent(in), dimension(:,:)  :: array
        integer, intent(in), optional         :: axis
        real(RK), dimension(:,:), allocatable :: output
        ! local variables
        integer                             :: this_axis, i, num
        real(RK), dimension(:), allocatable :: temp
        if (present(axis)) then
            this_axis = axis
        else
            this_axis = 1
        end if
        select case (this_axis)
            case (1)
                num = size(array, 2)
            case (2)
                num = size(array, 1)
            case default
                error stop 'Bad value for cumsum axis.'
        end select
        allocate(output, mold=array)
        allocate(temp(num))
        temp = 0._RK
        do i = lbound(array, dim=this_axis), ubound(array, dim=this_axis)
            if (this_axis == 1) then
                temp = temp + array(i, :)
                output(i, :) = temp
            else
                temp = temp + array(:, i)
                output(:, i) = temp
            end if
        end do
    end function cumsum_2d

    !! diff
    pure function diff_1d(array, axis) result(output)
        real(RK), intent(in), dimension(:), allocatable :: array
        integer, intent(in), optional                   :: axis
        real(RK), dimension(:), allocatable             :: output
        ! local variables
        integer :: lb, ub
        if (present(axis)) then
            if ((axis > 1) .or. (axis < 1)) then
                error stop 'Bad axis for single dimension array.'
            end if
        end if
        lb = lbound(array, 1)
        ub = ubound(array, 1)
        output = array(lb+1:ub) - array(lb:ub-1)
    end function diff_1d

    !! discretize
    pure function discretize_real(x, bins, right) result(output)
        !> Returns the indices of the bins to which each value in input array belongs.
        real(RK), intent(in), dimension(:) :: x ! Input array to be binned
        real(RK), intent(in), dimension(:) :: bins ! Array of bins. It has to be 1-dimensional and monotonic
        logical, intent(in), optional      :: right ! Indicating whether the intervals include the right or the left bin
                                                                 ! edge. Default behavior is (right==False) indicating that the interval
                                                                 ! does not include the right edge. The left bin end is open in this
                                                                 ! case, i.e., bins(i-1) <= x < bins(i) is the default behavior for
                                                                 ! monotonically increasing bins.
        integer, dimension(:), allocatable :: output
        ! local variables
        real(RK), parameter :: tolerance = 1.e-13_RK
        integer :: i, lb, ub
        logical :: this_right
        logical, dimension(:), allocatable :: mask
        ! optional arguments
        if (present(right)) then
            this_right = right
        else
            this_right = .false.
        end if
        ! check for NaNs
        if (any(is_nan(x))) then
            error stop 'Some values were NaN.'
        end if
        ! check the bounds
        lb = lbound(bins, 1)
        ub = ubound(bins, 1)
        bounds_right: if (this_right) then
            if (any(x < bins(lb)-tolerance) .or. any(x >= bins(ub)+tolerance)) then
                error stop 'Some values of x are outside the given bins.'
            end if
        else
            if (any(x <= bins(lb)-tolerance) .or. any(x > bins(ub)+tolerance)) then
                error stop 'Some values of x are outside the given bins.'
            end if
        end if bounds_right
        ! Do real calculation
        allocate(output(size(x)))
        do i=1, ub-lb
            if (this_right) then
                mask = (x > bins(lb+i-1)) .and. (x <= bins(lb+i))
            else
                mask = (x >= bins(lb+i-1)) .and. (x < bins(lb+i))
            end if
            where(mask) output = i
        end do
    end function discretize_real

    pure elemental function maximum(x, y, ignore_nans) result(z)
        real(RK), intent(in)           :: x
        real(RK), intent(in)           :: y
        logical,  intent(in), optional :: ignore_nans
        real(RK)                       :: z
        ! local variables
        logical :: ignore
        ! optional inputs
        if (present(ignore_nans)) then
            ignore = ignore_nans
        else
            ignore = .false.
        end if
        if (ignore) then
            ! If ignoring NaNs, then return the one that is not NaN, if both NaN, then return the first one
            if (is_nan(x)) then
                if (is_nan(y)) then
                    z = x
                    return
                else
                    z = y
                end if
            else
                if (is_nan(y)) then
                    z = x
                end if
            end if
        end if
        ! Neither are NaNs, or not ignoring them
        if (x < y) then
            z = y
        else
            z = x
        end if
    end function maximum

    pure elemental function minimum(x, y, ignore_nans) result(z)
        real(RK), intent(in)           :: x
        real(RK), intent(in)           :: y
        logical,  intent(in), optional :: ignore_nans
        real(RK)                       :: z
        ! local variables
        logical :: ignore
        ! optional inputs
        if (present(ignore_nans)) then
            ignore = ignore_nans
        else
            ignore = .false.
        end if
        if (ignore) then
            ! If ignoring NaNs, then return the one that is not NaN, if both NaN, then return the first one
            if (is_nan(x)) then
                if (is_nan(y)) then
                    z = x
                    return
                else
                    z = y
                end if
            else
                if (is_nan(y)) then
                    z = x
                end if
            end if
        end if
        ! Neither are NaNs, or not ignoring them
        if (x > y) then
            z = y
        else
            z = x
        end if
    end function minimum

    pure elemental function logical_to_num(x) result(y)
        logical, intent(in) :: x
        real(RK)            :: y
        if (x) then
            y = ONE
        else
            y = ZERO
        end if
    end function logical_to_num

    pure function mean(x) result(y)
        real(RK), intent(in), dimension(:) :: x
        real(RK)                           :: y
        ! local variables
        integer :: num
        ! calculations
        num = size(x)
        if (num > 0) then
            y = sum(x) / real(num, RK)
        else
            y = NAN
        endif
    end function mean

end module matlab
