!----------------------------------------------------------------------
!                            UTILS module
!        Copyright 2019 David C. Stauffer, All Rights Reserved
!----------------------------------------------------------------------

!> @brief   Utils module file for the forsat code.  It contains generic utilities that can be independently
!>          defined and used throughout the rest of the code.
!>
!> @author  David C. Stauffer
!> @version 1.0
!> Written by David C. Stauffer in September 2019.

module utils

    use asserts,     only: assert
    use constants,   only: MONTHS_PER_YEAR, NAN, ONE, RK, STDERR, STDOUT, ZERO
    use matlab,      only: cumsum, discretize, is_nan
    use operators,   only: rdivide
    use prng_nums,   only: prng_t, get_rand
    use text_output, only: int2str

    implicit none

    private

    public :: calculate_bin, cat_counts, find_those_alive, get_time_string, histcounts, setup_dir, unit_vec, load_seeds

    interface unit_vec
        module procedure :: unit_vec_1d
        module procedure :: unit_vec_2d
    end interface unit_vec

contains

    !>
    !! get_time_string formats a delta time in seconds to a string for printing.
    !! @param[in]           seconds    delta time [seconds]
    !! @param[in, optional] formatter  optional format specifier to use in conversion
    !! @return              str        output string
    !<
    pure function get_time_string(seconds) result(str)
        integer,          intent(in)  :: seconds
        character(len=:), allocatable :: str
        ! local variables
        integer, parameter :: ONE_DAY    = 86400
        integer, parameter :: ONE_HOUR   = 3600
        integer, parameter :: ONE_MINUTE = 60
        integer, parameter :: NUM_DIGITS = 2
        ! create string
        str = int2str(seconds/ONE_DAY, NUM_DIGITS) // ' ' // int2str(mod(seconds, ONE_DAY)/ONE_HOUR, NUM_DIGITS) // ':' // &
            int2str(mod(seconds, ONE_HOUR)/ONE_MINUTE, NUM_DIGITS) // ':' // int2str(mod(seconds, ONE_MINUTE), NUM_DIGITS)
    end function get_time_string

    !! find_those_alive
    ! 1.  Written by David C. Stauffer in Mar 2015.
    pure elemental function find_those_alive(age) result(alive)
        ! inputs and outputs
        real(RK), intent(in) :: age
        logical              :: alive
        !> Finds those alive, meaning age is not NaN and age > 0.
        if (is_nan(age)) then
            alive = .false.
        else
            if (age > ZERO) then
                alive = .true.
            else
                alive = .false.
            end if
        end if
    end function find_those_alive

    !%% Functions - calculate_bin
    function calculate_bin(num, dist, prng, check) result(bin)
        integer, intent(in)                 :: num ! Number to distribute
        real(RK), intent(in), dimension(:)  :: dist ! Distributions
        class(prng_t), intent(inout)        :: prng ! pseudo-random number generator instance
        logical, intent(in), optional       :: check ! Whether to check that distribution sums to 1, default is true
        integer, dimension(:), allocatable  :: bin ! Bin number
        ! local variables
        logical :: local_check
        character(len=:), allocatable       :: msg
        real(RK), dimension(:), allocatable :: cum_dist, rand_nums
        ! optional inputs
        if (present(check)) then
            local_check = check
        else
            local_check = .true.
        end if
        ! calculate the cumulative distribution
        cum_dist = [ZERO, cumsum(dist)]

        if (local_check) then
            ! optionally check that distribution sums to 1
            msg = 'Distribution does not sum to 1.'
            call assert((abs(cum_dist(ubound(cum_dist, 1)) - ONE) < 1.e-12_RK), msg)
        end if

        ! determine which bin someone is in based on the distribution
        rand_nums = get_rand(num, prng)
        bin = discretize(rand_nums, cum_dist)
    end function calculate_bin

    !%% Functions - cat_counts
    pure function cat_counts(x, cats) result(num)
        integer, intent(in), dimension(:)  :: x ! Things to count
        integer, intent(in), dimension(:)  :: cats ! Categories to count things of
        integer, dimension(:), allocatable :: num ! Number of things in each category
        ! local variables
        integer :: i
        ! count the number of things in each category
        num = [(count(x == cats(i)), i=lbound(cats,1), ubound(cats,1))]
    end function cat_counts

    !! Functions - histcounts
    !> Count the number of points in each of the given bins.
    pure function histcounts(x, edges, right, check_bounds) result(hist)
        real(RK), intent(in), dimension(:) :: x ! Input array to be binned.
        real(RK), intent(in), dimension(:) :: edges ! Array of bins. It has to be 1d and monotonic.
        logical,  intent(in), optional     :: right ! Indicating whether the intervals include the right or the left bin
                                                    ! edge. Default behavior is (right==False) indicating that the interval
                                                    ! does not include the right edge. The left bin end is closed in this
                                                    ! case, i.e., bins[i-1] <= x < bins[i] is the default behavior for
                                                    ! monotonically increasing bins.
        logical,  intent(in), optional     :: check_bounds
        integer, dimension(:), allocatable :: hist
        ! local variables
        integer  :: c, i, num_bins
        logical  :: local_right, local_check
        real(RK) :: bound1, bound2
        ! optional inputs
        if (present(right)) then
            local_right = right
        else
            local_right = .false.
        end if
        if (present(check_bounds)) then
            local_check = check_bounds
        else
            local_check = .true.
        end if

        ! get the number of bins
        c = lbound(edges, 1) - 1
        num_bins = size(edges) - 1

        ! preallocate the output
        hist = [(0, i=1, num_bins)]

        ! loop through the bins and count the entries
        do i=1, num_bins
            bound1 = edges(c+i)
            bound2 = edges(c+i+1)
            if (local_right) then
                if (i >= 1) then
                    hist(i) = count(x >  bound1 .and. x <= bound2)
                else
                    hist(i) = count(x >= bound1 .and. x <= bound2)
                end if
            else
                if (i < num_bins) then
                    hist(i) = count(x >= bound1 .and. x <  bound2)
                else
                    hist(i) = count(x >= bound1 .and. x <= bound2)
                end if
            end if
        end do

        ! check the bounds
        ! Note: just checks that everything was counted.  If bins are not monotonic, then no guarantees are made
        if (local_check) then
            if (sum(hist) /= size(x)) then
                error stop 'Some values were not put into any bins.'
            end if
        end if
    end function histcounts

    subroutine setup_dir(folder, recurse)
        ! inputs and outputs
        character(len=*), intent(in)           :: folder
        logical,          intent(in), optional :: recurse
        ! local variables
        integer                       :: exit_status
        logical                       :: local_recurse, dir_exists
        character(len=:), allocatable :: make_command
        ! hard-coded values
#ifdef _WIN32
        make_command = 'md'
#else
        make_command = 'mkdir -p'
#endif
        ! optional arguments
        if (present(recurse)) then
            local_recurse = recurse
        else
            local_recurse = .false.
        end if
        ! check for an empty string and exit
        if (len(folder) == 0) then
            return
        end if
        ! check if the directory already exists
        inquire(file=folder, exist=dir_exists)
        if (dir_exists) then
            ! TODO: write the rest of this
!            # Loop through the contained files/folders
!            for this_elem in os.listdir(folder):
!                # alias the fullpath of this file element
!                this_full_elem = os.path.join(folder, this_elem)
!                # check if a folder or file
!                if os.path.isdir(this_full_elem):
!                    # if a folder, then delete recursively if rec is True
!                    if rec:
!                        setup_dir(this_full_elem)
!                        os.rmdir(this_full_elem)
!                elif os.path.isfile(this_full_elem):
!                    # if a file, then remove it
!                    os.remove(this_full_elem)
!                else:
!                    raise RuntimeError('Unexpected file type, neither file nor folder: "{}".'\
!                        .format(this_full_elem)) # pragma: no cover
!            logger.info('Files/Sub-folders were removed from: "' + folder + '"')
        else
            ! create the directory if it does not exist
            call execute_command_line(make_command // ' ' // folder, exitstat=exit_status)
            if (exit_status == 0) then
                write(STDOUT, '(A)') 'Created directory: "' // folder // '"'
            else
                write(STDERR, '(A,I0,A)') __FILE__ , __LINE__ , 'Failed to make directory at "' // folder // '"'
            end if
        end if
    end subroutine setup_dir

    function unit_vec_1d(vec) result(output)
        ! inputs and ouputs
        real(RK), intent(in),   dimension(:) :: vec
        real(RK), dimension(:), allocatable  :: output
        ! local variables
        real(RK) :: mag, mag_sqrd
        ! calculations
        mag_sqrd = sum(vec**2, dim=1)
        if (mag_sqrd > 0) then
            mag = sqrt(mag_sqrd)
        else
            mag = ONE
        end if
        output = vec / mag
    end function unit_vec_1d

    function unit_vec_2d(vec, dim) result(output)
        ! inputs and ouputs
        real(RK), intent(in), dimension(:,:)  :: vec
        integer,  intent(in), optional        :: dim
        real(RK), dimension(:,:), allocatable :: output
        ! local variables
        integer :: axis, num
        real(RK), dimension(:),   allocatable :: mag, mag_sqrd
        real(RK), dimension(:,:), allocatable :: full_mag
        if (present(dim)) then
            axis = dim
        else
            axis = 1
        end if
        select case (axis)
            case (1)
                mag_sqrd = sum(vec**2, dim=1)
                print *, mag_sqrd
                where (mag_sqrd > 0)
                    mag = sqrt(mag_sqrd)
                elsewhere
                    mag = ONE
                end where
                num      = size(vec, 2)
                full_mag = spread(mag, dim=2, ncopies=num)
            case (2)
                mag_sqrd = sum(vec**2, dim=2)
                where (mag_sqrd > 0)
                    mag = sqrt(mag_sqrd)
                elsewhere
                    mag = ONE
                end where
                num      = size(vec, 1)
                full_mag = spread(mag, dim=1, ncopies=num)
            case default
                error stop 'Bad dim.'
        end select
        output = vec / full_mag
    end function unit_vec_2d

    function load_seeds(filename) result(seeds)
        ! inputs and outputs
        character(len=*), intent(in)         :: filename
        integer, dimension(:,:), allocatable :: seeds
        ! local variables
        integer :: i, fid, io_stat
        logical :: file_exists
        character(len=:), allocatable :: err_msg
        integer, dimension(33*1000) :: buffer
        ! hard-coded values
        integer, parameter :: num_rows = 33
        integer, parameter :: num_cols = 1000 ! TODO: size this appropriately
        ! check if file exists
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) then
            write(STDERR, '(A)') ' Warning, seeds file not found, using defaults instead.'
            seeds = spread([(i, i=1, num_rows)], dim=2, ncopies=num_cols)
            return
        end if
        ! open file
        open(newunit=fid, file=filename, status='old', action='read', access='stream', form='unformatted', &
            iostat=io_stat, iomsg=err_msg) ! TODO: use convert= option?
        if (io_stat /= 0) then
            error stop 'Error opening: "' // filename // '": ' // err_msg
        end if
        ! read data
        read(unit=fid, iostat=io_stat, iomsg=err_msg) buffer
        if (io_stat /= 0) then
            error stop 'Error reading data. ' // err_msg
        end if
        ! close file
        close(unit=fid, iostat=io_stat, iomsg=err_msg)
        if (io_stat /= 0) then
            error stop 'Error closing file: "' // filename // '": ' // err_msg
        end if
        ! reshape data
        seeds = reshape(buffer, [num_rows, num_cols])
    end function load_seeds

end module utils