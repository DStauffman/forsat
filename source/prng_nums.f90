module prng_nums
    ! This module provides a class wrapper to the built-in pseudo random number generator to
    ! maintain its state among instances of the class, so they can be used in different threads
    ! without conflicting

    use constants,   only: LARGE, ONE, NAN, NEG_LARGE, RK, SMALL, TWO, TWO_PI, ZERO
    use matlab,      only: find
    use text_output, only: int2str

    implicit none

    private

    public :: get_rand, get_randn

    type, public :: prng_t
        integer :: n
        integer, dimension(:), allocatable :: seeds
    contains
        procedure :: rand_vec     => prng_rand_vector
        procedure :: rand_sca     => prng_rand_scalar
        generic   :: rand         => rand_vec, rand_sca
        generic   :: uniform      => rand_vec, rand_sca
        procedure :: set_seed     => prng_set_seed
        procedure :: randn        => prng_randn, prng_randn_single
        procedure :: normal_bound => bounded_normal_draw
        procedure :: normal       => prng_normal
    end type prng_t
    interface prng_t
        module procedure prng_init
    end interface

contains
    function prng_init(seeds) result(this)
        integer, intent(in), dimension(:), allocatable, optional :: seeds
        type(prng_t)                                   :: this
        ! Determine the size needed for the seeds (likely 12, actually 33??)
        call random_seed(size=this%n)
        ! Allocate the seeds array to the correct size
        allocate(this%seeds(this%n))
        if (present(seeds)) then
            this%seeds(:) = seeds
        end if
    end function prng_init

    subroutine prng_set_seed(this, seeds)
        class(prng_t), intent(inout)                   :: this
        integer, intent(in), dimension(:), allocatable :: seeds
        ! TODO: handle case where dimensions don't match
        this%seeds(:) = seeds
    end subroutine prng_set_seed

    subroutine prng_rand_vector(this, output)
        class(prng_t), intent(inout)          :: this
        real(RK), intent(inout), dimension(:) :: output
        ! simple test to exit early
        if (size(output) == 0) then
            return
        end if
        ! Don't let parallel tasks run this part at the same time!
        !$omp critical
        call random_seed(put=this%seeds)
        call random_number(output)
        call random_seed(get=this%seeds)
        !$omp end critical
    end subroutine prng_rand_vector

    subroutine prng_rand_scalar(this, output)
        class(prng_t), intent(inout) :: this
        real(RK),      intent(inout) :: output
        ! Don't let parallel tasks run this part at the same time!
        !$omp critical
        call random_seed(put=this%seeds)
        call random_number(output)
        call random_seed(get=this%seeds)
        !$omp end critical
    end subroutine prng_rand_scalar

    subroutine prng_randn_single(this, output)
        ! inputs and outputs
        class(prng_t), intent(inout) :: this
        real(RK),      intent(inout) :: output
        ! local variables
        real(RK) :: u1, u2
        ! persistent variables
        real(RK), save :: z1, z2
        logical, save  :: generate = .false.

        generate = .not. generate

        if (.not. generate) then
            output = z2
            return
        end if

        u1 = ZERO
        do while (u1 <= SMALL)
            call prng_rand_scalar(this, u1)
            call prng_rand_scalar(this, u2)
        end do

        z1 = sqrt(-TWO * log(u1)) * cos(TWO_PI * u2)
        z2 = sqrt(-TWO * log(u1)) * sin(TWO_PI * u2)

        output = z1
        return
    end subroutine prng_randn_single

    subroutine prng_randn(this, output)
        ! inputs and outputs
        class(prng_t), intent(inout)          :: this
        real(RK), intent(inout), dimension(:) :: output
        ! local variables
        integer                             :: half_generate, num_generate, num_needed, num_have
        integer,  dimension(:), allocatable :: ix
        real(RK), dimension(:), allocatable :: u1, u2, z1, z2, temp_rand
        ! persistents
        logical, save  :: saved_extra = .false.
        real(RK), save :: extra_value = NAN
        ! simple test to exit early
        if (size(output) == 0) then
            return
        end if
        ! get number of even values to generate
        if (saved_extra) then
            num_generate = size(output) - 1
        else
            num_generate = size(output)
        end if
        if (mod(num_generate, 2) == 1) then
            num_generate = num_generate + 1 ! TODO: need to generate half of these.
        end if
        half_generate = num_generate / 2
        ! generate uniform values, enforcing u1 to not be too small
        allocate(u1(half_generate))
        allocate(u2(half_generate))
        call prng_rand_vector(this, u1)
        call prng_rand_vector(this, u2)
        ix = find(u1 <= SMALL)
        do while (size(ix) > 0)
            allocate(temp_rand(size(ix)))
            call prng_rand_vector(this, temp_rand)
            u1(ix) = temp_rand
            deallocate(temp_rand)
            ix = find(u1 <= SMALL)
        end do
        ! generate the output normals
        z1 = sqrt(-TWO * log(u1)) * cos(TWO_PI * u2)
        z2 = sqrt(-TWO * log(u1)) * sin(TWO_PI * u2)
        ! determine if keeping an extra value for next time
        num_needed = size(output)
        if (saved_extra) then
            num_have = num_generate + 1
        else
            num_have = num_generate
        end if
        if (num_needed == num_have) then
            if (saved_extra) then
                output(1:num_needed-2:2) = z1
                output(2:num_needed-1:2) = z2
                output(num_needed)       = extra_value
            else
                output(1:num_needed-1:2) = z1
                output(2:num_needed:2)   = z2
            end if
            extra_value                  = NAN
            saved_extra                  = .false.
        else if (num_needed == num_have - 1) then
            if (saved_extra) then
                output(1)                = extra_value
                output(2:num_needed:2)   = z1
                output(3:num_needed-1:2) = z2(1:half_generate-1)
            else
                output(1:num_needed:2)   = z1
                output(2:num_needed-1:2) = z2(1:half_generate-1)
            end if
            extra_value                  = z2(half_generate)
            saved_extra                  = .true.
        else
            error stop 'Wrong number of values generated, needed: "' // int2str(num_needed) // '", have "' // &
                int2str(num_have) // '".'
        end if
    end subroutine prng_randn

    subroutine prng_normal(this, output, mu, sigma)
        ! inputs and outputs
        class(prng_t), intent(inout)          :: this
        real(RK), intent(inout), dimension(:) :: output
        real(RK), intent(in)                  :: mu
        real(RK), intent(in)                  :: sigma
        ! local variables
        real(RK), dimension(:), allocatable :: temp_rand
        ! simple test to exit early
        if (size(output) == 0) then
            return
        end if
        ! call randn and scale by appropriate mean and standard deviation
        allocate(temp_rand(size(output)))
        call prng_randn(this, temp_rand)
        output(:) = temp_rand * sigma + mu
    end subroutine prng_normal

    !! Functions - bounded_normal_draw
    !> Create a normalized distribution with the given mean and standard deviations.
    subroutine bounded_normal_draw(this, output, mu, sigma, min_value, max_value)
        ! inputs and outputs
        class(prng_t), intent(inout)               :: this
        real(RK),      intent(inout), dimension(:) :: output
        real(RK),      intent(in),    optional     :: mu        ! mean
        real(RK),      intent(in),    optional     :: sigma     ! standard deviation
        real(RK),      intent(in),    optional     :: min_value ! minimum value to cap distribution
        real(RK),      intent(in),    optional     :: max_value ! maximum value to cap distribution
        ! local variables
        real(RK) :: this_mean, this_std, this_min, this_max
        ! simple test to exit early
        if (size(output) == 0) then
            return
        end if
        ! optional arguments
        if (present(mu)) then
            this_mean = mu
        else
            this_mean = ZERO
        end if
        if (present(sigma)) then
            this_std  = sigma
        else
            this_std  = ONE
        end if
        if (present(min_value)) then
            this_min  = min_value
        else
            this_min  = NEG_LARGE
        end if
        if (present(max_value)) then
            this_max  = max_value
        else
            this_max  = LARGE
        end if
        ! calculate the normal distribution
        if (this_std == ZERO) then
            output(:) = this_mean
        else
            call prng_normal(this, output, this_mean, this_std)
        end if
        ! enforce the min and maxes
        where (output > this_max) output = this_max
        where (output < this_min) output = this_min
    end subroutine bounded_normal_draw

    function get_rand(num, prng) result(temp_rand)
        ! inputs and outputs
        integer,      intent(in)            :: num
        type(prng_t), intent(inout)         :: prng
        real(RK), dimension(:), allocatable :: temp_rand
        ! allocate output
        allocate(temp_rand(num))
        ! get random numbers
        call prng%rand(temp_rand)
    end function get_rand

    function get_randn(num, prng) result(temp_rand)
        ! inputs and outputs
        integer,      intent(in)            :: num
        type(prng_t), intent(inout)         :: prng
        real(RK), dimension(:), allocatable :: temp_rand
        ! allocate output
        allocate(temp_rand(num))
        ! get normal random numbers
        call prng%randn(temp_rand)
    end function get_randn

end module prng_nums
