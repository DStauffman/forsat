module stats

    use constants, only: ONE, RK, ZERO

    implicit none

    private

    public :: prob_to_rate, rate_to_prob

contains
    pure elemental function prob_to_rate(prob, time) result(rate)
        real(RK), intent(in)           :: prob
        real(RK), intent(in), optional :: time
        real(RK)                       :: rate
        ! local variables
        real(RK) :: this_time
        ! bounds checking
        if (prob < ZERO) then
            error stop 'Probability must be >= 0'
        end if
        if (prob > ONE) then
            error stop 'Probability must be <= 1'
        end if
        if (present(time)) then
            this_time = time
        else
            this_time = ONE
        end if
        ! calculate rate
        rate = -log(ONE - prob) / this_time
        ! prevent code from returning a bunch of negative zeros when prob is exactly zero
        rate = rate + ZERO
    end function prob_to_rate

    pure elemental function rate_to_prob(rate, time) result(prob)
        real(RK), intent(in)           :: rate
        real(RK), intent(in), optional :: time
        real(RK)                       :: prob
        ! local variables
        real(RK) :: this_time
        ! bounds checking
        if (rate < ZERO) then
            error stop 'Rate must be >= 0'
        end if
        if (present(time)) then
            this_time = time
        else
            this_time = ONE
        end if
        ! calculate rate
        prob = ONE - exp(-rate * this_time)
    end function rate_to_prob

end module stats
