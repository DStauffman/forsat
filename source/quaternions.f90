module quaternions

    use constants, only: RK, ZERO, ONE

    implicit none

    private

    type, public :: quat_t
        real(RK) :: x
        real(RK) :: y
        real(RK) :: z
        real(RK) :: s
    contains
        procedure :: mag      => quat_mag
        procedure :: norm     => quat_norm
    end type quat_t
    interface operator (+)
        module procedure quat_add
    end interface
    interface operator (-)
        module procedure quat_sub
    end interface
    interface operator (*)
        module procedure quat_mult
        module procedure quat_mult_real
        module procedure real_quat_mult
        !module procedure quat_mult_vec
        !module procedure vec_mult_quat
    end interface
    interface operator (/)
        module procedure quat_div
    end interface
    interface quat_t
        module procedure :: quat_init_xyzs
        module procedure :: quat_init_vec
    end interface quat_t

contains
    !! Initializtaions
    pure function quat_init_xyzs(x, y, z, s) result(q_out)
        ! inputs and outputs
        real(RK), intent(in), optional :: x
        real(RK), intent(in), optional :: y
        real(RK), intent(in), optional :: z
        real(RK), intent(in), optional :: s
        type(quat_t)                   :: q_out
        ! local variables
        logical, dimension(4) :: components
        ! initialize variables
        components = [present(x), present(y), present(z), present(s)]
        if (all(components)) then
            q_out%x = x
            q_out%y = y
            q_out%z = z
            q_out%s = s
        else if (.not. any(components)) then
            q_out%x = ZERO
            q_out%y = ZERO
            q_out%z = ZERO
            q_out%s = ONE
        else
            error stop 'You must initialize all four components if you initialize any of them.'
        end if
    end function quat_init_xyzs
    pure function quat_init_vec(vec) result(q_out)
        ! inputs and outputs
        real(RK), dimension(4), intent(in) :: vec
        type(quat_t)                       :: q_out
        ! local variables
        integer :: start_ix
        ! get first index
        start_ix = lbound(vec, 1)
        ! initialize vector
        q_out%x = vec(start_ix)
        q_out%y = vec(start_ix + 1)
        q_out%z = vec(start_ix + 2)
        q_out%s = vec(start_ix + 3)
    end function quat_init_vec
    
    !! Addition
    pure elemental function quat_add(q1, q2) result(q_out)
        ! inputs and outputs
        class(quat_t), intent(in) :: q1
        class(quat_t), intent(in) :: q2
        type(quat_t)              :: q_out
        ! initialize output
        q_out = quat_t()
        ! sum the components
        q_out%x = q1%x + q2%x
        q_out%y = q1%y + q2%y
        q_out%z = q1%z + q2%z
        q_out%s = q1%s + q2%s
    end function quat_add

    !! Subtraction
    pure elemental function quat_sub(q1, q2) result(q_out)
        ! inputs and outputs
        class(quat_t), intent(in) :: q1
        class(quat_t), intent(in) :: q2
        type(quat_t)              :: q_out
        ! initialize output
        q_out = quat_t()
        ! subtract the components
        q_out%x = q1%x - q2%x
        q_out%y = q1%y - q2%y
        q_out%z = q1%z - q2%z
        q_out%s = q1%s - q2%s
    end function quat_sub

    !! Multiplication
    pure elemental function quat_mult(q1, q2) result(q_out)
        ! inputs and outputs
        class(quat_t), intent(in) :: q1
        class(quat_t), intent(in) :: q2
        type(quat_t)              :: q_out
        ! initialize the output
        q_out = quat_t()
        ! multiple the quaternions
        q_out%x =  q1%s*q2%x + q1%z*q2%y - q1%y*q2%z + q1%x*q2%s
        q_out%y = -q1%z*q2%x + q1%s*q2%y + q1%x*q2%z + q1%y*q2%s
        q_out%z =  q1%y*q2%x - q1%x*q2%y + q1%s*q2%z + q1%z*q2%s
        q_out%s = -q1%x*q2%x - q1%y*q2%y - q1%z*q2%z + q1%s*q2%s
        ! ensure positive scalar component
        if (q_out%s < ZERO) then
            q_out%x = -q_out%x
            q_out%y = -q_out%y
            q_out%z = -q_out%z
            q_out%s = -q_out%s
        end if
    end function quat_mult
    pure elemental function quat_mult_real(q1, r2) result(q_out)
        ! inputs and outputs
        class(quat_t), intent(in) :: q1
        real(RK),      intent(in) :: r2
        type(quat_t)              :: q_out
        ! initialize the output
        q_out = quat_t()
        ! multiply the components
        q_out%x = q1%x * r2
        q_out%y = q1%y * r2
        q_out%z = q1%z * r2
        q_out%s = q1%s * r2
    end function quat_mult_real
    pure elemental function real_quat_mult(r1, q2) result(q_out)
        ! inputs and outputs
        real(RK),      intent(in) :: r1
        class(quat_t), intent(in) :: q2
        type(quat_t)              :: q_out
        ! initialize the output
        q_out = quat_t()
        ! multiply the components
        q_out%x = r1 * q2%x
        q_out%y = r1 * q2%y
        q_out%z = r1 * q2%z
        q_out%s = r1 * q2%s
    end function real_quat_mult

    !! Division
    pure elemental function quat_div(q1, q2) result(q_out)
        ! inputs and outputs
        class(quat_t), intent(in) :: q1
        class(quat_t), intent(in) :: q2
        type(quat_t)              :: q_out
        ! define quaternion division as the inverse of multiplication
        q_out = quat_mult(q2,q1)
    end function quat_div

    !! Magnitude
    pure elemental function quat_mag(q1) result(output)
        ! inputs and outputs
        class(quat_t), intent(in) :: q1
        real(RK)                  :: output
        output = q1%x**2 + q1%y**2 + q1%z**2 + q1%s**2
    end function quat_mag

    !! Norm
    pure elemental function quat_norm(q1) result(q_out)
        ! inputs and outputs
        class(quat_t), intent(in) :: q1
        type(quat_t)              :: q_out
        ! local variables
        real(RK) :: mag
        ! calculate the magnitude
        mag = quat_mag(q1)
        ! normalize by the magnitude
        q_out = quat_mult_real(q1,ONE/mag)
    end function quat_norm

end module quaternions
