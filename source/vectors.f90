module vector_3d

    use constants, only: RK, ZERO

    implicit none

    private

    type, public :: vector_3d_t
        real(RK) :: x
        real(RK) :: y
        real(RK) :: z
    contains
        generic   :: operator(+) => vector_add
        generic   :: operator(-) => vector_subtract
        generic   :: operator(*) => vector_multiply
        generic   :: operator(/) => vector_divide
        generic   :: operator(.dot.) => vector_dot
        generic   :: operator(.cross.) => vector_cross
        procedure :: mag => vector_mag
        procedure :: norm => vector_norm
    interface vector_3d_t
        module procedure :: vector_init_xyz
        module procedure :: vector_init_vec
    end interface

contains
    function vector_init_xyz(x, y, z) result(vec_out)
        ! inputs and outputs
        real(RK), optional :: x
        real(RK), optional :: y
        real(RK), optional :: z
        type(vector_3d_t)  :: vec_out
        ! local variables
        logical, dimension(3) :: components
        ! initialize variables
        components = [present(x), present(y), present(z)]
        if (all(components)) then
            vec_out%x = x
            vec_out%y = y
            vec_out%z = z
        else if (.not. any(components)) then
            vec_out%x = ZERO
            vec_out%y = ZERO
            vec_out%z = ZERO
        else
            error stop 'You must initialize all three components if you initialize any of them'
        end if
    end function vector_init_xyz

    function vector_init_vec(vec) result(vec_out)
        ! inputs and outputs
        real(RK), dimension(3) :: vec
        type(vector_3d_t)      :: vec_out
        ! local variables
        integer :: start_ix
        ! get first index
        start_ix = lbound(vec, 1)
        ! initialize vector
        vec_out%x = vec(start_ix)
        vec_out%y = vec(start_ix + 1)
        vec_out%z = vec(start_ix + 2)
    end function vector_init_vec

    pure elemental function vector_add(vec1, vec2) result(vec_out)
        ! inputs and outputs
        type(vector_3d_t), intent(in) :: vec1
        type(vector_3d_t), intent(in) :: vec2
        type(vector_3d_t)             :: vec_out
        ! initialize output
        vec_out = vector_3d_t()
        ! sum the components
        vec_out%x = vec1%x + vec2%x
        vec_out%y = vec1%y + vec2%y
        vec_out%z = vec1%z + vec2%z
    end function vector_add

    pure elemental function vector_subtract(vec1, vec2) result(vec_out)
        ! inputs and outputs
        type(vector_3d_t), intent(in) :: vec1
        type(vector_3d_t), intent(in) :: vec2
        type(vector_3d_t)             :: vec_out
        ! initialize output
        vec_out = vector_3d_t()
        ! sum the components
        vec_out%x = vec1%x - vec2%x
        vec_out%y = vec1%y - vec2%y
        vec_out%z = vec1%z - vec2%z
    end function vector_subtract
    
    pure elemental function vector_multiply(vec, scalar) result(vec_out)
        ! inputs and outputs
        type(vector_3d_t), intent(in) :: vec
        real(RK),          intent(in) :: scalar
        type(vector_3d_t)             :: vec_out
        ! initialize output
        vec_out = vector_3d_t()
        ! sum the components
        vec_out%x = scalar * vec%x
        vec_out%y = scalar * vec%y
        vec_out%z = scalar * vec%z
    end function vector_subtract
    
    pure elemental function vector_divide(vec, scalar) result(vec_out)
        ! inputs and outputs
        type(vector_3d_t), intent(in) :: vec
        real(RK),          intent(in) :: scalar
        type(vector_3d_t)             :: vec_out
        ! initialize output
        vec_out = vector_3d_t()
        ! sum the components
        vec_out%x = scalar / vec%x
        vec_out%y = scalar / vec%y
        vec_out%z = scalar / vec%z
    end function vector_subtract

    pure elemental function vector_dot(vec1, vec2) result(output)
        ! inputs and outputs
        type(vector_3d_t), intent(in) :: vec1
        type(vector_3d_t), intent(in) :: vec2
        real(RK)                      :: output
        ! sum the components
        output = (vec1%x * vec2%x) + (vec1%y * vec2%y) + (vec1%z * vec2%z)
    end function vector_dot

    pure elemental function vector_cross(vec1, vec2) result(vec_out)
        ! inputs and outputs
        type(vector_3d_t), intent(in) :: vec1
        type(vector_3d_t), intent(in) :: vec2
        type(vector_3d_t)             :: vec_out
        ! initialize output
        vec_out = vector_3d_t()
        ! cross the components
        vec_out%x = (vec1%y * vec2%z) - (vec1%z * vec2%y)
        vec_out%y = (vec1%z * vec2%x) - (vec1%x * vec2%z)
        vec_out%z = (vec1%x * vec2%y) - (vec1%y * vec2%x)
    end function vector_cross

    pure elemental function vector_mag(vec) result(output)
        ! inputs and outputs
        type(vector_3d_t), intent(in) :: vec
        real(RK)                      :: output
        ! calculations
        output = sqrt(vec%x**2 + vec%y**2 + vec%z**2)
    end function vector_mag

    pure elemental function vector_norm(vec) result(output)
        ! inputs and outputs
        type(vector_3d_t), intent(in) :: vec
        type(vector_3d_t)             :: vec_out
        ! calculations
        vec_out = vec / vector_mag(vec)
    end function vector_mag

end module vector_3d
