module vector_3d

    use constants, only: RK, ZERO

    implicit none

    private

    type, public :: vec3_t
        real(RK) :: x
        real(RK) :: y
        real(RK) :: z
    contains
        generic   :: operator(+) => vector_add, vector_add_real, real_add_vector
        generic   :: operator(-) => vector_subtract, vector_subtract_real, real_subtract_vector
        generic   :: operator(*) => vector_multiply, vector_mult_real, real_mult_vector
        generic   :: operator(/) => vector_divide, vector_div_real, real_div_vector
        generic   :: operator(.dot.)   => dot_product_fn
        generic   :: operator(.cross.) => cross_product
        procedure :: mag  => vector_mag
        procedure :: norm => vector_norm
    end type vec3_t
    interface vec3_t
        module procedure :: vector_init_xyz
        module procedure :: vector_init_vec
    end interface vec3_t

contains
    !! Initializations
    function vector_init_xyz(x, y, z) result(vec_out)
        ! inputs and outputs
        real(RK), optional :: x
        real(RK), optional :: y
        real(RK), optional :: z
        type(vec3_t)       :: vec_out
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
            error stop 'You must initialize all three components if you initialize any of them.'
        end if
    end function vector_init_xyz
    function vector_init_vec(vec) result(vec_out)
        ! inputs and outputs
        real(RK), dimension(3) :: vec
        type(vec3_t)           :: vec_out
        ! local variables
        integer :: start_ix
        ! get first index
        start_ix = lbound(vec, 1)
        ! initialize vector
        vec_out%x = vec(start_ix)
        vec_out%y = vec(start_ix + 1)
        vec_out%z = vec(start_ix + 2)
    end function vector_init_vec
    
    !! Addition
    pure elemental function vector_add(v1, v2) result(vec_out)
        ! inputs and outputs
        type(vec3_t), intent(in) :: v1
        type(vec3_t), intent(in) :: v2
        type(vec3_t)             :: vec_out
        ! initialize output
        vec_out = vec3_t()
        ! sum the components
        vec_out%x = v1%x + v2%x
        vec_out%y = v1%y + v2%y
        vec_out%z = v1%z + v2%z
    end function vector_add
    pure elemental function vector_add_real(v1, r2) result(vec_out)
        ! inputs and outputs
        type(vec3_t), intent(in) :: v1
        real(RK),     intent(in) :: r2
        type(vec3_t)             :: vec_out
        ! initialize output
        vec_out = vec3_t()
        ! sum the components
        vec_out%x = v1%x + r2
        vec_out%y = v1%y + r2
        vec_out%z = v1%z + r2
    end function vector_add_real
    pure elemental function real_add_vector(r1, v2) result(vec_out)
        ! inputs and outputs
        real(RK),     intent(in) :: r1
        type(vec3_t), intent(in) :: v2
        type(vec3_t)             :: vec_out
        ! initialize output
        vec_out = vec3_t()
        ! sum the components
        vec_out%x = r1 + v2%x
        vec_out%y = r1 + v2%y
        vec_out%z = r1 + v2%z
    end function vector_add_real

    !! Subtraction
    pure elemental function vector_subtract(v1, v2) result(vec_out)
        type(vec3_t), intent(in) :: v1
        type(vec3_t), intent(in) :: v2
        type(vec3_t)             :: vec_out
        ! initialize output
        vec_out = vec3_t()
        ! subtract the components
        vec_out%x = v1%x - v2%x
        vec_out%y = v1%y - v2%y
        vec_out%z = v1%z - v2%z
    end function vector_subtract
    pure elemental function vector_subtract_real(v1, r2) result(vec_out)
        ! inputs and outputs
        type(vec3_t), intent(in) :: v1
        real(RK),     intent(in) :: r2
        type(vec3_t)             :: vec_out
        ! initialize output
        vec_out = vec3_t()
        ! subtract the components
        vec_out%x = v1%x - r2
        vec_out%y = v1%y - r2
        vec_out%z = v1%z - r2
    end function vector_subtract_real
    pure elemental function real_subtract_vector(r1, v2) result(vec_out)
        ! inputs and outputs
        real(RK),     intent(in) :: r1
        type(vec3_t), intent(in) :: v2
        type(vec3_t)             :: vec_out
        ! initialize output
        vec_out = vec3_t()
        ! subtract the components
        vec_out%x = r1 - v2%x
        vec_out%y = r1 - v2%y
        vec_out%z = r1 - v2%z
    end function vector_subtract_real

    !! Multiplication
    pure elemental function vector_times_real(v1, r2) result(vec_out)
        ! inputs and outputs
        type(vec3_t), intent(in) :: v1
        real(RK),     intent(in) :: r2
        type(vec3_t)             :: vec_out
        ! initialize output
        vec_out = vec3_t()
        ! multiply the components
        vec_out%x = v1%x * r2
        vec_out%y = v1%y * r2
        vec_out%z = v1%z * r2
    end function vector_times_real
    pure elemental function real_times_vector(r1, v2) result(vec_out)
        ! inputs and outputs
        real(RK),     intent(in) :: r1
        type(vect_t), intent(in) :: v2
        type(vect_t) :: vec_out
        ! initialize output
        vec_out = vec3_t()
        ! multiply the components
        real_times_vect%x = r1 * v2%x
        real_times_vect%y = r1 * v2%y
        real_times_vect%z = r1 * v2%z
    end function real_times_vector

    !! Division
    function vector_divide_real(v1, r2) result(vec_out)
        ! inputs and outputs
        type(vect_t), intent(in) :: v1
        real(RK),     intent(in) :: r2
        type(vect_t)             :: vec_out
        ! initialize output
        vec_out = vec3_t()
        ! divide the components
        vec_out%x = v1%x / r2
        vec_out%y = v1%y / r2
        vec_out%z = v1%z / r2
    end function vect_divide_real
    function real_divide_vector(r1, v2) result(vec_out)
        real(RK),     intent(in) :: r1
        type(vect_t), intent(in) :: v2
        type(vect_t)             :: vec_out
        ! initialize output
        vec_out = vec3_t()
        ! divide the components
        vec_out%x = r1 / v2%x
        vec_out%y = r1 / v2%y
        vec_out%z = r1 / v2%z
    end function real_divide_vector
    
    !! Dot product
    pure elemental function vector_dot(v1, v2) result(output)
        ! inputs and outputs
        type(vec3_t), intent(in) :: v1
        type(vec3_t), intent(in) :: v2
        real(RK)                 :: output
        ! sum the components
        output = (v1%x * v2%x) + (v1%y * v2%y) + (v1%z * v2%z)
    end function vector_dot

    !! Cross product
    pure elemental function vector_cross(v1, v2) result(vec_out)
        ! inputs and outputs
        type(vec3_t), intent(in) :: v1
        type(vec3_t), intent(in) :: v2
        type(vec3_t)             :: vec_out
        ! initialize output
        vec_out = vec3_t()
        ! cross the components
        vec_out%x = (v1%y * v2%z) - (v1%z * v2%y)
        vec_out%y = (v1%z * v2%x) - (v1%x * v2%z)
        vec_out%z = (v1%x * v2%y) - (v1%y * v2%x)
    end function vector_cross

    !! Magnitude
    pure elemental function vector_mag(vec) result(output)
        ! inputs and outputs
        type(vec3_t), intent(in) :: vec
        real(RK)                 :: output
        ! calculations
        output = sqrt(vec%x**2 + vec%y**2 + vec%z**2)
    end function vector_mag

    !! Norm
    pure elemental function vector_norm(vec) result(vec_out)
        ! inputs and outputs
        type(vec3_t), intent(in) :: vec
        type(vec3_t)             :: vec_out
        ! calculations
        vec_out = vec / vector_mag(vec)
    end function vector_mag

end module vector_3d
