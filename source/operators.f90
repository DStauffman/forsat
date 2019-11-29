module operators

    use constants, only: RK
    !integer, public, parameter :: RK = selected_real_kind(15, 307)

    implicit none

    private

    public :: rdivide, idivide
    ! TODO: get operators working for built-in types
    !public :: operator(.rdivide.), operator(,idivide.)

    interface rdivide
        module procedure int_divide_int
        module procedure int_divide_real
        module procedure real_divide_int
        module procedure real_divide_real
    end interface rdivide
    interface idivide
        module procedure int_floor_divide_int
    end interface idivide

    interface operator (.rdivide.)
        module procedure int_divide_int
        module procedure int_divide_real
        module procedure real_divide_int
        module procedure real_divide_real
    end interface operator (.rdivide.)
    interface operator (.idivide.)
        module procedure int_floor_divide_int
    end interface operator (.idivide.)

contains

    ! Functions to divide integers as reals without doing casts in the code
    pure elemental function int_divide_int(x, y) result(output)
        integer,  intent(in) :: x
        integer,  intent(in) :: y
        real(RK)             :: output
        output = real(x, RK) / real(y, RK)
    end function int_divide_int
    pure elemental function int_divide_real(x, y) result(output)
        integer,  intent(in) :: x
        real(RK), intent(in) :: y
        real(RK)             :: output
        output = real(x, RK) / y
    end function int_divide_real
    pure elemental function real_divide_int(x, y) result(output)
        real(RK), intent(in) :: x
        integer,  intent(in) :: y
        real(RK)             :: output
        output = x / real(y, RK)
    end function real_divide_int
    pure elemental function real_divide_real(x, y) result(output)
        real(RK), intent(in) :: x
        real(RK), intent(in) :: y
        real(RK)             :: output
        output = x / y
    end function real_divide_real

    ! Functions to do flooring/integer division without doing casts in the code
    ! Note that this is just a normal divide for two integers
    pure elemental function int_floor_divide_int(x, y) result(output)
        integer,  intent(in) :: x
        integer,  intent(in) :: y
        integer              :: output
        output = x / y
    end function int_floor_divide_int

end module operators
