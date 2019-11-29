module constants

    ! Constants module file for the Forsat code.  It defines constants used throughout the rest of the code.
    !
    ! Notes
    ! -----
    ! 1.  Written by David C. Stauffer in September 2019.

    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use, intrinsic :: iso_c_binding, only: C_LONG_LONG

    implicit none

    private

    ! Constants - characters
    character(len=1), public, parameter :: WIN_PATHSEP     = '\'
    character(len=1), public, parameter :: UNIX_PATHSEP    = '/'
    ! Note: path logic seems to work by always using Unix slashes
    character(len=1), public, parameter :: PATHSEP         = UNIX_PATHSEP
    character(len=1), public, parameter :: LF = char(10)
    character(len=1), public, parameter :: CR = char(13)
    character(len=1), public, parameter :: NEWLINE = new_line('A') ! OS dependent
    character(len=2), public, parameter :: CRLF = CR // LF

    ! Constants - kinds and units
    integer, public, parameter :: SP = selected_real_kind(6, 37)
    integer, public, parameter :: DP = selected_real_kind(15, 307)
    integer, public, parameter :: QP = selected_real_kind(33, 4931)
    ! Note that alternatives exist in new modules, but this is the most generic option
    ! Could use: real64 from iso_fortran_env or C_DOUBLE from iso_c_binding
    integer, public, parameter :: RK = selected_real_kind(15, 307)
    integer, public, parameter :: LONG_INT = C_LONG_LONG
    integer, public, parameter :: STDOUT = output_unit
    integer, public, parameter :: STDERR = error_unit

    ! Constants - integers
    integer, public, parameter :: GUI_TOKEN       = -1
    integer, public, parameter :: INT_TOKEN       = -1
    integer, public, parameter :: LARGE_INT       = 1073741824 ! 2**30
    integer, public, parameter :: NEG_LARGE_INT   = -1073741824 ! -2**30

    ! Constants - reals
    real(RK), public, parameter :: PI              = 4._RK * atan(1._RK)
    real(RK), public, parameter :: TWO_PI          = 2._RK * pi
    real(RK), public, parameter :: NAN             = transfer(-2251799813685248_LONG_INT, 1._RK)
    real(RK), public, parameter :: ZERO            = 0._RK
    real(RK), public, parameter :: ONE             = 1._RK
    real(RK), public, parameter :: TWO             = 2._RK
    real(RK), public, parameter :: LARGE           = huge(0._RK)
    real(RK), public, parameter :: NEG_LARGE       = -huge(0._RK)
    real(RK), public, parameter :: SMALL           = tiny(0._RK)
    
    ! Constants - conversions
    real(RK), public, parameter :: MONTHS_PER_YEAR = 12._RK

end module
