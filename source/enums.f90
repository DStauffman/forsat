!----------------------------------------------------------------------
!                            ENUMS module
!        Copyright 2019 David C. Stauffer, All Rights Reserved
!----------------------------------------------------------------------

!> @brief   Enums module file for the forsat code.  It defines the enumerators used by the rest of the code.
!>
!> @author  David C. Stauffer
!> @version 1.0
!> Written by David C. Stauffer in September 2019.
module enums

    implicit none

    private

    !------------------------------------------------------------------
    !>
    !! Gender Enumerators
    !<
    integer, public, parameter :: GENDER_NULL        = 1 !< not set, used for preallocation
    integer, public, parameter :: GENDER_FEMALE      = 2 !< female
    integer, public, parameter :: GENDER_MALE        = 3 !< male
    integer, public, parameter :: GENDER_UNCIRC_MALE = 4 !< uncircumcised male
    integer, public, parameter :: GENDER_CIRC_MALE   = 5 !< circumcised male
    integer, public, parameter :: GENDER_NUM_VALUES  = 5 !< number of gender values
    integer, public, parameter, dimension(GENDER_NUM_VALUES) :: GENDER_LIST_OF_VALUES = [ &
        GENDER_NULL, GENDER_FEMALE, GENDER_MALE, GENDER_UNCIRC_MALE, GENDER_CIRC_MALE] !< list of gender values

    !------------------------------------------------------------------
    !
    ! Public functions
    !
    public :: GENDER_is_female, GENDER_is_male

contains

    !------------------------------------------------------------------
    !
    ! Gender Functions
    !
    !>
    !! GENDER_is_female returns a boolean for whether the person is female or not.
    !! @param[in]  gender     gender of person
    !! @return     is_female  whether that person is female
    !<
    pure elemental function GENDER_is_female(gender) result(is_female)
        ! inputs and outputs
        integer, intent(in)  :: gender
        logical              :: is_female
        ! determine gender
        if (gender == GENDER_FEMALE) then
        is_female = .true.
        else
            is_female = .false.
        end if
    end function GENDER_is_female

    !>
    !! GENDER_is_male returns a boolean for whether the person is male or not.
    !! @param[in]  gender   gender of person
    !! @return     is_male  whether that person is male
    !<
    pure elemental function GENDER_is_male(gender) result(is_male)
        ! inputs and outputs
        integer, intent(in) :: gender
        logical             :: is_male
        ! determine gender
        if ((gender == GENDER_MALE) .or. (gender == GENDER_UNCIRC_MALE) .or. (gender == GENDER_CIRC_MALE)) then
        is_male = .true.
        else
            is_male = .false.
        end if
    end function GENDER_is_male

end module enums
