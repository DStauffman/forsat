!----------------------------------------------------------------------
! ASSERTS  defines procedures related to calling assert commands
!
!> @author
!> David C. Stauffer
!
! Change Log:
!     1.  Written by David C. Stauffer in September 2019.
!----------------------------------------------------------------------
module asserts

    use :: text_output, only: int2str

    implicit none

    private

    public :: assert

contains

    pure subroutine assert(check, msg, filename, line)
        ! inputs and outputs
        logical,          intent(in)           :: check
        character(len=*), intent(in), optional :: msg
        character(len=*), intent(in), optional :: filename
        integer,          intent(in), optional :: line
        ! local variables
        character(len=:), allocatable :: full_msg
#if SKIP_ASSERTS
#else
        ! First see if assert is valid before processing anything else
        if (.not. check) then
            ! If not valid, then process the error message
            if (present(msg)) then
                if (present(filename) .and. present(line)) then
                    full_msg = trim(msg) // ' in ' // filename // ' at ' // int2str(line) // ' '
                else
                    full_msg = trim(msg) // ' '
                end if
                error stop full_msg
            else
                error stop
            end if
        end if
#endif
    end subroutine assert

end module asserts
