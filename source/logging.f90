module logging

    use constants, only: STDOUT

    implicit none

    private

    integer, public, parameter :: LOG_NOTSET   = 10
    integer, public, parameter :: LOG_DEBUG    = 9
    integer, public, parameter :: LOG_INFO     = 7
    integer, public, parameter :: LOG_WARNING  = 5
    integer, public, parameter :: LOG_ERROR    = 3
    integer, public, parameter :: LOG_CRITICAL = 1

    integer, public, save :: ACTIVE_LOG_LEVEL  = 5

    public :: logger

contains

    subroutine logger(log_level, msg)
        ! inputs and outputs
        integer, intent(in) :: log_level
        character(len=*)    :: msg
        ! TODO: add option to log to a file instead of STDOUT
        ! log data depending on level
        if (log_level <= ACTIVE_LOG_LEVEL) then
            write(STDOUT, '(A)') trim(msg)
        end if
    end subroutine logger

end module logging
