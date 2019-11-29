!----------------------------------------------------------------------
!                           PARAMETERS module
!        Copyright 2019 David C. Stauffer, All Rights Reserved
!----------------------------------------------------------------------

!> @brief   Parameters module file for the forsat code.  It defines the param class used by the rest of the code.
!>
!> @author  David C. Stauffer
!> @version 1.0
!> Written by David C. Stauffer in October 2019.
module parameters

    use asserts,     only: assert
    use constants,   only: INT_TOKEN, LARGE, ONE, RK, STDERR, STDOUT, ZERO
    use logging,     only: ACTIVE_LOG_LEVEL, LOG_DEBUG
    use text_input,  only: get_value_from_str
    use text_output, only: int2str, L1, L2, L3, L4, L5, num2str, write_line
    use utils,       only: load_seeds

    implicit none

    private

    !------------------------------------------------------------------
    !>
    !! Configuration parameters module file for the HESAT code.  It contains the generic
    !! configurations that deal with how the simulation is run.
    !<
    type, public :: config_t
        !> High level model configuration parameters.
        integer                               :: log_level ! log level for how much information to display while running
        character(len=:),         allocatable :: output_folder ! output folder/files
        character(len=:),         allocatable :: output_results
        character(len=:),         allocatable :: output_params
        character(len=:),         allocatable :: seeds_path
        logical                               :: repeatable_randomness ! randomness
        integer,  dimension(:,:), allocatable :: repeatable_seed
        integer                               :: repeatable_offset
        logical                               :: use_parfor ! parallelization
        integer                               :: max_cores
    end type config_t
    interface config_t
        module procedure                      :: config_init
    end interface

    !------------------------------------------------------------------
    !>
    !! Parameters class that wraps all the parameters needed for the simulation into one
    !! structure that can be passed around as necessary.
    !<
    type, public :: param_t
        type(config_t), pointer :: config
    contains
        procedure               :: print           => param_print
        procedure, nopass       :: read            => param_read
        procedure               :: write           => param_write
        procedure               :: validate_inputs => param_validate_inputs
    end type param_t
    interface param_t
        module procedure param_init
    end interface

contains
    !>
    !! config_init returns a config_t instance with default values.
    !! @return     config  Overall simulation configuration parameters.
    !<
    function config_init() result(config)
        ! inputs and outputs
        type(config_t), target :: config
        ! initialize fields with default values
        config%log_level             = LOG_DEBUG
        config%output_folder         = ''
        config%output_results        = 'results.bin'
        config%output_params         = 'params.txt'
        config%seeds_path            = '../data/seeds.bin'
        config%repeatable_randomness = .true.
        config%repeatable_seed       = load_seeds(config%seeds_path)
        config%repeatable_offset     = 0
        config%use_parfor            = .false.
        config%max_cores             = 8
    end function config_init

    function param_init() result(param)
        type(param_t)                :: param
        ! local variables
        type(config_t), target, save :: config
        ! Call all the initialize functions
        config       =  config_t()
        param%config => config
    end function param_init

    subroutine param_print(param, unit, prefix, log_level)
        ! inputs and outputs
        class(param_t),   intent(in)           :: param
        integer,          intent(in), optional :: unit
        character(len=*), intent(in), optional :: prefix
        integer,          intent(in), optional :: log_level
        ! local variables
        integer                       :: i, unit_, orig_log_level
        logical                       :: fmt_
        character(len=:), allocatable :: ps, cs
        if (present(unit)) then
            unit_ = unit
        else
            unit_ = STDOUT
        end if
        if (present(prefix)) then
            ps = prefix // '%'
        else
            ps = 'param%'
        end if
        if (present(log_level)) then
            ! TODO: potential OMP issues
            orig_log_level = ACTIVE_LOG_LEVEL
            ACTIVE_LOG_LEVEL = log_level
        end if
        if (unit_ == STDOUT) then
            fmt_ = .true.
        else
            fmt_ = .false.
        end if
        ! Write high level information
        call write_line(L1, unit_, fmt_, '! Parameters')
        call write_line(L5, unit_, fmt_, '!')
        call write_line(L5, unit_, fmt_, '!')
        call write_line(L5, unit_, fmt_, '!')
        call write_line(L5, unit_, fmt_, '!')
        call write_line(L3, unit_, fmt_, '')
        call write_line(L2, unit_, fmt_, '! Root parameters')
        call write_line(L3, unit_, fmt_, '')
        ! Write config parameters
        cs = ps // 'config%'
        call write_line(L2, unit_, fmt_, '! Configuration parameters')
        call write_line(L2, unit_, fmt_, cs // 'log_level             = ' // num2str(param%config%log_level))
        call write_line(L5, unit_, fmt_, cs // 'output_folder         = ''' // param%config%output_folder // '''')
        call write_line(L5, unit_, fmt_, cs // 'output_results        = ''' // param%config%output_results // '''')
        call write_line(L5, unit_, fmt_, cs // 'output_params         = ''' // param%config%output_params // '''')
        call write_line(L5, unit_, fmt_, cs // 'seeds_path            = ''' // param%config%seeds_path // '''')
        call write_line(L5, unit_, fmt_, cs // 'repeatable_randomness = ' // &
            num2str(param%config%repeatable_randomness))
        ! Note, this field is not printed, as it's way too long!
        ! call write_line(L10, unit_, fmt_, cs // 'repeatable_seed       = ' // num2str(param%config%repeatable_seed))
        call write_line(L5, unit_, fmt_, cs // 'repeatable_offset     = ' // num2str(param%config%repeatable_offset))
        call write_line(L5, unit_, fmt_, cs // 'use_parfor            = ' // num2str(param%config%use_parfor))
        call write_line(L5, unit_, fmt_, cs // 'max_cores             = ' // num2str(param%config%max_cores))
        ! Write end of file
        call write_line(L2, unit_, fmt_, '')
        call write_line(L2, unit_, fmt_, '! End of file')
        ! restore original log level
        if (present(log_level)) then
            ACTIVE_LOG_LEVEL = orig_log_level
        end if
    end subroutine param_print

    function param_read(filename) result(param)
        ! inputs and outputs
        character(len=*), intent(in) :: filename
        type(param_t)                :: param
        ! local variables
        integer                             :: fid, i, io_stat, num
        character(len=:), allocatable       :: err_msg, cs
        character(len=2500)                 :: this_line
        ! open file
        open(newunit=fid, file=filename, status='old', action='read', form='formatted', iostat=io_stat, iomsg=err_msg)
        if (io_stat /= 0) then
            ! file open failed, report error
            error stop 'Error opening parameters file: ' // err_msg
        end if
        ! read file to be able to override values without recompiling
        ! Read data - header
        read(unit=fid, fmt='(A)') this_line
        read(unit=fid, fmt='(A)') this_line
        read(unit=fid, fmt='(A)') this_line
        read(unit=fid, fmt='(A)') this_line
        read(unit=fid, fmt='(A)') this_line
        ! read high level information
        read(unit=fid, fmt='(A)') this_line
        read(unit=fid, fmt='(A)') this_line
        call assert(trim(this_line) == '! Root parameters', 'Root parameters not synced.')
        ! initialize param with the correct sizes
        param = param_init()
        ! read config parameters
        cs = 'param%config%'
        read(unit=fid, fmt='(A)') this_line
        read(unit=fid, fmt='(A)') this_line
        call assert(trim(this_line) == '! Configuration parameters', 'Configuration parameters not synced.')
        read(unit=fid, fmt='(A)') this_line
        call get_value_from_str(trim(this_line), param%config%log_level, cs // 'log_level')
        read(unit=fid, fmt='(A)') this_line
        call get_value_from_str(trim(this_line), param%config%output_folder, cs // 'output_folder')
        read(unit=fid, fmt='(A)') this_line
        call get_value_from_str(trim(this_line), param%config%output_results, cs // 'output_results')
        read(unit=fid, fmt='(A)') this_line
        call get_value_from_str(trim(this_line), param%config%output_params, cs // 'output_params')
        read(unit=fid, fmt='(A)') this_line
        call get_value_from_str(trim(this_line), param%config%seeds_path, cs // 'seeds_path')
        read(unit=fid, fmt='(A)') this_line
        call get_value_from_str(trim(this_line), param%config%repeatable_randomness, cs // 'repeatable_randomness')
        read(unit=fid, fmt='(A)') this_line
        call get_value_from_str(trim(this_line), param%config%repeatable_offset, cs // 'repeatable_offset')
        read(unit=fid, fmt='(A)') this_line
        call get_value_from_str(trim(this_line), param%config%use_parfor, cs // 'use_parfor')
        read(unit=fid, fmt='(A)') this_line
        call get_value_from_str(trim(this_line), param%config%max_cores, cs // 'max_cores')
        ! read end of file contents
        read(unit=fid, fmt='(A)') this_line
        read(unit=fid, fmt='(A)') this_line
        call assert(trim(this_line) == '! End of file', 'End of file not synced.')
        ! close file when down
        close(unit=fid)
    end function param_read

    subroutine param_write(param, filename)
        ! inputs and outputs
        class(param_t),   intent(in) :: param
        character(len=*), intent(in) :: filename
        ! local variables
        integer                       :: fid, io_stat
        character(len=:), allocatable :: err_msg
        ! open file for writing
        open(newunit=fid, file=filename, status='replace', action='write', form='unformatted', access='stream', iostat=io_stat, &
            iomsg=err_msg)
        if (io_stat /= 0) then
            ! file open failed, report error
            error stop 'Error opening file: ' // err_msg
        end if
        ! call print function
        call param%print(unit=fid, prefix='param')
        ! close file
        close(unit=fid)
    end subroutine param_write

    !! validate_inputs
    !> Validates the input parameters when certain rules need to be maintained for code algorithms to function correctly.
    subroutine param_validate_inputs(param)
        ! inputs and outputs
        class(param_t), intent(in) :: param
        ! local variables
        character(len=:), allocatable :: assert_msg
        !! Config checks
        ! TODO: none to do here
    end subroutine param_validate_inputs

end module parameters
