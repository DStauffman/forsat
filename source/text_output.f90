module text_output
    ! This module provides methods to write text out to files that doesn't suck, like
    ! the built-in namelists and read and write methods.

    use constants, only: NEWLINE, STDOUT, RK
    use logging,   only: ACTIVE_LOG_LEVEL, LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR, LOG_CRITICAL
    use matlab,    only: is_nan

    implicit none

    private

    integer, private, parameter :: WRITE_DOUBLE    = 2
    integer, private, parameter :: WRITE_INTEGER   = 1
    logical, private, parameter :: NUM2STR_SQUEEZE = .true.

    ! TODO: decide on numbering scheme, either mimic Python, or just use 1-10?
    integer, public, parameter :: L5 = LOG_DEBUG
    integer, public, parameter :: L4 = LOG_INFO
    integer, public, parameter :: L3 = LOG_WARNING
    integer, public, parameter :: L2 = LOG_ERROR
    integer, public, parameter :: L1 = LOG_CRITICAL
    integer, public, parameter :: L0 = 0

    public :: int2str, num2str, write_line, write_data

    interface num2str
        module procedure bool2str
        module procedure int2str
        module procedure real2str
        module procedure ivec2str
        module procedure rvec2str
        module procedure imat2str
        module procedure rmat2str
    end interface num2str

    interface write_data
        module procedure write_data_int_vec
        module procedure write_data_int_mat
        module procedure write_data_real_vec
        module procedure write_data_real_mat
    end interface write_data

contains
    subroutine write_line(log_level, unit, formatted, str)
        ! inputs and outputs
        integer,          intent(in) :: log_level
        integer,          intent(in) :: unit
        character(len=*), intent(in) :: str
        logical,          intent(in) :: formatted
        ! check if logging at this level
        if (log_level > ACTIVE_LOG_LEVEL) then
            return
        end if
        ! write either formatted or unformatted version
        if (formatted) then
            write(unit=unit, fmt='(A,/)', advance='no') str
        else
            write(unit) str // NEWLINE
        end if
    end subroutine write_line

    subroutine write_data_int_vec(fid, this_data, this_name, fields_written)
        ! inputs and outputs
        integer, intent(in)                  :: fid
        integer, intent(in), dimension(:)    :: this_data
        character(len=*), intent(in)         :: this_name
        integer, intent(inout)               :: fields_written
        ! local variables
        character(len=40)                    :: fixed_name
        ! write data
        fixed_name = this_name
        write(fid) fixed_name, WRITE_INTEGER, 1, size(this_data, 1)
        write(fid) this_data
        fields_written = fields_written + 1
    end subroutine write_data_int_vec

    subroutine write_data_int_mat(fid, this_data, this_name, fields_written)
        ! inputs and outputs
        integer, intent(in)                  :: fid
        integer, intent(in), dimension(:,:)  :: this_data
        character(len=*), intent(in)         :: this_name
        integer, intent(inout)               :: fields_written
        ! local variables
        character(len=40)                    :: fixed_name
        ! write data
        fixed_name = this_name
        write(fid) fixed_name, WRITE_INTEGER, size(this_data, 1), size(this_data, 2)
        write(fid) this_data
        fields_written = fields_written + 1
    end subroutine write_data_int_mat

    subroutine write_data_real_vec(fid, this_data, this_name, fields_written)
        ! inputs and outputs
        integer,  intent(in)                 :: fid
        real(RK), intent(in), dimension(:)   :: this_data
        character(len=*), intent(in)         :: this_name
        integer, intent(inout)               :: fields_written
        ! local variables
        character(len=40)                    :: fixed_name
        ! write data
        fixed_name = this_name
        write(fid) fixed_name, WRITE_DOUBLE, 1, size(this_data, 1)
        write(fid) this_data
        fields_written = fields_written + 1
    end subroutine write_data_real_vec

    subroutine write_data_real_mat(fid, this_data, this_name, fields_written)
        ! inputs and outputs
        integer, intent(in)                  :: fid
        real(RK), intent(in), dimension(:,:) :: this_data
        character(len=*), intent(in)         :: this_name
        integer, intent(inout)               :: fields_written
        ! local variables
        character(len=40)                    :: fixed_name
        ! write data
        fixed_name = this_name
        write(fid) fixed_name, WRITE_DOUBLE, size(this_data, 1), size(this_data, 2)
        write(fid) this_data
        fields_written = fields_written + 1
    end subroutine write_data_real_mat

    pure function bool2str(bool) result(str)
        ! inputs and outputs
        logical, intent(in) :: bool
        character(len=:), allocatable :: str
        ! process results
        if (bool) then
            str = '.true.'
        else
            str = '.false.'
        end if
    end function bool2str

    pure recursive function int2str(num, dig) result(str)
        ! inputs and outputs
        integer, intent(in)           :: num
        integer, intent(in), optional :: dig
        character(len=:), allocatable :: str
        ! local variables
        integer          :: abs_num, i, pad_zeros
        logical          :: was_negative
        character(len=1) :: temp_str
        ! use positive version of number for most steps, but remember if it was negative at the start
        if (num < 0) then
            was_negative = .true.
        else
            was_negative = .false.
        endif
        abs_num = abs(num)
        ! find appropriate digit
        select case (mod(abs_num, 10))
            case (0)
                temp_str = '0'
            case (1)
                temp_str = '1'
            case (2)
                temp_str = '2'
            case (3)
                temp_str = '3'
            case (4)
                temp_str = '4'
            case (5)
                temp_str = '5'
            case (6)
                temp_str = '6'
            case (7)
                temp_str = '7'
            case (8)
                temp_str = '8'
            case (9)
                temp_str = '9'
            case default
                error stop
        end select
        ! call recursively until all digits are calculated
        if (abs_num < 10) then
            str = temp_str
        else
            str = int2str(abs_num / 10) // temp_str
        end if
        ! check for a minimum number of digits to use
        if (present(dig)) then
            pad_zeros = dig - len(str)
            do i=1, pad_zeros
                str = '0' // str
            end do
        end if
        ! handle negative case at conclusion of recursion
        if (was_negative) then
            str = '-' // str
        end if
    end function int2str

    pure function real2str(num, dig) result(str)
        ! inputs and outputs
        real(RK), intent(in)          :: num
        integer, intent(in), optional :: dig
        character(len=:), allocatable :: str
        ! local variables
        integer                       :: ending_nines, i, int_part, local_dig, str_ix
        real(RK)                      :: abs_num, frac_part
        character(len=1)              :: last_char
        character(len=:), allocatable :: frac_str
        ! hard-coded values
        integer,  parameter :: round_nines = 4
        real(RK), parameter :: max_value   = 1e20_RK
        real(RK), parameter :: min_value   = 1e-20_RK
        ! optional inputs
        if (present(dig)) then
            local_dig = dig
        else
            local_dig = 14 ! will be truncated later
        end if
        ! use the absolute value for most calculations
        abs_num = abs(num)
        ! get parts
        int_part = floor(abs_num)
        frac_part = abs_num - real(int_part, RK)
        ! bounds checks
        if (is_nan(num)) then
            str = 'NAN'
        else if (abs_num == 0) then
            str = '0.0'
        else if (abs_num > max_value) then
            str = 'LARGE'
        else if (abs_num < min_value) then
            str = 'SMALL'
        else
            frac_str = ''
            do i=1, local_dig
                frac_part = mod(10._RK * frac_part, 10._RK)
                frac_str = frac_str // int2str(floor(frac_part))
            end do
            str = int2str(int_part) // '.' // frac_str
        end if
        ! prepend negative sign if necessary
        if (num < 0) then
            str = '-' // str
        end if
        ! deal with rounding issues
        if (.not. present(dig)) then
            ! count the number of nines at the end
            str_ix = len(str)
            ending_nines = 0
            do
                if (str(str_ix:str_ix) == '9') then
                    ending_nines = ending_nines + 1
                    str_ix = str_ix - 1
                else
                    exit
                end if
            end do
            ! if lots of nines, then round
            if (ending_nines >= round_nines) then
                ! get the last character before the last nine
                last_char = str(str_ix:str_ix)
                if (last_char == '.') then
                    ! if this character is a '.', then round up, like 1.9999999 => 2. or -2.999999 => -3.
                    str = int2str(int_part + 1) // '.'
                    if (num < 0) str = '-' // str
                else
                    ! any other case, then increment the last digit and end there, like 1.78999999 => 1.79
                    last_char = char(ichar(str(str_ix:str_ix)) + 1)
                    str = str(1:str_ix-1) // last_char
                end if
            end if
            do
                str_ix = len(str)
                if (str(str_ix:str_ix) == '0') then
                    str = str(1:str_ix-1)
                else
                    if (str(str_ix:str_ix) == '.' .and. local_dig > 0) then
                        str = str // '0'
                    end if
                    exit
                end if
            end do
        end if
    end function real2str

    pure function ivec2str(vec, squeeze) result(str)
        ! inputs and outputs
        integer, intent(in), dimension(:) :: vec
        logical, intent(in), optional     :: squeeze
        character(len=:), allocatable     :: str
        ! local variables
        integer :: i, num
        logical :: local_squeeze
        ! optional inputs
        if (present(squeeze)) then
            local_squeeze = squeeze
        else
            local_squeeze = NUM2STR_SQUEEZE
        end if
        ! process values
        num = size(vec)
        if (num == 1 .and. local_squeeze) then
            str = int2str(vec(1))
            return
        end if
        str = '['
        do i=1, num
            if (i < num) then
                str = str // int2str(vec(i)) // ', '
            else
                str = str // int2str(vec(i))
            end if
        end do
        str = str // ']'
    end function ivec2str

    pure function rvec2str(vec, dig, squeeze) result(str)
        ! inputs and outputs
        real(RK), intent(in), dimension(:) :: vec
        integer, intent(in), optional      :: dig
        logical, intent(in), optional      :: squeeze
        character(len=:), allocatable      :: str
        ! local variables
        integer :: i, num
        logical :: local_squeeze
        ! optional inputs
        if (present(squeeze)) then
            local_squeeze = squeeze
        else
            local_squeeze = NUM2STR_SQUEEZE
        end if
        ! process values
        num = size(vec)
        if (num == 1 .and. local_squeeze) then
            str = real2str(vec(1))
            return
        end if
        str = '['
        do i=1, num
            if (i < num) then
                str = str // real2str(vec(i), dig) // ', '
            else
                str = str // real2str(vec(i), dig)
            end if
        end do
        str = str // ']'
    end function rvec2str

    pure function imat2str(mat, squeeze) result(str)
        ! inputs and outputs
        integer, intent(in), dimension(:,:) :: mat
        logical, intent(in), optional       :: squeeze
        character(len=:), allocatable       :: str
        ! local variables
        integer :: i, j, row, col, numel
        logical :: local_squeeze
        ! optional inputs
        if (present(squeeze)) then
            local_squeeze = squeeze
        else
            local_squeeze = NUM2STR_SQUEEZE
        end if
        ! process values
        row = size(mat, dim=1)
        col = size(mat, dim=2)
        if (row == 1 .and. local_squeeze) then
            str = ivec2str(mat(1, :), squeeze=local_squeeze)
            return
        else if (col == 1 .and. local_squeeze) then
            str = ivec2str(mat(:, 1), squeeze=local_squeeze)
            return
        end if
        numel = row * col
        str = 'reshape(['
        do j=1, col
            do i=1, row
                if (i*j < numel) then
                    str = str // int2str(mat(i, j)) // ', '
                else
                    str = str // int2str(mat(i, j))
                end if
            end do
        end do
        str = str // '], [' // int2str(row) // ', ' // int2str(col) // '])'
    end function imat2str

    pure function rmat2str(mat, dig, squeeze) result(str)
        ! inputs and outputs
        real(RK), intent(in), dimension(:,:) :: mat
        integer, intent(in), optional        :: dig
        logical, intent(in), optional        :: squeeze
        character(len=:), allocatable        :: str
        ! local variables
        integer :: i, j, row, col, numel
        logical :: local_squeeze
        ! optional inputs
        if (present(squeeze)) then
            local_squeeze = squeeze
        else
            local_squeeze = NUM2STR_SQUEEZE
        end if
        ! process values
        row = size(mat, 1)
        col = size(mat, 2)
        if (row == 1 .and. local_squeeze) then
            str = rvec2str(mat(1, :), squeeze=local_squeeze)
            return
        else if (col == 1 .and. local_squeeze) then
            str = rvec2str(mat(:, 1), squeeze=local_squeeze)
            return
        end if
        numel = row * col
        str = 'reshape(['
        do j=1, col
            do i=1, row
                if (i*j < numel) then
                    str = str // real2str(mat(i, j), dig) // ', '
                else
                    str = str // real2str(mat(i, j), dig)
                end if
            end do
        end do
        str = str // '], [' // int2str(row) // ', ' // int2str(col) // '])'
    end function rmat2str

end module text_output
