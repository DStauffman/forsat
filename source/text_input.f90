module text_input
    ! This module provides methods to read text from files that doesn't suck, like
    ! the built-in namelists and read and write methods

    use asserts,   only: assert
    use constants, only: LARGE, LARGE_INT, NAN, NEG_LARGE, NEG_LARGE_INT, RK, SMALL

    implicit none

    private

    character(26), public, parameter :: UPPERCASE_LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(26), public, parameter :: LOWERCASE_LETTERS = 'abcdefghijklmnopqrstuvwxyz'

    public :: lower, upper, get_value_from_str, str2num

    interface str2num
        module procedure str2bool
        module procedure str2int
        module procedure str2int_vec
        module procedure str2real
        module procedure str2real_vec
        module procedure str2str
    end interface str2num

    interface get_value_from_str
        module procedure get_value_bool
        module procedure get_value_int
        module procedure get_value_int_vec
        module procedure get_value_real
        module procedure get_value_real_vec
        module procedure get_value_str
    end interface get_value_from_str

contains
    pure subroutine parse_line(str, lhs, rhs)
        ! inputs and outputs
        character(len=*), intent(in)               :: str
        character(len=:), intent(out), allocatable :: lhs
        character(len=:), intent(out), allocatable :: rhs
        ! local variables
        integer :: i, num
        ! initialize output
        lhs = ''
        rhs = ''
        ! get sizes
        num = len_trim(str)
        ! process string
        if (str(1:1) == '!') then
            return
        end if
        do i=1, num
            if (str(i:i) == '=') then
                lhs = trim(adjustl(str(1:i-1)))
                rhs = trim(adjustl(str(i+1:num)))
                return
            end if
        end do
    end subroutine parse_line

    pure function get_comma_locations(str, token) result(comma_locs)
        ! inputs and outputs
        character(len=*), intent(in)           :: str
        character(len=1), intent(in), optional :: token
        integer, dimension(:), allocatable     :: comma_locs
        ! local variables
        integer          :: i, counter, num
        character(len=1) :: this_token
        ! optional arguments
        if (present(token)) then
            this_token = token
        else
            this_token = ','
        end if
        num = len_trim(str)
        ! first pass to count commas
        counter = 0
        do i=1, num
            if (str(i:i) == this_token) then
                counter = counter + 1
            end if
        end do
        allocate(comma_locs(counter))
        ! second pass to find them
        counter = 1
        do i=1, num
            if (str(i:i) == this_token) then
                comma_locs(counter) = i
                counter = counter + 1
            end if
        end do
    end function get_comma_locations

    pure subroutine str2bool(str, bool)
        ! inputs and outputs
        character(len=*), intent(in)  :: str
        logical,          intent(out) :: bool
        ! local variables
        character(len=:), allocatable :: temp_str
        temp_str = lower(str)
        ! process string
        select case (temp_str)
            case ('t', '.true.', 'true')
                bool = .true.
            case ('f', '.false.', 'false')
                bool = .false.
            case default
                error stop 'Unexpected value for str.'
        end select
    end subroutine str2bool

    pure subroutine str2int(str, output)
        ! inputs and outputs
        character(len=*), intent(in)  :: str
        integer,          intent(out) :: output
        ! local variables
        integer :: io_stat
        character(len=:), allocatable :: err_msg, local_str
        ! trim any extra whitespace
        local_str = trim(adjustl(str))
        ! handle special cases
        if (local_str == 'LARGE_INT') then
            output = LARGE_INT
            return
        end if
        if (local_str == 'NEG_LARGE_INT') then
            output = NEG_LARGE_INT
            return
        end if
        ! process remaining values
        read(local_str, *, iostat=io_stat, iomsg=err_msg) output ! TODO: write my own recursive function to do this without the read command!
        if (io_stat /= 0) then
            error stop 'Unable to read integer from str: "' // str // '" with error message: "' // err_msg // '".'
        end if
    end subroutine str2int

    pure subroutine str2int_vec(str, output)
        ! inputs and outputs
        character(len=*), intent(in)                    :: str
        integer, dimension(:), allocatable, intent(out) :: output
        ! local variables
        integer                            :: i, counter, num_values, str_len
        integer, dimension(:), allocatable :: comma_locs
        character(len=:), allocatable      :: this_str
        ! count commas
        comma_locs = get_comma_locations(str)
        num_values = size(comma_locs) + 1
        str_len    = len(str)
        ! check for brackets
        if (str(1:1) /= '[') then
            error stop 'Vector doesn''t start with a ''['' in "' // str // '".'
        end if
        if (str(str_len:str_len) /= ']') then
            error stop 'Vector doesn''t end with a '']'' in "' // str // '".'
        end if
        ! allocate output
        allocate(output(size(comma_locs)+1))
        counter = 2 ! skip first bracket character
        do i = 1, num_values-1
            this_str = str(counter:comma_locs(i)-1)
            call str2int(this_str, output(i))
            counter = comma_locs(i) + 1
        end do
        this_str = str(counter:str_len-1) ! skip last bracket character
        call str2int(this_str, output(num_values))
    end subroutine str2int_vec

    pure subroutine str2real(str, output)
        ! inputs and outputs
        character(len=*), intent(in)  :: str
        real(RK),         intent(out) :: output
        ! local variables
        integer :: io_stat
        character(len=:), allocatable :: err_msg, local_str
        ! trim any extra whitespace
        local_str = trim(adjustl(str))
        ! handle special cases
        select case (local_str)
            case ('LARGE')
                output = LARGE
                return
            case ('NEG_LARGE')
                output = NEG_LARGE
                return
            case ('NAN')
                output = NAN
                return
            case ('SMALL')
                output = SMALL
                return
        end select
        ! process remaining values
        read(local_str, *, iostat=io_stat, iomsg=err_msg) output ! TODO: write my own recursive function to do this without the read command!
        if (io_stat /= 0) then
            error stop 'Unable to read real from str: "' // str // '" with error message: "' // err_msg // '".'
        end if
    end subroutine str2real

    pure subroutine str2real_vec(str, output)
        ! inputs and outputs
        character(len=*), intent(in)                     :: str
        real(RK), dimension(:), allocatable, intent(out) :: output
        ! local variables
        integer                            :: i, counter, num_values, str_len
        integer, dimension(:), allocatable :: comma_locs
        character(len=:), allocatable      :: this_str
        ! count commas
        comma_locs = get_comma_locations(str)
        num_values = size(comma_locs) + 1
        str_len    = len(str)
        ! check for brackets
        if (str(1:1) /= '[') then
            error stop 'Vector doesn''t start with a ''['' in "' // str // '".'
        end if
        if (str(str_len:str_len) /= ']') then
            error stop 'Vector doesn''t end with a '']'' in "' // str // '".'
        end if
        ! allocate output
        allocate(output(num_values))
        counter = 2 ! skip first bracket character
        do i = 1, num_values-1
            this_str = str(counter:comma_locs(i)-1)
            call str2real(this_str, output(i))
            counter = comma_locs(i) + 1
        end do
        this_str = str(counter:str_len-1) ! skip last bracket character
        call str2real(this_str, output(num_values))
    end subroutine str2real_vec

    pure subroutine str2str(str, output)
        ! inputs and outputs
        character(len=*),              intent(in)  :: str
        character(len=:), allocatable, intent(out) :: output
        ! local variables
        integer :: str_len, start_char, final_char
        ! hard-coded values
        integer, parameter:: single_quote = ichar("'")
        integer, parameter:: double_quote = ichar('"')
        ! pull out first and last characters
        str_len    = len(str)
        start_char = ichar(str(1:1))
        final_char = ichar(str(str_len:str_len))
        ! check for quotes
        if (start_char /= single_quote .and. start_char /= double_quote) then
            error stop 'String doesn''t start with a '' or " in "' // str // '".'
        end if
        if (final_char /= single_quote .and. final_char /= double_quote) then
            error stop 'String doesn''t end with a '' or " in "' // str // '".'
        end if
        ! All is good, pull out the remaining string
        output = str(2:str_len-1)
    end subroutine str2str

    pure function lower(str) result (output)
        ! inputs and outputs
        character(len=*), intent(in)  :: str
        character(len=:), allocatable :: output
        ! local variables
        integer :: i, c
        ! initialize output
        output = str(:)
        do i=1, len_trim(str)
            ! find the index to the lowercase version
            c = index(UPPERCASE_LETTERS, str(i:i))
            if (c > 0) then
                output(i:i) = LOWERCASE_LETTERS(c:c)
            end if
        end do
    end function lower

    pure function upper(str) result (output)
        ! inputs and outputs
        character(len=*), intent(in)  :: str
        character(len=:), allocatable :: output
        ! local variables
        integer :: i, c
        ! initialize output
        output = str(:)
        do i=1, len_trim(str)
            ! find the index to the lowercase version
            c = index(LOWERCASE_LETTERS, str(i:i))
            if (c > 0) then
                output(i:i) = UPPERCASE_LETTERS(c:c)
            end if
        end do
    end function upper

    pure subroutine get_value_bool(this_line, value, prefix)
        ! inputs and outputs
        character(len=*), intent(in)           :: this_line
        logical,          intent(out)          :: value
        character(len=*), intent(in), optional :: prefix
        ! local variables
        character(len=:), allocatable :: lhs, rhs
        ! process string
        call parse_line(this_line, lhs, rhs)
        ! compare prefix
        if (present(prefix)) then
            call assert(lhs == prefix, 'Prefix of "' // lhs // '" didn''t match "' // prefix // '"')
        end if
        ! get values
        call str2bool(rhs, value)
    end subroutine get_value_bool

    pure subroutine get_value_int(this_line, value, prefix)
        ! inputs and outputs
        character(len=*), intent(in)           :: this_line
        integer,          intent(out)          :: value
        character(len=*), intent(in), optional :: prefix
        ! local variables
        character(len=:), allocatable :: lhs, rhs
        ! process string
        call parse_line(this_line, lhs, rhs)
        ! compare prefix
        if (present(prefix)) then
            call assert(lhs == prefix, 'Prefix of "' // lhs // '" didn''t match "' // prefix // '"')
        end if
        ! get values
        call str2int(rhs, value)
    end subroutine get_value_int

    pure subroutine get_value_int_vec(this_line, value, prefix)
        ! inputs and outputs
        character(len=*), intent(in)                    :: this_line
        integer, intent(out), dimension(:), allocatable :: value
        character(len=*), intent(in), optional          :: prefix
        ! local variables
        character(len=:), allocatable :: lhs, rhs
        ! process string
        call parse_line(this_line, lhs, rhs)
        ! compare prefix
        if (present(prefix)) then
            call assert(lhs == prefix, 'Prefix of "' // lhs // '" didn''t match "' // prefix // '"')
        end if
        ! get values
        call str2int_vec(rhs, value)
    end subroutine get_value_int_vec

    pure subroutine get_value_real(this_line, value, prefix)
        ! inputs and outputs
        character(len=*), intent(in)           :: this_line
        real(RK),         intent(out)          :: value
        character(len=*), intent(in), optional :: prefix
        ! local variables
        character(len=:), allocatable :: lhs, rhs
        ! process string
        call parse_line(this_line, lhs, rhs)
        ! compare prefix
        if (present(prefix)) then
            call assert(lhs == prefix, 'Prefix of "' // lhs // '" didn''t match "' // prefix // '"')
        end if
        ! get values
        call str2real(rhs, value)
    end subroutine get_value_real

    pure subroutine get_value_real_vec(this_line, value, prefix)
        ! inputs and outputs
        character(len=*), intent(in)                     :: this_line
        real(RK), intent(out), dimension(:), allocatable :: value
        character(len=*), intent(in), optional           :: prefix
        ! local variables
        character(len=:), allocatable :: lhs, rhs
        ! process string
        call parse_line(this_line, lhs, rhs)
        ! compare prefix
        if (present(prefix)) then
            call assert(lhs == prefix, 'Prefix of "' // lhs // '" didn''t match "' // prefix // '"')
        end if
        ! get values
        call str2real_vec(rhs, value)
    end subroutine get_value_real_vec

    pure subroutine get_value_str(this_line, value, prefix)
        ! inputs and outputs
        character(len=*), intent(in)               :: this_line
        character(len=:), allocatable, intent(out) :: value
        character(len=*), intent(in), optional     :: prefix
        ! local variables
        character(len=:), allocatable :: lhs, rhs
        ! process string
        call parse_line(this_line, lhs, rhs)
        ! compare prefix
        if (present(prefix)) then
            call assert(lhs == prefix, 'Prefix of "' // lhs // '" didn''t match "' // prefix // '"')
        end if
        ! get values
        call str2str(rhs, value)
    end subroutine get_value_str

end module text_input
