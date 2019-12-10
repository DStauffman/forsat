program forsat

use constants, only: RK

implicit none

! variables
integer :: i
real(RK), dimension(:), allocatable :: x

x = [(real(i, RK), i=1, 3)]

print *, x

end program forsat
