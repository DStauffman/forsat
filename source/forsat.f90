program forsat

use constants, only: RK, ONE, ZERO

use utils, only: unit_vec

implicit none

! variables
integer :: i
real(RK), dimension(:), allocatable :: x, x_norm
!real(RK), dimension(:,:), allocatable :: x, x_norm

!x = [(real(i, RK), i=1, 3)]
x = [-ONE, ZERO, ONE]
! TODO: this case isn't working:
!x = reshape([-ONE, ZERO, ONE], [3, 1])

print *, x

x_norm = unit_vec(x)

print *, x_norm

end program forsat
