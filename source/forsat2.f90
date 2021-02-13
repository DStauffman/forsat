program forsat

use constants, only: RK

use kalman, only: mat_divide

implicit none

! variables
real(RK), dimension(:),    allocatable :: b, x, x2
real(RK), dimension(:, :), allocatable :: a, c, x3

! Least square solver tests
a = reshape([1._RK, 3._RK, 2._RK, 4._RK], [2, 2])
x = [1._RK, -1._RK]
b = matmul(a, x)
allocate(c(size(b, 1), 1))
c(:, 1) = b

x2 = mat_divide(a, b)

x3 = mat_divide(a, c)

print *, x - x2
print *, x - x3(:, 1)

end program forsat
