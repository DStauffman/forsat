program forsat

use constants, only: RK

use prng_nums, only: get_rand, get_randn, get_randg, get_randb, prng_t

implicit none

! variables
integer :: n
real(RK) :: a, b
real(RK), dimension(:), allocatable :: x_u, x_n, x_g1, x_g2, x_b
type(prng_t) :: prng

n = 10

a = 2
b = 5
allocate(x_u(n))
allocate(x_n(n))
allocate(x_g1(n))
allocate(x_g2(n))

prng = prng_t()

x_u  = get_rand(n, prng)
x_n  = get_randn(n, prng)
x_g1 = get_randg(n, a, prng)
x_g2 = get_randg(n, b, prng)
x_b  = get_randb(n, a, b, prng)


print *, x_b

end program forsat
