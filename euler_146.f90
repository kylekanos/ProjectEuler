include "EulerCommon.f90"
program euler_146
   use EulerCommon
   integer(int64) :: n, ct=1242490, num=3


   ! (1) the numbers must be even since we're adding 1,3,7,9,13,27 to the square
   ! (2) the numbers also need to be multiples of 5 (n^2=1 mod 5, etc), so we go by 10's
   ! (3) we start at 1,000,000 since we know the solution to that point
   do n=10**6,150*10**6,10
      if (test_prime_adds(n)) then
         ct = ct + n
         num = num + 1
         print *,n
      end if
   end do !- n
   print '("There are ",i0," such primes, summing to ",i0)',num,ct

contains
   logical function test_prime_adds(n) result(bool)
      integer(int64), intent(in) :: n
      integer(int64) :: p, k

      bool = .false.
      p = n*n + 1
      if (.not. is_prime(p)) return

      k = p
      p = p + 2 ! n*n + 3
      if (next_prime(k) /= p) return

      k = p
      p = p + 4 ! n*n + 7
      if (next_prime(k) /= p) return

      k = p
      p = p + 2 ! n*n + 9
      if (next_prime(k) /= p) return

      k = p
      p = p + 4 ! n*n + 13
      if (next_prime(k) /= p) return

      k = p
      p = p + 14 ! n*n + 27
      if (next_prime(k) /= p) return

      bool = .true.
      return
   end function test_prime_adds
end program euler_146
