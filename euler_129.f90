program euler_129
   use EulerCommon
   integer(int64) :: n,L
   integer(int64), parameter :: zero=0, one=1, two=2, ten=10
   
   L = 10**8_int64
   n = L-one		! note we start the cycle by adding 2, so we're really starting at L+1
   do
      n = n + two
      ! ensure divisibility
      if (gcd(n,ten) /= one) continue
      
      ! check repunit
      if (RepDiv(n) > L) then
         print '("Largest repunit divisor > ",i0," is ",i0)',L,n
         stop
      end if
      
   end do

contains
   integer(int64) function RepDiv(n) result(A)
      integer(int64), intent(in) ::n
      integer(int64) :: x
      x = one; A = one
      do while (x /= zero)
         x = mod(x*ten + one, n)
         A = A + one
      end do
   end function RepDiv
end program euler_129
