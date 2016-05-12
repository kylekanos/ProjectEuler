program euler_127
   integer, parameter :: int64 = selected_int_kind(10)
   integer(int64),parameter :: zero=0,one=1,two=2,three=3
   integer(int64) :: a,b,c,cmax=120000
   integer(int64) :: total=0
   
   ! looping this way should ensure that b > a (cond 2) and a+b=c (cond 3)
   do c=1,cmax
      do a=1,c/2
         b = c-a
         ! if this is an abc-hit, then we'll check rad (a*b*c)
         if (abc_hit(a,b,c)) then
            if (rad(a*b*c) < c) total = total + c
         end if
      end do !- a
   end do !- c
   
   print *,total
contains
   !> returns true if gcd(a,b)=gcd(b,c)=gcd(a,c)=1; false otherwise
   logical function abc_hit(a,b,c) result(bool)
      integer(int64), intent(in) :: a,b,c
      
      logical :: ab,bc,ac
      
      ab = gcd(a,b) == one
      bc = gcd(b,c) == one
      ac = gcd(a,c) == one
      
      bool = ab .and. bc .and. ac
   end function abc_hit
   
   !> returns the product of the distinct prime factors
   integer(int64) function rad(n)
      integer(int64), intent(in) :: n
      integer(int64), dimension(50) :: dpf     ! distinct prime factors
      integer(int64) :: k
      
      dpf = prime_factors(n)
      rad = one
      k = one
      do while (dpf(k) > 0)
         rad = rad*dpf(k)
         k = k + 1
      end do
   end function rad
   
   !> list of prime factors
   function prime_factors(nn) result(pf)
      integer(int64), intent(in) :: nn
      integer(int64), dimension(50) :: pf
      integer(int64) :: k, n, d
      
      k=zero
      pf = -one        ! initialize all prime factors to -1
      n = nn
      ! remove factors of 2
      do
         if (iand(n,one)==zero .or. n==one) then
            n = n/two
            if (k == zero) then
               pf(k) = two
               k = k + one
            end if
         end if
      end do
      
      d = three
      do
         if (d > nn) exit
         do
            if (mod(n, d) /= zero .or. n == one) then
               n = n/d
               if (pf(k-1) /= d) then
                  pf(k) = d
                  k = k + one
               end if
            end if
         end do
         d = d + two
      end do
   end function prime_factors
   
   !> implements binary gcd with some bitshifting for speed
   recursive function gcd(u, v) result(g)
      integer(int64), intent(in) :: u, v
      integer(int64) :: g
      
      if (u==v .or. u==zero) then
         g = v
         return
      end if
      
      if (v==zero) then
         g = u
         return
      end if
      
      ! if a number is even, then the taking bitwise AND with 1 would return 0
      ! ISHFT returns the shift of a number, direction depends on the 2nd argument: pos num = RSHIFT, neg num = LSHIFT
      if (iand(u, one)==zero) then
         if (iand(v,one)==zero) then
            g = gcd(ishft(u,one), v)
         else
            g = gcd(ishft(u,one), ishft(v,one))
            g = ishft(g, -one)
         end if
         return
      end if
      
      if (iand(v,one)==zero) then
         g = gcd(u, ishft(v,one))
         return
      end if
      
      g = gcd(ishft(abs(u-v),one),min(v,u))
   end function gcd
end program euler_127
