!> solves choose(n,k) % p
integer(int64) function choosemod(n, k, p)
   integer(int64), intent(in) :: n, k, p
   if (k < 0 .or. n < k) then
      choosemod = 0
      return
   else if (k==0 .or. k==n) then
      choosemod = 1
      return
   end if
   Integer exp_np = factor_exponent(n,p)
   Integer exp_kp = factor_exponent(k,p)
   Integer exp_nk = factor_exponent(n-k,p)
   if (exp_np > exp_kp + exp_nk) then
      choosemod = 0
      return
   end if
   
   choosemod = choosemod_one(n, k, p)

contains
   !> returns the power to which p is raised in factoring n!
   integer(int64) factor_exponent(n, p) result(e)
      integer(int64), intent(in) :: n, p
      Integer(int64) :: e, m
      e = 0
      m = n
      do while (m>0)
         m = m/p
         e = e+m
      end do
      return
   end function factor_exponent
   
   !> returns the number k modulo prime p
   integer(int64) function invmod(k, p)
      integer(int64), intent(in) :: k, p
      integer(int64) :: p1, k1, q, r, m1, tmp
      logical :: neg
      if (p==0) then
         invmod = merge(k, 0, abs(k)==1)
         return
      end if
      if (p < 0) p = -p
      k = mod(k,p)
      if (k < 0) k = k+p
      neg = .true.
      p1 = 1; p2 = 0
      k1 = k; m1 = p
      do while (k1>0)
         q = m1/k1; r = mod(m1,k1)
         tmp = q*p1 + p2
         p2 = p1
         p1 = tmp
         m1 = k1
         k1 = r
         neg = not(neg)
      end do
      invmod = merge(p-p2, p2, neg)
   end function invmod
   
   !> first helperfunction for choosemod
   integer(int64) function choosemod_one(n, k, p)
      integer(int64), intent(in) :: n, k, p
      integer(int64) :: choose, qn, rn, qk, rk
      if (k < p) then
         choosemod_one = choosemod_two(n, k, p)
         return
      end if
      qn = n/p; rn = mod(n,p)
      qk = k/p; rk = mod(k,p)
      choose = choosemod_two(rn, rk, p)
      choose = choose * choosemod_two(qn, qk, p)
      choosemod_one = mod(choose, p)
      return
   end function choosemod_one
   
   !> second helperfunction for choosemod
   integer(int64) function choosemod_two(nn, kk, p)
      integer(int64), intent(in) :: nn, kk, p
      integer(int64) :: n, k, den, num, i
      n = nn; k = kk; den = 1; num = n
      n = mod(n,p)
      if (n < k) then
         choosemod_two = 0
         return
      else if (n==0 .or. k==0) then
         choosemod_two = 1
         return
      end if
      if (2*k > n) k = n - k
      n = n-1
      do while(k>1)
         num = mod(num*n, p)
         den = mod(den*k, p)
         n = n-1; k = k-1
      end do
      den = invmod(den, p)
      choosemod_two = mod(num*den, p)
   end function choosemod_two
   
end function choosemod
