program euler_156
   use EulerCommon
   ! we choose a max of 8*10^11 due to OEIS search
   integer(int64), parameter :: nmax=80000000000_int64
   integer(int64), dimension(0:9) :: F=0
   integer(int64) :: n, d, a, i, k, total

   n=1_int64
   total = 0
   do while (n < nmax)
      k = n
      do while (k > 0_int64)
         d = mod(k, 10_int64)
         F(d) = F(d) + 1
         k = k / 10_int64
      end do !- k
      total = total + n*count(F==n)
      n = n + 1
      if (mod(n,1000000) == 0) then
         print *,n,total
      end if
   end do !- n
   print '("The number of digital equivalences is ",i0)',total
end program euler_156
