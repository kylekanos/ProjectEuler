program euler226
   integer, parameter :: dp = selected_real_kind(15,307)
   real(dp) :: x, dx, F

   x = lower_bound()
   dx = (0.5 - x)/50000.0
   F = 0.0
   do while(x < 0.5)
      F = F + dx*something()
   end do !- 
   print *,"The area of overlap is %0.8f",F

contains
   !> finds the lower bound for integrating
   real(dp) function lower_bound()
      real(dp) :: p, q, r
      p = 0.0
      q = 0.5
      do while (abs(p-q) > 1e-15)
         r = 0.5*(q + p)
         f = something()
         g = something()
         h = something()
         if (f*h < 0.0) then
            q = r
         else if (g*h < 0.0) then
            p = r
         end if
      end do !-
      lower_bound = 0.5*(q + p)
   end function lower_bound
   
   !> snipped out relevant function(s)
end program euler226
