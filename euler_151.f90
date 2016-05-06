program euler_151
   integer, parameter :: dp = selected_int_kind(14,40), int64=selected_int_kind(10)
   real(dp) :: res
   
   res = paper(1,1,1,1)
   print '(f0.6)', res
contains
   recursive function paper(a2,a3,a4,a5) result(p)
      integer(int64), intent(in) :: a2,a3,a4,a5
      real(dp) :: p
      real(dp) :: p2,p3,p4,p5
      if (a5==1 .and. (a2+a3+a4)==0) then
         p = 0.0
         return
      end if
      if (a4==1 .and. (a2+a3+a5)==0) then
         p = paper(0,0,0,1) + 1.0
         return
      end if
      if (a3==1 .and. (a2+a4+a5)==0) then
         p = paper(0,0,1,1) + 1.0
         return
      end if
      if (a2==1 .and. (a3+a4+a5)==0) then
         p = paper(0,1,1,1) + 1.0
         return
      end if
      
      p2 = 0; p3 = 0; p4 = 0; p5 = 0
      if (a2 > 0) p2 = a2*paper(a2-1,a3+1,a4+1,a5+1)
      if (a3 > 0) p3 = a3*paper(a2  ,a3-1,a4+1,a5+1)
      if (a4 > 0) p4 = a4*paper(a2  ,a3  ,a4-1,a5+1)
      if (a5 > 0) p5 = a5*paper(a2  ,a3  ,a4  ,a5-1)
      
      P = (p2+p3+p4+p5) / real(a2+a3+a4+a5,dp)
      return
   end function paper
end program euler_151
