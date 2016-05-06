program euler_056
   use EulerCommon
   integer(int64), dimension(200) :: ab
   integer(int64) :: a,b, s, max_s ,max_a, max_b
   
   max_s = 0
   do a=90,99
      do b=90,99
         call powers(a,b,ab)
         s = sum(ab)
         if (s > max_s) then
            max_s = s
            max_a = a
            max_b = b
         end if
      end do
   end do
   print *,max_a, max_b, max_s
   
contains
   subroutine powers(a,b,ab)
      integer(int64), intent(in) :: a,b
      integer(int64), intent(out) :: ab(200)
      integer(int64) :: i, max_n
      
      ab(:) = 0
      ab(1) = a
      max_n = max(200_int64,b*int(log10(real(a)),int64)+3_int64)
      call checkTensInArray(max_n,ab)
      do i=2,b
         ab(:) = ab(:)*a
         call checkTensInArray(max_n,ab)
      end do !- i
   end subroutine powers

end program euler_056
