program euler_551
   use EulerCommon
   integer(int64), parameter :: iMax=10**15
   integer(int64) :: i=1, total=0
   
   
   print *,a(iMax)

contains
   recursive function a(n) result(R)
      integer(int64), intent(in) :: n
      integer(int64)  :: R, k
      if (n<=1) then
         R = 1
      else
         k = a(n-1)
         R = k + ???
      end if
   end function
   
   
end program euler_551
