program euler_213
   integer, parameter :: int64 = selected_int_kind(10)
   integer, parameter :: dp = selected_real_kind(15,307)
   integer(int64), parameter :: runs=10000
   integer(int64) :: grid(30,30), i, j, ctr, m, n, mv(2)
   
   ctr = 0
   do i=1,runs
      ! initialize the grid
      grid(:,:) = 1
      do j=1,50
         do m=1,30; do n=1;30
            mv = randdir(m,n)
            grid(m,n) = grid(m,n) - 1
            grid(mv(1),mv(2)) = grid(mv(1),mv(2)) + 1
         end do; end do
      end do
      ctr = ctr + count(grid==0)
   end do !- i
   
   print *,ctr/real(runs,dp)

contains
   function randdir(i,j) result(a)
      integer(int64), intent(in) :: i,j
      integer(int64), dimension(2) :: a
      integer(int64) :: ud,rl
      real(dp) :: x,y
      

      
      if (i==1 .and. j==1) then
       ! at bottom-left corner
         call random_number(x)
         a = merge((/0,1/), (/1, 0/), x > 0.5_dp)
      else if (i==1 .and. j==30) then
       ! at bottom-right corner
         call random_number(x)
         a = merge((/0,1/), (/-1,0/), x > 0.5_dp)
      else if (i==30 .and. j==1) then
       ! at top-left corner
         call random_number(x)
         a = merge((/0,1/), (/-1, 0/), x > 0.5_dp)
      else if (i==30 .and. j==30) then
       ! at top-right corner
         call random_number(x)
         a = merge((/0,-1/), (/-1, 0/), x > 0.5_dp)
      else if (i==1 .or. i==30) then
       ! at left/right edge
         call random_number(x)
         if (x <= 1d0/3d0) then
            a = (/0, 1/)
         else if (x > 1d0/3d0 .and. x <= 2d0/3d0) then
            a = (/0, -1/)
         else
            a = merge((/1, 0/), (/-1, 0/), i==1)
         end if
      else if (j==1 .or. j==30) then
       ! at top/bottom edge
         call random_number(x)
         if (x <= 1d0/3d0) then
            a = (/1, 0/)
         else if (x > 1d0/3d0 .and. x <= 2d0/3d0) then
            a = (/-1, 0/)
         else
            a = merge((/0, 1/), (/0, -1/), i==1)
         end if
      else
       ! anywhere else
         call random_number(x)
         if (x <= 0.25_dp) then
            a = (/0, 1/)
         else if (x > 0.25_dp .and. x <= 0.5_dp) then
            a = (/0, -1/)
         else if (x > 0.5_dp .and. x <= 0.75_dp) then
            a = (/1, 0/)
         else
            a = (/-1,0/)
         end if
      end if
   end function randdir
end program euler_213
