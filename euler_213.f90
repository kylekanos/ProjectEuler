program euler_213
   integer, parameter :: int64 = selected_int_kind(10)
   integer, parameter :: dp = selected_real_kind(15,307)
   integer(int64), parameter :: runs=10000
   integer(int64) :: i, j, ctr, m, n, mv(2), mm, nn, temp(30,30), nfleas
   integer(int64) :: grid(30,30), temp(30,30)
   
   
   ctr = 0
   do i=1,runs
      ! initialize the grid
      grid(:,:) = 1
      do j=1,50,2
         ! run through once, storing to temp
         do m=1,30
            do n=1,30
               ! copy the cell regardless
               temp(m,n) = grid(m,n)
               ! test for fleas
               if (grid(m,n) == 0) continue
               nfleas = grid(m,n)
               ! run through fleas
               do f=1,grid(m,n)
                  mv = randdir(m,n)
                  mm = m + mv(1); nn = n + mv(2)
                  ! take 1 from the original cell and move it to the next cell
                  temp( m, n) = temp( m, n) - 1
                  temp(mm,nn) = temp(mm,nn) + 1
               end do !- f
            end do !- n
         end do !- m
         
         ! run through a second time, copying back to grid -- this saves a copy by doing some work instead
         do m=1,30
            do n=1,30
               ! copy the cell regardless
               grid(m,n) = temp(m,n)
               ! test for fleas
               if (temp(m,n) == 0) continue
               ! run through fleas
               do f=1,temp(m,n)
                  mv = randdir(m,n)
                  mm = m + mv(1); nn = n + mv(2)
                  ! take 1 from the original cell and move it to the next cell
                  grid( m, n) = grid( m, n) - 1
                  grid(mm,nn) = grid(mm,nn) + 1
               end do !- f
            end do !- n
         end do !- m
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
       ! at bottom-left corner; can only move R and U
         call random_number(x)
         a(:) = merge((/0,1/), (/1, 0/), x > 0.5_dp)
      else if (i==1 .and. j==30) then
       ! at bottom-right corner; can only move L and U
         call random_number(x)
         a(:) = merge((/0,1/), (/-1,0/), x > 0.5_dp)
      else if (i==30 .and. j==1) then
       ! at top-left corner; can only move R and D
         call random_number(x)
         a(:) = merge((/0,1/), (/-1, 0/), x > 0.5_dp)
      else if (i==30 .and. j==30) then
       ! at top-right corner; can only move L and D
         call random_number(x)
         a(:) = merge((/0,-1/), (/-1, 0/), x > 0.5_dp)
      else if (i==1 .or. i==30) then
       ! at left/right edge; can only move L/R and U and D
         call random_number(x)
         if (x <= 1d0/3d0) then
            a(:) = (/0, 1/)
         else if (x > 1d0/3d0 .and. x <= 2d0/3d0) then
            a(:) = (/0, -1/)
         else
            a(:) = merge((/1, 0/), (/-1, 0/), i==1)
         end if
      else if (j==1 .or. j==30) then
       ! at top/bottom edge; can only move
         call random_number(x)
         if (x <= 1d0/3d0) then
            a(:) = (/1, 0/)
         else if (x > 1d0/3d0 .and. x <= 2d0/3d0) then
            a(:) = (/-1, 0/)
         else
            a(:) = merge((/0, 1/), (/0, -1/), j==1)
         end if
      else
       ! anywhere else
         call random_number(x)
         if (x <= 0.25_dp) then
            a(:) = (/0, 1/)
         else if (x > 0.25_dp .and. x <= 0.5_dp) then
            a(:) = (/0, -1/)
         else if (x > 0.5_dp .and. x <= 0.75_dp) then
            a(:) = (/1, 0/)
         else
            a(:) = (/-1,0/)
         end if
      end if
   end function randdir
end program euler_213
