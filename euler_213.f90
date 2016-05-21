program euler_213
   use EulerCommon
   integer(int64) :: r, f, i, j, k, l
   real(dp) :: tot, ctr, st, en
   real(dp) :: grid(30,30,900), temp(30,30,900)

   call cpu_time(st)
   ! simulate the sample
   k = 1; l = 1
   do f=1,900
      grid(k,l,f) = 1.0_dp
      do r=1,50,2
         call run_step(grid(:,:,f), temp(:,:,f))
         call run_step(temp(:,:,f), grid(:,:,f))
      end do !- r
      k = k + 1
      if (k > 30) then
         k = 1
         l = l + 1 
      end if
   end do !- f

   ctr = 0.0_dp
   do j=1,30
      do i=1,30
         tot = product(1.0_dp - grid(i,j,:))
         ctr = ctr + tot
      end do !- i
   end do !- j
   call cpu_time(en)
   print '(f0.6)',ctr
   print '("took ",f0.6," sec")', en-st

contains
   subroutine run_step(aIn, aOut)
      real(dp), intent(in) :: aIn(30,30)
      real(dp), intent(out) :: aOut(30,30)
      integer(int64) :: i, j, d, m, n, ii, jj
      real(dp) :: tot

      aOut = 0.0_dp
      
      ! process first array
      do j=1,30
         do i=1,30
            ! set possible moves
            tot = 0.0_dp
            do n=-1,1
               do m=-1,1
                  if (abs(m) == abs(n)) cycle
                  if (legal_move(i+m,j+n)) tot = tot + 1.0_dp
               end do !- m
            end do !- n
            tot = 1.0_dp / tot

            ! process the jumps
            do n=-1,1
               do m=-1,1
                  if (abs(m) == abs(n)) cycle
                  if (legal_move(i+m,j+n)) aOut(i+m,j+n) = aOut(i+m,j+n)  + aIn(i,j)*tot
               end do !- m
            end do !- n
         end do !- i
      end do !- j
   end subroutine run_step

   logical function legal_move(a,b)
      integer(int64), intent(in) :: a,b
      legal_move = (a > 0) .and. (a < 31) .and. (b > 0) .and. (b < 31)
   end function
   
end program euler_213
