program euler_201
   use EulerCommon
   implicit none
   integer(int64), parameter :: kmax=100
   integer(int64), dimension(kmax) :: sq
   integer(int64) :: i, k, tot, ctr=0, one=1, zero=0
   integer(int64) :: max_sum
   type dictionary
      integer(int64) :: key,val
   end type dictionary
   type(dictionary), allocatable :: sums(:)
   real(dp) :: stt, fin
   call cpu_time(stt)

   ! set squares
   do i=1,kmax
      sq(i) = i*i
   end do !- i
   max_sum = sum(sq(:50))*1.06

   ! set the sums
   allocate(sums(1))
   sums(1)%key = 0
   sums(1)%val = 0

   ctr = zero

   call explore(one,zero,zero)

   tot = 0
   do i=1,size(sums)
      if (sums(i)%val == one) tot = tot + one
   end do !- i
   call cpu_time(fin)
   print *,tot*sum(sq)
   print '("took ",f0.6," seconds")',fin-stt

contains
   recursive subroutine explore(start, curr_sum, lev)
      integer(int64), intent(in) :: start, curr_sum, lev
      integer(int64) :: m, p_ct, m1, cm, l1
      
      ! step 1: check something
      if (???) then
         call ???
         ctr = ctr + one
         return
      end if

      ! explore somre more
      do m=start,kmax
         cm = curr_sum + m*m
         if (cm > max_sum) return

         p_ct = ctr
         m1 = m + one
         l1 = lev + one
         call explore(m1, cm, l1)
         if (???) exit
      end do !- m
   end subroutine explore

! removed two procedures since solutions should not be full for some of the more difficult ones....
end program euler_201

