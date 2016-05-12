program euler_185
   use EulerCommon
   integer, parameter :: kmax = 100
   
   character(len=16) :: best_key
   integer(int64) :: best_val, k, i, j
   real :: x
   
   type dictionary
      character(len=16) :: key
      integer(int64) :: val
   end type
   type(dictionary), dimension(22) :: guesses
   type(dictionary), dimension(kmax) :: parents
   type(dictionary), dimension(kmax) :: children
   type(dictionary) :: child, best

   call init_random_seed()
   
   call fill_guesses(guesses)
   
   call fill_parents()
   
   best_key = parents(1)%key
   best_val = parents(1)%val
   best = parents(1)
   
   ! infinite loop....
   do
      ! print if we're the best!
      if (parents(1)%val < best_val) then
         best_key = parents(1)%key
         best_val = parents(1)%val
         print *, best_val,", ", best_key
      end if
      
      ! create new children by modifying them
      children(1) = parents(1)
      k = 2
      do
         ! cross-over
         call random_number(x);   i = int(15.0*x) + 1
         call random_number(x);   j = int(15.0*x) + 1
         child%key = crossover(parents(i)%key, parents(j)%key)
         child%val = fitness(child%key)
         children(k) = child
         k = k + 1
         if (k >= kmax) exit
!~          print *,parents(i)%key, " -> ", child%key, ' (crs)'
         
         ! mutate
         call random_number(x)
         i = int(15.0*x) + 1
         child%key = mutate(parents(i)%key)
         child%val = fitness(child%key)
         children(k) = child
         k = k + 1
         if (k >= kmax) exit
!~          print *,parents(i)%key, " -> ", child%key, ' (mut)'
         
         ! invert
         call random_number(x)
         i = int(15.0*x) + 1
         child%key = invert(parents(i)%key)
         child%val = fitness(child%key)
         children(k) = child
         k = k + 1
         if (k >= kmax) exit
!~          print *,parents(i)%key, " -> ", child%key, ' (inv)'
         
         ! exchange
         call random_number(x)
         i = int(15.0*x) + 1
         child%key = exchange(parents(i)%key)
         child%val = fitness(child%key)
         children(k) = child
         k = k + 1
         if (k >= kmax) exit
!~          print *,parents(i)%key, " -> ", child%key, ' (prm)'

!~          stop
      end do
      
      ! find the best solution (minimum val)
      call get_best_child()
      
      ! check if we have a valid solution
      if (children(1)%val == 0) then
         print *, children(1)%key
         exit
      end if
      
      ! copy the children to the parent
      do k=1,kmax
         parents(k) = children(k)
!~          print *,parents(k)%val, parents(k)%key
      end do
!~       stop
   end do

contains

   subroutine fill_parents()
      integer(int64) :: i
      
      do i=1,kmax
         parents(i)%key = '4640135623535321'
         parents(i)%val = 22
      end do
   end subroutine fill_parents

   subroutine fill_guesses(hash)
      type(dictionary), intent(out) :: hash(22)
      character(len=16), dimension(22) :: key = ['5616185650518293', '3847439647293047', '5855462940810587', '9742855507068353', &
                                                 '4296849643607543', '3174248439465858', '4513559094146117', '7890971548908067', &
                                                 '8157356344118483', '2615250744386899', '8690095851526254', '6375711915077050', &
                                                 '6913859173121360', '6442889055042768', '2321386104303845', '2326509471271448', &
                                                 '5251583379644322', '1748270476758276', '4895722652190306', '3041631117224635', &
                                                 '1841236454324589', '2659862637316867' ]
      integer(int64), dimension(22) :: val = [2,1,3,3,3,1,2,3,1,2,3,1,1,2,0,2,2,3,1,3,3,2]
      integer(int64) :: i

      do i=1,22
         hash(i)%key   = key(i)
         hash(i)%val = val(i)
      end do
      
   end subroutine fill_guesses

   !> swap the smallest and first cells
   subroutine get_best_child()
      integer(int64) :: i
      type(dictionary) :: child

      child = children(1)
      j = 1
      do i=2,kmax
         if (children(i)%val < child%val) then
            child = children(i)
         end if
      end do !- i
      children(1) = child
   end subroutine get_best_child

   !> shell sort
   subroutine sort_children()
      integer(int64) :: i, j, inc
      type(dictionary) :: child

      inc = kmax / 2
      do while (inc > 0)
         do i=inc+1,kmax
            j = i
            child = children(i)
            do while (j >= inc+1 .and. children(j-inc)%val > child%val)
               children(j) = children(j-inc)
               j = j - inc
            end do !- while
            children(j) = child

            if (inc == 2) then
               inc = 1
            else
               inc = inc * 5 / 11
            end if
         end do !- i
      end do !- while
   end subroutine sort_children

   !> merge two strings
   function crossover(a, b) result(c)
      character(len=16), intent(in) :: a,b
      character(len=16) :: c
      integer(int64) :: cut, i
      real :: x
      
      call random_number(x)
      cut = int(15.0*x) + 1
      c = a
      do i=cut,len(a)
         c(i:i) = b(i:i)
      end do !- i
   end function crossover

   !> exchange two numbers
   function exchange(a) result(b)
      character(len=16), intent(in) :: a
      character(len=16) :: b
      character(len=1) :: s
      integer(int64) :: c1, c2, i
      real :: x
      
      call random_number(x)
      c1 = int(15.0*x) + 1
      call random_number(x)
      c2 = int(15.0*x) + 1
      
      b = a
      s = a(c1:c1)
      b(c1:c1) = b(c2:c2)
      b(c2:c2) = s(1:1)
   end function exchange

   !> change one value in the string
   function mutate(a) result(b)
      character(len=16), intent(in) :: a
      character(len=16) :: b
      character(len=1) :: s
      integer(int64) :: idx, m
      real :: x
      
      ! the index is (1:!6)
      call random_number(x)
      idx = int(15.0*x) + 1
      
      ! the mutation m is any number from 0 to 9
      call random_number(x)
      write(s,'(i1)') int(9.0*x)
      if (s(1:1) == b(idx:idx)) then
         call random_number(x)
         write(s,'(i1)') int(9.0*x)
      end if
      
      ! copy a to b, then replace b(idx) with m
      b = a
      b(idx:idx) = s(1:1)
   end function mutate

   !> invert a segment of a string
   function invert(a) result(b)
      character(len=16), intent(in) :: a
      character(len=16) :: b, G
      character(len=1) :: s
      integer(int64) :: i1,i2,j,l
      real :: x
      
      ! set the left index
      call random_number(x)
      i1 = int(15.0*x) + 1
      
      ! set the right index
      call random_number(X)
      i2 = int(15.0*x) + 1
      
      ! ensure i2 > i1 & bounds
      if (i2 < i1) then
         j = i1
         i1 = i2
         i2 = j
      end if
      if (i2 - i1 == 1) then
         if (i2 < len(a)) then
            i2 = i2 + 1
         else
            i1 = i1 - 1
         end if
      end if
      
      ! loop through array and swap strings
      b = a
      l = len(a)+1
      do j=i1,i2
         s = b(j:j)
         b(j:j) = b(l-j:l-j)
         b(l-j:l-j) = s(1:1)
      end do !- j
   end function invert

   !> determine the fitness of the function
   function fitness(str) result(F)
      character(len=16), intent(in) :: str
      integer(int64) :: F
      character(len=16) :: test
      integer(int64) :: i,j, fitLevel, correct
      
      F = 0
      do i=1,22
         correct = 0
         test = guesses(i)%key
         do j=1,len(str)
            if (test(j:j) == str(j:j)) correct = correct + 1
         end do !- j
         F = F + abs(correct - guesses(i)%val)
      end do !- i
      
   end function fitness
end program euler_185
