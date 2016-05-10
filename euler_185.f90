program euler_185
   integer, parameter :: int64 = selected_int_kind(10)
   integer, parameter :: kmax = 100
   
   character(len=16) :: best_key
   integer(int64) :: best_val, k, i
   real :: x
   
   type dictionary
      character(len=16) :: key
      integer(int64) :: val
   end type
   type(dictionary), dimension(22) :: guesses
   type(dictionary), dimension(kmax) :: parents
   type(dictionary), dimension(0:kmax) :: children
   type(dictionary) :: child
   
   call fill_guesses(guesses)
   
   call fill_parents()
   
   best_key = parents(1)%key
   best_val = parents(1)%val
   
   ! infinite loop....
   do
      ! print if we're the best!
      if (parents(1)%val < best_val) then
         best_key = parents(1)%key
         best_val = parents(1)%val
         print *, best_val,", ", best_key
      end if
      
      ! save the first parent (which should be the best)
      children(0) = parents(1)
      
      ! create new children by modifying them
      k = 1
      do while k <= kmax
         ! cross-over
         call random_number(x)
         i = int(15.0*x) + 1
         child(k)%key = crossover(parents(i)%key, parents(i)%key)
         child(k)%val = fitness(child(k)%key)
         k = k + 1
         
         ! mutate
         call random_number(x)
         i = int(15.0*x) + 1
         child(k)%key = mutate(parents(i)%key)
         child(k)%val = fitness(child(k)%key)
         k = k + 1
         
         ! invert
         call random_number(x)
         i = int(15.0*x) + 1
         child(k)%key = invert(parents(i)%key)
         child(k)%val = fitness(child(k)%key)
         k = k + 1
         
         ! permute
         call random_number(x)
         i = int(15.0*x) + 1
         child(k)%key = permute(parents(i)%key)
         child(k)%val = fitness(child(k)%key)
         k = k + 1
      end do
      
      ! sort the children by fitness
      call sort_children()
      
      ! check if we have a valid solution
      if (children(0)%val == 0) then
         print *, children(0)%val
         exit
      end if
      
      ! copy the children to the parent
      do k=1,kmax
         parents(k)%val = children(k-1)%val
         parents(k)%key = children(k-1)%key
      end do
   end do

contains

   subroutine fill_parents()
      integer(int64) :: i
      
      do i=1,kmax
         parents(i)%key = '1122334455667789'
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
         hash(i)%key   = key(1)
         hash(i)%val = val(1)
      end do
      
   end subroutine fill_guesses
   
   subroutine sort_children()
      integer(int64) :: i, j, K
      character(len=16) :: V
      
      ! at 100 elements, simple swap is fine
      do i=0,kmax-1
         K = children(i)%key
         V = children(i)%val
         do j=i+1,kmax
            
         end do !- j
      end do
   end subroutine sort_children
   
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
   
   function permute(a) result(b)
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
   end function permute
   
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
      m = int(9.0*x)
      write(s,'(i1)') m
      
      ! copy a to b, then replace b(idx) with m
      b = a
      b(idx:idx) = s(1:1)
   end function mutate
   
   function invert(a) result(b)
      character(len=16), intent(in) :: a
      character(len=16) :: b
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
         b(j:j) = b(l-i:l-i)
         b(l-i:l-i) = s(1:1)
      end do !- j
   end function invert
   
   function fitness(str) result(F)
      character(len=16), intent(in) :: str
      integer(int64) :: F, fitLevel, correct
      character(len=16) :: test
      integer(int64) :: i,j
      
      fitLevel = 0
      do i=1,size(guesses)
         correct = 0
         test = guesses(i)%key
         do j=1,len(str)
            if (test(j:j) == str(j:j)) correct = correct + 1
         end do !- j
         if (correct == guesses(i)%val) fitLevel = fitLevel + 1
      end do !- i
      
      F = size(guesses) - fitLevel
   end function fitness
end program euler_185




