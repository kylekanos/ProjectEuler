program euler_052
   integer, parameter :: int64 = selected_int_kind(10)
   integer(int64) :: i
   
   i = 100000
   do 
      if (all_digits(i)) then
         print *,i
         stop
      end if
      i = i + 1
   end do !- i
   
contains
   logical function all_digits(n) result (bool)
      integer(int64), intent(in) :: n
      character(len=6) :: s1,s2
      integer(int64) :: i,j
      
      write(s1,'(i6)') n
      call sort(s1)
      
      do i=1,6
         write(s2,'(i6)') i*n
         call sort(s2)
         
         do j=1,len(s1)
            if (s1(j:j) /= s2(j:j)) then
               bool = .false.
               return
            end if
         end do !- j
      end do
      
      bool = .true.
      return
   end function all_digits
   
   pure subroutine sort(s)
      character(len=*), intent(inout) :: s
      character(len=1) :: t
      integer :: i, j
      
      do i=2,len(s)
         j = i-1
         t(1:1) = s(i:i)
         do while ( j>= 1 .and. s(j:j)>t(1:1))
            s(j+1:j+1) = s(j:j)
            j = j - 1
         end do !- j
         s(j+1:j+1) = t(1:1)
      end do !- i
   end subroutine sort
   
end program euler_052
