program euler_169
    integer, parameter :: int64 = selected_int_kind(15)
    integer, dimension(26) :: twos=0  ! use an array of length 26 to represent 10**25
    integer(int64) :: a, b, i=0
    twos(1)  = 1
    twos(26) = 1  ! this is needed due to algorithm
    
    a = 1
    b = 0
    i = 0
    do
       if (mod(twos(26),2)==1) then
          b = a + b
       else
          a = a + b
       endif
       call divide_by_two(twos)
       i = i + 1
       print '(3(i0,2x),26(i0))',i,a,b,twos
       if (sum(twos)==0) exit
    end do 
    print '("The hyperbinary representation of 10**25 = ",i0)',b

contains
   subroutine divide_by_two(twos)
      integer, dimension(26), intent(inout) :: twos
      integer :: i, tmp(26)
      tmp = 0
      ! do first round of integer division
      do i=1,26
         tmp(i) = twos(i)/2
      end do !- i
      ! add "0.5" term to odd terms
      do i=2,26
         if (mod(twos(i-1),2)==1) then
            tmp(i) = tmp(i) + 5
         end if
      end do ! -i
      twos(:) = tmp(:)
   end subroutine divide_by_two
end program euler_169
