
program main
   implicit none
   use iso_c_binding
   
   integer(c_long_long) :: min_prime=1000003,max_prime=9999999967
   character(1), dimension(max_prime) :: prime_list
   integer(c_int), dimension(10) :: digits = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
   integer(c_long_long) :: i, 
   
   ! create the prime number list (remember, they're 1 byte chars for memory purposes)
   prime_list = prime_list_gen(min_prime, max_prime)
   
   ! start near end of list and move down by 2's since we know even numbers aren't prime
   do i = max_prime-2, min_prime, -2
      if (prime_list(i) == 'T') then
         if (check_digits(i)) then
            print i
            exit
         endif
      end if
   end do
   
   stop
   
contains
   !> return an array of 1 bit chars that specify true/false
   function prime_list_gen(a, b) result(list)
      integer(c_long_long), intent(in) :: a, b
      character(1), dimension(b) :: list
      
      ! wheel of 7
      list = 'T'
      list(4:b:2) = 'F'
      list(9:b:3) = 'F'
      list(25:b:5) = 'F'
      list(49:b:7) = 'F'
      
      ! loop through all from 11 to max primes
      do i=11,b
         if (list(i)=='T') then
            list(i*i:b:2*i) = 'F'
         end if
      end do
   end function prime_list_gen
   
   !> check to see if all digits are in the number
   logical function check_digits(i) result(is_permutation)
      integer(c_long_long), intent(in) :: i
      integer, allocatable :: test(:)
      integer(c_long_long) :: rem
      integer :: ix, num_digits
      logical :: is_permutation
      
      num_digits = floor(log10(real(num))+1)
      allocate(test(num_digits))
      rem = i
      ! this actually puts them in *reverse* order, but since we sort it anyway, that's irrelevant
      do ix=1, num_digits
         test(i) = rem - (rem/10)*10
         rem = rem/10
      end do
      
      ! sort the digits
      call sort(num_digits,test)
      
      ! check that all numbers are used
      do ix=1,num_digits
         if (test(i) /= digits(i)) then
            is_permutation = .false.
            return
         end if
      end do
      is_permutation = .true.
   end function check_digits
   
   !> sort the table
   recursive subroutine sort(X)
      integer, intent(inout) :: X(:)
      integer :: iq
      
      if (size(X)>1) then
         i = partition(X)
         call sort(X(:i-1))
         call sort(X(i:))
      endif
   end subroutine sort
   
   integer function partition(A) result(marker)
      integer, intent(inout) :: A(:)
      integer :: marker
      integer :: i, j, temp, x
      
      marker=0
      
      x = A(1)
      i = 0
      j = size(A)+1
      
      do
         j = j-1
         do
            if (A(j) <= x) exit
            j = j - 1
         end do
         i = i+1
         do
            if (A(i) >= x) exit
            i = i+1
         end do
         
         if (i < j) then
            temp = A(i)
            A(i) = A(j)
            A(j) = temp
         else if (i==j) then
            marker = i+1
            return
         else
            marker = i
            return
         endif
      end do
   end function partition
end program