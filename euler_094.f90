program euler_094
   integer, parameter :: int64 = selected_int_kind(10)
   integer(int64) :: L = 10**9
   integer(int64) :: a, c, s, p, m
   integer(int64) :: new(4)

   a = 1
   c = 1
   s = 0
   p = 0
   m = 1
   ! use generating function n(i) = 4*n(i-1) - n(i-2); cf. wiki on Heronian Triangles
   do while (p < L)
      ! if the next line doesn't work, get a better compiler
      new = [c, 4*c - a + 2*m, -m, s + p]
      a = new(1); c = new(2); m = new(3); s = new(4)
      p = 3*c - m
   end do
   
   print *,s
   
end program euler_094ents here
