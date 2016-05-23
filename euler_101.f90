program euler_101
   integer, parameter :: int64 = selected_int_kind(10)
   integer(int64) :: un(12), i, T
   
   T = 0
   do i=1,10
   end do


contains
   !> find first bad answer
   integer(int64) function OP(i,n) result(Er)
   end function OP
   
   !> initialize the array
   subroutine fill_un()
     integer(int64) :: n
     
     ! basically is sum((-n)**i,i=0,10)
     do n=1,12
        un(n) = 1 - n*(1 - n*(1 - n*(1 - n*(1 - n*(1 - n*(1 - n*(1 - n*(1 - n*(1 - n)))))))))
     end do
   end subroutine fill_un

end program euler_101
