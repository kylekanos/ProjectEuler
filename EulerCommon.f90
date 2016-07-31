module EulerCommon
   implicit none
   integer, parameter :: int64 = selected_int_kind(15)
   integer, parameter :: int32 = selected_int_kind(10)
   integer, parameter :: dp = selected_real_kind(15,307)
   real(dp), parameter :: varphi = 0.5_dp + (0.5_dp)*sqrt(5.0_dp)
   
contains
   !> check to see if all digits are in the number
   logical function is_pandigital(n)
      integer(kind=int64), intent(in) :: n
      character(9) :: str, digits
      character(1) :: repin, repout
      integer :: i
      
      write(str,*) n
      digits = '         '
      do i=1,int(log10(real(n))+1)
         write(digits(i:i), *) i
      end do
      repout = ' '

      do i=1,len(trim(digits))
         repin = str(i:i)
         digits = replace_text(digits, repin, repout)
      end do

      is_pandigital = len(trim(digits)) == 0
   end function is_pandigital
   

   !> replaces text in str with rep
   function replace_text(str, text, rep) result(outs)
      character(*) :: str, text, rep
      character(len=len(str)) :: outs
      integer :: i, nt, nr

      outs = str
      nt = len_trim(text)
      nr = len_trim(rep)
      do
         i = index(outs, text(:nt))
         if (i == 0) exit
         outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
      end do
   end function replace_text

   !> returns true if the two input numbers are permutations of each other
   logical function is_permutation(a,b,n)
      integer(int64), intent(in) :: a, b, n
      character(len=n) :: t1, t2
      character(len=1) :: repin, repout
      integer(int64) :: i

      write(t1,'(i4)') a
      write(t2,'(i4)') b
      repout = ' '

      do i=1,n
         repin = t1(i:i)
         t2 = replace_text(t2, repin, repout)
      end do !- i

      is_permutation = len(trim(t2)) == 0
   end function is_permutation

   !> quick sort function
   recursive subroutine sort(X)
      integer, intent(inout) :: X(:)
      integer :: iq
      
      if (size(X)>1) then
         iq = partition(X)
         call sort(X(:iq-1))
         call sort(X(iq:))
      endif
   end subroutine sort

   !> helper for sorting
   integer function partition(A) result(marker)
      integer, intent(inout) :: A(:)
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

   !> primality test using 6 complement
   logical function is_prime6(n) result(res)
     integer(int64), intent(in) :: n
     integer(int64) :: x, i
     
     if (n < 2) then
        res = .false.
        return
     end if
     
     if (any(n==[2,3])) then
        res = .true.
        return
     end if
     
     if (mod(n,2)==0 .or. mod(n,3)==0) then
        res = .false.
        return
     end if
     
     ! test by 6k+/-1 complement
     x = int(sqrt(real(n)))+1
     do i=6,x,6
        if (mod(n,i-1)==0 .or. mod(n,i+1)==0) then
           res = .false.
           return
        end if
     end do

     res = .true.
     return
   end function is_prime6

   !> more efficient is_prime function, adapated from http://stackoverflow.com/a/5694432/1276356
   logical function is_prime(x) result(bool)
      integer(int64), intent(in) :: x
      integer(int64) ::N, i, p, q
      N = 10
      ! test small primes for a match
      if (any(x==small_primes)) then
         bool = .true.
         return
      end if

      ! test small primes for divisbility
      do i=1,N
         p = small_primes(i)
         q = x/p
         if (q < p) then
            bool = .true.
            return
         end if
         if (x == q*p) then
            bool = .false.
            return
         end if
      end do !- i

      ! test other primes
      i = 31_int64
      do
         q = x/i
         if (q < i) then
            bool = .true.
            return
         end if
         if (x == q*i) then
            bool = .false.
            return
         end if
         i = i + 6

         q = x/i
         if (q < i) then
            bool = .true.
            return
         end if
         if (x == q*i) then
            bool = .false.
            return
         end if
         i = i + 4

         q = x/i
         if (q < i) then
            bool = .true.
            return
         end if
         if (x == q*i) then
            bool = .false.
            return
         end if
         i = i + 2

         q = x/i
         if (q < i) then
            bool = .true.
            return
         end if
         if (x == q*i) then
            bool = .false.
            return
         end if
         i = i + 4

         q = x/i
         if (q < i) then
            bool = .true.
            return
         end if
         if (x == q*i) then
            bool = .false.
            return
         end if
         i = i + 2

         q = x/i
         if (q < i) then
            bool = .true.
            return
         end if
         if (x == q*i) then
            bool = .false.
            return
         end if
         i = i + 4

         q = x/i
         if (q < i) then
            bool = .true.
            return
         end if
         if (x == q*i) then
            bool = .false.
            return
         end if
         i = i + 6

         q = x/i
         if (q < i) then
            bool = .true.
            return
         end if
         if (x == q*i) then
            bool = .false.
            return
         end if
         i = i + 2
      end do !-
      bool = .true.
      return
   end function is_prime

   !> returns the next primes above n
   integer(int64) function next_prime(n) result(p)
      integer(int64), intent(in) :: n
      integer(int64) :: st
      ! take care of the only even prime
      if (n==2) then
         p = 3
         return
      end if

      ! starting number should otherwise be odd
      st = merge(n+1, n+2, mod(n,2)==0)

      ! loop through and test primes, using Bertrand's postulate
      do p=st,2*n,2
         if (is_prime(p)) return
      end do !- p
   end function next_prime

   !> generate a logical array indicating if index i is prime or composite
   subroutine prime_sieve(n, p)
      integer(int64), intent(in) :: n
      logical, dimension(n), intent(out) :: p
      integer(int64) :: i, q(5)=[2,3,5,7,11], r

      p(:) = .true.
      p(1) = .false.
      ! start sieve of 11
      do i=1,5
         r=q(i)
         p(r*r:n:r) = .false.
      end do

      r = int(sqrt(real(n)))
      do i=13,r,2
         if (p(i)) then
            p(2*i:n:i) = .false.
         end if
      end do
   end subroutine prime_sieve

  !> find the factors of a number
   subroutine find_factors(n, d)
      integer(int64), intent(in) :: n
      integer(int64), dimension(:), intent(out) :: d
      integer(int64) :: div, next, rest, i

      div  = 2
      next = 3
      rest = n
      i    = 1

      d(:) = 0

      do while (rest /= 1)
         do while (mod(rest, div) == 0)
            d(i) = div
            i = i + 1
            rest = rest / div
         end do
         div = next
         next = next + 2
      end do
   end subroutine find_factors

   !> factorize a number n
   function factor(n) result(factors)
      integer(int64), intent(in) :: n
      integer(int64), dimension(200) :: factors
      integer(int64) :: m, p, e, k

      factors = 0
      if (any(n==[-1,0,1])) return

      m = merge(-n, n, n<0)

      k = 1

      do while (m /= 1)
         p = trial_division(m)
         e = 1
         m = m/p
         do while (mod(n,p) == 0)
            e = e + 1
            m = m/p
         end do
         factors(k) = p
         k = k + 1
      end do
   end function factor

   !> returns the largest divisor of n
   integer(int64) function trial_division(n) result(F)
      integer(int64), intent(in) :: n
      integer(int64), parameter :: dif(8)=[6,4,2,4,2,4,6,2], p(3)=[2,3,5]
      integer(int64) :: i, m
      
      if (n == 1) then
         F = 1_int64
         return
      end if

      do i=1,3
         if (mod(n,p(i))==0) then
            F = p(i)
            return
         end if
      end do

      m = 7; i = 1
      do while (m < n .and. m*m <= n)
         if (mod(n,m) == 0) then
            F = m
            return
         end if
         m = m + dif(mod(i,8))
         i = i + 1
      end do

      F = n
      return
   end function

   !> returns true if there are not any duplicated entries in the array, false otherwise
   logical function no_dupes(A) result(bool)
      integer(int64), intent(in) :: A(:)
      integer(int64) :: i, k

      do i=1,size(A)
         k = A(i)
         if (any(k == A(i+1:))) then
            bool = .false.
            return
         end if
      end do
      bool = .true.
      return
   end function no_dupes

   !> works for first 70-ish Fibonacci numbers; uses real-precision based computation of analytic solution
   integer(int64) function fibonacci_approx(n) result(F)
      integer(int64), intent(in) :: n
      real(dp), parameter :: sq5_inv = 1.0_dp / sqrt(5.0_dp)
      F = int(sq5_inv * varphi**n + 0.5_dp,int64)
   end function fibonacci_approx

   !> returns the nth fibonacci number by choosing a method (picks fastest by default)
   integer(int64) function fibonacci(n,methOpt) result(F)
      integer(int64), intent(in) :: n
      integer(int64), optional :: methOpt
      integer(int64) :: m

      m = 1_int64
      if (present(methOpt)) m = methOpt

      f = 0_int64

      select case(m)
        case(1)
           F = fibonacci_doubling(n)
        case(2)
           F = fibonacci_matrix(n)
        case(3)
           F = fibonacci_approx(n)
      end select
   end function fibonacci

   !> returns the nth fibonacci number using matrix exponentiation (fast!)
   integer(int64) function fibonacci_matrix(n) result(F)
      integer(int64), intent(in) :: n
      integer(int64), dimension(2,2) :: Q, S
      integer(int64) :: i
      Q = reshape([0, 1, 1, 1], [2,2])
      S = Q
      do i=1,n-1
!~          Q(1,1) = Q(1,1)*S(1,1) + Q(1,2)*S(2,1); Q(1,2) = Q(1,1)*S(1,2) + Q(1,2)*S(2,2)
!~          Q(2,1) = Q(2,1)*S(1,1) + Q(2,2)*S(2,1); Q(2,2) = Q(2,1)*S(1,2) + Q(2,2)*S(2,2)
!~           ! curiously, the above is giving incorrect answers
         Q = matmul(Q,S)
      end do
      F = Q(2,1)
   end function fibonacci_matrix

   !> returns the nth fibonacci number using the fast-doubling method (fastest!)
   integer(int64) function fibonacci_doubling(n) result(F)
      integer(int64), intent(in) :: n
      integer(int64) :: i, a, b, c, d, t
      i = n - 1_int64
      a = 1_int64
      b = 0_int64
      c = 0_int64
      d = 1_int64

      ! test small numbers
      if (n <= 0_int64) then
         F = 0
         return
      end if

      ! iterate through
      do while (i > 0_int64)
         ! need odd numbers
         do while (mod(i,2_int64)==0_int64)
            t = d*(2_int64*c + d)
            c = c*c + d*d
            d = t
            i = i/2_int64
         end do
         t = d*(b + a) + c*b
         a = d*b + c*a
         b = t
         i = i - 1_int64
      end do
      F = a+b
   end function fibonacci_doubling

   !> returns the triangle sequence
   subroutine set_triangle(n, T)
      integer(int64), intent(in) :: n
      integer(int64), dimension(n) :: T
      integer(int64) :: i

      do i=1,n
         T(i) = triangular(i)
      end do
   end subroutine set_triangle

   !> returns the triangular number of n
   integer(int64) function triangular(n) result(s)
      integer(int64), intent(in) :: n
      s = n*(n+1_int64) / 2_int64
   end function triangular

   !> returns the sum of the characters in the string
   integer(int64) function char_sum(str_in) result(T)
      character(len=*), intent(in) :: str_in
      character(len=len(str_in)) :: str
      integer(int64) :: i

      ! ensure the string is uppercase before running char_map
      str = to_upper(str_in)

      T = 0
      do i=1,len(str)
         T = T + char_map(str(i:i))
      end do
   end function char_sum

   !> returns the letter number based on it's position in the alphabet
   integer(int64) function char_map(char) result(T)
      character(len=1), intent(in) :: char
      character(len=26) :: A = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

      T = index(A, char)
   end function char_map

   !> returns the sum of the ascii values of the string
   integer(int64) function ascii_sum(str) result(T)
      character(len=*), intent(in) :: str
      integer(int64) :: i

      T = 0
      do i=1,len(str)
         T = T + iachar(str(i:i))
      end do
   end function ascii_sum

   !> convert string to upper case
   function to_upper(strIn) result(strOut)
      character(len=*), intent(in) :: strIn
      character(len=len(strIn)) :: strOut
      integer(int64) :: i,j

      do i = 1, len(strIn)
         j = iachar(strIn(i:i))
         if (j>= iachar("a") .and. j<=iachar("z") ) then
            strOut(i:i) = achar(iachar(strIn(i:i))-32)
         else
            strOut(i:i) = strIn(i:i)
         end if
      end do
   end function to_upper

   !> find the permutation index
   integer(int64) function kperm(n,m)
      integer(int64), intent(in):: n
      integer(int64), intent(inout), dimension(N):: m
      integer(int64) :: i, max, r, s

      r = N
      kperm = 0

      do while (r > 1_int64)
         s = 1_int64
         max = m(1)
         do i=1,r
            if (m(i) > max) then
               s = i
               max = m(i)
            end if
         end do
         kperm = kperm*r + s - 1_int64
         m(s) = m(r)
         m(r) = max
         r = r - 1_int64
      end do
      return
   end function kperm

   !> find the next permutation depending on the size of M
   subroutine nextPermutation(M)
      integer(int64), dimension(:), intent(inout) :: M
      integer(int64) :: K, J, CJ, Mm, n

      n = size(M,dim=1)

      k = kperm(n,m)

      do j = 2, n
         cj = mod(k, j)
         k = (k - cj) / j
         mm = m(j)
         m(j) = m(cj+1)
         m(cj+1) = mm
      end do
   end subroutine nextPermutation

   !> returns the factorial using either simple loop or log-gamma
   integer(int64) function factorial(n) result(F)
      integer(int64), intent(in) :: n
      integer(int64) :: i

      if (n < 30_int64) then
        F = 1_int64
        do i=1,n
          F = F * i
        end do
      else
         F = nint(exp(gammaln(n+1._dp)))
      end if
   end function factorial

   !> the log-gamma function (in case lgamma(x) isn't available w/ compiler)
   real(dp) function gammaln(n) result(lg)
      real(dp), intent(in) :: n
      real(dp) :: ser, tmp, x, y
      real(dp), parameter :: cof(6) = [76.18009172947146_dp, -86.5053203941677_dp, &
        24.01409824083091_dp, -1.231739572450155_dp, 0.1208650973866179e-2_dp, &
        -0.5395239384953e-5_dp]
      real(dp), parameter :: stp = 2.5066282746310005_dp
      integer :: j

      x = n*n
      y = x
      tmp = x + 5.5_dp
      tmp = (x + 0.5_dp)*log(tmp) - tmp
      ser = 1.000000000190015_dp
      do j=1,6
         y = y + 1.0_dp
         ser = ser + cof(j) / y
      end do !- j
      lg = tmp + log(stp*ser / x)
   end function gammaln

   !> turns an array into a single number--assumes that size(n) <= 19 digits
   integer(int64) function arrayAsNumber(n) result(A)
      integer(int64), intent(in) :: n(:)
      integer(int64) :: i
      A = 0
      do i=size(n),1,-1
         A = A*10_int64 + n(i)
      end do
   end function arrayAsNumber

   !> ensures the array doesn't have tens by shifting them upwards
   !>  useful for simulating big integers with linear arrays
   subroutine checkTensInArray(n,test)
      integer(int64), intent(in) :: n
      integer(int64), intent(inout) :: test(:)
      integer(int64) :: i,k,m

      do i=1,n
         if (test(i) > 9) then
            m = test(i)
            k = int(test(i)*0.1)
            test(i+1) = test(i+1) + k
            test(i) = m - 10_int64*k
         end if
      end do

      test(n:) = 0
   end subroutine checkTensInArray

   !> returns the permutation of M
   subroutine PERMNX(M,D,N,RT)
      integer(int64), intent(in) :: n
      integer(int64), intent(inout) :: RT
      integer(int64), intent(inout), dimension(n) :: m, d
      integer(int64) :: DK,T,K,J,KL
!                          BRANCH TO 2 IF WE ARE IN THE MIDDLE OF THINGS
      if (rt == 0) then
         d(1:n) = 0_int64
      end if

      RT = 0_int64
      j = -1_int64
      KL = 1_int64
      do k=2,n
         dk = d(k)
         if (dk /= kl) then
            rt = 1_int64
            exit
         end if
         d(k) = 0_int64
         j = -j
         kl = k
      end do

      if (rt == 0_int64) return

      d(k) = dk + 1_int64
      dk = d(k)
      if (j /= 1_int64 .and. dk > 2_int64) kl = k - d(k)

      T = m(k)
      m(k) = m(kl)
      m(kl) = t                                                    
   end subroutine PERMNX


   !> return the next permutation
   function next_perm_str(str) result(outs)
      character(len=*), intent(in) :: str
      character(len=len(str)) :: outs
      character(len=1) :: swap
      integer(int64) :: i, j

      outs = str

      ! step 1: find highest index such that str(i) < str(i+1)
      i = len(outs) - 1_int64
      do while (i /= 1_int64)
         if (iachar(outs(i:i)) > iachar(outs(i+1:i+1))) exit
         i = i - 1_int64
      end do

      ! step 2: find j>1 such that str(j) > str(i)
      j = len(outs)
      do while (j /= i)
         if (iachar(outs(j:j)) > iachar(outs(i:i))) exit
      end do

      ! step 3: swap str(i) and str(j)
      swap = outs(i:i)
      outs(i:i) = outs(j:j)
      outs(j:j) = swap

      ! step 4: reverse str(i+1) to end
      j = len(outs)
      do while (i /= len(outs))
         outs(i:i) = str(j:j)
         i = i + 1_int64
         j = j - 1_int64
      end do
      
   end function next_perm_str

   !> count the number of repetitions of the subarray in the array
   integer(int64) function count_repetitions(sub, array) result(num)
      integer(int64), intent(in) :: sub(:), array(:)
      integer(int64) :: ns, na, i
      ns = size(sub); na = size(array)
      num = 0_int64

      do i=1,na
         if (all(sub(:) == array(i:i+ns-1))) num = num + 1_int64
      end do
   end function count_repetitions

   !> count substrings, from https://www.rosettacode.org/wiki/Count_occurrences_of_a_substring#Fortran
   integer(int64) function count_substr(str, sub) result(ctr)
      character(*), intent(in) :: str, sub
      integer(int64) :: ns, na, i
      ns = len(str)
      na = len(sub)
      ctr = 0_int64

      do i=1,ns
         if (sub == str(i:i+na-1)) ctr = ctr + 1_int64
      end do
   end function count_substr

   !> return the number of primes in the string given replacement
   integer(int64) function count_primes(str,d,p_list) result(ctr)
      character(len=*), intent(in) :: str, d
      logical, dimension(:), intent(in) :: p_list
      character(len=10) :: digits='0123456789'
      character(len=len(str)) :: new_str
      integer(int64) :: nd,i,val,nlist,j,ns

      nd = len(d)
      if (nd /= 1_int64) then
         ctr = -1_int64
         return
      end if

      nlist = size(p_list)
      ns = len(str)

      ctr = 0_int64
      do i=1,10
         do j=1,ns
            new_str(j:j) = merge(digits(i:i),str(j:j),str(j:j)==d(1:1))
         end do
         read(new_str,*) val
         if (val < 100000_int64) continue
         if (val > nlist) continue
         if (p_list(val)) ctr = ctr + 1_int64
      end do
   end function count_primes

   !> returns the binomial coefficient
   integer(int64) function binomial(n, k) result(nk)
      integer(int64), intent(in) :: n,k
      nk = factorial(n) / (factorial(k) * factorial(n - k))
   end function binomial

   !> uses log-gamma to compute the binomial coefficient
   integer(int64) function choice(n, k) result(nk)
      integer(int64), intent(in) :: n,k
      nk = nint(exp(factl(n) - factl(k) - factl(n-k)))
   end function choice
   
   !> uses iterative approach to binomial function
   integer(int64) function choose(n, k) result(C)
      integer(int64), intent(in) :: n, k
      integer(int64) :: d
      
      if (k > n) then
         C = -1_int64
      else if (k == 1_int64) then
         C = 1_int64
      else
         C = 1
         do d=1,k
            C = (C*n)/d  ! ensure n/k isn't done first, as that can give 0
            n = n - 1
         end do !- d
      end if
   end function choose

   !> returns the factorial using the Gamma function
   real(dp) function factl(n) result(F)
      integer(int64), intent(in) :: n
      real(dp) :: x
      x = real(n,dp)
      F = exp(gammaln(x+1_dp))
   end function factl

   !> initialize the random number generator based on clock time
   subroutine init_random_seed()
      integer(int32), allocatable :: seed(:)
      integer(int32) :: i, un, istat, dt(8), pid
      integer(int32) :: t
      integer :: n
      
      call random_seed(size = n)
      allocate(seed(n))
      ! First try if the OS provides a random number generator
      open(newunit=un, file="/dev/urandom", access="stream", &
           form="unformatted", action="read", status="old", iostat=istat)
      if (istat == 0) then
         read(un) seed
         close(un)
      else
         ! Fallback to XOR:ing the current time and pid. The PID is
         ! useful in case one launches multiple instances of the same
         ! program in parallel.
         call system_clock(t)
         if (t == 0) then
            call date_and_time(values=dt)
            t = (dt(1) - 1970) * 365 * 24 * 60 * 60 * 1000 &
                 + dt(2) * 31 * 24 * 60 * 60 * 1000 &
                 + dt(3) * 24 * 60 * 60 * 1000 &
                 + dt(5) * 60 * 60 * 1000 &
                 + dt(6) * 60 * 1000 + dt(7) * 1000 &
                 + dt(8)
         end if
         pid = getpid()
         t = ieor(t, int(pid, kind(t)))
         do i = 1, n
            seed(i) = lcg(t)
         end do
      end if
      call random_seed(put=int(seed))
   contains
    ! This simple PRNG might not be good enough for real work, but is
    ! sufficient for seeding a better PRNG.
      integer(int32) function lcg(s)
        integer(int32) :: s
        if (s == 0) then
           s = 104729_int32
        else
           s = mod(s, 4294967296_int32)
        end if
        s = mod(s * 279470273_int32, 4294967291_int32)
        lcg = int(mod(s, int(huge(0))))
      end function lcg
   end subroutine init_random_seed

   !> swap two variables values
   pure subroutine swap(a,b)
      integer(int64), intent(inout) :: a,b
      integer(int64) :: T
      t = a
      a = b
      b = t
   end subroutine swap

   !> implements iterative greatest common denominator
   recursive function gcd(a, b) result(g)
      integer(int64), intent(in) :: a, b
      integer(int64), parameter :: zero=0, one=1, two=2
      integer(int64) :: u, v, t, k
      integer(int64) :: g

      u = abs(a)
      v = abs(b)

      if (u < v) call swap(u,v)

      if (v == zero) then
         g = u
         return
      end if

      k = one
      do while ( (mod(u,two) == zero) .and. (mod(v,two) == zero))
         u = u / two
         v = v / two
         k = k * two
      end do

      t = merge(u, -v, mod(u,two)==zero)

      do while (t /= zero)
         do while (mod(t, two) == zero)
            t = t / two
         end do

         if (t > zero) then
            u = t
         else
            v = -t
         endif
         t = u - v
      end do

      g = u*k
      return
   end function gcd

   !> extended greatest common denominator
   integer(int64) function egcd(a,b) result(g)
      integer(int64), intent(in) :: a,b
      integer(int64) :: lastx,lasty,x,y,u,v,q,t
      u = a; v = b
      lastx=1_int64; lasty=0_int64
      x=0_int64; y=1_int64
      do while (v /= 0_int64)
         q = u/v
         t = v
         v = mod(a,v)
         u = t

         t = x
         x = lastx - q*x
         lastx = t

         t = y
         y = lasty - q*u
         lasty = t
      end do
      g = lastx
   end function egcd

   !> least common multiple of two numbers
   integer(int64) function lcm(a,b)
      integer(int64), intent(in) :: a,b
      lcm = a*b/gcd(a,b)
   end function lcm

   !> returns the length of the Collatz sequence starting at n
   !> taken from https://github.com/aphirst/EulerFTN/blob/master/functions.f90
   recursive function Collatz(n, lengths, memoise) result(coll)
      integer(int64), intent(in) :: n
      integer(int64), intent(inout) :: lengths(:)
      logical, intent(in), optional :: memoise
      integer(int64) :: coll
      
      if (n <= size(lengths)) then
         if (lengths(n) /= 0_int64) then
            coll = lengths(n)
            return
         else if (n == 1_int64) then
            coll = 1_int64
            lengths(1) = 1_int64
            return
         end if
      end if

      if ( modulo(n,2_int64) == 0_int64 ) then
         coll = 1_int64 + Collatz(n/2_int64 , lengths, .true.)
      else
         coll = 1_int64 + Collatz(3_int64*n + 1_int64 , lengths, .true.)
      end if

      if (present(memoise)) then
         if (memoise) then
            if (n <= size(lengths)) lengths(n) = coll
         end if
      end if
  end function Collatz

    !>  Miller-Rabin primality test
   logical function mr_prime(n,kopt) result(bool)
      integer(int64), intent(in) :: n
      integer(int64), optional :: kopt
      integer(int64), parameter :: two=2_int64
      integer(int64) :: k
      integer(int64) :: s, d, i, a, x, r

      
      k = 30_int64
      if (present(kopt)) k = kopt

      ! test the small, known primes
      if (any(n==small_primes)) then
         bool = .true.
         return
      end if

      ! eliminate numbers less than 30 that aren't prime as above
      if (n < 30_int64) then
         bool = .false.
         return
      end if

      ! eliminate factors of the small known primes
      do i=1,size(small_primes,1)
         if (mod(n,small_primes(i))==0_int64) then
            bool = .false.
            return
         end if
      end do

      s = 1_int64
      do while ((mod(n-1_int64, two**s))==0_int64)
         s = s + 1_int64
      end do !- while

      d = (n-1_int64) / two**s

      do i=1,k
         a = randrange(two, n-two)
         x = mod(a**d, n)
         if (x < 1_int64 .or. x > (n -1_int64)) then
            do r=1,s-1
               x = mod(x**(two*r), n)
               if (x == 1_int64) then
                  bool = .false.
                  return
               end if
            end do !-r
         end if
      end do !- i

      bool = .true.
      return
   end function mr_prime

   !> returns a random number in the range (a,b)
   integer(int64) function randrange(a, b) result(x)
      integer(int64), intent(in) :: a, b
      real(dp) :: xx
      call random_number(xx)
      x = floor(a + xx * real(max(a,b) - min(a, b)))
   end function randrange
   
   !> Legendre symbol (a/p); see https://en.wikipedia.org/wiki/Legendre_symbol
   integer(int64) function legendre(a, p) result(L)
      integer(int64), intent(in) :: a, p

      if (.not.is_prime6(p)) then
         print '("error: p=",i0,", which is not prime")',p
         stop
      end if

      ! check simple case of p=2, then mod(a,p)==0
      if (p == 2_int64) then
         L = merge(1_int64, 0_int64, mod(a,2) == 0_int64)
      else if (mod(a,p) == 0_int64) then
         L = 0_int64
      else
         L = powmod(a, (p-1)/2, p)
         if (L /= 1_int64) then
            L = -1_int64
         end if
      end if
   end function legendre

   !> sum the digits (size(a)=20 is larger than what int64 holds)
   integer(int64) function digit_sum(n) result(D)
      integer(int64), intent(in) :: n
      integer(int64) :: m, a(20), b=10_int64, i

      a=0_int64
      m=n
      i=1_int64
      do while (m /= 0_int64)
         a(i) = mod(m,b)
         m = m / b
         i = i + 1_int64
      end do
      D = sum(a)
   end function digit_sum

   !> returns divisor sum square
   function sigma2(pow, n) result(divs)
      integer(int64) :: n, pow
      integer(int64), dimension(0:n) :: divs
      integer(int64) :: i, d, ip

      divs(0:1) = [0_int64,1_int64]
      do i=2,n
         divs(i) = i**pow + 1_int64
      end do !- i
      do i=2,int(n**0.5)+1_int64
         ip = i**pow
         divs(i*i) = divs(i*i) + ip
         do d=i*(i+1),n,i
            divs(d) = divs(d) + ip + (d/i)**pow
         end do !- d
      end do !- i
   end function sigma2

   !> returns true if a & b are within [1,S] and false otherwise
   logical function legal_move(a,b,S)
      integer(int64), intent(in) :: a,b,S
      legal_move = (a > 0_int64) .and. (a < S+1_int64) &
               .and. (b > 0_int64) .and. (b < S+1_int64)
   end function
   
   !> returns the lagged fibonacci generator (for Euler 149)
   subroutine lfg(n,S)
      integer(int64), intent(in) :: n
      integer(int64), intent(inout) :: S(n)
      integer(int64) :: k

      ! fill primer array
      do k=1,55
         S(k) = mod(100003_int64 - k*(200003_int64 - k*k*300007_int64), 1000000_int64) - 500000_int64
      end do !- k

      do k=56,n
         S(k) = mod(S(k-24) + S(k-55) + 1000000_int64, 1000000_int64) - 500000_int64
      end do !- k
   end subroutine lfg
   
   !> returns the nth Catalan number (recall 0-based indexing for such numbers)
   integer(int64) function catalan(n) result(C)
      integer(int64), intent(in) :: n
      integer(int64) :: k, nm, dm
      nm = 1
      dm = 1
      do k=2,n
         nm = nm * (n+k)
         dm = dm * k
      end do !- k
      C = nm/dm
   end function catalan
   
   !> computes the integer division & remainder of two numbers
   function divmod(a,b) result(R)
      integer(int64), intent(in) :: a,b
      integer(int64) :: R(2)
      R(1) = a/b
      R(2) = mod(a,b)
   end function divmod
   
   !> given the string str, returns the nth permutation of it (untested, may not compile)
   recursive subroutine perm2(n, str) result(S)
      integer(int64), intent(in) :: n
      character(len=*), intent(in) :: str
      character(len=len(str)) :: S
      integer(int64) :: q(2)
      if (len(str)==1) return
      q = divmod(n, factorial(len(str)-1_int64))
      S = str(q(1):q(1)) + perm2(q(2), str(:q) // str(q+1:))
   end subroutine perm2
   
   !> returns the digital root of the number (e.g., 635 -> 6+3+5=14 -> 1+4=5)
   integer(int64) function digital_root(n) result(dr)
      integer(int64), intent(in) :: n
      dr = 1_int64 + mod(n-1,9_int64)
   end function digital_root
   
    !> divmod subroutine to get faster division & remainder functions
   subroutine divmod(num,den,remainder,quotient)
      integer(int64), intent(in) :: num, den
      integer(int64), intent(out) :: remainder, quotient
      quotient = int(num / den, int64)
      remainder = num - quotient * den
   end subroutine divmod
   
   !> returns the hyperbinary representation of m
   integer(int64) function hyper_binary(m) result(b)
      integer(int64), intent(in) :: m
      integer(int64) :: a, n
      n = m
      a = 1
      b = 0
      do while (n > 0)
         if (mod(n,2) == 1) then
            b = a + b
         else
            a = a + b
         end if
         n = n/2_int64
      end do
   end function hyper_binary
   
   !> returns mod(a^p, m) but more efficiently (~O(log(p)))
   recursive integer(int64) function powmod(a, p, m) result(R)
      integer(int64), intent(in) :: a, p, m
      integer(int64) :: temp

      ! take care of simple cases first
      if (p == 0_int64) then
         R = 1_int64
      else if (p == 1_int64) then
         R = mod(a, m)
      else
         temp = powmod(a, p/2_int64, m)
         if (mod(p,2_int64) == 0_int64) then
            R = mod(temp*temp, m)
         else
            R = mod(a*mod(temp*temp, m), m)
         end if
      end if
   end function powmod
   
   !> generate boolean array of square-free numbers using a sieve
   subroutine squarefree_sieve(n, sqfp)
      integer(int64), intent(in) :: n
      logical, intent(out) :: sqfp(n)
      integer(int64), dimension(n) :: mu

      ! get the moebius mu to n
      call moebius_mu_sieve(n, mu)

      ! the square-free numbers are the non-zero entries here
      sqfp(1:n) = mu(1:n) /= 0_int64
   end subroutine squarefree_sieve

   !> generates the moebius mu function
   subroutine moebius_mu_sieve(n, mu)
      integer(int64), intent(in) :: n
      integer(int64), dimension(n), intent(out) :: mu
      logical, dimension(n) :: primes
      integer(int64) :: k, p

      ! get the primes <= to n
      call prime_sieve(n, primes)

      ! generate the mu
      mu(:) = 1_int64
      do k=2,n
         if (primes(k)) then
            mu(k:n:k) = -mu(k:n:k)
            p = k*k
            mu(p:n:p) = 0_int64
         end if
      end do !- k
   end subroutine moebius_mu_sieve
   
   integer(int64) function isqrt(n) result(x)
      integer(int64), intent(in) :: n
      integer(int64) :: y
      x = y
      y = (x + 1) / 2
      do while (y < x)
         x = y
         y = (x + n / x) / 2
      end do !- 
   end function isqrt
end module EulerCommon
