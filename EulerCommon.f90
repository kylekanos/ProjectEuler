module EulerCommon
   implicit none
   integer, parameter :: int64 = selected_int_kind(10)
   integer, parameter :: dp = selected_real_kind(15,307)
   
contains
   !> check to see if all digits are in the number
   logical function isPandigital(n)
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
         digits = replaceText(digits, repin, repout)
      end do

      isPandigital = len(trim(digits)) == 0
      
      return
       
   end function isPandigital

   

   !> replaces text in str with rep
   function replaceText(str, text, rep) result(outs)
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
   end function

   !> returns true if the two input numbers are permutations of each other
   logical function isPermutation(a,b,n)
      integer(int64), intent(in) :: a, b, n
      character(len=n) :: t1, t2
      character(len=1) :: repin, repout
      integer(int64) :: i

      write(t1,'(i4)') a
      write(t2,'(i4)') b
      repout = ' '

      do i=1,n
         repin = t1(i:i)
         t2 = replaceText(t2, repin, repout)
      end do !- i

      isPermutation = len(trim(t2)) == 0
      
      return
      
   end function isPermutation

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

   !> primality test (not terribly efficient)
   logical function isPrime(n) result(res)
    integer(int64), intent(in) :: n
    integer(int64) :: x, i
 
    if (n <= 1 .or. mod(n,2)==0) then
       res = .false.; return
    else if (any(n == [2,3,5,7,11])) then
       res = .true.; return
    else
       x = int(sqrt(real(n)))
       do i=3,x,2
          if (mod(n, i) == 0) then
             res = .false.; return
          end if
       end do
       res = .true.
    endif

    return
  end function isPrime

   !> generate a list of primes to n
   subroutine prime_sieve(n,p)
      integer(int64), intent(in) :: n
      logical, dimension(n) :: p
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
   subroutine find_factors(n, d, i)
      integer(int64), intent(in) :: n
      integer(int64), dimension(:), intent(out) :: d
      integer(int64), intent(out) :: i

      integer(int64) :: div, next, rest

      i    = 1
      div  = 2
      next = 3
      rest = n

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
      ! remove the last one
      i = i - 1
      ! remove the duplicates
      div=1
      do
         if (d(div) == d(div+1)) i = i - 1
      end do
   end subroutine find_factors

   function factor(n) result(factors)
      integer(int64), intent(in) :: n
      integer(int64), dimension(20) :: factors
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

   integer(int64) function trial_division(n) result(F)
      integer(int64), intent(in) :: n
      integer(int64) :: m, dif(8), p(3)=[2,3,5]
      integer(int64) :: i
      
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

      dif = [6,4,2,4,2,4,6,2]
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

!~ def trial_division(n, bound=None):
!~     if n == 1: return 1
!~     for p in [2, 3, 5]:
!~         if n%p == 0: return p
!~     if bound == None: bound = n
!~     dif = [6, 4, 2, 4, 2, 4, 6, 2]
!~     m = 7; i = 1
!~     while m <= bound and m*m <= n:
!~         if n%m == 0:
!~             return m
!~         m += dif[i%8]
!~         i += 1
!~     return n

   logical function noDuplicates(array) result(bool)
      integer(int64), intent(in) :: array(:)
      integer(int64) :: i, k

      do i=1,size(array)
         k = array(i)
         if (any(k == array(i+1:)) .or. any(k == array(:i-1))) then
            bool = .false.
            return
         end if
      end do
      bool = .true.
      return
      
   end function noDuplicates

   integer(int64) function fibonacci(n) result(F)
      integer(int64), intent(in) :: n
      integer(int64), dimension(2,2) :: Q, S
      integer(int64) :: i
      Q = reshape([0, 1, 1, 1], [2,2])
      S = Q
      do i=1,n-1
         Q = matmul(S,Q)
      end do
      F = Q(2,1)
   end function fibonacci

   !> returns the triangle sequence
   subroutine setTriangleSequence(n, T)
      integer(int64), intent(in) :: n
      integer(int64), dimension(n) :: T
      integer(int64) :: i

      do i=1,n
         T(i) = int(i * (i + 1) / 2, int64)
      end do
   end subroutine setTriangleSequence

   !> returns the sum of the characters in the string
   integer(int64) function charSum(str_in) result(T)
      character(len=*), intent(in) :: str_in
      character(len=len(str_in)) :: str
      integer(int64) :: i

      str = to_upper(str_in)

      T = 0
      do i=1,len(str)
         T = T + charmap(str(i:i))
      end do
      
   end function charSum

   !> returns the letter number
   integer(int64) function charMap(char) result(T)
      character(len=1), intent(in) :: char
      character(len=26) :: A = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

      T = index(A, char)
      
   end function charMap

   !> returns the sum of the ascii values of the string
   integer(int64) function asciiSum(str) result(T)
      character(len=*), intent(in) :: str
      integer(int64) :: i

      T = 0
      do i=1,len(str)
         T = T + iachar(str(i:i))
      end do
   end function asciiSum

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

   !> permute numbers
   recursive subroutine generate(nmin, nmax, position, permutation)
      integer(int64), intent(in) :: nmin, nmax, position
      integer(int64), dimension(nmin:nmax) :: permutation
      integer(int64) :: value
      
      if (position > nmax) then
         print '(4(1x,i2))',permutation
      else
         do value = nmin, nmax
            if (.not. any (permutation (: position - 1) == value)) then
               permutation (position) = value
               call generate (nmin, nmax, position + 1_int64, permutation)
            end if
         end do
      end if
   end subroutine generate

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

   integer(int64) function factorial(n) result(F)
      integer(int64), intent(in) :: n
      integer(int64) :: i

      F = 1
      do i=1,n
         F = F * i
      end do
      
   end function

   !> turns an array into a single number
   integer(int64) function arrayAsNumber(n) result(A)
      integer(int64), intent(in) :: n(:)
      integer(int64) :: i
      A = 0
      do i=1,size(n)
         A = A*10_int64 + n(i)
      end do
   end function arrayAsNumber

   !> ensures the array doesn't have tens by shifting them upwards
   subroutine checkTensInArray(n,test)
      integer(int64), intent(in) :: n
      integer(int64), intent(inout) :: test(:)
      integer(int64) :: i,k,m

      do i=1,n
         if (test(i) > 9) then
            m = test(i)
            k = int(test(i)*0.1)
            test(i+1) = test(i+1) + k
            test(i) = m - 10*k
         end if
      end do

      test(n:) = 0
   end subroutine checkTensInArray

   subroutine PERMNX(M,D,N,RT)
      integer(int64), intent(in) :: n
      integer(int64), intent(inout) :: RT
      integer(int64), intent(inout), dimension(n) :: m, d
      integer(int64) :: DK,T,K,J,KL
!                          BRANCH TO 2 IF WE ARE IN THE MIDDLE OF THINGS
      if (rt == 0) then
         d(1:n) = 0
      end if

      RT = 0
      j = -1
      KL = 1
      do k=2,n
         dk = d(k)
         if (dk /= kl) then
            rt = 1
            exit
         end if
         d(k) = 0
         j = -j
         kl = k
      end do

      if (rt == 0) return

      d(k) = dk + 1
      dk = d(k)
      if (j /= 1 .and. dk > 2) kl = k - d(k)

      T = m(k)
      m(k) = m(kl)
      m(kl) = t                                                    
      return                                                         
   end subroutine PERMNX


   !> return the next permutation
   function nextPermutationStr(str) result(outs)
      character(len=*), intent(in) :: str
      character(len=len(str)) :: outs
      character(len=1) :: swap
      integer(int64) :: i, j

      outs = str

      ! step 1: find highest index such that str(i) < str(i+1)
      i = len(outs)-1
      do while (i /= 1)
         if (iachar(outs(i:i)) > iachar(outs(i+1:i+1))) exit
         i = i - 1
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
         i = i + 1
         j = j - 1
      end do
      
   end function nextPermutationStr

   !> count the number of repetitions of the subarray in the array
   integer(int64) function count_repetitions(sub, array) result(num)
      integer(int64), intent(in) :: sub(:), array(:)
      integer(int64) :: ns, na, i
      ns = size(sub); na = size(array)
      num = 0

      do i=1,na
         if (all(sub(:) == array(i:i+ns-1))) num = num + 1
      end do
   end function count_repetitions

   !> count substrings in string
   integer(int64) function count_substr(str, sub) result(ctr)
      character(*), intent(in) :: str, sub
      integer(int64) :: i, na, ns
      na = len(str); ns = len(sub)
      
      ctr = 0_int64
      do i=1,ns
         if (sub == str(i:i+ns-1)) ctr = ctr + 1
      end do !- i
   end function count_substr


   !> RNG seed initializer, taken directly from https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html
   subroutine init_random_seed()
      integer, allocatable :: seed(:)
      integer :: i, n, un, istat, dt(8), pid
      integer(int64) :: t
      
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
            t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
                 + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
                 + dt(3) * 24_int64 * 60 * 60 * 1000 &
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
      call random_seed(put=seed)
   contains
    ! This simple PRNG might not be good enough for real work, but is
    ! sufficient for seeding a better PRNG.
      function lcg(s)
        integer :: lcg
        integer(int64) :: s
        if (s == 0) then
           s = 104729
        else
           s = mod(s, 4294967296_int64)
        end if
        s = mod(s * 279470273_int64, 4294967291_int64)
        lcg = int(mod(s, int(huge(0), int64)), kind(0))
      end function lcg
   end subroutine init_random_seed
   
   !>  Solovay-Strassen primality test (currently experimental)
   logical function ss_prime(n,kopt) result(bool)
      integer(int64), intent(in) :: n
      integer(int64), optional :: kopt
      integer(int64) :: k, a, x, y
      real :: xx
      k = 20
      if (present(kopt)) k = kopt
      
      if (any(n==[2,3,5,7,11])) then
         bool = .true.
         return
      end if
      ! even numbers have a 0 for the first bit
      if (iand(n,1_int64)==0) then
         bool = .false.
         return
      end if
      
      do while (k > 0)
         call random_number(xx)
         a = int((n-3)*xx) + 2
         x = legendre(a, n)
         y = mod(a**((n-1)/2), n)
         if ((x==0) .or. (y /= mod(x,n))) then
            bool = .false.
            return
         end if
         k = k - 1
      end do
      bool = .true.
   end function ss_prime
   
   !> Legendre symbol (a/p); see https://en.wikipedia.org/wiki/Legendre_symbol
   recursive function legendre(a, p) result(L)
      integer(int64), intent(in) :: a, p
      integer(int64) :: L, r
      ! if the next line doesn't work, get a better compiler
      if (any(a==[0,1])) then
         L = a
         return
      end if
      if (iand(a,1_int64)==0) then
         L = legendre(a/2, p)
         if (iand(p*p-1, 8_int64) /= 0) L = -L
      else
         L = legendre(mod(p,a), a)
         if (iand((a-1)*(p-1), 4_int64) /= 0) L = -L
      end if
      return
   end function legendre
end module EulerCommon
