

program test_stats
  !$ use omp_lib

    implicit none
    real(8) :: startt, stopt
    integer (8), parameter :: nmax =  1000000
    integer, parameter :: it_max=100
    real,    allocatable, dimension(:) :: A
    integer, allocatable, dimension(:) :: Aind
    real,    allocatable, dimension(:,:) :: B
    real:: val(5)
    !  integer, allocatable, dimension(:) :: Bind
    integer(8) :: count1, count2, count3, count4, rate
    integer(8) ::  i, thr
    integer, dimension(12) :: seedx
    integer, dimension(33) :: seed8
    integer (8) :: n,nA
    integer (4) :: m,nAsp,it, pos1,pos2
    real :: t1, t2, t3, t4,  t1min, t2min, t3min, t4min, t2m, t5
    seedx = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 /)
    seed8 = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,&
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,1, 2, 3, 4, 5, 6, 7, 8, 9 /)
    call system_clock(count_rate=rate)
    !     write(*,*) __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__
    !     write(*,*) FC_COMPILER_VERSION
    !#if(__GNUC__ >= 7) || defined(INTEL)
    call random_seed(put = seed8)
    !#else
    !     call random_seed(put = seedx)
    !#endif

    t1=0.;t2=0.;t3=0.;t4=0.;t2m=0.
    write(*,*) "   N          MEDIAN    MEDIAN_NOCOPY     BAPPROX      BMEDIAN       SELEC"
    do m=18,4,-1
        nAsp = INT(2**m,4)-1
        if(nAsp > nmax) cycle
   ! do nAsp=10,1000


        allocate (A(nAsp));allocate ( Aind(nAsp))
        Aind = (/(i, i=1, nAsp )/)
        do it=1,it_max
            call make_data4(A,nAsp)

        call system_clock(count1)
        val(1) = median(A)
        call system_clock(count2)
        t1 = real(count2-count1)/(real(rate))
        enddo
        deallocate(A); deallocate(Aind)

        allocate (A(nAsp))
         do it=1,it_max
           call make_data4(A,nAsp)
        !    write (*,*) "Qsort"
        call system_clock(count1)
        val(2) = median_nocopy(A)
        call system_clock(count2)
        t2 = real(count2-count1)/(real(rate))
        enddo
        deallocate(A)

        allocate (A(nAsp))
        do it=1,it_max
            call make_data4(A,nAsp)
            !    write (*,*) "Qsort"
            call system_clock(count1)
            val(3)= bapprox(nAsp,A)
            call system_clock(count2)
            t3 = real(count2-count1)/(real(rate))
        enddo
        ! write (*,*) "First and last in sorted list"
        ! write (*,*) A(1), A(nAsp)
        ! write (*,*) "Execution time in seconds:"
        ! write (*,*) real(count2-count1)/real(rate)
        !  t2=real(count2-count1)/real(rate)
        deallocate(A)



        allocate (A(nAsp))
        allocate(Aind(nAsp))
        do it=1,it_max
           call make_data4(A,nAsp)
           call system_clock(count1)
           if( mod(nAsp,2) == 0 )then
               val(4)= bmedian(nAsp-1,A(1:nAsp-1))
           else
               val(4)= bmedian(nAsp,A)
           end if
        call system_clock(count2)
        t4 = real(count2-count1)/real(rate)
        enddo
        deallocate(Aind)
        deallocate(A)

        allocate (A(nAsp))
        allocate(Aind(nAsp))
        do it=1,it_max
            call make_data4(A,nAsp)
            call system_clock(count1)
            if(  mod(nAsp,2) == 0 )then
                pos1 = nAsp/2
                pos2 = pos1+1
                val(5)= quickselect(pos1,nAsp,A)
                val(5)= val(5)+ quickselect(pos2,nAsp,A)
                val(5)= val(5)/2.
            else
                pos1 = nint(real(nAsp)/2.)
                val(5)= quickselect(pos1,nAsp,A)
            endif


            call system_clock(count2)
            t5 = real(count2-count1)/real(rate)
        enddo
        deallocate(Aind)
        deallocate(A)



        call print_table(INT(nAsp,8), t1, t2, t3, t4,t5)

    end do
    t1=0.;t2=0.;t3=0.;t4=0.;t2m=0.

    write(*,*) ""
    write(*,*) "EVEN "

    write(*,*) "   N          MEDIAN    MEDIAN_NOCOPY     BAPPROX      BMEDIAN       SELEC"
    do m=18,4,-1
        nAsp = INT(2**m,4)
        if(nAsp > nmax) cycle
    !do nAsp=10,1000
        allocate (A(nAsp));allocate ( Aind(nAsp))
        Aind = (/(i, i=1, nAsp )/)
        do it=1,it_max
        call make_data4(A,nAsp)
        call system_clock(count1)
        val(1) = median(A)
        call system_clock(count2)
        t1 = real(count2-count1)/(real(rate))
        enddo
        deallocate(A); deallocate(Aind)

        allocate (A(nAsp))
        do it=1,it_max
           call make_data4(A,nAsp)
        !    write (*,*) "Qsort"
        call system_clock(count1)
        val(2) = median_nocopy(A)
        call system_clock(count2)
        t2 = real(count2-count1)/(real(rate))
        enddo
        deallocate(A)

        allocate (A(nAsp))
        do it=1,it_max
            call make_data4(A,nAsp)
            !    write (*,*) "Qsort"
            call system_clock(count1)
            val(3)= bapprox(nAsp,A)
            call system_clock(count2)
            t3 = real(count2-count1)/(real(rate))
        enddo
        ! write (*,*) "First and last in sorted list"
        ! write (*,*) A(1), A(nAsp)
        ! write (*,*) "Execution time in seconds:"
        ! write (*,*) real(count2-count1)/real(rate)
        !  t2=real(count2-count1)/real(rate)
        deallocate(A)



        allocate (A(nAsp))
        allocate(Aind(nAsp))
        do it=1,it_max
           call make_data4(A,nAsp)
           call system_clock(count1)
           val(4)= bapproxmp(nAsp,A)

!           if( is_even(nAsp) )then
!               val(4)= bmedian(nAsp-1,A(1:nAsp-1))
!           else
!               val(4)= bmedian(nAsp,A)
!           end if
        call system_clock(count2)
        t4 = real(count2-count1)/real(rate)
        enddo
        deallocate(Aind)
        deallocate(A)

        allocate (A(nAsp))
        allocate(Aind(nAsp))
        do it=1,it_max
            call make_data4(A,nAsp)
            call system_clock(count1)
            if(  mod(nAsp,2) == 0 )then
                pos1 = nAsp/2
                pos2 = pos1+1
                val(5)= quickselect(pos1,nAsp,A)
                val(5)= val(5)+ quickselect(pos2,nAsp,A)
                val(5)= val(5)/2.
            else
                pos1 = nint(real(nAsp)/2.)
                val(5)= quickselect(pos1,nAsp,A)
            endif


            call system_clock(count2)
            t5 = real(count2-count1)/real(rate)
        enddo
        deallocate(Aind)
        deallocate(A)



        call print_table(INT(nAsp,8), t1, t2, t3, t4,t5)

    end do


contains

    subroutine make_data8(A,nA)

        ! DUMMY ARGUMENTS
        integer (8), intent(in) :: nA
        real (4), dimension(nA), intent(out) :: A

        ! LOCAL VARIABLES
        integer (8) :: i
        real :: random

        do i = 1, nA
            call random_number(random)
            A(i) = 25.0*random
        end do

    end subroutine make_data8

    subroutine make_data4(A,nAsp)

        ! DUMMY ARGUMENTS
        integer (4), intent(in) :: nAsp
        real (4), dimension(nAsp), intent(out) :: A

        ! LOCAL VARIABLES
        integer (4) :: i
        real :: random

        do i = 1, nAsp
            call random_number(random)
            A(i) = 25.0*random
        end do

    end subroutine make_data4

    subroutine print_table(nA, t1, t2, t3, t4, t5)
      integer(8) :: nA
      real :: t1, t2, t3, t4
      real, optional :: t5
      character(len=:), allocatable :: redc
      character(len=:), allocatable :: rede
      redc=achar(27)//'[31m'
      rede=achar(27)//'[0m'



      write(*,'(I6,1x)', advance='no') nA
      if (t1 < t2  .and. t1<t3 .and. t1<t4 )then
!         write(*,'(a,1x)', advance='no')  adjustr(format_str(trim(real2str(t1)),C_RED)//" ")
          write(*,'(a,ES15.8,a,1x)', advance='no') redc,t1,rede
      else
         write(*,'(ES15.8,2x)', advance='no') t1
      endif
      write(*,'(3x)', advance='no')
      if (t2 < t1  .and. t2<t3 .and. t2<t4 )then
         !write(*,'(a,1x)', advance='no')  adjustr(format_str(trim(real2str(t2)),C_RED))
          write(*,'(a,ES15.8,a,1x)', advance='no') redc,t2,rede
      else
         write(*,'(ES15.8,2x)', advance='no') t2
      endif
      write(*,'(3x)', advance='no')
      if (t3 < t2  .and. t3<t1 .and. t3<t4 )then
!          write(*,'(a,1x)', advance='no') adjustr(format_str(trim(real2str(t3)),C_RED))
          write(*,'(a,ES15.8,a,1x)', advance='no') redc,t3,rede
      else
         write(*,'(ES15.8,2x)', advance='no') t3
      endif
      write(*,'(3x)', advance='no')
      if (t4 < t1 .and. t4<t2 .and. t4<t3 )then
!          write(*,'(a,1x)', advance='no') adjustr(format_str(trim(real2str(t4)),C_RED))
          write(*,'(a,ES15.8,a,1x)', advance='no') redc,t4,rede
      else
         write(*,'(ES15.8,2x)', advance='no') t4
      endif
      write(*,'(3x)', advance='no')

      if(present(t5))then
         if (t5 < t1 .and. t5< t2  .and. t5<t3 .and. t5<t4 )then
             !write(*,'(a,1x)', advance='no')  adjustr(format_str(trim(real2str(t5)),C_RED))
             write(*,'(a,ES15.8,a,1x)', advance='no') redc,t5,rede

         else
            write(*,'(ES20.8,1x)', advance='no') t5
         endif
      endif
      write(*,*) ""

    end subroutine print_table


    !>   for calculating the median
    function median( arr ) result( val )
        real, intent(in)  :: arr(:)
        real              :: copy(size(arr))
        real    :: val, val1, val2
        integer :: n, pos1, pos2
        n = size(arr)
        if(  mod(n,2) == 0 )then
            pos1 = n/2
            pos2 = pos1+1
        else
            pos1 = nint(real(n)/2.)
            pos2 = pos1
        endif
        copy = arr
        if( pos1 == pos2 )then
            !dir$ loop count min(256)
            val  = selec(pos1,n,copy)
        else
            !dir$ loop count min(256)
            val1 = selec(pos1,n,copy)
            !dir$ loop count min(256)
            val2 = selec(pos2,n,copy)
            val  = (val1+val2)/2.
        endif
    end function median

    !>   for calculating the median
    function median_nocopy( arr ) result( val )
        real, intent(inout) :: arr(:)
        real    :: val, val1, val2
        integer :: n, pos1, pos2
        integer, volatile :: idbg
        n = size(arr)
        if(  mod(n,2) == 0 )then
            pos1 = n/2
            pos2 = pos1+1
        else
            pos1 = nint(real(n)/2.)
            pos2 = pos1
        endif
        if( pos1 == pos2 )then
            !dir$ loop count min(256)
            val  = selec(pos1,n,arr)
        else
            !dir$ loop count min(256)
            val1 = selec(pos1,n,arr)
            !dir$ loop count min(256)
            val2 = selec(pos2,n,arr)
            val  = (val1+val2)/2.
        endif
    end function median_nocopy


    ! http://www.stat.cmu.edu/~ryantibs/median/
    ! Some sample Fortran code for the binapprox algorithm.
    real function bapproxmp(n,x)
        !$ use omp_lib
      integer n
      real x(n)
      integer i,bottom,counts(0:1000),bin,k,j,count,medbin
      real mu,sigma,scalefac,leftend,rghtend,xsq

      !     Compute the mean and standard deviation
      if(n<1000)then
          mu=sum(x)/n
          sigma=sqrt(dot_product(x-mu,x-mu)/n)
      else
      !$omp parallel do reduction(+:mu) private(i)
      do i=1,n
          mu = mu+x(i)
      end do
      !$omp end parallel do
      mu=mu/n
      xsq = dotprod(x - mu, x-mu,n)
      sigma = sqrt(xsq/n)
      endif
      !     Bin x across the interval [mu-sigma, mu+sigma]
      bottom = 0
      counts = 0
      scalefac = 1000/(2*sigma)
      leftend = mu-sigma
      rghtend = mu+sigma
      !$omp parallel do reduction(+:bottom) private(i,bin)
      do  i = 1,n
         if (x(i).lt.leftend) then
            bottom = bottom+1
         else if (x(i).lt.rghtend) then
            bin = int((x(i)-leftend) * scalefac)
            counts(bin) = counts(bin)+1
         endif
     end do
     !$omp end parallel do
      !     If n is odd
      if (mod(n,2).eq.1) then
         !        Find the bin that contains the median
         k = (n+1)/2
         count = bottom
         do  i = 0,1000
            count = count+counts(i)
            if (count.ge.k) then
               bapproxmp = (i+0.5)/scalefac + leftend
               return
            endif
         end do
         !     If n is even
      else
         !        Find the bins that contain the medians
         k = n/2
         count = bottom
         do  i = 0,1000
            count = count+counts(i)
            if (count.ge.k) then
               j = i
               do while  (count.eq.k)
                  j = j+1
                  count = count+counts(j)
               end do
               bapproxmp = (i+j+1)/(2*scalefac) + leftend
               return
            endif
         end do
      endif
    end function bapproxmp


    function dotprod (B, C, n) result(sum)
        real :: B(N), C(N), sum
        integer :: N, i
        sum = 0.0e0
        !$omp target map(to: B, C) map(tofrom: sum)
        !$omp teams num_teams(8) thread_limit(16) reduction(+:sum)
        !$omp distribute parallel do reduction(+:sum) &
        !$omp& dist_schedule(static, 1024) schedule(static, 64)
        do i = 1, N
            sum = sum + B(i) * C(i)
        end do
        !$omp end teams
        !$omp end target
    end function dotprod




    !http://www.stat.cmu.edu/~ryantibs/median/
    ! Some sample Fortran code for the binapprox algorithm.

    real function bapprox(n,x)
      integer n
      real x(n)
      integer i,bottom,counts(0:1000),bin,k,j,count
      real mu,sigma,scalefac,leftend,rghtend

      !     Compute the mean and standard deviation
      !dir$ loop count min(256)
      mu = sum(x)/n
      !dir$ loop count min(128)
      sigma = sqrt(dot_product(x-mu,x-mu)/n)

      !     Bin x across the interval [mu-sigma, mu+sigma]
      bottom = 0
      counts = 0
      scalefac = 1000/(2*sigma)
      leftend = mu-sigma
      rghtend = mu+sigma
      !dir$ parallel
      !dir$ loop count min(256)
      do  i = 1,n
         if (x(i).lt.leftend) then
            bottom = bottom+1
         else if (x(i).lt.rghtend) then
            bin = int((x(i)-leftend) * scalefac)
            counts(bin) = counts(bin)+1
         endif
      end do
      !     If n is odd
      if (mod(n,2).eq.1) then
         !        Find the bin that contains the median
         k = (n+1)/2
         count = bottom
         do  i = 0,1000
            count = count+counts(i)
            if (count.ge.k) then
               bapprox = (i+0.5)/scalefac + leftend
               return
            endif
         end do
         !     If n is even
      else
         !        Find the bins that contain the medians
         k = n/2
         count = bottom
         do  i = 0,1000
            count = count+counts(i)
            if (count.ge.k) then
               j = i
               do while  (count.eq.k)
                  j = j+1
                  count = count+counts(j)
               end do
               bapprox = (i+j+1)/(2*scalefac) + leftend
               return
            endif
         end do
      endif
    end function bapprox

! http://www.stat.cmu.edu/~ryantibs/median/binmedian.f
    real function bmedian_orig(n,x)
        integer n
        real x(n)
        integer i,bottom,counts(0:1000),bin,k,count,medbin,r,oldbin,j
        real mu,sigma,scalefac,leftend,rghtend,a,oldscale,oldleft
        logical samepts;

        !     Compute the mean and standard deviation
        !dir$ loop count min(256)
        mu = sum(x)/n
        !dir$ loop count min(128)
        sigma = sqrt(dot_product(x-mu,x-mu)/n)

        !     Bin x across the interval [mu-sigma, mu+sigma]
        bottom = 0
        counts = 0
        scalefac = 1000/(2*sigma)
        leftend = mu-sigma
        rghtend = mu+sigma
        !dir$ parallel
        !dir$ loop count min(256)
        do 1 i = 1,n
            if (x(i).lt.leftend) then
                bottom = bottom+1
            else if (x(i).lt.rghtend) then
                bin = int((x(i)-leftend) * scalefac)
                counts(bin) = counts(bin)+1
            endif
1       continue

        !     If n is odd
        if (mod(n,2).eq.1) then
            k = (n+1)/2
            r = 1

            !        Beginning of recursive step:
            !        Find the bin that contains the median, and the order
            !        of the median within that bin
2           count = bottom
            do 3 i = 0,1000
                count = count+counts(i)
                if (count.ge.k) then
                    medbin = i
                    k = k - (count-counts(i))
                    goto 4
                endif
3           continue

4           bottom = 0
            counts = 0
            oldscale = scalefac
            oldleft = leftend
            scalefac = 1000*oldscale
            leftend = medbin/oldscale + oldleft
            rghtend = (medbin+1)/oldscale + oldleft

            !        Determine which points map to medbin, and put
            !        them in spots r,...n
            i = r
            r = n+1
5           if (i.lt.r) then
                oldbin = int((x(i)-oldleft) * oldscale)

                if (oldbin.eq.medbin) then
                    r = r-1
                    !              Swap x(i) and x(r)
                    a = x(i)
                    x(i) = x(r)
                    x(r) = a

                    !              Re-bin on a finer scale
                    if (x(r).lt.leftend) then
                        bottom = bottom+1
                    else if (x(r).lt.rghtend) then
                        bin = int((x(r)-leftend) * scalefac)
                        counts(bin) = counts(bin)+1
                    endif
                else
                    i = i+1
                endif
                goto 5
            endif

            !        Stop if all points in medbin are the same
            samepts = .TRUE.
            do 6 i = r+1,n
                if (.not.is_equal(x(i),x(r))) then
                    samepts = .FALSE.
                    goto 7
                endif
6           continue
7           if (samepts) then
                bmedian_orig = x(r)
                return
            endif

            !        Stop if there's <= 20 points left
            if ((n+1-r).le.20) then
                goto 8
            endif
            goto 2

            !        Perform insertion sort on the remaining points,
            !        and then pick the kth smallest
8           do 9 i = r+1,n
                a = x(i)
                do 10 j = i-1,r,-1
                    if (x(j).lt.a) goto 11
                    x(j+1) = x(j)
10              continue
                j = r-1
11              x(j+1) = a
9           continue

            bmedian_orig = x(r-1+k)
            return

        !     If n is even
        else
            bmedian_orig = 0
            return
        endif
     end function

     !     Some sample Fortran code for the binmedian algorithm.
     !     Note: I haven't written the code for n even yet.
     real function bmedian(n,x)
    integer n
    real x(n)
    integer i,bottom,counts(0:1000),bin,k,count,medbin,r,oldbin,j
    real mu,sigma,scalefac,leftend,rghtend,a,oldscale,oldleft
    logical samepts;

    !     Compute the mean and standard deviation
    !dir$ loop count min(256)
    mu = sum(x)/n
    !dir$ loop count min(128)
    sigma = sqrt(dot_product(x-mu,x-mu)/n)
    !     Bin x across the interval [mu-sigma, mu+sigma]
    bottom = 0
    counts = 0
    scalefac = 1000/(2*sigma)
    leftend = mu-sigma
    rghtend = mu+sigma
    !dir$ parallel
    !dir$ loop count min(256)
    do  i = 1,n
        if (x(i).lt.leftend) then
            bottom = bottom+1
        else if (x(i).lt.rghtend) then
            bin = int((x(i)-leftend) * scalefac)
            counts(bin) = counts(bin)+1
        endif
    end do
    !     If n is odd
    if (mod(n,2).eq.1) then
        k = (n+1)/2
        r = 1
        !        Beginning of recursive step:
        !        Find the bin that contains the median, and the order
        !        of the median within that bin
        do
            count = bottom
            do  i = 0,1000
                count = count+counts(i)
                if (count.ge.k) then
                    medbin = i
                    k = k - (count-counts(i))
                    exit
                endif
            end do
            bottom = 0
            counts = 0
            oldscale = scalefac
            oldleft = leftend
            scalefac = 1000*oldscale
            leftend = medbin/oldscale + oldleft
            rghtend = (medbin+1)/oldscale + oldleft

            !        Determine which points map to medbin, and put
            !        them in spots r,...n
            i = r
            r = n+1
            do while (i.lt.r)
                oldbin = int((x(i)-oldleft) * oldscale)
                if (oldbin.eq.medbin) then
                    r = r-1
                    !              Swap x(i) and x(r)
                    a = x(i)
                    x(i) = x(r)
                    x(r) = a

                    !              Re-bin on a finer scale
                    if (x(r).lt.leftend) then
                        bottom = bottom+1
                    else if (x(r).lt.rghtend) then
                        bin = int((x(r)-leftend) * scalefac)
                        counts(bin) = counts(bin)+1
                    endif
                else
                    i = i+1
                endif
            end do
            !        Stop if all points in medbin are the same
            samepts = .TRUE.
            do  i = r+1,n
                if (.not.is_equal(x(i),x(r))) then
                    samepts = .FALSE.
                    exit
                endif
            end do
            if (samepts) then
                bmedian = x(r)
                return
            endif
            !        Stop if there's <= 20 points left
            if ((n+1-r).le.20) then
                exit
            endif
        end do
        !        Perform insertion sort on the remaining points,
        !        and then pick the kth smallest
        do  i = r+1,n
            a = x(i)
            do  j = i-1,r,-1
                if (x(j).lt.a) goto 12
                x(j+1) = x(j)
            end do
            j = r-1
12                                  x(j+1) = a
        end do

        bmedian = x(r-1+k)
        return
        !     If n is even
    else
        bmedian = 0
        return
    endif

end function bmedian

! function median_filter( arr, percen ) result( arr_med )
!     real, intent(in)  :: arr(:), percen
!     real, allocatable :: arr_med(:)
!     integer :: n, winsz, i, ileft, iright
!     n     = size(arr)
!     allocate( arr_med(n) )
!     winsz = round2even( (percen / 100.) * real(n) ) / 2
!     do i=winsz + 1,n - winsz
!         ileft  = i - winsz
!         iright = i + winsz
!         arr_med(i) = median(arr(ileft:iright))
!     end do
!     arr_med(:winsz) = arr_med(winsz + 1)
!     arr_med(n - winsz + 1:) = arr_med(n - winsz)
! end function median_filter

function stdev (a)
    real, intent(in) :: a(:)
    real    :: stdev, avg, SumSQR, var, n
    integer, volatile :: idbg
    n= real(size(a))
    if(n < 2) return
    !dir$ loop count min(256)
    avg = sum(a)/n
    !dir$ loop count min(32)
    SumSQR = sum(sqrt(a))/n
    var = (SumSQR - avg*avg/n)/(n-1)
    stdev = sqrt(var)
end function stdev


!     taken from Numerical Recipes in Fortran 77.
real function quickselect(k,n,arr)
    integer k,n
    real arr(n)
    integer i,ir,j,l,mid
    real a,temp

    l = 1
    ir = n
    do while (ir-l.gt.1)
        mid = (l+ir)/2
        temp = arr(mid)
        arr(mid) = arr(l+1)
        arr(l+1) = temp
        if (arr(l).gt.arr(ir)) then
            temp = arr(l)
            arr(l) = arr(ir)
            arr(ir) = temp
        endif
        if (arr(l+1).gt.arr(ir)) then
            temp = arr(l+1)
            arr(l+1) = arr(ir)
            arr(ir) = temp
        endif
        if (arr(l).gt.arr(l+1)) then
            temp = arr(l)
            arr(l) = arr(l+1)
            arr(l+1) = temp
        endif
        i = l+1
        j = ir
        a = arr(l+1)
        do
            i = i+1
            if (arr(i).lt.a) cycle
            j = j-1
            do while (arr(j).gt.a)
                j=j-1
            end do
            if (j.lt.i) exit
            temp = arr(i)
            arr(i) = arr(j)
            arr(j) = temp
        end do
        arr(l+1) = arr(j)
        arr(j) = a
        if (j.ge.k) ir = j-1
        if (j.le.k) l = i
    end do
    if (ir-1.eq.1) then
        if (arr(ir).lt.arr(l)) then
            temp = arr(l)
            arr(l) = arr(ir)
            arr(ir) = temp
        endif
    endif
    quickselect = arr(k)

end function quickselect


!>   for selecting kth largest, array is modified
real function selec(k,n,arr)
    integer, intent(in)    :: k,n
    real,    intent(inout) :: arr(n)
    integer :: i,ir,j,l,mid
    real    :: a,temp
    l = 1
    ir = n
22                           if (ir-l.le.1) then
        if (ir-1.eq.1) then
            if (arr(ir).lt.arr(l)) then
                temp = arr(l)
                arr(l) = arr(ir)
                arr(ir) = temp
            endif
        endif
        selec_1 = arr(k)
        return
    else
        mid = (l+ir)/2
        temp = arr(mid)
        arr(mid) = arr(l+1)
        arr(l+1) = temp
        if (arr(l).gt.arr(ir)) then
            temp = arr(l)
            arr(l) = arr(ir)
            arr(ir) = temp
        endif
        if (arr(l+1).gt.arr(ir)) then
            temp = arr(l+1)
            arr(l+1) = arr(ir)
            arr(ir) = temp
        endif
        if (arr(l).gt.arr(l+1)) then
            temp = arr(l)
            arr(l) = arr(l+1)
            arr(l+1) = temp
        endif
        i = l+1
        j = ir
        a = arr(l+1)
23                               continue
        i = i+1
        if (arr(i).lt.a) goto 23
24                               continue
        j = j-1
        if (arr(j).gt.a) goto 24
        if (j.lt.i) goto 25
        temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
        goto 23
25       arr(l+1) = arr(j)
        arr(j) = a
        if (j.ge.k) ir = j-1
        if (j.le.k) l = i
    endif
    goto 22
end function selec

    !>   selecting kth largest, array is modified
    !    integer function selec_2(k,n,arr)
    !      integer :: k,n
    !      integer :: arr(n)
    !      integer :: i,ir,j,l,mid
    !      integer ::  a,temp
    !      l = 1
    !      ir = n
    ! 2    if (ir-l.le.1) then
    !         if (ir-1.eq.1) then
    !            if (arr(ir).lt.arr(l)) then
    !               temp = arr(l)
    !               arr(l) = arr(ir)
    !               arr(ir) = temp
    !            endif
    !         endif
    !         selec_2 = arr(k)
    !         return
    !      else
    !         mid = (l+ir)/2
    !         temp = arr(mid)
    !         arr(mid) = arr(l+1)
    !         arr(l+1) = temp
    !         if (arr(l).gt.arr(ir)) then
    !            temp = arr(l)
    !            arr(l) = arr(ir)
    !            arr(ir) = temp
    !         endif
    !         if (arr(l+1).gt.arr(ir)) then
    !            temp = arr(l+1)
    !            arr(l+1) = arr(ir)
    !            arr(ir) = temp
    !         endif
    !         if (arr(l).gt.arr(l+1)) then
    !            temp = arr(l)
    !            arr(l) = arr(l+1)
    !            arr(l+1) = temp
    !         endif
    !         i = l+1
    !         j = ir
    !         a = arr(l+1)
    ! 3       continue
    !         i = i+1
    !         if (arr(i).lt.a) goto 3
    ! 4       continue
    !         j = j-1
    !         if (arr(j).gt.a) goto 4
    !         if (j.lt.i) goto 5
    !         temp = arr(i)
    !         arr(i) = arr(j)
    !         arr(j) = temp
    !         goto 3
    ! 5       arr(l+1) = arr(j)
    !         arr(j) = a
    !         if (j.ge.k) ir = j-1
    !         if (j.le.k) l = i
    !      endif
    !      goto 2
    !  end function selec_2


end program test_stats
