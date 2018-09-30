program test_sort
    !$ use omp_lib
    use heapsort_m
    use qsort_m
    use qsort_basic
    use quicksort_m
    use radix_sort
    use merge_sort_m
    use bitonic_sort_m
    use bitonic_sort2_m
    use qsort_c, only: sortp_1r4
    use multi_threaded_qsort

    implicit none
    real(8) :: startt, stopt
    integer , parameter  :: nexamples = 9
    integer (8), parameter :: nmax =  10000000
    integer, parameter :: it_max=10
    real,    allocatable, dimension(:) :: A,Aret,trec
    integer, allocatable, dimension(:) :: Aind, Aindtmp
    real,    allocatable, dimension(:,:) :: B
    !  integer, allocatable, dimension(:) :: Bind
    integer(8) :: count1, count2, count3, count4, rate
    integer(8) ::  i, thr
    integer, dimension(12) :: seedx
    integer, dimension(33) :: seed8
    integer (8) :: n,nA
    integer (4) :: m,nAsp, itrec
    real :: t1, t2, t3, t4,  t1min, t2min, t3min, t4min, t2m, t5
    real :: trun(nexamples), tmin()

    seed8 = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,&
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,1, 2, 3, 4, 5, 6, 7, 8, 9 /)
    call system_clock(count_rate=rate)
    call random_seed(put = seed8)
    allocate (A(nAsp));allocate(Aind(nAsp));allocate(Aret(nAsp))
#define RUNSORT( X ) call make_data4(nAsp,A,Aind);X;call check_result(A)
    !
    ! Validate sorting routines
    !
    write(*,*) " HPSORT  "
    RUNSORT( call hpsort(A, Aind) )
    RUNSORT( call qsortf(A) )
    RUNSORT( call quicksort_m_sp(A,1_4,nAsp) )
    RUNSORT( call sortp_1r4(INT(nAsp,4),Aind,A) )
    RUNSORT( call MTSortS(A,nAsp,"Ascending") )
    RUNSORT( call radixsort(Aret,A,nAsp);A=Aret; )
    RUNSORT( call mergesort_r4(A,nAsp,Aret) )
    RUNSORT( call bitonic_sort(.true., A) )
    RUNSORT( call bitSort(.true., nAsp, A) )

    deallocate(A,Aind,Aret)

#define TIMESORT( X ) call make_data4(nAsp,A,Aind);\
    call system_clock(count1);\
    X;\
    call system_clock(count2);\
    trec(itrec)=real(count2-count1)/(real(rate)); itrec=itrec+1


    !
    ! Test timing of sorting routines
    !
    trec=0.;itrec=1
    write(*,*) " SORTING ROUTINES:  SINGLE PRECISION FLOATING POINT "
    write(*,*) "   M       HPSORT         QSORT     RecursiveQsort      C-QSORT     MergeSort BitonicSort BitonicSort2    MT-QSORT"
    do m=25,6,-1
        nAsp = INT(2**m,4) + 1
        if(nAsp > nmax) cycle
        trec=0.;itrec=1
        allocate (A(nAsp));allocate(Aind(nAsp))

        TIMESORT( call hpsort(A, Aind) )
        TIMESORT( call qsortf(A) )
        TIMESORT( call quicksort_m_sp(A,1_4,nAsp) )
        TIMESORT( call sortp_1r4(INT(nAsp,4),Aind,A) )
        TIMESORT( call MTSortS(A,nAsp,"Ascending")  )

        TIMESORT( call radixsort(Aret,A,nAsp);A=Aret; )
        TIMESORT( call mergesort_r4(A,nAsp,Aret) )
        TIMESORT( call bitonic_sort(.true.,A) )
        TIMESORT( calL bitSort(.true.,nAsp,A) )


        call print_table(nAsp, trec)

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
            A(i) = random
        end do

    end subroutine make_data8

    subroutine make_data4(nAsp,A, Aind)

        ! DUMMY ARGUMENTS
        integer (4), intent(in) :: nAsp
        real (4), dimension(nAsp), intent(inout) :: A, Aind

        ! LOCAL VARIABLES
        integer (4) :: i
        real :: random

        do i = 1, nAsp
            Aind(i)=i
            call random_number(random)
            A(i) = random
        end do

    end subroutine make_data4

    subroutine check_result(nAsp, A)
        integer (4), intent(in) :: nAsp
        real (4), dimension(nAsp), intent(in) :: A
        integer :: i
        do i=1, nAsp-1
            if( A(i) > A(i+1))then
                print *, " Array not sorted "
                return
            endif
        end do
    end subroutine check_result

    subroutine print_table(nA, t)
        integer(8) :: nA
        real,intent(in) :: t(:)
        character(len=*),parameter :: redc=achar(27)//'[31m'
        character(len=*),parameter :: rede=achar(27)//'[0m'
        integer :: i,nt, idx_best
        integer,allocatable:: idx(:)
        real,allocatable :: tsorted(:)
        nt=size(t)
        allocate(tsorted(nt),source=t)
        allocate(idx(nt))
        do i=1,nt
            idx(i)=i
        end do

        call hpsort(tsorted,idx)
        idx_best =idx(1)

        write(*,'(I15,1x)', advance='no') nA
        do i=1,nt
            if(i == idx_best)then
                write(*,'(a,ES15.8,a,1x)', advance='no') redc,t(i),rede
            else
                write(*,'(ES15.8,2x)', advance='no') t(i)
            endif
        end do

        write(*,*) ""
        deallocate(tsorted,idx)
    end subroutine print_table




end program test_sort
