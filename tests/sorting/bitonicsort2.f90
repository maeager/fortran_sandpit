
module bitonic_sort2_m
implicit none
private

!! Bitonic sort
public :: bitSort
logical, parameter :: noassert = .true.
contains

    pure subroutine assertion(cond)
        implicit none
        logical, intent(in) :: cond
        !real, volatile :: r
        real :: r
        if ( noassert ) return   !<=== Arjen M special note: compiler will detect .true. and eliminate assertion function
        r = 1.0
        if (.not. cond) r = r / 0.0
    end subroutine assertion

    pure subroutine bitonic_kernel(up, a, p,  q)
        real,    intent(inout):: a(:)
        integer, intent(in)  :: p,q
        integer d, i,id
        real tmp
        logical, intent(in):: up

        d=ishft(1,-(p-q))

        do i=1, size(a)
            ! up = iand(ishft((i-1) , p) , 2) == 0
            id=ior(i,d)
            if ((iand(i-1, d) == 0) .and. ((a(i) > a(id)) .eqv. up)) then
                tmp = a(i); a(i) = a(id); a(id) = tmp;
            endif
        enddo
    end subroutine bitonic_kernel

    recursive subroutine bitSort(up, aN,  a)
        real,    intent(inout):: a(:)
        integer, intent(in)   :: aN
        logical, intent(inout):: up
        integer i,j,logn
        logn=int(log10(real(size(a)))/log10(2.0))
        call assertion((aN == size(a)) .and. (aN == ishft(1,-logn)))
        !        assert a.length == 1 << logn;
        do  i=1,logn
            do j=1, i
                call bitonic_kernel(up, a, i, j)
            end do
        end do
    end subroutine bitSort

end module bitonic_sort2_m
