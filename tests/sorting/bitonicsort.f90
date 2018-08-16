
module bitonic_sort_m
implicit none
private

!! Bitonic sort
public ::  bitonic_sort
contains

    recursive subroutine bitonic_sort(up, x,p1,p2)
        logical, intent(inout):: up
        real,intent(inout) :: x(:)
        integer, optional :: p1,p2
        logical s1,s2
        if(.not.present(p1))then
            p1=1
            p2=size(x)
        endif
        if (p2-p1 > 1)then
            s1=.true.;s2=.false.
            call bitonic_sort(s1, x, p1, p1-1+(p2-p1)/2)
            call bitonic_sort(s2, x, p1+(p2-p1)/2, p2)
            call bitonic_merge(up, x, p1, p2)
        end if
    end subroutine bitonic_sort
    recursive subroutine bitonic_merge(up, x,p1, p2)
        logical, intent(inout) :: up
        real, intent(inout):: x(:)
        integer, intent(in) :: p1,p2
        ! assume input x is bitonic, and sorted list is returned
        if (p2-p1 > 1) then
            call bitonic_compare(up, x,p1,p2)
            call bitonic_merge(up, x, p1, p1+(p2-p1)/2)
            call bitonic_merge(up, x, p1+(p2-p1)/2,p2)
        end if
    end subroutine bitonic_merge
    subroutine  bitonic_compare(up, x,p1,p2)
        logical, intent(in):: up
        real, intent(inout) :: x(:)
        integer, intent(in) :: p1,p2
        real::tmp
        integer i, dist
        dist = p1 + (p2-p1)/2
        do i=p1,dist
            if ((x(i) > x(i + dist)) .eqv. up)then
                tmp=x(i);
                x(i)=x(i + dist)
                x(i + dist)=x(i)
            end if
        end do
    end subroutine bitonic_compare


end module bitonic_sort_m
