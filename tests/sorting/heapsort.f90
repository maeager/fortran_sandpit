module heapsort_m
    implicit none

    private

    public :: hsort
contains
    ! Standard Heap sort
    subroutine hsort(n, ra)
        integer, intent(in) :: n
        real(8), dimension(:), intent(inout) :: ra
        integer :: i, j, ir, l
        real(8) rra
        intrinsic ishft

        l = ishft(n,-1)
        ir = n
        do
            if (l > 1) then
                l = l-1
                rra = ra(l)
            else
                rra = ra(ir)
                ra(ir) = ra(1)
                ir = ir-1
                if (ir == 1) then
                    ra(1) = rra
                    return
                end if
            end if
            i = l
            j = ishft(l,1)
            do
                if (j > ir) exit
                if (j < ir .and. ra(j) < ra(j+1)) then
                    j = j+1
                end if
                if (rra < ra(j)) then
                    ra(i) = ra(j)
                    i = j
                    j = j+i
                else
                    j = ir + 1
                end if
            end do
            ra(i) = rra
        end do

    end subroutine hsort
end module heapsort_m
