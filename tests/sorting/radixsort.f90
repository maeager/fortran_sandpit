
module radix_sort_m
implicit none
private

!! Radix sort
public :: radixsort
logical, parameter :: noassert = .true.
contains


    !! radix sort -- only for integers
    subroutine radixsort (ix, iw, n)
        implicit none
        integer n
        integer :: ix(n), iw(n)
        integer :: i                        ! count bits
        integer :: ilim                     ! bits in an integer
        integer :: j                        ! count array elements
        integer :: p1old, p0old, p1, p0     ! indices to ones and zeros
        integer :: swap
        logical odd                         ! even or odd bit position

        !      if (n < 2) return      ! validate
        !
        ilim = bit_size(i)    !get the fixed number of bits
        ! alternate between putting data into iw and into ix
        p1 = n+1
        p0 = n                ! read from 1, n on first pass thru
        odd = .false.
        do i = 0, ilim-2
            p1old = p1
            p0old = p0         ! save the value from previous bit
            p1 = n+1
            p0 = 0                 ! start a fresh count for next bit

            if (odd) then
                do j = 1, p0old, +1             ! copy data from the zeros
                    if ( btest(iw(j), i) ) then
                        p1 = p1 - 1
                        ix(p1) = iw(j)
                    else
                        p0 = p0 + 1
                        ix(p0) = iw(j)
                    end if
                end do
                do j = n, p1old, -1             ! copy data from the ones
                    if ( btest(iw(j), i) ) then
                        p1 = p1 - 1
                        ix(p1) = iw(j)
                    else
                        p0 = p0 + 1
                        ix(p0) = iw(j)
                    end if
                end do

            else
                do j = 1, p0old, +1             ! copy data from the zeros
                    if ( btest(ix(j), i) ) then
                        p1 = p1 - 1
                        iw(p1) = ix(j)
                    else
                        p0 = p0 + 1
                        iw(p0) = ix(j)
                    end if
                end do
                do j = n, p1old, -1            ! copy data from the ones
                    if ( btest(ix(j), i) ) then
                        p1 = p1 - 1
                        iw(p1) = ix(j)
                    else
                        p0 = p0 + 1
                        iw(p0) = ix(j)
                    end if
                end do
            end if  ! even or odd i

            odd = .not. odd
        end do  ! next i
        !        the sign bit
        p1old = p1
        p0old = p0
        p1 = n+1
        p0 = 0

        !          if sign bit is set, send to the zero end
        do j = 1, p0old, +1
            if ( btest(iw(j), ilim-1) ) then
                p0 = p0 + 1
                ix(p0) = iw(j)
            else
                p1 = p1 - 1
                ix(p1) = iw(j)
            end if
        end do
        do j = n, p1old, -1
            if ( btest(iw(j), ilim-1) ) then
                p0 = p0 + 1
                ix(p0) = iw(j)
            else
                p1 = p1 - 1
                ix(p1) = iw(j)
            end if
        end do
        !       reverse the order of the greater value partition
        p1old = p1
        do j = n, (p1old+n)/2+1, -1
            swap = ix(j)
            ix(j) = ix(p1)
            ix(p1) = swap
            p1 = p1 + 1
        end do
        return
    end subroutine radixsort

end module radix_sort_m
