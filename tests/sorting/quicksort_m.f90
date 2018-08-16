
module quicksort_m
implicit none
private

!! Quicksort microbench
public :: quicksort_m_sp, quicksort_m_dp, quicksort_m_int
contains

    recursive subroutine quicksort_m_dp(a, lo0, hi)
        real, intent(inout) :: a(:)
        integer(8), intent(in) :: lo0, hi
        integer(8) :: i, j, lo
        real :: pivot, t
        lo = lo0
        i = lo
        j = hi
        do while (i < hi)
            pivot = a((lo+hi)/2)
            do while (i <= j)
                do while (a(i) < pivot)
                    i = i + 1
                end do
                do while (a(j) > pivot)
                    j = j - 1
                end do
                if (i <= j) then
                    t = a(i)
                    a(i) = a(j)
                    a(j) = t
                    i = i + 1
                    j = j - 1
                end if
            end do
            if (lo < j) call quicksort_m_dp(a, lo, j)
            lo = i
            j = hi
        end do
    end subroutine quicksort_m_dp

    recursive subroutine quicksort_m_sp(a, lo0, hi)
        real, intent(inout) :: a(:)
        integer(4), intent(in) :: lo0, hi
        integer(4) :: i, j, lo
        real :: pivot, t
        lo = lo0
        i = lo
        j = hi
        do while (i < hi)
            pivot = a((lo+hi)/2)
            do while (i <= j)
                do while (a(i) < pivot)
                    i = i + 1
                end do
                do while (a(j) > pivot)
                    j = j - 1
                end do
                if (i <= j) then
                    t = a(i)
                    a(i) = a(j)
                    a(j) = t
                    i = i + 1
                    j = j - 1
                end if
            end do
            if (lo < j) call quicksort_m_sp(a, lo, j)
            lo = i
            j = hi
        end do
    end subroutine quicksort_m_sp

    recursive subroutine quicksort_m_int(a, lo0, hi)
        integer, intent(inout) :: a(:)
        integer(4), intent(in) :: lo0, hi
        integer(4) :: i, j, lo
        integer :: pivot, t
        lo = lo0
        i = lo
        j = hi
        do while (i < hi)
            pivot = a((lo+hi)/2)
            do while (i <= j)
                do while (a(i) < pivot)
                    i = i + 1
                end do
                do while (a(j) > pivot)
                    j = j - 1
                end do
                if (i <= j) then
                    t = a(i)
                    a(i) = a(j)
                    a(j) = t
                    i = i + 1
                    j = j - 1
                end if
            end do
            if (lo < j) call quicksort_m_int(a, lo, j)
            lo = i
            j = hi
        end do
    end subroutine quicksort_m_int

end module quicksort_m
