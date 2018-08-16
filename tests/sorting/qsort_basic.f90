
module qsort_basic
implicit none
private

!! Quicksort
public :: qsortf
private :: partition
contains

    ! Recursive Fortran 95 quicksort routine
    ! sorts real numbers into ascending numerical order
    ! Author: Juli Rew, SCD Consulting (juliana@ucar.edu), 9/03
    ! Based on algorithm from Cormen et al., Introduction to Algorithms,
    ! 1997 printing

    ! Made F conformant by Walt Brainerd

    recursive subroutine qsortf(A)
        real, intent(in out), dimension(:) :: A
        integer(8) :: iq

        if(size(A) > 1) then
            call partition(A, iq)
            call qsortf(A(:iq-1))
            call qsortf(A(iq:))
        endif
    end subroutine qsortf

    subroutine partition(A, marker)
        real, intent(in out), dimension(:) :: A
        integer(8), intent(out) :: marker
        integer(8) :: i, j
        real :: temp
        real :: x      ! pivot point
        x = A(1)
        i= 0
        j= size(A) + 1

        do
            j = j-1
            do
                if (A(j) <= x) exit
                j = j-1
            end do
            i = i+1
            do
                if (A(i) >= x) exit
                i = i+1
            end do
            if (i < j) then
                ! exchange A(i) and A(j)
                temp = A(i)
                A(i) = A(j)
                A(j) = temp
            elseif (i == j) then
                marker = i+1
                return
            else
                marker = i
                return
            endif
        end do
    end subroutine partition


end module qsort_basic
