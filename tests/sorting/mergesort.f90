
module mergesort_m
implicit none
private

!! Quicksort
public :: MergeSort
contains

    !! https://rosettacode.org/wiki/Sorting_algorithms/Merge_sort
    subroutine Merge(A,NA,B,NB,C,NC)

        integer, intent(in) :: NA,NB,NC         ! Normal usage: NA+NB = NC
        integer, intent(in out) :: A(NA)        ! B overlays C(NA+1:NC)
        integer, intent(in)     :: B(NB)
        integer, intent(in out) :: C(NC)

        integer :: I,J,K

        I = 1; J = 1; K = 1;
        do while(I <= NA .and. J <= NB)
            if (A(I) <= B(J)) then
                C(K) = A(I)
                I = I+1
            else
                C(K) = B(J)
                J = J+1
            endif
            K = K + 1
        enddo
        do while (I <= NA)
            C(K) = A(I)
            I = I + 1
            K = K + 1
        enddo
        return

    end subroutine

    recursive subroutine MergeSort(A,N,T)

        integer, intent(in) :: N
        integer, dimension(N), intent(in out) :: A
        integer, dimension((N+1)/2), intent (out) :: T

        integer :: NA,NB,V

        if (N < 2) return
        if (N == 2) then
            if (A(1) > A(2)) then
                V = A(1)
                A(1) = A(2)
                A(2) = V
            endif
            return
        endif
        NA=(N+1)/2
        NB=N-NA

        call MergeSort(A,NA,T)
        call MergeSort(A(NA+1),NB,T)

        if (A(NA) > A(NA+1)) then
            T(1:NA)=A(1:NA)
            call Merge(T,NA,A(NA+1),NB,A,N)
        endif
        return

    end subroutine MergeSort

    subroutine Merge_r4(A,NA,B,NB,C,NC)

        integer, intent(in) :: NA,NB,NC         ! Normal usage: NA+NB = NC
        real, intent(in out) :: A(NA)        ! B overlays C(NA+1:NC)
        real, intent(in)     :: B(NB)
        real, intent(in out) :: C(NC)

        integer :: i,j,k

        i = 1; j = 1; k = 1;
        do while(i <= NA .and. j <= NB)
            if (A(i) <= B(j)) then
                C(k) = A(i)
                i = i+1
            else
                C(k) = B(j)
                j = j+1
            endif
            k = k + 1
        enddo
        do while (i <= NA)
            C(k) = A(i)
            i = i + 1
            k = k + 1
        enddo
        return

    end subroutine merge_r4

    recursive subroutine MergeSort_r4(A,N,T)

        integer, intent(in) :: N
        real, dimension(N), intent(in out) :: A
        real, dimension((N+1)/2), intent (out) :: T
        real :: V
        integer :: NA,NB

        if (N < 2) return
        if (N == 2) then
            if (A(1) > A(2)) then
                V = A(1)
                A(1) = A(2)
                A(2) = V
            endif
            return
        endif
        NA=(N+1)/2
        NB=N-NA

        call MergeSort_r4(A,NA,T)
        call MergeSort_r4(A(NA+1),NB,T)

        if (A(NA) > A(NA+1)) then
            T(1:NA)=A(1:NA)
            call Merge_r4(T,NA,A(NA+1),NB,A,N)
        endif
        return

    end subroutine MergeSort_r4


end module mergesort_m
