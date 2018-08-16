!! wrapper for intel's qsort in ifport
module intel_qsort

#ifdef __INTEL_COMPILER
    use ifport
#endif
implicit none
private
public :: iqsortr4, iqsortr8
real, parameter :: TINY = 1.0E-8
real(8), parameter :: DTINY = 1.0E-10
#ifdef __INTEL_COMPILER
interface
    ! Define an overload of the default QSORT signature
    ! with a signature using the shared type.
    !
    subroutine QSORT_intel_real(array, len, isize, comparator)
        integer(8) :: len
        real ::  array(len)
        integer :: isize
        integer(2), external :: comparator
    !DIR$ ATTRIBUTES ALIAS:'_qsort' :: QSORT_intel_real
    end subroutine QSORT_intel_real
    subroutine QSORT_intel_double(array, len, isize, comparator)
        integer(8) :: len
        real(8) ::  array(len)
        integer :: isize
        integer(2), external :: comparator
        !DIR$ ATTRIBUTES ALIAS:'_qsort' :: QSORT_intel_double
    end subroutine QSORT_intel_double
end interface
#endif
contains
    pure integer(2) function ascending(p1,p2)
        real, intent(in) :: p1,p2
        ascending = 1 ! p1 is gt p2
        if( p1 - p2 < 0.)then
            ascending = -1
        else if( abs(p1 - p2) < TINY )then
            ascending = 0
        endif
    end function ascending

    subroutine iqsortr4(A)
        real, intent(inout) :: A(:)
        integer(8) :: size_of_array
        integer, parameter :: size_of_element = 4
        size_of_array = size(A)
#ifdef __INTEL_COMPILER
        call QSort_intel_real(A, size_of_array, size_of_element, ascending)
#else
        print *," qsort not available (this is not an INTEL compiler)"
#endif
    end subroutine iqsortr4

    function ascending8(p1,p2)
        real(8) :: p1,p2
        integer(2) :: ascending8
        ascending8 = 1 ! p1 is gt p2
        if( p1 - p2 < 0.)then
            ascending8 = -1
        else if( abs(p1 - p2) < DTINY )then
            ascending8 = 0
        endif
    end function ascending8

    subroutine iqsortr8(A, size_of_array)
        integer(8) :: size_of_array
        real(8), intent(inout) :: A(size_of_array)
        integer, parameter :: size_of_element = 8
#ifdef __INTEL_COMPILER
        call QSort_intel_double(A, size_of_array, size_of_element, ascending8)
#else
        print *," qsort not available (this is not an INTEL compiler)"
#endif
    end subroutine iqsortr8

end module intel_qsort
