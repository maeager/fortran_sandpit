program simple_test_iso_varying_strings
include 'simple_lib.f08'
use iso_varying_strings

    implicit none
    type :: vstring !! dummy public iso_varying_string type
        character(LEN=1), dimension(:), allocatable :: chars
    end type vstring
    type :: field_data_t
        type(varying_string), dimension(:), allocatable :: name
    end type field_data_t

    call test_pr79344
    call test_VST28
    call test_pr65894_fix
    call test_elemental_scalar_args
    !    call  VST_2
   call  test_iso_varying_strings_module
contains

! iso_varying_string module test
    subroutine test_iso_varying_strings_module
        ! iso_varying_string module test
        use, intrinsic :: ISO_FORTRAN_ENV
        implicit none
        integer iostat_code
        integer line_count
        type(varying_string) :: line
        integer :: write_unit,read_unit
        character(len=*), parameter :: test_file = 'varyingstringtest.txt'

        ! open the test input file
        call fopen( write_unit, file=test_file, iostat=iostat_code)
        write(write_unit,'(a)') "Line one"
        write(write_unit,'(a)') "Line                two"
        write(write_unit,'(a)') "Last line"
        call fclose (write_unit)
        !*****************************************************************************
        ! open the test input file
        open( read_unit, file=test_file, iostat=iostat_code, status='old',  &
            action='read' )
        if (iostat_code /= 0) then
            write (*,*) 'open on ' // trim(test_file) // ' gave iostat of ', iostat_code
            stop
        end if

        line_count = 1
        do
            call get(read_unit, line, iostat=iostat_code)
            if( iostat_code /= 0 .and. (.not.(is_iostat_end(iostat_code) .or. is_iostat_eor(iostat_code))) )then
                write (*,*)'line:',line_count, ' gave an iostat of:', iostat_code
                stop
            endif
            if (iostat_code /= 0 .and. is_iostat_end(iostat_code)) then
                write (*,*)'line:',line_count, ' gave an iostat of:', iostat_code
                exit
            end if
            write (*,*) 'line:', line_count, ' gave an iostat of:',iostat_code, ' and had content "', char(line),'"'
            line_count = line_count + 1
        end do

        close(read_unit)

    end subroutine test_iso_varying_strings_module
    subroutine  test_pr79344
        ! use iso_varying_strings, string_t => varying_string
        implicit none


        type(field_data_t) :: model, model2
        allocate(model%name(2))
        model%name(1) = "foo"
        model%name(2) = "bar"
        call copy_pr79344(model, model2)

    end subroutine test_pr79344
    subroutine copy_pr79344(prt, prt_src)
        implicit none
        type(field_data_t), intent(inout) :: prt
        type(field_data_t), intent(in) :: prt_src
        integer :: i
        if (allocated (prt_src%name)) then
            if (prt_src%name(1) /= "foo") STOP 1
            if (prt_src%name(2) /= "bar") STOP 2

            if (allocated (prt%name))  deallocate (prt%name)
            allocate (prt%name (size (prt_src%name)), source = prt_src%name)
            ! The issue was, that prt_src was empty after sourced-allocate.
            if (prt_src%name(1) /= "foo") STOP 3
            if (prt_src%name(2) /= "bar") STOP 4
            if (prt%name(1) /= "foo") STOP 5
            if (prt%name(2) /= "bar") STOP 6
        end if
    end subroutine copy_pr79344


    subroutine test_VST28
        ! use iso_varying_strings
        character(len=10) :: char_a
        type(VARYING_STRING) :: res
      !  integer::i
        char_a = "abcdefghij"
      !  res = var_str_reserve(2)!len(char_a))
       ! res%chars=' '
        res = insert(res, 1, char_a(5:5), 1 )
        res = insert(res, 2, char_a(6:6),1 )
        if(len(res) /= 2 .or. .not.any(res /= ['e','f'])) then
            write(*,*) 'ERROR: should be ef, got: ', trim(char(res)), len(res)
            STOP 1
        end if
    end subroutine test_VST28

    subroutine test_pr65894_fix
        use beams
        use model_data
        type(model_data_t) :: model
        call model%init_sm_test()
        call beam_1 (6)
        call beam_2 (6, model)
    end subroutine test_pr65894_fix


    ! Tests the fix for elemental functions not being allowed in
    ! specification expressions in pure procedures.
    !
    ! Testcase from iso_varying_string by Rich Townsend <rhdt@star.ucl.ac.uk>
    ! The allocatable component has been changed to a pointer for this testcase.

    pure function char_auto (string) result (char_string)
        type(varying_string), intent(in) :: string
        character(LEN=len(string))       :: char_string ! Error was here
        char_string = ""
    end function char_auto


    subroutine test_elemental_scalar_args

        ! Test the fix for PR55618, in which character scalar function arguments to
        ! elemental functions would gain an extra indirect reference thus causing
        ! failures in Vst17.f95, Vst 30.f95 and Vst31.f95 in the iso_varying_string
        ! testsuite, where elemental tests are done.
        !
        ! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
        !
        integer, dimension (2) :: i = [1,2]
        integer :: j = 64
        character (len = 2) :: chr1 = "lm"
        character (len = 1), dimension (2) :: chr2 = ["r", "s"]
        if (any (foo (i, bar()) .ne. ["a", "b"])) STOP 1! This would fail
        if (any (foo (i, "xy") .ne. ["x", "y"])) STOP 2! OK - not a function
        if (any (foo (i, chr1) .ne. ["l", "m"])) STOP 3! ditto
        if (any (foo (i, char (j)) .ne. ["A", "B"])) STOP 4! This would fail
        if (any (foo (i, chr2) .ne. ["s", "u"])) STOP 5! OK - not a scalar
        if (any (foo (i, bar2()) .ne. ["e", "g"])) STOP 6! OK - not a scalar function
    end subroutine test_elemental_scalar_args
    elemental character(len = 1) function foo (arg1, arg2)
            integer, intent (in) :: arg1
            character(len = *), intent (in) :: arg2
            if (len (arg2) > 1) then
                foo = arg2(arg1:arg1)
            else
                foo = char (ichar (arg2) + arg1)
            end if
        end function foo
        character(len = 2) function bar ()
            bar = "ab"
        end function bar
        function bar2 () result(res)
            character (len = 1), dimension(2) :: res
            res = ["d", "e"]
        end function bar2


!     subroutine  VST_2
!     !    USE ISO_VARYING_STRINGS
!         IMPLICIT NONE

!         CHARACTER(LEN=5)     :: char_arb(2)
!         CHARACTER(LEN=1)     :: char_elm(10)
!         equivalence (char_arb, char_elm)
!         type(VARYING_STRING) :: str_ara(2)
!  !       character(len=:),allocatable :: str_arb
!         char_arb(1)= "Hello"
!         char_arb(2)= "World"
!         str_ara = char_arb
!         !allocate(str_arb(len(str_ara)))
! !        call op_assign_CH_VS(str_arb,str_ara)
!         if ( any( (str_ara(1) /= assign_CH_CHARRAY(char_elm(1:5)) ) ) )STOP 1
!         if ( any( (str_ara(2) /= assign_CH_CHARRAY(char_elm(6:10)) ) ) ) STOP 2
!     end subroutine VST_2


end program simple_test_iso_varying_strings
