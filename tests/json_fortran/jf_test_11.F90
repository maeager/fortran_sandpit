!*****************************************************************************************
!> author: Izaak Beekman
!  date: 3/13/2015
!
! Module for the 11th unit test to test unicode support if enabled.

module jf_test_11_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

    private
    public :: test_11

    character(len=*),parameter :: dir = '../files/inputs/'  !! working directory
#ifdef USE_UCS4
    character(len=*),parameter :: unicode_file = 'hello-world-ucs4.json'
#endif
    character(len=*),parameter :: ascii_equivalent = 'hello-world-ascii.json'

contains

    subroutine test_11(error_cnt)

    !! Read the file and extract some data from it.

    implicit none

    integer,intent(out) :: error_cnt
    character(kind=json_CK,len=:),allocatable :: cval
    type(json_file) :: json    !the JSON structure read from the file:
# ifdef USE_UCS4
    type(json_file) :: clone
# endif

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 11'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

# ifdef USE_UCS4
    ! parse the json file:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'parsing file: '//dir//unicode_file

    call json%load_file(filename = dir//unicode_file)

    if (json%failed()) then    !if there was an error reading the file

        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1

    else

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'reading data from file...'

        write(error_unit,'(A)') ''
        call json%get('UCS4 support?', cval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'UCS4 support? '//cval
        end if

        write(error_unit,'(A)') ''
        call json%get('hello world.Amharic', cval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'hello world.Amharic : '//cval
        end if

        write(error_unit,'(A)') ''
        call json%get('hello world.Portuguese', cval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'hello world.Portuguese : '//cval
        end if

        write(error_unit,'(A)') ''
        call json%get('hello world.Russian', cval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'hello world.Russian : '//cval
        end if

        write(error_unit,'(A)') ''
        call json%get('hello world.Hebrew', cval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'hello world.Hebrew : '//cval
        end if

        write(error_unit,'(A)') ''
        call json%get('hello world.Urdu', cval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'hello world.Urdu : '//cval
        end if

        write(error_unit,'(A)') ''
        call json%print_to_string(cval)
        if (json%failed()) then
           call json%print_error_message(error_unit)
           error_cnt = error_cnt + 1
        else
           write(error_unit,'(A)') 'The contents of the file were:'
           write(error_unit,'(A)') cval
        end if

        write(error_unit,'(A)') ''
        call clone%load_from_string(cval)
        if ( clone%failed()) then
           call clone%print_error_message(error_unit)
           error_cnt = error_cnt + 1
        end if

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'Printing same file, but now to stdout:'
        call clone%print_file(output_unit)
        if (clone%failed()) then
           call clone%print_error_message(error_unit)
           error_cnt = error_cnt + 1
        end if

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'Writing json file object to "../files/'//unicode_file//'"'
        call clone%print_file('../files/'//unicode_file)
        if ( clone%failed() ) then
           call clone%print_error_message(error_unit)
           error_cnt = error_cnt + 1
        end if

    end if

    ! clean up
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'destroy...'
    call json%destroy()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call clone%destroy()
    if (clone%failed()) then
        call clone%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

# endif
    ! parse the json file:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'parsing file: '//dir//ascii_equivalent
    write(error_unit,'(A)') 'This is the ascii equivalent of "../files/inputs/hello-world-ucs4.json"'

    call json%load_file(filename = dir//ascii_equivalent)

    if (json%failed()) then    !if there was an error reading the file

        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1

    else

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'reading data from file...'

        write(error_unit,'(A)') ''
        call json%get('UCS4 support?', cval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'UCS4 support? '//cval
        end if

        write(error_unit,'(A)') ''
        call json%get('hello world.Amharic', cval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'hello world.Amharic : '//cval
        end if

        write(error_unit,'(A)') ''
        call json%get('hello world.Portuguese', cval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'hello world.Portuguese : '//cval
        end if

        write(error_unit,'(A)') ''
        call json%get('hello world.Russian', cval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'hello world.Russian : '//cval
        end if

        write(error_unit,'(A)') ''
        call json%get('hello world.Hebrew', cval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'hello world.Hebrew : '//cval
        end if

        write(error_unit,'(A)') ''
        call json%get('hello world.Urdu', cval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'hello world.Urdu : '//cval
        end if

        write(error_unit,'(A)') ''
        call json%print_to_string(cval)
        if (json%failed()) then
           call json%print_error_message(error_unit)
           error_cnt = error_cnt + 1
        else
           write(error_unit,'(A)') 'The contents of the file were:'
           write(error_unit,'(A)') cval
        end if

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'Printing same file, but now to stdout:'
        call json%print_file(output_unit)
        if (json%failed()) then
           call json%print_error_message(error_unit)
           error_cnt = error_cnt + 1
        end if

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'Writing json file object to "../files/'//ascii_equivalent//'"'
        call json%print_file('../files/'//ascii_equivalent)
        if ( json%failed() ) then
           call json%print_error_message(error_unit)
           error_cnt = error_cnt + 1
        end if

    end if

    ! clean up
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'destroy...'
    call json%destroy()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine test_11

end module jf_test_11_mod
!*****************************************************************************************

#ifndef INTERGATED_TESTS
!*****************************************************************************************
program jf_test_11

    !! 11th unit test.

    use jf_test_11_mod , only: test_11
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_11(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_11
!*****************************************************************************************
#endif
