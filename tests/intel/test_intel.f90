program test_intel
    include 'simple_lib.f08'
    use intel_blas
    use simple_image
#ifdef INTEL
    use vector_stat
#endif
    integer :: i, ierr,istat, cuVer, cuMem, cuFree
    logical :: errflag
    integer(timer_int_kind) :: t1
    write (*,'(A)') ' INTEL TEST '

    errflag=.false.
!    call test_omp(errflag)
#ifdef INTEL
    call test_intel_blas(errflag)
    call test_mkl_fftw(errflag)
    call basic_sp_real_dft_2d(errflag)
    call config_thread_limit(errflag)
    call MKL_VSL_GAUSSIAN
#else
    write (*,'(A)') 'INTEL TEST: unable to run INTEL/MKL with this compiler'
#endif


    if (errflag)then
        write (*,'(A)') 'INTEL TEST: ALL TESTS PASSED'
    else
        write (*,'(A)') 'INTEL TEST: FAILED'
    endif

end program simple_test_intel
