! Fortran 95 free source form program kinds.f90 by J F Harper, 
! Mathematics, Victoria University Wellington New Zealand, 20 Dec 2017.
! email: john DOT harper AT vuw DOT ac DOT nz
! It prints various properties of the available real and integer kinds,
! It compiles and runs correctly with Sun/Oracle f95, Intel ifort, g95,
! and with gfortran if the -freal... options are not used (see 
! WARNING 1 below.) If there are at most 5 real kinds with different 
! precisions or 6 integer kinds with different ranges, all are tested
! (but see WARNING 2 below.)
! If not, you will be told that there may be more kinds.
!
! IEEE arithmetic is in the f2003 standard but not f95, and compilers 
! may but need not provide its three intrinsic modules. Only 5 of its
! properties are tested: whether 'NAN','INF', -INF' are readable into 
! variables of each real kind, and if so whether NaN /= NaN, Inf > huge,
! -Inf < -huge, and what you get from reading 'INF'. (Some compilers 
! give Inf, some give +Inf, and g95 real(10) gives NaN). This program 
! does not use the intrinsic modules; some compilers that don't provide 
! them do support NaN and the Infs. The program also does not test 
! overflowing arithmetic operations. If it did, some compilers would
! not compile it, some would crash at run-time, some would run happily.
!
! WARNING 1: gfortran has options that change the precision of various 
!    real and integer kinds. Using -freal-M-real-N (where M = 4 or 8,
!    N = 4, 8, 10 or 16, but N /= M) with this program may fail to find
!    some valid kinds, because selected_real_kind ignores these options
!    and literal constants with _ (e.g. 1.0_4) are unchanged. 
!    These options can also make krN /= rkN below.
! WARNING 2: Silverfrost has (had?) a bug giving -1 for selected_int_kind(0)
!    so this program starts with selected_int_kind(1).That prevents it
!    detecting an integer kind using 4 or fewer bits even if one exists.
! WARNING 3: Intel Fortran (ifort) has options -assume byterecl and
!    -assume nobyterecl that return RECL in bytes or longwords. That affects
!    the values of IOlength given by this program; see
!    https://software.intel.com/sites/default/files/m/f/8/5/8/0/6366-ifort.txt
! Recent revisions:
! 14 May 2015 Warning 2 provided.
!  8 Jan 2016 reading -Inf tested
! 11 May 2016 iolen, iolen1 initialised to avoid gfortran warning
! 20 Dec 2017 cninf = "NAN INF -INF" not "NaN Inf -Inf" to agree with
!               f2003 standard clause 10.6.1.2.1 after note 10.10.  
! 20 Dec 2017 IOlengths given; tf now says 'Yes' or ' No' or '   '; Warning 3
module realintkinds
  implicit none
  private
  public realkinds,intkinds
! set up real kinds (compilers must offer at least two of them)
  integer  ,parameter:: rk1 = selected_real_kind(1), maxkr = 5
  real(rk1),parameter::  r1 = 1
  integer  ,parameter:: kr1 = kind(r1) ! see WARNING 1
  integer  ,parameter::srk2 = selected_real_kind(precision(r1)+1)
  integer  ,parameter:: rk2 = (srk2+rk1+sign(1,srk2)*(srk2-rk1))/2
  real(rk2),parameter::  r2 = 1
  integer  ,parameter:: kr2 = kind(r2)
  integer  ,parameter::srk3 = selected_real_kind(precision(r2)+1)
  integer  ,parameter:: rk3 = (srk3+rk2+sign(1,srk3)*(srk3-rk2))/2
  real(rk3),parameter::  r3 = 1
! rk3 = srk3 if that's a valid real kind, rk2 if not
  integer  ,parameter:: kr3 = kind(r3)
  integer  ,parameter::srk4 = selected_real_kind(precision(r3)+1)
  integer  ,parameter:: rk4 = (srk4+rk3+sign(1,srk4)*(srk4-rk3))/2
  real(rk4),parameter::  r4 = 1
! rk4 = srk4 if that's a valid real kind, rk3 if not
  integer  ,parameter:: kr4 = kind(r4)
  integer  ,parameter::srk5 = selected_real_kind(precision(r4)+1)
  integer  ,parameter:: rk5 = (srk5+rk4+sign(1,srk5)*(srk5-rk4))/2
  real(rk5),parameter::  r5 = 1
! rk5 = srk5 if that's a valid real kind, rk4 if not
  integer  ,parameter:: kr5 = kind(r5)
  integer  ,parameter:: kraray(0:maxkr) = (/-1,kr1,kr2,kr3,kr4,kr5/)
! set up integer kinds (compilers must offer at least one)
  integer    ,parameter::maxik = 6, dp = kind(1d0), &
       & ik1 = selected_int_kind(1) ! Can't have (0): Silverfrost bug
  integer(ik1),parameter::  i1 = 1_ik1
  integer    ,parameter:: sik2 = selected_int_kind(range(i1)+1), &
       & ik2 = (sign(1,sik2)*(sik2-ik1) + sik2+ik1)/2
  integer(ik2),parameter::  i2 = 1_ik2
! ik2 = sik2 if that's a valid integer kind, ik1 if not
  integer    ,parameter:: sik3 = selected_int_kind(range(i2)+1), &
       & ik3 = (sign(1,sik3)*(sik3-ik2) + sik3+ik2)/2
  integer(ik3),parameter::  i3  = 1_ik3
! ik3 = sik3 if that's a valid integer kind, ik2 if not
  integer    ,parameter:: sik4 = selected_int_kind(range(i3)+1), &
       & ik4 = (sign(1,sik4)*(sik4-ik3) + sik4+ik3)/2
  integer(ik4),parameter::  i4 = 1_ik4
! ik4 = sik4 if that's a valid integer kind, ik3 if not
  integer    ,parameter:: sik5 = selected_int_kind(range(i4)+1), &
       & ik5 = (sign(1,sik5)*(sik5-ik4) + sik5+ik4)/2
  integer(ik5),parameter::  i5 = 1_ik5
! ik5 = sik5 if that's a valid integer kind, ik4 if not
  integer    ,parameter:: sik6 = selected_int_kind(range(i5)+1), &
       & ik6 = (sign(1,sik6)*(sik6-ik5) + sik6+ik5)/2
  integer(ik6),parameter::  i6 = 1_ik6
! ik6 = sik6 if that's a valid integer kind, ik5 if not
contains
  character(3) function tf(ok,ios) ! returns 'Yes', ' No', or '   '
    logical, intent(in)::  ok
    integer, intent(in)::     ios
    tf =  merge(merge('Yes',' No',ok),'   ',ios==0)
  end function tf
 
  subroutine realkinds
    integer  :: i,nkr ! nkr = min(4, no. of different real kinds)
    integer,parameter:: nilist = 9 ! no. of integer properties of reals
    real(kr5):: rlist(maxkr,3)
    integer  :: ilist(maxkr,nilist),ios(maxkr),iolen=0,iolen1=0,tnum=26
    logical,dimension(maxkr) :: nanok,posinfok,neginfok
    character(3),dimension(maxkr) :: neg0
    real(kr1):: ninf1(3),neg01 = -0.0_kr1
    real(kr2):: ninf2(3),neg02 = -0.0_kr2
    real(kr3):: ninf3(3),neg03 = -0.0_kr3
    real(kr4):: ninf4(3),neg04 = -0.0_kr4
    real(kr5):: ninf5(3),neg05 = -0.0_kr5
    character(32):: ifmt, rfmt, cfmt, tfmt, cninf = "NAN INF -INF",&
         & linewithE*128,crlist(3)*11,cilist(nilist)*15,ck(maxkr)*8,&
         & fmtfmt = '(99(A,:,I0,:))'
    character(4) :: ninfkinds(maxkr) 
    data cilist/'kind','minexponent','maxexponent','range','digits', &
         & 'precision','radix','bits needed','  from iolength'/
    data crlist/'epsilon','huge','tiny'/
    do nkr = maxkr,1,-1 ! loop to set nkr = no. of different precisions
       if(kraray(nkr) /= kraray(nkr-1)) exit
    end do
!    print "(A,99I4)",'Debug: rkN =',rk1,rk2,rk3,rk4,rk5 
!    print "(A,99I4)",'Debug: krN =',kr1,kr2,kr3,kr4,kr5 
    write(ifmt ,fmtfmt) '(2X,A,T',tnum,',',nkr,'I11)'  
    write(rfmt ,fmtfmt) '(2X,A,T',tnum,',',nkr,'ES11.2E4)'
    write(cfmt ,fmtfmt) '(2X,A,T',tnum,',',nkr,'A11)'
    write(tfmt,fmtfmt) '(1X,A,T',tnum+7,',A4,:,6X,99(A5,:,6X))'  
    rlist(1,:) = real((/epsilon(r1),huge(r1),tiny(r1)/),kr5)
    rlist(2,:) = real((/epsilon(r2),huge(r2),tiny(r2)/),kr5)
    rlist(3,:) = real((/epsilon(r3),huge(r3),tiny(r3)/),kr5)
    rlist(4,:) = real((/epsilon(r4),huge(r4),tiny(r4)/),kr5)
    rlist(5,:) = real((/epsilon(r5),huge(r5),tiny(r5)/),kr5)
    ilist(1,1:7) = (/kr1,minexponent(r1),maxexponent(r1),range(r1), &
         & digits(r1),precision(r1),radix(r1)/)
    ilist(2,1:7) = (/kr2,minexponent(r2),maxexponent(r2),range(r2), &
         & digits(r2),precision(r2),radix(r2)/)
    ilist(3,1:7) = (/kr3,minexponent(r3),maxexponent(r3),range(r3), &
         & digits(r3),precision(r3),radix(r3)/)
    ilist(4,1:7) = (/kr4,minexponent(r4),maxexponent(r4),range(r4), &
         & digits(r4),precision(r4),radix(r4)/)
    ilist(5,1:7) = (/kr5,minexponent(r5),maxexponent(r5),range(r5), &
         & digits(r5),precision(r5),radix(r5)/)
    inquire(iolength=iolen1) 1.0
    do i = 1,nkr
       if (i==1) inquire(iolength=iolen) r1
       if (i==2) inquire(iolength=iolen) r2
       if (i==3) inquire(iolength=iolen) r3
       if (i==4) inquire(iolength=iolen) r4
       if (i==5) inquire(iolength=iolen) r5
       ilist(i,9) = iolen*bit_size(1)/iolen1
       if (iolen == iolen1) then
          ck(i) = 'single'
       else if (iolen==2*iolen1) then
          ck(i) = 'double'
       else if (iolen==4*iolen1.and.ilist(i,6)>4*ilist(1,6)) then
          ck(i) = 'quad'
       else if (iolen >2*iolen1.and.iolen<=4*iolen1) then
          ck(i) = 'extended'
       else
          ck(i) = merge('lower ','higher',iolen < iolen1)
       end if
       ilist(i,8) = ilist(i,5) + Lr(ilist(i,7),real(ilist(i,3),kr5))&
            & + merge(2,1,index(ck(i),'extended')>0)
    end do
    print *
    print cfmt,'"Precision"  ',adjustr(ck(1:nkr)) 
! Print integer properties of real kinds (kind,minexpoment,maxexponent,
!     range, digits, precision, radix, bits needed (2 ways)
    print ifmt,(cilist(i),ilist(1:nkr,i),i=1,nilist)
! Print real properties of real kinds (epsilon,huge,tiny)
    do i = 1,3
       write(linewithE,rfmt) crlist(i),rlist(1:nkr,i)
       print "(A)",trim(elowcase(linewithE))
       print ifmt,merge('  ~','  =',i==2)//' 2.0**      ', &
            & Lr(2,rlist(1:nkr,i))
    end do
! Read NAN, +INF,-INF, write array neg0
    do i = 1,nkr
       if(i==1)read(cninf,*,iostat=ios(i)) ninf1
       if(i==2)read(cninf,*,iostat=ios(i)) ninf2
       if(i==3)read(cninf,*,iostat=ios(i)) ninf3
       if(i==4)read(cninf,*,iostat=ios(i)) ninf4
       if(i==5)read(cninf,*,iostat=ios(i)) ninf5
    end do
    write(neg0,'(F3.0)') neg01,neg02,neg03,neg04,neg05
! Report on readability of NAN etc into each real kind
    print tfmt,' NaN,Inf,-Inf all readable?',(tf(ios(i)==0, 0),i=1,nkr)
    if(any(ios(1:nkr)==0))then
       nanok = (/ninf1(1)/=ninf1(1),ninf2(1)/=ninf2(1),&
            & ninf3(1)/=ninf3(1),ninf4(1)/=ninf4(1),ninf5(1)/=ninf5(1)/)
       posinfok = (/ninf1(2)>huge(r1),ninf2(2)>huge(r2),&
            & ninf3(2)>huge(r3),ninf4(2)>huge(r4),ninf5(2)>huge(r5)/)
       neginfok = (/ninf1(3)<-huge(r1),ninf2(3)<-huge(r2),&
            & ninf3(3)<-huge(r3),ninf4(3)<-huge(r4),ninf5(3)<-huge(r5)/)
       print tfmt,' NaN /= NaN?  (should be Yes)', &
            (tf(nanok(i),ios(i)),i=1,nkr)
       print tfmt,'+Inf > huge?  (should be Yes)', &
            (tf(posinfok(i),ios(i)),i=1,nkr)
       print tfmt,'-Inf < -huge? (should be Yes)', &
            (tf(neginfok(i),ios(i)),i=1,nkr)
       write(ninfkinds,'(F4.0)') &
            ninf1(2),ninf2(2),ninf3(2),ninf4(2),ninf5(2) 
       print tfmt," Reading 'Inf' gives:",ninfkinds(1:nkr)
      write(ninfkinds,'(F4.0)') &
            ninf1(3),ninf2(3),ninf3(3),ninf4(3),ninf5(3) 
       print tfmt," Reading '-Inf' gives:",ninfkinds(1:nkr)
       print *, '(IEEE modules and '// &
            'arithmetic NaNs and overflows were not tested.)'
       print *
    end if
    print tfmt,'-0.0 printed with - sign:',&
         (tf(neg0(i)=='-0.',0),i=1,nkr)
    print *,'(Compiler options may change how -0.0 is printed)'
    print *
    print *,'Note: '//trim(merge( &
         & 'No higher-precision real kind is available.', &
         & 'Warning: there may be more real kinds.     ',nkr<maxkr)) 
  end subroutine realkinds
  
  elemental integer function Lr(r,x) ! Gives nearest integer to log_r(x)
    integer,intent(in)   ::     r    ! Needed as g95 quad precision log
    real(kr5),intent(in) ::       x  ! is not yet provided, and kr5 is
    real(kr2) sigfigs, tenpower      ! quad precision in some systems. 
    character(45) xchar        
    write(xchar,"(ES45.34E4)") x
    read(xchar(:index(xchar,'E')-1 ),*) sigfigs
    read(xchar( index(xchar,'E')+1:),*) tenpower
    Lr = nint((log(sigfigs)+log(10d0)*tenpower)/log(real(r)))
  end function Lr

! Give useful properties of the (first 6) integer kinds
  subroutine intkinds
    integer,parameter::ikarray(0:maxik) = (/-1,ik1,ik2,ik3,ik4,ik5,ik6/)
    integer:: i,i1c,i16c,iolen=0,k,nik ! nik will be min(6,number of int. kinds)
    integer(ik6):: ilist(maxik,6)
    character  :: ck(maxik)*22 = ' ', linewithE*128, near*3
    character(*),parameter:: fsu = ' file storage unit',&
         surprise = ' - surprising?'
    ilist(1,1:3) = (/digits(i1),radix(i1),range(i1)/) ! RHS default int.
    ilist(2,1:3) = (/digits(i2),radix(i2),range(i2)/)
    ilist(3,1:3) = (/digits(i3),radix(i3),range(i3)/)
    ilist(4,1:3) = (/digits(i4),radix(i4),range(i4)/)
    ilist(5,1:3) = (/digits(i5),radix(i5),range(i5)/)
    ilist(6,1:3) = (/digits(i6),radix(i6),range(i6)/)
! ilist(:,4) becomes iolen in do loop later
! Doing ilist(:,5:6) element-by-element works around an ifort 12.1 bug
    ilist(1,5)   = bit_size(i1) ! RHS kind ik1
    ilist(2,5)   = bit_size(i2) ! RHS kind ik2
    ilist(3,5)   = bit_size(i3) ! RHS kind ik3
    ilist(4,5)   = bit_size(i4) ! RHS kind ik4
    ilist(5,5)   = bit_size(i5) ! RHS kind ik5
    ilist(6,5)   = bit_size(i6) ! RHS kind ik6
    ilist(1,6)   = huge(i1) ! RHS kind ik1
    ilist(2,6)   = huge(i2) ! RHS kind ik2
    ilist(3,6)   = huge(i3) ! RHS kind ik3
    ilist(4,6)   = huge(i4) ! RHS kind ik4
    ilist(5,6)   = huge(i5) ! RHS kind ik5
    ilist(6,6)   = huge(i6) ! RHS kind ik6
    do nik = maxik,1,-1 ! this loop finds nik
       if (ikarray(nik) /= ikarray(nik-1)) exit
    end do
    do k = 1,nik
       if (k==1) inquire(iolength=iolen) i1
       if (k==2) inquire(iolength=iolen) i2
       if (k==3) inquire(iolength=iolen) i3
       if (k==4) inquire(iolength=iolen) i4
       if (k==5) inquire(iolength=iolen) i5
       if (k==6) inquire(iolength=iolen) i6 
       ilist(k,4) = iolen
       if (ikarray(k)==kind(1)) ck(k) = '(default integer kind)'
    end do
    
    print "(/,A)",' kind digits radix range iolen bit_  huge '
    print "(  A)",'                               size '
    do k = 1,nik
       print "(I4,4I6,I5,4X,I0,1X,A)", &
            & ikarray(k),(ilist(k,i),i=1,6),trim(ck(k))
       near = merge(' = ',' ~ ',ilist(k,6)<1000)
       write(linewithE,"(T42,A,T55,A,ES10.3)" ) &
            & e2less1(k,ilist),near,real(ilist(k,6),dp)
       print "(A)",trim(elowcase(linewithE))
    end do
    print *,'Note: ',trim(merge( &
         & 'No higher integer kind is available.     ',&
         & 'Warning: there may be more integer kinds.',nik<maxik))
    print "(/,A,/)",' IOlength for 1,16 characters:'
    inquire(iolength=i1c) 'A'
    inquire(iolength=i16c)'0123456789ABCDEF'  
    print "(1X,A,T24,I3,A)",'for  1 character:',i1c,fsu//plural(i1c),&
         'for 16 characters:',i16c,fsu//plural(i16c)//&
         trim(merge(surprise,repeat(" ",len(surprise)),i16c/=i1c*16))
  end subroutine intkinds

  character(1) function plural(n)
    integer,  intent(in) ::    n
    plural = merge('s',' ',n>1)
  end function plural
    
  character(12) function e2less1(k,ilist) 
! k = kind; checks huge = radix**digits-1
    integer     , intent(in) ::  k
    integer(ik6), intent(in) ::    ilist(:,:)
    integer(ik6) :: OK
    character(1):: op
    OK = ilist(k,6)-2_ik6**ilist(k,1)+1_ik6  
    op = merge('=',merge('>','<',OK>0),OK==0)
    write(e2less1,"(2(A,I0),A)")op//' ',ilist(k,2), &
         & '**',ilist(k,1),' - 1'
  end function e2less1

  function elowcase(      string) result (elow) ! convert E to e
    character,intent(in)::string*(*)
    character(len_trim(string))::         elow
    integer i
    forall(i=1:len(elow)) &
         & elow(i:i) = merge('e',string(i:i),string(i:i)=='E') 
  end function elowcase

end module realintkinds

program kinds
  use realintkinds, only: realkinds,intkinds
  implicit none
  print "(/,A)", ' Properties of real kinds:'
  call realkinds
  print "(/,A)", ' Properties of integer kinds:'
  call intkinds
  print "(/,A)",' ISO_FORTRAN_ENV was not tested: it''s an F2003 feature.'
end program kinds
