! Program quaternion.f90 to combine finite rotations by using quaternions.
! By J F Harper, Mathematics, Victoria University of Wellington, New Zealand,
! john DOT harper AT vuw DOT ac DOT nz 27 Sep 2012 revised 24 Apr 2017.
!  
! The method is in Le Pichon et al, "Plate Tectonics", Elsevier, 1973.
! It asks for and reads from standard input the lat, long, rotation angle 
! of two Euler poles in degrees, and when it has read those six numbers it
! writes to standard output the quaternion form of each and the rotation 
! recalculated from that quaternion form, and then it will do the same
! for the resultant finite rotation in both possible orders, by using the
! normal rules for multiplying quaternions (which do not commute). 
! To do a lot of poles put the lat,long,angle in a file (with each group of 
! six numbers starting on a new line). If execfile was the result of 
! compiling this program and you are using a Unix/Linux system you can use 
! ./execfile < infile > outfile 
! if the input and output files are called infile and outfile.
! Output longitudes will be between -180.0 and +180.0 
!
! The arithmetic is in kind dp = selected_real_kind(15), which is double
! precision in many systems. Example: if you make no changes in the program,
! and infile is this line without the "!" : 
! 90 0 -90 0 90 90
! then outfile will be:
! Give lat1,lon1,angle1, lat2,lon2,angle2 (deg) of 2 rotations to combine
! q1 quaternion using format (A,/,4F19.13)
!    0.7071067812   -0.0000000000   -0.0000000000   -0.7071067812
! q1 lat lon angle 
!  -90.0000000000    0.0000000000   90.0000000000
! q2 quaternion
!    0.7071067812    0.0000000000    0.7071067812    0.0000000000
! q2 lat lon angle
!    0.0000000000   90.0000000000   90.0000000000
! q1*q2 quaternion
!    0.5000000000    0.5000000000    0.5000000000   -0.5000000000
! q1*q2 lat lon angle 
!  -35.2643896828   45.0000000000 120.0000000000
! q2*q1 quaternion
!    0.5000000000   -0.5000000000    0.5000000000   -0.5000000000
! q2*q1 lat lon angle 
!  -35.2643896828 135.0000000000 120.0000000000
! Give lat1,lon1,angle1, lat2,lon2,angle2 (deg) of 2 rotations to combine
!
! Fortran technicalities: free source form Fortran 95 or later.
! Quaternion a + b*i + c*j + d*k is treated as the derived type qu with one 
! component, an array(0:3) of (/ a,b,c,d /). 
! Public entities in module qarithmetic:
!   Constants: 
!      integer dp = selected_real_kind(15);
!      character(*) qformat = '(A,/,4F19.13)'
!   Derived type qu with one private component, real(dp) qarray(0:3)    
!   Public pure functions: 
!      q2a to convert quaternion to real(dp) array (0:3); 
!      a2q to convert real(dp) array (0:3) to quaternion;
!      pole2q to find the quaternion of the finite rotation (Euler pole)
!         through angle ang about lat,lon; all 3 angles in degrees;
!      q2pole to find the Euler pole of a quaternion;
!   Operations: 
!      * to multiply quaternion by quaternion, real(dp) or integer
! (Quaternion operations +,-,/ could be done with q2a and a2q e.g. 
! q1+q2 = a2q(q2a(q1)+q2a(q2)), but plate tectonics does not need them.)

module qarithmetic
  implicit none
  private
  public dp, qformat, qu, operator(*), pole2q, q2pole, a2q, q2a
  integer ,parameter:: dp = selected_real_kind(15)
  real(dp),parameter:: zero = 0._dp, one = 1._dp, two = 2._dp, &
       pi = 3.1415926535897932384626433832795_dp, pi180=pi/180, &
       roothalf = 0.707106781186547524400844362104849_dp ! sqrt(0.5_dp )
  character(*),parameter:: qformat = '(A,/,4F19.13)'
  type qu
     real(dp) qarray(0:3)
  end type qu
  interface operator(*)
     module procedure qtimesq,qtimesdp,qtimesi,dptimesq,itimesq
  end interface operator(*)
contains
  pure function qtimesq( x,y)
    type(qu) :: qtimesq
    type(qu),intent(in)::x,y
    real(dp) ax(0:3),ay(0:3)
    ax = q2a(x)
    ay = q2a(y)
    qtimesq = a2q( (/ ax(0)*ay(0)-sum(ax(1:3)*ay(1:3)), & ! component 0
         ax(0)*ay(1:3) + ax(1:3)*ay(0) + (/ &             ! components 1:3
         ax(2)*ay(3)-ax(3)*ay(2),ax(3)*ay(1)-ax(1)*ay(3), &
         ax(1)*ay(2)-ax(2)*ay(1) /) /) )
  end function qtimesq

  pure function qtimesdp(q,x)
    type(qu) :: qtimesdp
    type(qu),intent(in)::q
    real(dp),intent(in)::  x
    qtimesdp = a2q(q2a(q)*x)
  end function qtimesdp

  pure function qtimesi( q,i)
    type(qu) :: qtimesi
    type(qu),intent(in)::q
    integer,intent(in)::   i
    qtimesi = a2q(q2a(q)*i)
  end function qtimesi

  pure function dptimesq(x,q)
    type(qu) :: dptimesq 
    real(dp),intent(in)::x
    type(qu),intent(in)::  q
    dptimesq = a2q(q2a(q)*x) 
  end function dptimesq

  pure function itimesq(i,q)
    type(qu) :: itimesq 
    integer,intent(in)::i
    type(qu),intent(in):: q
    itimesq = a2q(q2a(q)*i) 
  end function itimesq

  pure function q2a(     x)
    real(dp) :: q2a(0:3)
    type(qu),intent(in)::x
    q2a = x%qarray
  end function q2a

  pure function a2q(     x)
    type(qu) :: a2q
    real(dp),intent(in)::x(0:3)
    a2q%qarray = x
  end function a2q

  pure function pole2q(lat,lon,angl)
    type(qu) :: pole2q
    real(dp),intent(in)::lat,lon,angl ! degrees
    real(dp):: cang2,sang2,clat,slat,clon,slon
    cang2 = cos(angl*pi180/two)
    sang2 = sin(angl*pi180/two)
    clat  = cos(lat*pi180)
    clon  = cos(lon*pi180)
    slat  = sin(lat*pi180)
    slon  = sin(lon*pi180)
    pole2q = a2q( (/ cang2,sang2*(/clat*clon,clat*slon,slat/) /) )
  end function pole2q

  pure function q2pole(quat)
    real(dp) :: q2pole(3) ! return lat,lon,angle in degrees
    type(qu),intent(in)::quat
    real(dp) lat,lon,ang,slat,clat,aquat(0:3),qnorm,qnormed(0:3)
    aquat = q2a(quat)
    qnorm = sum(aquat*aquat) ! Birkhoff & MacLane p237 norm, not Euclidean.
    if (abs(qnorm)<epsilon(one)) then ! Can't happen with pole2q output,
       q2pole = (/zero,zero,zero/)  ! but quat might come from elsewhere.
    else
       qnormed  = aquat/sqrt(qnorm)
       if (qnormed(0)<zero) qnormed = -qnormed ! Le Pichon et al p277
       ang = acos(qnormed(0))*two ! radians
       if (ang<epsilon(pi))then ! lat,lon are irrelevant in this case
          q2pole = (/zero,zero,zero/)
       else
          slat = qnormed(3)/sin(ang/two)
          if (abs(slat)<roothalf) then ! asin(slat) better than acos(clat) 
             lat  = asin(slat) ! radians
          else                         ! acos(clat) better than asin(slat)
             clat = sqrt(qnormed(1)**2+qnormed(2)**2)/sin(ang/two)
             lat  = sign(acos(clat),slat) ! radians
          end if
          if (abs(slat) > one-epsilon(pi)) then
             lon = zero ! its actual value doesn't matter at N or S pole
          else
             lon = atan2(qnormed(2),qnormed(1)) ! radians
          end if
          q2pole = (/ lat,lon,ang /) / pi180 ! convert radians to degrees
       end if
    end if
  end function q2pole
end module qarithmetic

program rotationtest
  use qarithmetic, only: dp,qu,qformat,pole2q,q2pole,operator(*)
  implicit none
  real(dp):: lat1,lon1,angle1,lat2,lon2,angle2
  type(qu):: q1,q2
  integer :: io
  do
     print *, 'Give lat1,lon1,angle1, lat2,lon2,angle2 (deg) of 2 '// &
          'rotations to combine'
     read (*,*,iostat=io) lat1,lon1,angle1,lat2,lon2,angle2
     if (io/=0) exit     
     q1 = pole2q(lat1,lon1,angle1)
     print qformat, ' q1 quaternion using format '//qformat,q1
     print qformat, ' q1 lat lon angle =',q2pole(q1)
     q2 = pole2q(lat2,lon2,angle2)
     print qformat, ' q2 quaternion =',q2
     print qformat, ' q2 lat lon angle =',q2pole(q2)
     print qformat, ' q1*q2 quaternion =',q1*q2
     print qformat, ' q1*q2 lat lon angle =',q2pole(q1*q2)
     print qformat, ' q2*q1 quaternion =',q2*q1
     print qformat, ' q2*q1 lat lon angle =',q2pole(q2*q1)
  end do
end program rotationtest
