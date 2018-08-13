! NAN, +INF, -INF are OK as real input in f2003 but not in the f95 standard. 
! Sun f95, gfortran, g95, ifort, all allow them even though some don't 
! have the IEEE intrinsic modules of f2003. This program prints them
! and their hex codes in single and double precision. It assumes 32-bit real,
! 64-bit double precision
implicit none
integer io,i2(2)
real(kind(1.0)) rnan, rplusinf, rneginf ! default real
real(kind(1d0)) dnan, dplusinf, dneginf ! double precision
character(13) :: ch = 'NAN +INF -INF'
character(20) :: rfmt = '(3(F8.0,2X))', dfmt = '(3(F17.0,2X))' 
character(20) :: rhex = '(3(Z8.8,2X))',dhex = '(3(Z8.8,1X,Z8.8,2X))'
read(ch,*,iostat=io) rnan, rplusinf, rneginf 
if(io/=0) stop 'NAN or +INF or -INF invalid real input'
print rfmt, rnan, rplusinf, rneginf
print rhex, transfer(rnan,1),transfer(rplusinf,1),transfer(rneginf,1)
read(ch,*,iostat=io) dnan, dplusinf, dneginf 
if(io/=0) stop 'NAN or +INF or -INF invalid double precicion input'
print dfmt, dnan, dplusinf, dneginf
print dhex, transfer(dnan,i2),transfer(dplusinf,i2),transfer(dneginf,i2)
end
