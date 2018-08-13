! A Fortran 95 program that tests whether enormous numbers can be printed 
! in F0.0 format
integer,parameter:: big = selected_real_kind(r=2000) 
if (big<0) then
   print *,'Selected_real_kind(r=2000) is not available'
else
   print "(F0.0)", huge(1.0_big)
end if
end
