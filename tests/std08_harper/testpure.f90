! A Fortran 95 program that compiles and runs with some compilers, but
! others can't calculate lengths of both spf and dpf. The output should be  
! two lines, saying 
! Double precision: "-1.000"
! Single precision: "-1.000" 
module findbug
  implicit none
  integer,parameter:: dp=kind(1d0)
  interface f
     module procedure dpf,spf
  end interface f
contains
  pure function findf(   x) result (out)! with length 50
    real(dp),intent(in)::x
    character(len = 50)::           out
    write(out,"(F0.3)")  x
  end function findf

  pure function dpf(     x) ! trimmed
    real(dp),intent(in)::x
    character:: dpf*(len_trim(findf(x)))
    dpf = trim(findf(real(x,dp)))
  end function dpf

  pure function spf( x) ! trimmed
    real,intent(in)::x
    character:: spf*(len_trim(dpf(real(x,dp))))
    spf = trim(dpf(real(x,dp)))
  end function spf

end module findbug

program tryf
  use findbug, only: dp,f
  implicit none
  real(dp):: dx = -1
  real    :: sx = -1
  print *,' Double precision: "'//f(dx)//'"'
  print *,' Single precision: "'//f(sx)//'"'
end program tryf
