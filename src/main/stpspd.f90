subroutine stpspd(ru,rv,su,sv,out,sges,dru,drv,dsu,dsv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpspd  calculate penalty and stepsize terms
!                for wind speed, with nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and stepsize terms for wind speed
!
! program history log:
!   1991-02-26  derber
!   1998-02-03  derber
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpspd and stpspd_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output b1 and b3
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!
!   input argument list:
!     ru       - search direction for u
!     rv       - search direction for v
!     su       - analysis increment for u
!     sv       - analysis increment for v
!     sges     - step size estimates (4)
!     dru      - search direction for time derivative of u
!     drv      - search direction for time derivative of v
!     dsu      - analysis increment for time derivative of u
!     dsv      - analysis increment for time derivative of v
!
!   output argument list 
!     out(1)   - contribution to penalty from wind speed sges(1)
!     out(2)   - contribution to penalty from wind speed sges(2)
!     out(3)   - contribution to penalty from wind speed sges(3)
!     out(4)   - contribution to penalty from wind speed sges(4)
!     out(5)   - contribution to numerator from wind speed
!     out(6)   - contribution to denomonator from wind speed
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: spdhead,spdptr
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(4),intent(in):: sges
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: ru,rv,su,sv,dru,drv,dsu,dsv

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4,time_spd
  real(r_kind) spd0,spd1,spd2,spd3,valu,valv,ucur,vcur, &
       u0,u1,u2,u3,v0,v1,v2,v3,spd
  real(r_kind) cg_spd,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

  spdptr => spdhead
  do while (associated(spdptr))

    if(spdptr%luse)then
     j1 = spdptr%ij(1)
     j2 = spdptr%ij(2)
     j3 = spdptr%ij(3)
     j4 = spdptr%ij(4)
     w1 = spdptr%wij(1)
     w2 = spdptr%wij(2)
     w3 = spdptr%wij(3)
     w4 = spdptr%wij(4)
     time_spd=spdptr%time

     valu=w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4)+ &
         (w1*dru(j1)+w2*dru(j2)+w3*dru(j3)+w4*dru(j4))*time_spd
     valv=w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4)+ &
         (w1*drv(j1)+w2*drv(j2)+w3*drv(j3)+w4*drv(j4))*time_spd
     ucur=w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)+ &
         (w1*dsu(j1)+w2*dsu(j2)+w3*dsu(j3)+w4*dsu(j4))*time_spd+spdptr%uges
     vcur=w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)+ &
         (w1*dsv(j1)+w2*dsv(j2)+w3*dsv(j3)+w4*dsv(j4))*time_spd+spdptr%vges
     u0=ucur+sges(1)*valu; v0=vcur+sges(1)*valv
     u1=ucur+sges(2)*valu; v1=vcur+sges(2)*valv
     u2=ucur+sges(3)*valu; v2=vcur+sges(3)*valv
     u3=ucur+sges(4)*valu; v3=vcur+sges(4)*valv

     spd0=sqrt(u0*u0+v0*v0)-spdptr%res
     spd1=sqrt(u1*u1+v1*v1)-spdptr%res
     spd2=sqrt(u2*u2+v2*v2)-spdptr%res
     spd3=sqrt(u3*u3+v3*v3)-spdptr%res

     pencur = spd0*spd0*spdptr%err2
     pen1   = spd1*spd1*spdptr%err2
     pen2   = spd2*spd2*spdptr%err2
     pen3   = spd3*spd3*spdptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. spdptr%pg > tiny_r_kind .and. &
                          spdptr%b  > tiny_r_kind) then
        cg_spd=cg_term/spdptr%b
        wnotgross= one-spdptr%pg
        wgross = spdptr%pg*cg_spd/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

     cc  = (pen1+pen3-two*pen2)*spdptr%raterr2
     out(1) = out(1)+pencur*spdptr%raterr2
     out(2) = out(2)+pen1  *spdptr%raterr2
     out(3) = out(3)+pen2  *spdptr%raterr2
     out(4) = out(4)+pen3  *spdptr%raterr2
     out(5) = out(5)+(pen1-pen3)*spdptr%raterr2*bcoef1+cc*bcoef2
     out(6) = out(6)+cc*ccoef
    end if
    
    spdptr => spdptr%llpoint

  end do
  return
end subroutine stpspd
