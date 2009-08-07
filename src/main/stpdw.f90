subroutine stpdw(ru,rv,su,sv,out,sges,dru,drv,dsu,dsv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpdw  calculate contribution to penalty and
!                stepsize from dw, with nonlinear qc added.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate contribution to penalty and stepsize from lidar winds
!
! program history log:
!   1991-02-26  derber
!   1999-11-22  yang
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpdw and stpdw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2007-07-28  derber   - modify to use new inner loop obs data structure
!                        - unify NL qc
!   2007-02-15  rancic - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!
!   input argument list:
!     ru   - search direction for u
!     rv   - search direction for v
!     su   - current analysis increment for u
!     sv   - current analysis increment for v
!     dru  - search direction for time derivative of u
!     drv  - search direction for time derivative of v
!     dsu  - current analysis increment for time derivative of u
!     dsv  - current analysis increment for time derivative of v
!     sges - step size estimates (4)
!
!   output argument list:                                      
!     out(1) - penalty contribution from lidar winds sges(1)
!     out(2) - penalty contribution from lidar winds sges(2)
!     out(3) - penalty contribution from lidar winds sges(3)
!     out(4) - penalty contribution from lidar winds sges(4)
!     out(5) - contribution to numerator from lidar winds
!     out(6) - contribution to denomenator from lidar winds
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: dwhead,dwptr
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: ru,rv,su,sv,dru,drv,dsu,dsv
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) valdw,facdw,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc,time_dw
  real(r_kind) cg_dw,dw0,dw1,dw2,dw3,pen1,pen2,pen3,pencur,wgross,wnotgross

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

  dwptr => dwhead
  do while (associated(dwptr))
    if(dwptr%luse)then
     j1=dwptr%ij(1)
     j2=dwptr%ij(2)
     j3=dwptr%ij(3)
     j4=dwptr%ij(4)
     j5=dwptr%ij(5)
     j6=dwptr%ij(6)
     j7=dwptr%ij(7)
     j8=dwptr%ij(8)
     w1=dwptr%wij(1)
     w2=dwptr%wij(2)
     w3=dwptr%wij(3)
     w4=dwptr%wij(4)
     w5=dwptr%wij(5)
     w6=dwptr%wij(6)
     w7=dwptr%wij(7)
     w8=dwptr%wij(8)

     time_dw=dwptr%time
     valdw=(w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4)+&
            w5* ru(j5)+w6* ru(j6)+w7* ru(j7)+w8* ru(j8))*dwptr%sinazm+&
           (w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4)+&
            w5* rv(j5)+w6* rv(j6)+w7* rv(j7)+w8* rv(j8))*dwptr%cosazm+&
          ((w1*dru(j1)+w2*dru(j2)+w3*dru(j3)+w4*dru(j4)+&
            w5*dru(j5)+w6*dru(j6)+w7*dru(j7)+w8*dru(j8))*dwptr%sinazm+&
           (w1*drv(j1)+w2*drv(j2)+w3*drv(j3)+w4*drv(j4)+&
            w5*drv(j5)+w6*drv(j6)+w7*drv(j7)+w8*drv(j8))*dwptr%cosazm)*time_dw

     facdw=(w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)+&
            w5* su(j5)+w6* su(j6)+w7* su(j7)+w8* su(j8))*dwptr%sinazm+&
           (w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)+&
            w5* sv(j5)+w6* sv(j6)+w7* sv(j7)+w8* sv(j8))*dwptr%cosazm+&
          ((w1*dsu(j1)+w2*dsu(j2)+w3*dsu(j3)+w4*dsu(j4)+&
            w5*dsu(j5)+w6*dsu(j6)+w7*dsu(j7)+w8*dsu(j8))*dwptr%sinazm+&
           (w1*dsv(j1)+w2*dsv(j2)+w3*dsv(j3)+w4*dsv(j4)+&
            w5*dsv(j5)+w6*dsv(j6)+w7*dsv(j7)+w8*dsv(j8))*dwptr%cosazm)*time_dw&
           -dwptr%res
     dw0=facdw+sges(1)*valdw
     dw1=facdw+sges(2)*valdw
     dw2=facdw+sges(3)*valdw
     dw3=facdw+sges(4)*valdw

     pencur = dw0*dw0*dwptr%err2
     pen1   = dw1*dw1*dwptr%err2
     pen2   = dw2*dw2*dwptr%err2
     pen3   = dw3*dw3*dwptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. dwptr%pg > tiny_r_kind .and. dwptr%b > tiny_r_kind) then
        cg_dw=cg_term/dwptr%b
        wnotgross= one-dwptr%pg
        wgross = dwptr%pg*cg_dw/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

     cc     = (pen1+pen3-two*pen2)*dwptr%raterr2
     out(1) = out(1)+pencur*dwptr%raterr2
     out(2) = out(2)+pen1  *dwptr%raterr2
     out(3) = out(3)+pen2  *dwptr%raterr2
     out(4) = out(4)+pen3  *dwptr%raterr2
     out(5) = out(5)+(pen1-pen3)*dwptr%raterr2*bcoef1+cc*bcoef2
     out(6) = out(6)+cc*ccoef
   end if
   
   dwptr => dwptr%llpoint

  end do

  return
end subroutine stpdw
