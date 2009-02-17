subroutine stpsrw(ru,rv,su,sv,out,sges,dru,drv,dsu,dsv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpsrw      apply nonlin qc op for radar superob wind
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: apply operator for radar superob wind and calculation of 
!             step size using nonlinear qc.
!
! program history log:
!   2004-06-22  parrish, document
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpsrw and stpsrw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output values of b1 and b3
!   2007-02-15  rancic - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!
!   input argument list:
!     ru       - search direction for u
!     su       - analysis increment for u
!     rv       - search direction for v
!     sv       - analysis increment for v
!     sges     - step size estimates (4)
!     dru      - search direction for time derivative of u
!     dsu      - analysis increment for time derivative of u
!     drv      - search direction for time derivative of v
!     dsv      - analysis increment for time derivative of v
!
!   output argument list  
!     out(1)   - penalty for srw obs - sges(1)
!     out(2)   - penalty for srw obs - sges(2)
!     out(3)   - penalty for srw obs - sges(3)
!     out(4)   - penalty for srw obs - sges(4)
!     out(5)   - contribution to numerator for srw obs
!     out(6)   - contribution to denomenator for srw obs
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: srwhead,srwptr
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
  real(r_kind) valu,facu,valv,facv,w1,w2,w3,w4,w5,w6,w7,w8,time_srw
  real(r_kind) bigu11,bigu12,bigu21,bigu22,facsrw1,facsrw2,valsrw1,valsrw2
  real(r_kind) cg_srw,pen1,pen2,pen3,pencur,u0,u1,u2,u3,v0,v1,v2,v3,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

  srwptr => srwhead
  do while (associated(srwptr))
    if(srwptr%luse)then
     j1=srwptr%ij(1)
     j2=srwptr%ij(2)
     j3=srwptr%ij(3)
     j4=srwptr%ij(4)
     j5=srwptr%ij(5)
     j6=srwptr%ij(6)
     j7=srwptr%ij(7)
     j8=srwptr%ij(8)
     w1=srwptr%wij(1)
     w2=srwptr%wij(2)
     w3=srwptr%wij(3)
     w4=srwptr%wij(4)
     w5=srwptr%wij(5)
     w6=srwptr%wij(6)
     w7=srwptr%wij(7)
     w8=srwptr%wij(8)

     time_srw=srwptr%time
     bigu11=srwptr%rsrw(1)
     bigu21=srwptr%rsrw(2)
     bigu12=srwptr%rsrw(3)
     bigu22=srwptr%rsrw(4)
     valu=w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4) &
         +w5* ru(j5)+w6* ru(j6)+w7* ru(j7)+w8* ru(j8) &
        +(w1*dru(j1)+w2*dru(j2)+w3*dru(j3)+w4*dru(j4) &
         +w5*dru(j5)+w6*dru(j6)+w7*dru(j7)+w8*dru(j8))*time_srw

     valv=w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4) &
         +w5* rv(j5)+w6* rv(j6)+w7* rv(j7)+w8* rv(j8) &
        +(w1*drv(j1)+w2*drv(j2)+w3*drv(j3)+w4*drv(j4) &
         +w5*drv(j5)+w6*drv(j6)+w7*drv(j7)+w8*drv(j8))*time_srw
     
     valsrw1=bigu11*valu+bigu12*valv
     valsrw2=bigu21*valu+bigu22*valv
    
     facu=w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4) &
         +w5* su(j5)+w6* su(j6)+w7* su(j7)+w8* su(j8) &
        +(w1*dsu(j1)+w2*dsu(j2)+w3*dsu(j3)+w4*dsu(j4) &
         +w5*dsu(j5)+w6*dsu(j6)+w7*dsu(j7)+w8*dsu(j8))*time_srw

     facv=w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4) &
         +w5* sv(j5)+w6* sv(j6)+w7* sv(j7)+w8* sv(j8) &
        +(w1*dsv(j1)+w2*dsv(j2)+w3*dsv(j3)+w4*dsv(j4) &
         +w5*dsv(j5)+w6*dsv(j6)+w7*dsv(j7)+w8*dsv(j8))*time_srw
     
     facsrw1=bigu11*facu+bigu12*facv-srwptr%res1
     facsrw2=bigu21*facu+bigu22*facv-srwptr%res2
     
     u0=facsrw1+sges(1)*valsrw1
     u1=facsrw1+sges(2)*valsrw1
     u2=facsrw1+sges(3)*valsrw1
     u3=facsrw1+sges(4)*valsrw1
     v0=facsrw2+sges(1)*valsrw2
     v1=facsrw2+sges(2)*valsrw2
     v2=facsrw2+sges(3)*valsrw2
     v3=facsrw2+sges(4)*valsrw2

     pencur = (u0*u0+v0*v0)*srwptr%err2
     pen1   = (u1*u1+v1*v1)*srwptr%err2
     pen2   = (u2*u2+v2*v2)*srwptr%err2
     pen3   = (u3*u3+v3*v3)*srwptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. srwptr%pg > tiny_r_kind .and.  &
                          srwptr%b  > tiny_r_kind) then
        cg_srw=cg_term/srwptr%b
        wnotgross= one-srwptr%pg
        wgross = srwptr%pg*cg_srw/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

     cc     = (pen1+pen3-two*pen2)*srwptr%raterr2
     out(1) = out(1)+pencur*srwptr%raterr2
     out(2) = out(2)+pen1  *srwptr%raterr2
     out(3) = out(3)+pen2  *srwptr%raterr2
     out(4) = out(4)+pen3  *srwptr%raterr2
     out(5) = out(5)+(pen1-pen3)*srwptr%raterr2*bcoef1+cc*bcoef2
     out(6) = out(6)+cc*ccoef
    end if

    srwptr => srwptr%llpoint

  end do
 return
end subroutine stpsrw
