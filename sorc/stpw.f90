subroutine stpw(ru,rv,su,sv,out,sges,dru,drv,dsu,dsv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpw        calculate penalty and contribution to stepsize
!                            from winds, using nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from winds,
!              using nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1998-02-03  derber
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpw and stpw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-07-28  derber  - modify output for b1 and b3
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!
!   input argument list:
!     ru       - search direction for u
!     rv       - search direction for v
!     su       - analysis increment for u
!     sv       - analysis increment for v
!     sges     - step size estimates  (4)
!     dru      - search direction for time derivative of u
!     drv      - search direction for time derivative of v
!     dsu      - analysis increment for time derivative of u
!     dsv      - analysis increment for time derivative of v
!
!   output argument list  
!     out(1)   - current penalty using sges(1)
!     out(2)   - current penalty using sges(2)
!     out(3)   - current penalty using sges(3)
!     out(4)   - current penalty using sges(4)
!     out(5)   - contribution to numerator for winds
!     out(6)   - contribution to denomonator for winds
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: whead,wptr
  use qcmod, only: nlnqc_iter,c_varqc
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term,zero_quad
  use gridmod, only: latlon1n
  use jfunc, only: iter,jiter,niter_no_qc,jiterstart
  implicit none

! Declare passed variables
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: ru,rv,su,sv,dru,drv,dsu,dsv
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) valu,facu,valv,facv,w1,w2,w3,w4,w5,w6,w7,w8,time_w
  real(r_kind) cg_w,pen1,pen2,pen3,pencur,u1,u2,u3,v1,v2,v3,wgross,wnotgross,w_pg,varqc_iter
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc,u0,v0

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef
  
  wptr => whead
  do while (associated(wptr))
    if(wptr%luse)then
     j1=wptr%ij(1)
     j2=wptr%ij(2)
     j3=wptr%ij(3)
     j4=wptr%ij(4)
     j5=wptr%ij(5)
     j6=wptr%ij(6)
     j7=wptr%ij(7)
     j8=wptr%ij(8)
     w1=wptr%wij(1)
     w2=wptr%wij(2)
     w3=wptr%wij(3)
     w4=wptr%wij(4)
     w5=wptr%wij(5)
     w6=wptr%wij(6)
     w7=wptr%wij(7)
     w8=wptr%wij(8)
     time_w=wptr%time

     valu=w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4) &
         +w5* ru(j5)+w6* ru(j6)+w7* ru(j7)+w8* ru(j8) &
        +(w1*dru(j1)+w2*dru(j2)+w3*dru(j3)+w4*dru(j4) &
         +w5*dru(j5)+w6*dru(j6)+w7*dru(j7)+w8*dru(j8))*time_w

     facu=w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4) &
         +w5* su(j5)+w6* su(j6)+w7* su(j7)+w8* su(j8) &
        +(w1*dsu(j1)+w2*dsu(j2)+w3*dsu(j3)+w4*dsu(j4) &
         +w5*dsu(j5)+w6*dsu(j6)+w7*dsu(j7)+w8*dsu(j8))*time_w - wptr%ures

     valv=w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4) &
         +w5* rv(j5)+w6* rv(j6)+w7* rv(j7)+w8* rv(j8) &
        +(w1*drv(j1)+w2*drv(j2)+w3*drv(j3)+w4*drv(j4) &
         +w5*drv(j5)+w6*drv(j6)+w7*drv(j7)+w8*drv(j8))*time_w

     facv=w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4) &
         +w5* sv(j5)+w6* sv(j6)+w7* sv(j7)+w8* sv(j8) &
        +(w1*dsv(j1)+w2*dsv(j2)+w3*dsv(j3)+w4*dsv(j4) &
         +w5*dsv(j5)+w6*dsv(j6)+w7*dsv(j7)+w8*dsv(j8))*time_w - wptr%vres
     
     u0=facu+sges(1)*valu
     u1=facu+sges(2)*valu
     u2=facu+sges(3)*valu
     u3=facu+sges(4)*valu
     v0=facv+sges(1)*valv
     v1=facv+sges(2)*valv
     v2=facv+sges(3)*valv
     v3=facv+sges(4)*valv

     pencur = (u0*u0+v0*v0)*wptr%err2
     pen1   = (u1*u1+v1*v1)*wptr%err2
     pen2   = (u2*u2+v2*v2)*wptr%err2
     pen3   = (u3*u3+v3*v3)*wptr%err2

!  Modify penalty term if nonlinear QC
!    Variational qc is gradually increased to avoid possible convergence problems
     if(jiter == jiterstart .and. nlnqc_iter .and. wptr%pg > tiny_r_kind) then
        varqc_iter=c_varqc*(iter-niter_no_qc(1)+one)
        if(varqc_iter >=one) varqc_iter= one
        w_pg=wptr%pg*varqc_iter
     else
        w_pg=wptr%pg
     endif

     if (nlnqc_iter .and. wptr%pg > tiny_r_kind .and.  &
                          wptr%b  > tiny_r_kind) then
        cg_w=cg_term/wptr%b
        wnotgross= one-w_pg
        wgross =w_pg*cg_w/wnotgross
        pencur = -two*log((exp(-half*pencur)+wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  )+wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  )+wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  )+wgross)/(one+wgross))
     endif

     out(1) = out(1)+pencur*wptr%raterr2
     out(2) = out(2)+pen1*wptr%raterr2
     out(3) = out(3)+pen2*wptr%raterr2
     out(4) = out(4)+pen3*wptr%raterr2
     cc     = (pen3+pen1-two*pen2)*wptr%raterr2
     out(5) = out(5)+(pen1-pen3)*wptr%raterr2*bcoef1+cc*bcoef2
     out(6) = out(6)+cc*ccoef
    end if

    wptr => wptr%llpoint

  end do
  return
end subroutine stpw
