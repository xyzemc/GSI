subroutine stppcp(rt,rq,ru,rv,rcwm,st,sq,su,sv,scwm, &
     rpred,spred,out,sges,drt,drq,dru,drv,drcwm,dst,dsq,dsu,dsv,dscwm)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppcp     compute contribution to penalty and
!                           stepsize from pcp, with nonlinear qc
!   prgmmr: treadon          org:  np23               date: 2003-09-13
!
! abstract: compute contribution to penalty and stepsize from precipitation
!           observations
!
! program history log:
!   2003-12-18 treadon - initial routine
!   2004-06-15 treadon - update documentation
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07 parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stppcp and stppcp_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2006-07-28  derber   - modify to use new inner loop obs data structure
!                        - unify NL qc
!   2006-09-18  derber   - modify output b1 and b3 
!   2007-01-19  derber   - limit pcp_ges* > zero
!   2007-02-15  rancic   - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!
!   input argument list:
!     rt       - search direction for temperature
!     rq       - search direction for moisture 
!     ru       - search direction for zonal wind
!     rv       - search direction for meridional wind
!     rcwm     - search direction for cloud condensate mixing ratio
!     st       - input temperature correction field
!     sq       - input q correction field
!     su       - input u correction field
!     sv       - input v correction field
!     scwm     - input cwm correction field
!     rpred    - search direction for bias correction predictors
!     spred    - input precipitation bias correction values
!     sges     - step size estimates (4)
!     drt      - search direction for time derivative of temperature
!     drq      - search direction for time derivative of moisture 
!     dru      - search direction for time derivative of zonal wind
!     drv      - search direction for time derivative of meridional wind
!     drcwm    - search direction for time derivative of cloud condensate mixing ratio
!     dst      - input time derivative of temperature correction field
!     dsq      - input time derivative of q correction field
!     dsu      - input time derivative of u correction field
!     dsv      - input time derivative of v correction field
!     dscwm    - input time derivative of cwm correction field
!
!   output argument list:
!     out(1)   - contribution to penalty from precipitation rate - sges(1)
!     out(2)   - contribution to penalty from precipitation rate - sges(2)
!     out(3)   - contribution to penalty from precipitation rate - sges(3)
!     out(4)   - contribution to penalty from precipitation rate - sges(4)
!     out(5)   - contribution to numerator from precipitation rate
!     out(6)   - contribution to denomonator from precipitation rate
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use pcpinfo, only: npcptype,npredp,b_pcp,pg_pcp
  use obsmod, only: pcpptr,pcphead
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term,zero_quad
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon11,nsig,latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(4),intent(in):: sges
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: rt,st,rq,sq,ru,su,&
       rv,sv,rcwm,scwm,drt,dst,drq,dsq,dru,dsu,drv,dsv,drcwm,dscwm
  real(r_kind),dimension(npcptype,npredp),intent(in):: rpred,spred

! Declare local variables
  integer(i_kind) i,n,ncwm,nq,nt,nu,nv,kx
  integer(i_kind) j1,j2,j3,j4
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  real(r_kind) dt,dt0,w1,w2,w3,w4,w5,time_pcp
  real(r_kind) dq,dq0
  real(r_kind) du,du0
  real(r_kind) dv,dv0
  real(r_kind) dcwm,dcwm0
  real(r_kind) pcp_ges0,pcp_ges1,pcp_ges2,pcp_ges3,pcp_gest,pcp_ges
  real(r_kind) obsges0,obsges1,obsges2,obsges3
  real(r_kind) termges0,termges1,termges2,termges3
  real(r_kind) cg_pcp,pen1,pen2,pen3,pencur,wgross,wnotgross

! Initialize penalty, b1, and b3 to zero  
  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef


! Loop over number of observations.
  pcpptr => pcphead
  do while(associated(pcpptr))
    if(pcpptr%luse)then
     j1=pcpptr%ij(1)
     j2=pcpptr%ij(2)
     j3=pcpptr%ij(3)
     j4=pcpptr%ij(4)
     w1=pcpptr%wij(1)
     w2=pcpptr%wij(2)
     w3=pcpptr%wij(3)
     w4=pcpptr%wij(4)
     pcp_ges  = pcpptr%ges
     pcp_gest = zero
     time_pcp=pcpptr%time

!    Compute updates to simulated precipitation
     do n=1,nsig
        dt  =w1*   st(j1)+w2*   st(j2)+ w3*   st(j3)+w4*   st(j4)+ &
            (w1*  dst(j1)+w2*  dst(j2)+ w3*  dst(j3)+w4*  dst(j4))*time_pcp
        dq  =w1*   sq(j1)+w2*   sq(j2)+ w3*   sq(j3)+w4*   sq(j4)+ &
            (w1*  dsq(j1)+w2*  dsq(j2)+ w3*  dsq(j3)+w4*  dsq(j4))*time_pcp
        du  =w1*   su(j1)+w2*   su(j2)+ w3*   su(j3)+w4*   su(j4)+ &
            (w1*  dsu(j1)+w2*  dsu(j2)+ w3*  dsu(j3)+w4*  dsu(j4))*time_pcp
        dv  =w1*   sv(j1)+w2*   sv(j2)+ w3*   sv(j3)+w4*   sv(j4)+ &
            (w1*  dsv(j1)+w2*  dsv(j2)+ w3*  dsv(j3)+w4*  dsv(j4))*time_pcp
        dcwm=w1* scwm(j1)+w2* scwm(j2)+ w3* scwm(j3)+w4* scwm(j4)+ &
            (w1*dscwm(j1)+w2*dscwm(j2)+ w3*dscwm(j3)+w4*dscwm(j4))*time_pcp
        
        dt0  =w1*   rt(j1)+w2*   rt(j2)+ w3*   rt(j3)+w4*   rt(j4)+ &
             (w1*  drt(j1)+w2*  drt(j2)+ w3*  drt(j3)+w4*  drt(j4))*time_pcp
        dq0  =w1*   rq(j1)+w2*   rq(j2)+ w3*   rq(j3)+w4*   rq(j4)+ &
             (w1*  drq(j1)+w2*  drq(j2)+ w3*  drq(j3)+w4*  drq(j4))*time_pcp
        du0  =w1*   ru(j1)+w2*   ru(j2)+ w3*   ru(j3)+w4*   ru(j4)+ &
             (w1*  dru(j1)+w2*  dru(j2)+ w3*  dru(j3)+w4*  dru(j4))*time_pcp
        dv0  =w1*   rv(j1)+w2*   rv(j2)+ w3*   rv(j3)+w4*   rv(j4)+ &
             (w1*  drv(j1)+w2*  drv(j2)+ w3*  drv(j3)+w4*  drv(j4))*time_pcp
        dcwm0=w1* rcwm(j1)+w2* rcwm(j2)+ w3* rcwm(j3)+w4* rcwm(j4)+ &
             (w1*drcwm(j1)+w2*drcwm(j2)+ w3*drcwm(j3)+w4*drcwm(j4))*time_pcp
        
        nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig
        pcp_ges  = pcp_ges  +  pcpptr%dpcp_dvar(nt)  *dt + &
                               pcpptr%dpcp_dvar(nq)  *dq + &
                               pcpptr%dpcp_dvar(nu)  *du + &
                               pcpptr%dpcp_dvar(nv)  *dv + &
                               pcpptr%dpcp_dvar(ncwm)*dcwm
        pcp_gest = pcp_gest +  pcpptr%dpcp_dvar(nt)  *dt0+ &
                               pcpptr%dpcp_dvar(nq)  *dq0+ &
                               pcpptr%dpcp_dvar(nu)  *du0+ &
                               pcpptr%dpcp_dvar(nv)  *dv0+ &
                               pcpptr%dpcp_dvar(ncwm)*dcwm0
        
        j1=j1+latlon11
        j2=j2+latlon11
        j3=j3+latlon11
        j4=j4+latlon11
        
     end do
     pcp_ges0 = pcp_ges + sges(1)*pcp_gest
     pcp_ges1 = pcp_ges + sges(2)*pcp_gest
     pcp_ges2 = pcp_ges + sges(3)*pcp_gest
     pcp_ges3 = pcp_ges + sges(4)*pcp_gest
     
!    Logrithmic formulation.  Ensure pcp_ges > zero
     pcp_ges0 = max(pcp_ges0,zero)
     pcp_ges1 = max(pcp_ges1,zero)
     pcp_ges2 = max(pcp_ges2,zero)
     pcp_ges3 = max(pcp_ges3,zero)

     termges0 = log(one+pcp_ges0)
     termges1 = log(one+pcp_ges1)
     termges2 = log(one+pcp_ges2)
     termges3 = log(one+pcp_ges3)

!   Compute obs-ges (innovation)
     obsges0= pcpptr%obs - termges0
     obsges1= pcpptr%obs - termges1
     obsges2= pcpptr%obs - termges2
     obsges3= pcpptr%obs - termges3

     pencur = pcpptr%err2*obsges0*obsges0
     pen1   = pcpptr%err2*obsges1*obsges1
     pen2   = pcpptr%err2*obsges2*obsges2
     pen3   = pcpptr%err2*obsges3*obsges3

     kx=pcpptr%icxp
!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. pg_pcp(kx) > tiny_r_kind .and.  &
                          b_pcp(kx)  > tiny_r_kind) then
        cg_pcp=cg_term/b_pcp(kx)
        wnotgross= one-pg_pcp(kx)
        wgross = pg_pcp(kx)*cg_pcp/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

!    Accumulate stepsize terms
     cc     = (pen1+pen3-two*pen2)*pcpptr%raterr2
     out(1) = out(1)+ pencur * pcpptr%raterr2
     out(2) = out(2)+ pen1   * pcpptr%raterr2
     out(3) = out(3)+ pen2   * pcpptr%raterr2
     out(4) = out(4)+ pen3   * pcpptr%raterr2
     out(5) = out(5)+ (pen1-pen3)*pcpptr%raterr2*bcoef1+cc*bcoef2
     out(6) = out(6)+ cc*ccoef
    end if
     
    pcpptr => pcpptr%llpoint
  end do
  
  return
end subroutine stppcp
