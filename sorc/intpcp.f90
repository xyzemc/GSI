subroutine intpcp(rt,rq,ru,rv,rcwm,st,sq,su,sv,scwm,rpredp,spredp,&
                  drt,drq,dru,drv,drcwm,dst,dsq,dsu,dsv,dscwm)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpcp      precip rate nonlin qc obs operator
!   prgmmr: treadon          org: np23                date: 2003-09-13
!
! abstract: apply precipitation rate operator and adjoint with
!            addition of nonlinear qc operator
!
! program history log:
!   2003-12-18  treadon - initial routine
!   2004-06-15  treadon - reformat documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intpcp and intpcp_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-01-19  derber  - limit pcp_ges* > zero
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!
!   input argument list:
!     st       - input temperature correction field
!     sq       - input q correction field
!     su       - input zonal wind correction field
!     sv       - input meridional wind correction field
!     scwm     - input cloud condensate mixing ratio correction field
!     spredp   - input bias correction predictor values
!     dst      - input time derivative of temperature correction field
!     dsq      - input time derivative of q correction field
!     dsu      - input time derivative of zonal wind correction field
!     dsv      - input time derivative of meridional wind correction field
!     dscwm    - input time derivative of cloud condensate mixing ratio correction field
!
!   output argument list:
!     rt       - output t vector after inclusion of pcp. info.
!     rq       - output q vector after inclusion of pcp. info.
!     ru       - output u vector after inclusion of pcp. info.
!     rv       - output v vector after inclusion of pcp. info.
!     rcwm     - output cwm vector after inclusion of pcp. info.
!     rpredp   - output bias correction predictor vector after inclusion of pcp. info.
!     drt      - output time derivative of t vector after inclusion of pcp. info.
!     drq      - output time derivative of q vector after inclusion of pcp. info.
!     dru      - output time derivative of u vector after inclusion of pcp. info.
!     drv      - output time derivative of v vector after inclusion of pcp. info.
!     drcwm    - output time derivative of cwm vector after inclusion of pcp. info.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: pcpptr,pcphead
  use qcmod, only: nlnqc_iter
  use pcpinfo, only: npcptype,npredp,b_pcp,pg_pcp
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term
  use gridmod, only: nsig,latlon11,latlon1n,nsig5
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: st,sq,su,sv,scwm
  real(r_kind),dimension(latlon1n),intent(in):: dst,dsq,dsu,dsv,dscwm
  real(r_kind),dimension(npcptype,npredp),intent(in):: spredp
  real(r_kind),dimension(latlon1n),intent(inout):: rt,rq,ru,rv,rcwm
  real(r_kind),dimension(latlon1n),intent(inout):: drt,drq,dru,drv,drcwm
  real(r_quad),dimension(npcptype,npredp),intent(inout):: rpredp
  
! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,nq,nu,nv,ncwm,n,nt,kx
  real(r_kind) dt,dq,du,dv,dcwm,dcwm_ad,termges_ad,w1,w2,w3,w4
  real(r_kind) pcp_ges_ad,dq_ad,dt_ad,dv_ad,du_ad,pcp_ges
  real(r_kind) obsges,termges,time_pcp
  real(r_kind) cg_pcp,p0,wnotgross,wgross

  pcpptr => pcphead
  do while(associated(pcpptr))
     j1=pcpptr%ij(1)
     j2=pcpptr%ij(2)
     j3=pcpptr%ij(3)
     j4=pcpptr%ij(4)
     w1=pcpptr%wij(1)
     w2=pcpptr%wij(2)
     w3=pcpptr%wij(3)
     w4=pcpptr%wij(4)
     pcp_ges = pcpptr%ges 

     time_pcp=pcpptr%time

!    Compute updated simulated rain rate based on changes in t,q,u,v,cwm
     do n = 1,nsig
        dt = w1* st(j1)+w2* st(j2)+ w3* st(j3)+w4* st(j4) + &
            (w1*dst(j1)+w2*dst(j2)+ w3*dst(j3)+w4*dst(j4))*time_pcp
        dq = w1* sq(j1)+w2* sq(j2)+ w3* sq(j3)+w4* sq(j4) + &
            (w1*dsq(j1)+w2*dsq(j2)+ w3*dsq(j3)+w4*dsq(j4))*time_pcp
        du = w1* su(j1)+w2* su(j2)+ w3* su(j3)+w4* su(j4) + &
            (w1*dsu(j1)+w2*dsu(j2)+ w3*dsu(j3)+w4*dsu(j4))*time_pcp
        dv = w1* sv(j1)+w2* sv(j2)+ w3* sv(j3)+w4* sv(j4) + &
            (w1*dsv(j1)+w2*dsv(j2)+ w3*dsv(j3)+w4*dsv(j4))*time_pcp
        dcwm=w1* scwm(j1)+w2* scwm(j2)+  &
             w3* scwm(j3)+w4* scwm(j4)+  &
            (w1*dscwm(j1)+w2*dscwm(j2)+  &
             w3*dscwm(j3)+w4*dscwm(j4))*time_pcp
        
        nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig
        pcp_ges = pcp_ges + pcpptr%dpcp_dvar(nt)*dt + &
                            pcpptr%dpcp_dvar(nq)*dq + &
                            pcpptr%dpcp_dvar(nu)*du + &
                            pcpptr%dpcp_dvar(nv)*dv + &
                            pcpptr%dpcp_dvar(ncwm)*dcwm
        
        j1=j1+latlon11
        j2=j2+latlon11
        j3=j3+latlon11
        j4=j4+latlon11
        
     end do


!   Logrithmic obs - ges.  plus 1 added for zero rain rate.

!   Ensure rain rate is greater than a small zero
    pcp_ges = max(pcp_ges,zero)
    termges = log(one+pcp_ges)

!   Compute o-g 
    obsges = termges - pcpptr%obs


!   Adjoint model
    kx=pcpptr%icxp
    if (nlnqc_iter .and. pg_pcp(kx) > tiny_r_kind .and.  &
                         b_pcp(kx)  > tiny_r_kind) then
       cg_pcp=cg_term/b_pcp(kx)
       wnotgross= one-pg_pcp(kx)
       wgross = pg_pcp(kx)*cg_pcp/wnotgross
       p0   = wgross/(wgross+exp(-half*pcpptr%err2*obsges**2))
       obsges = obsges*(one-p0)
    endif

    termges_ad  = obsges*pcpptr%err2*pcpptr%raterr2

!   Adjoint for logrithmic forumulation
    pcp_ges_ad = termges_ad/(one+pcp_ges)

!   Adjoint of pcp_ges update

    j1=pcpptr%ij(1)
    j2=pcpptr%ij(2)
    j3=pcpptr%ij(3)
    j4=pcpptr%ij(4)
    do n=1,nsig
       nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig

       dcwm_ad = pcpptr%dpcp_dvar(ncwm)*pcp_ges_ad
       dv_ad   = pcpptr%dpcp_dvar(nv)*pcp_ges_ad
       du_ad   = pcpptr%dpcp_dvar(nu)*pcp_ges_ad
       dq_ad   = pcpptr%dpcp_dvar(nq)*pcp_ges_ad
       dt_ad   = pcpptr%dpcp_dvar(nt)*pcp_ges_ad

       rcwm(j4) = rcwm(j4) + w4*dcwm_ad
       rcwm(j3) = rcwm(j3) + w3*dcwm_ad
       rcwm(j2) = rcwm(j2) + w2*dcwm_ad
       rcwm(j1) = rcwm(j1) + w1*dcwm_ad

       rv(j4) = rv(j4) + w4*dv_ad
       rv(j3) = rv(j3) + w3*dv_ad
       rv(j2) = rv(j2) + w2*dv_ad
       rv(j1) = rv(j1) + w1*dv_ad

       ru(j4) = ru(j4) + w4*du_ad
       ru(j3) = ru(j3) + w3*du_ad
       ru(j2) = ru(j2) + w2*du_ad
       ru(j1) = ru(j1) + w1*du_ad

       rq(j4) = rq(j4) + w4*dq_ad
       rq(j3) = rq(j3) + w3*dq_ad
       rq(j2) = rq(j2) + w2*dq_ad
       rq(j1) = rq(j1) + w1*dq_ad

       rt(j4) = rt(j4) + w4*dt_ad
       rt(j3) = rt(j3) + w3*dt_ad
       rt(j2) = rt(j2) + w2*dt_ad
       rt(j1) = rt(j1) + w1*dt_ad

       dcwm_ad = time_pcp*dcwm_ad
       dv_ad   = time_pcp*dv_ad
       du_ad   = time_pcp*du_ad
       dq_ad   = time_pcp*dq_ad
       dt_ad   = time_pcp*dt_ad

       drcwm(j4) = drcwm(j4) + w4*dcwm_ad
       drcwm(j3) = drcwm(j3) + w3*dcwm_ad
       drcwm(j2) = drcwm(j2) + w2*dcwm_ad
       drcwm(j1) = drcwm(j1) + w1*dcwm_ad

       drv(j4) = drv(j4) + w4*dv_ad
       drv(j3) = drv(j3) + w3*dv_ad
       drv(j2) = drv(j2) + w2*dv_ad
       drv(j1) = drv(j1) + w1*dv_ad

       dru(j4) = dru(j4) + w4*du_ad
       dru(j3) = dru(j3) + w3*du_ad
       dru(j2) = dru(j2) + w2*du_ad
       dru(j1) = dru(j1) + w1*du_ad

       drq(j4) = drq(j4) + w4*dq_ad
       drq(j3) = drq(j3) + w3*dq_ad
       drq(j2) = drq(j2) + w2*dq_ad
       drq(j1) = drq(j1) + w1*dq_ad

       drt(j4) = drt(j4) + w4*dt_ad
       drt(j3) = drt(j3) + w3*dt_ad
       drt(j2) = drt(j2) + w2*dt_ad
       drt(j1) = drt(j1) + w1*dt_ad

       j1=j1+latlon11
       j2=j2+latlon11
       j3=j3+latlon11
       j4=j4+latlon11
            
    end do

    pcpptr => pcpptr%llpoint 
 end do

 return
end subroutine intpcp
