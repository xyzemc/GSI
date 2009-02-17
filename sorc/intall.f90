subroutine intall(rval,sval,svalt,svalp,svaluv,sval_x,sval_y,sval_t,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intall      calculate RHS for analysis equation
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: calculate RHS for all variables (nonlinear qc version)
!
!    A description of nonlinear qc follows:
!
!    The observation penalty Jo is defined as
!
!          Jo =  - (sum over obs) 2*log(Po)
!
!      where,
!
!          Po = Wnotgross*exp(-.5*(Hn(x+xb) - yo)**2 ) + Wgross
!            with
!                Hn = the forward model (possibly non-linear) normalized by 
!                     observation error
!                x  = the current estimate of the analysis increment
!                xb = the background state
!                yo = the observation normalized by observation error
!
!            Note:  The factor 2 in definition of Jo is present because the 
!                   penalty Jo as used in this code is 2*(usual definition 
!                   of penalty)
!
!          Wgross = Pgross*cg
!
!          Wnotgross = 1 - Wgross
!
!          Pgross = probability of gross error for observation (assumed
!                   here to have uniform distribution over the possible
!                   range of values)
!
!          cg = sqrt(2*pi)/2b
!
!          b = possible range of variable for gross errors, normalized by 
!              observation error
!
!    The values for the above parameters that Bill Collins used in the
!    eta 3dvar are:
!
!          cg = cg_term/b, where cg_term = sqrt(2*pi)/2 
!
!          b = 10.        ! range for gross errors, normalized by obs error
!
!          pg_q=.002      ! probability of gross error for specific humidity
!          pg_pw=.002     ! probability of gross error for precipitable water
!          pg_p=.002      ! probability of gross error for pressure
!          pg_w=.005      ! probability of gross error for wind
!          pg_t=.007      ! probability of gross error for temperature
!          pg_rad=.002    ! probability of gross error for radiances
!
!
!    Given the above Jo, the gradient of Jo is as follows:
!
!                                             T
!        gradx(Jo) = - (sum over observations) 2*H (Hn(x+xb)-yo)*(Po - Wgross)/Po
!
!      where, 
!
!          H = tangent linear model of Hn about x+xb
!
! 
!    Note that if Pgross = 0.0, then Wnotgross=1.0 and Wgross=0.0.  That is,
!    the code runs as though nonlinear quality control were not present
!    (which is indeed the case since the gross error probability is 0).  
!
!    As a result the same int* routines may be used for use with or without
!    nonlinear quality control.
!    
!
! program history log:
!   2003-12-18  derber
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - add nonlinear qc option
!   2004-10-06  kleist  - separate control vector for u,v, & convert int
!                         for wind components into int for st,vp
!   2004-11-30  treadon - add brightness temperatures to nonlinear 
!                         quality control
!   2004-12-03  treadon - replace mpe_iallreduce (IBM extension) with
!                         standard mpi_allreduce
!   2005-01-20  okamoto - add u,v to intrad
!   2005-02-23  wu      - changes related to normalized rh option
!   2005-04-11  treadon - rename intall_qc as intall
!   2005-05-18  yanqiu zhu - add 'use int*mod',and modify call interfaces for using these modules
!   2005-05-24  pondeca - take into consideration that npred=npredp=0
!                         for 2dvar only surface analysis option
!   2005-06-03  parrish - add horizontal derivatives
!   2005-07-10  kleist  - add dynamic constraint term
!   2005-09-29  kleist  - expand Jc term, include time derivatives vector
!   2005-11-21  kleist  - separate tendencies from Jc term, add call to calctends adjoint
!   2005-12-01  cucurull - add code for GPS local bending angle, add use obsmod for ref_obs
!   2005-12-20  parrish - add arguments to call to intt to allow for option of using boundary
!                         layer forward tlm.
!   2006-02-03  derber  - modify to increase reproducibility
!   2006-03-17  park    - correct error in call to intt--rval,sval --> rvaluv,svaluv
!                          in order to correctly pass wind variables.
!   2006-04-06  kleist  - include both Jc formulations
!   2006-07-26  parrish - correct inconsistency in computation of space and time derivatives of q
!                          currently, if derivatives computed, for q it is normalized q, but
!                          should be mixing ratio.
!   2006-07-26  parrish - add strong constraint initialization option
!   2006-09-20  derber  - add sensible temperatures for conventional temperatures
!   2006-10-12  treadon - replace virtual temperature with sensible in intpcp
!   2007-03-13  derber  - modify call to strong_bal_correction remove u_t_g, etc. arrays
!   2007-03-13  derber  - fix bug when nstrong > 1
!   2007-02-15  rancic  - add foto
!   2007-04-16  kleist  - modified calls to tendency and constraint routines
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2007-07-26 cucurull - update gps code to generalized vertical coordinate;
!                         get current solution for 3d pressure (sval3dp);
!                         move getprs_ad out of calctends_ad; add pri and 
!                         remove ps in calctends_ad argument list;
!                         add rval3dp; use getprs_ad
!   2007-08-08  derber - optimize - ensure that only necessary time derivatives are calculated
!
!   input argument list:
!     sval     - solution on grid
!     svalt  - t solution on grid
!     svalp  - psfc solution on grid
!     svaluv   - u,v solution on grid
!     sval_x   - x derivative solution on grid
!     sval_y   - y derivative solution on grid
!     sval_t   - t derivative solution on grid
!
!   output argument list:      
!     rval     - RHS on grid
!
!
! remarks:
!     if strong initialization, then svalt, svalp, svaluv, sval_x, sval_y, sval_t
!       are all grid fields after strong initialization.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use mpimod, only: ierror,mpi_comm_world,mpi_sum,mpi_rtype,levs_id,npe
  use jfunc, only: nclen,nclen1,nclen2,nrclen,nsclen,&
       npclen,ncw,np,nt,nsst,noz,nq,nst,nvp,nu,nv,nuvlen,switch_on_derivatives,&
       ntendlen,nut,nvt,ntt,nprst,nqt,nozt,ncwt,ndivt,nagvt,tendsflag,l_foto
  use constants, only: zero,zero_quad
  use gridmod, only: latlon1n,latlon11,lat2,lon2,nsig,nsig1o
  use jcmod, only: jcterm,jcdivt
  use obsmod, only: ref_obs
  use mod_vtrans,only: nvmodes_keep
  use mod_strong,only: nstrong
  implicit none
  
! Declare passed variables  
  integer(i_kind),intent(in):: mype
  real(r_kind),dimension(nclen),intent(in):: sval
  real(r_kind),dimension(latlon1n),intent(in):: svalt
  real(r_kind),dimension(latlon11),intent(in):: svalp
  real(r_kind),dimension(nuvlen),intent(in):: svaluv
  real(r_kind),dimension(nclen),intent(in):: sval_x,sval_y
  real(r_kind),dimension(ntendlen),intent(in):: sval_t
  real(r_kind),dimension(nclen),intent(out):: rval

! Declare local variables  	
  logical:: tracer
  integer(i_kind) i,k,nnn,nrclen1,mm1,n,j
  real(r_kind),dimension(max(1,nrclen),npe,2):: fhat_prdx,fhat_prdy
  real(r_quad),dimension(max(1,nrclen)):: fhat_prd
  real(r_kind),dimension(nuvlen):: rvaluv
  real(r_kind),dimension(latlon1n):: rvalt,rval_q,sval_q,rvaltsen,svaltsen
  real(r_kind),dimension(latlon11):: rvalp
  real(r_kind),dimension(nclen):: rval_x,rval_y
  real(r_kind),dimension(ntendlen):: rval_t
  real(r_kind),dimension(latlon1n):: rvaltsen_t,svaltsen_t
  real(r_kind),dimension(latlon1n+latlon11) :: sval3dp, rval3dp
  real(r_kind),dimension(latlon11):: ps_x,ps_y,ps_t
  integer(i_kind) istrong

!******************************************************************************

  nrclen1=max(1,nrclen)
  mm1=mype+1

! Zero gradient arrays
  do i=1,nclen
     rval(i)=zero
  end do
  if(switch_on_derivatives) then
    do i=1,nclen
       rval_x(i)=zero
       rval_y(i)=zero
    end do
  end if
  if (tendsflag) then
    do i=1,ntendlen
       rval_t(i)=zero
    end do
    do i=1,latlon1n
       rvaltsen_t(i)=zero
    end do
  end if
  do i=1,nuvlen
     rvaluv(i)=zero
  end do
  do i=1,latlon1n
     rvalt(i)=zero
     rvaltsen(i)=zero
  end do
  do i=1,latlon11
     rvalp(i)=zero
     ps_t(i)=zero
  end do
  do i=1,nrclen1
    fhat_prd(i)=zero_quad
  end do
  do i=1,latlon1n
     rval_q(i)=zero
  end do
  do i=1,latlon1n+latlon11
     rval3dp(i)=zero
  end do
     
! get current solution for 3d pressure
  call getprs_tl(svalp,svalt,sval3dp)

! Convert input normalized RH to q
  call normal_rh_to_q(sval(nq),svalt,sval3dp,sval_q)
  call tv_to_tsen(svalt,sval_q,svaltsen,sval_t(ntt),sval_t(nqt),svaltsen_t)

! RHS for conventional temperatures
  call intt(rvaltsen,svaltsen,rvalt,svalt,rval_q,sval_q,rvaluv(nu),svaluv(nu),&
            rvaluv(nv),svaluv(nv),rvalp,svalp,rval(nsst),sval(nsst),&
            rval_t(ntt),sval_t(ntt),rvaltsen_t,svaltsen_t,rval_t(nqt),sval_t(nqt),&
            rval_t(nut),sval_t(nut),rval_t(nvt),sval_t(nvt),&
            rval_t(nprst),sval_t(nprst))


! RHS for preciptitable water
  call intpw(rval_q,sval_q,rval_t(nqt),sval_t(nqt))

! RHS for conventional moisture
  call intq(rval_q,sval_q,rval_t(nqt),sval_t(nqt))

! RHS for conventional winds
  call intw(rvaluv(nu),rvaluv(nv),svaluv(nu),svaluv(nv),&
            rval_t(nut),rval_t(nvt),sval_t(nut),sval_t(nvt))

! RHS for radar superob winds
  call intsrw(rvaluv(nu),rvaluv(nv),svaluv(nu),svaluv(nv),&
              rval_t(nut),rval_t(nvt),sval_t(nut),sval_t(nvt))

! RHS for lidar winds
  call intdw(rvaluv(nu),rvaluv(nv),svaluv(nu),svaluv(nv),&
             rval_t(nut),rval_t(nvt),sval_t(nut),sval_t(nvt))

! RHS for radar winds
  call intrw(rvaluv(nu),rvaluv(nv),svaluv(nu),svaluv(nv),&
             rval_t(nut),rval_t(nvt),sval_t(nut),sval_t(nvt))

! RHS for wind speed observations
  call intspd(rvaluv(nu),rvaluv(nv),svaluv(nu),svaluv(nv),&
              rval_t(nut),rval_t(nvt),sval_t(nut),sval_t(nvt))

! RHS for ozone observations
  call intoz(rval(noz),sval(noz),rval_t(nozt),sval_t(nozt))

! RHS for surface pressure observations
  call intps(rvalp,svalp,rval_t(nprst),sval_t(nprst))

! RHS for GPS local observations
  call intgps(rvalt,rval_q,rval3dp,svalt,sval_q,sval3dp,&
              rval_t(ntt),rval_t(nqt),rval_t(nprst),&
              sval_t(ntt),sval_t(nqt),sval_t(nprst))

! RHS for conventional sst observations
  call intsst(rval(nsst),sval(nsst))

! RHS for moisture constraint
  call intlimq(rval(nq),sval(nq))

! RHS calculation for radiances
  call intrad(rvalt,rval_q,rval(noz),rvaluv(nu),rvaluv(nv),rval(nsst), &
              svalt,sval_q,sval(noz),svaluv(nu),svaluv(nv),sval(nsst), &
              fhat_prd(1),sval(nclen1+1),&
              rval_t(ntt),rval_t(nqt),rval_t(nozt),rval_t(nut),rval_t(nvt), &
              sval_t(ntt),sval_t(nqt),sval_t(nozt),sval_t(nut),sval_t(nvt))


! RHS calculation for precipitation
  call intpcp(rvaltsen,rval_q,rvaluv(nu),rvaluv(nv),rval(ncw), &
              svaltsen,sval_q,svaluv(nu),svaluv(nv),sval(ncw), &
              fhat_prd(nsclen+1),sval(nclen2+1),&
              rvaltsen_t,rval_t(nqt),rval_t(nut),rval_t(nvt),rval_t(ncwt),&
              svaltsen_t,sval_t(nqt),sval_t(nut),sval_t(nvt),sval_t(ncwt))


! RHS calculation for Jc term
  if (jcterm) then
    if (jcdivt) then
      call intjc_divt(rval_t(ndivt),rval_t(nagvt),sval_t(ndivt),sval_t(nagvt),mype)
    else
      call intjc(rval_t(nut),rval_t(nvt),rval_t(ntt),rval_t(nprst),&
                 sval_t(nut),sval_t(nvt),sval_t(ntt),sval_t(nprst),mype)
    end if
  end if

 call tv_to_tsen_ad(rvalt,rval_q,rvaltsen,rval_t(ntt),rval_t(nqt),rvaltsen_t)
 if (tendsflag) then
    if(nvmodes_keep > 0 .and. nstrong > 0) then
      nnn=0
      do k=1,nsig1o
         if (levs_id(k)/=0) nnn=nnn+1
      end do
      do istrong=nstrong,1,-1
        if(istrong < nstrong) then
          tracer=.true.
          call calctends_ad(rvaluv(nu),rvaluv(nv),rvalt ,             &
                            rval_q,rval(noz),rval(ncw),                         &
                            rval_x(nst),rval_y(nst),rval_x(nvp),rval_y(nvp),    &
                            rval_x(nt), rval_y(nt), rval_x(np), rval_y(np),     &
                            rval_x(nq), rval_y(nq), rval_x(noz),rval_y(noz),    &
                            rval_x(ncw),rval_y(ncw),mype,                &
                            rval_t(nut),rval_t(nvt),rval_t(ntt),rval_t(nprst),  &
                            rval_t(nqt),rval_t(nozt),rval_t(ncwt),rval3dp,tracer)
          do i=1,ntendlen
             rval_t(i)=zero
          end do
          call tget_derivatives( &
               rvaluv(nu) ,rvaluv(nv), rvalt     ,rvalp   ,  &
               rval_q     ,rval  (noz),rval  (nsst),rval  (ncw), &
               rval_x(nst),rval_x(nvp),rval_x(nt)  ,rval_x(np),  &
               rval_x(nq) ,rval_x(noz),rval_x(nsst),rval_x(ncw), &
               rval_y(nst),rval_y(nvp),rval_y(nt)  ,rval_y(np),  &
               rval_y(nq) ,rval_y(noz),rval_y(nsst),rval_y(ncw),nnn,mype)
          do i=1,nclen
             rval_x(i)=zero
             rval_y(i)=zero
          end do
        end if

        call normal_rh_to_q_ad(rval(nq),rvalt,rval3dp,rval_q)

        call getprs_ad(rvalp,rvalt,rval3dp)
        call getprs_ad(ps_t,rval_t(ntt),rval_t(nprst))

        call strong_bal_correction_ad(rval_t(nut),rval_t(nvt),rval_t(ntt),ps_t,&
                    mype,rvaluv(nu),rvaluv(nv),rvalt,rvalp)
!
        do i=nprst,nprst+latlon11+latlon1n-1
           rval_t(i)=zero
        end do
        do i=1,latlon11
           rval_t(nprst+i-1)=ps_t(i)
        end do

      end do
    end if

    if (jcdivt) call jcdivtends_ad(rval_t(nut),rval_t(nvt),rval_t(ntt),rval_t(nprst),  &
         mype,rval_t(ndivt),rval_t(nagvt))
    tracer=.true.
    call calctends_ad(rvaluv(nu),rvaluv(nv),rvalt,                        &
                      rval_q,rval(noz),rval(ncw),                         &
                      rval_x(nst),rval_y(nst),rval_x(nvp),rval_y(nvp),    &
                      rval_x(nt), rval_y(nt), rval_x(np), rval_y(np),     &
                      rval_x(nq), rval_y(nq), rval_x(noz),rval_y(noz),    &
                      rval_x(ncw),rval_y(ncw),mype,                       &
                      rval_t(nut),rval_t(nvt),rval_t(ntt),rval_t(nprst),  &
                      rval_t(nqt),rval_t(nozt),rval_t(ncwt),rval3dp,tracer)  
 end if

! add contributions from derivatives
  if(switch_on_derivatives) then
    nnn=0
    do k=1,nsig1o
       if (levs_id(k)/=0) nnn=nnn+1
    end do
    call tget_derivatives( &
         rvaluv(nu) ,rvaluv(nv), rvalt     ,rvalp   ,  &
         rval_q     ,rval  (noz),rval  (nsst),rval  (ncw), &
         rval_x(nst),rval_x(nvp),rval_x(nt)  ,rval_x(np),  &
         rval_x(nq) ,rval_x(noz),rval_x(nsst),rval_x(ncw), &
         rval_y(nst),rval_y(nvp),rval_y(nt)  ,rval_y(np),  &
         rval_y(nq) ,rval_y(noz),rval_y(nsst),rval_y(ncw),nnn,mype)
  end if

!   adjoint of convert input normalized RH to q to add contribution of moisture
!     to t, p , and normalized rh

  call normal_rh_to_q_ad(rval(nq),rvalt,rval3dp,rval_q)

  call getprs_ad(rvalp,rvalt,rval3dp)

!  adjoint of load dirxt and dirxp
  do i=1,latlon1n
    rval(nt+i-1)=rval(nt+i-1)+rvalt(i)
  end do
  do i=1,latlon11
    rval(np+i-1)=rval(np+i-1)+rvalp(i)
  end do

! Convert RHS calculations for u,v to st/vp for application of
! background error
  call getstvp(rvaluv(nu),rvaluv(nv),rval(nst),rval(nvp))

!  Break quad precision number into two double precision numbers 
!  on each processor for mpi_allreduce

  fhat_prdx=zero
  do i=1,nrclen1
    fhat_prdx(i,mm1,1)=fhat_prd(i)
    fhat_prdx(i,mm1,2)=fhat_prd(i)-fhat_prdx(i,mm1,1)
  end do
! Reduce RHS for bias correction terms from radiances and precip
  call mpi_allreduce(fhat_prdx,fhat_prdy,2*npe*nrclen1,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)

!  Combine double precision numbers together and sum into quad precision number

  fhat_prd=zero_quad
  do j=1,npe
    do i=1,nrclen1
      fhat_prd(i)=fhat_prd(i)+fhat_prdy(i,j,1)+fhat_prdy(i,j,2)
    end do
  end do
  do i=1,nrclen1
    rval(nclen1+i)=rval(nclen1+i)+fhat_prd(i)
  end do
    

  return
  end subroutine intall
