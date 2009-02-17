subroutine stpcalc(stpinout,xhat,xhatt,xhatp,xhatuv,xhat_x,xhat_y,xhat_t, &
     dirx,dirxt,dirxp,dirxuv,dirx_x,dirx_y,dirx_t,diry,penalty,mype,end_iter)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpcalc     calculate penalty and stepsize
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: calculate current penalty and estimate stepsize
!               (nonlinear qc version)
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
!    As a result the same stp* routines may be used for use with or without
!    nonlinear quality control.
!    
!    Please note, however, that using the nonlinear qc routines makes the
!    stp* and int* operators nonlinear.  Hence, the need to evaluate the
!    step size operators twice for each observation type, give the current
!    step size algorithm coded below. 
!
!
! program history log:
!   2003-12-18  derber,j.
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - add nonlinear qc option
!   2004-10-06  kleist  - separate control vector for u,v, get search
!                         direction for u,v from dir for st,vp
!   2004-11-30  treadon - add brightness temperatures to nonlinear
!                         quality control
!   2005-01-20  okamoto - add u,v to stprad_qc
!   2005-01-26  cucurull- implement local GPS RO linear operator
!   2005-02-10  treadon - add u,v to stprad_qc (okamoto change not present)
!   2005-02-23  wu      - add call to normal_rh_to_q to convert normalized 
!                         RH to q
!   2005-04-11  treadon - rename stpcalc_qc as stpcalc
!   2005-05-21  yanqiu zhu - add 'use stp*mod', and modify call interfaces for using these modules
!   2005-05-27  derber - remove linear stepsize estimate
!   2005-06-03  parrish - add horizontal derivatives
!   2005-07-10  kleist  - add dynamic constraint term (linear)
!   2005-09-29  kleist  - expand Jc term, include time derivatives vector
!   2005-11-21  kleist  - separate tendencies from Jc term, add call to calctends tlm
!   2005-12-01  cucurull - add code for GPS local bending angle, add use obsmod for ref_obs
!   2005-12-20  parrish - add arguments to call to stpt to enable boundary layer forward
!                         model option.
!   2006-04-18  derber - add explicit iteration over stepsize (rather than 
!                        repeated calls) - clean up and simplify
!   2006-04-24  kleist - include both Jc formulations
!   2006-05-26  derber - modify to improve convergence checking
!   2006-07-26  parrish - correct inconsistency in computation of space and time derivatives of q
!                          currently, if derivatives computed, for q it is normalized q, but
!                          should be mixing ratio.
!   2006-08-04  parrish - add strong constraint initialization option
!   2006-09-18  derber - modify output from nonlinear operators to make same as linear operators
!   2006-09-20  derber - add sensible temperatures for conventional obs.
!   2006-10-12  treadon - replace virtual temperature with sensible in stppcp
!   2007-02-15  rancic  - add foto
!   2007-04-16  kleist  - modified calls to tendency and constraint routines
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2007-07-26  cucurull - update gps code to generalized vertical coordinate;
!                          get current solution for 3d pressure (xhat_3dp);
!                          move getprs_tl out of calctends_tl; add dirx3dp
!                          and remove ps in calctends_tl argument list;
!                          use getprs_tl 
!   2007-08-08  derber - optimize, ensure that only necessary time derivatives are calculated
!
!   input argument list:
!     stpinout - guess stepsize
!     xhat     - current solution
!     xhatt  - current solution for temp
!     xhatp  - current solution for psfc
!     xhatuv   - current solution for u,v
!     xhat_x   - current solution x derivative
!     xhat_y   - current solution y derivative
!     xhat_t   - current solution t derivative
!     dirx     - search direction for x
!     diry     - search direction for y (B-1 dirx)
!     mype     - pe number
!     end_iter - end iteration flag
!
!   output argument list:
!     dirxt  - search direction for temp
!     dirxp  - search direction for psfc
!     dirxuv   - search direction for u,v
!     dirx_x   - x derivative of search direction for x 
!     dirx_y   - y derivative of search direction for x
!     dirx_t   - t derivative of search direction for x
!     stpinout - final estimate of stepsize
!     penalty  - penalty
!     end_iter - end iteration flag false if stepsize successful
!
!
! remarks:
!     The part of xhat and dirx containing temps and psfc are values before strong initialization,
!     xhatt, xhatp and dirxt, dirxp contain temps and psfc after strong initialization.
!     If strong initialization is turned off, then xhatt, etc are equal to the corresponding 
!     fields in xhat, dirx.
!     xhatuv, xhat_x, xhat_y, xhat_t and dirxuv, dirx_x, dirx_y, dirx_t are all after
!     strong initialization if it is turned on.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mpi_sum,levs_id,npe
  use constants, only:  zero,one_tenth,quarter,half,one,two,zero_quad
  use jfunc, only: noz,nq,nt,nsst,ncw,np,iout_iter,nst,nvp,&
       nclen,nclen1,nclen2,nsclen,npclen,xhatsave,yhatsave,factqmin,factqmax,&
       nuvlen,nu,nv,iter,switch_on_derivatives,ntendlen,nut,nvt,ntt,nprst,&
       nqt,nozt,ncwt,ndivt,nagvt,tendsflag,l_foto
  use gridmod, only: latlon1n,latlon11,lat2,lon2,nsig,nsig1o
  use jcmod, only: jcterm,jcdivt
  use obsmod, only: ref_obs
  use mod_vtrans,only: nvmodes_keep
  use mod_strong,only: nstrong
  use specmod, only: jcap
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
  real(r_kind),intent(inout):: stpinout
  logical,intent(inout):: end_iter
  real(r_kind),intent(out):: penalty
  real(r_kind),dimension(nuvlen),intent(out):: dirxuv
  real(r_kind),dimension(nclen),intent(in)::dirx,xhat,diry
  real(r_kind),dimension(latlon1n),intent(in)::xhatt
  real(r_kind),dimension(latlon11),intent(in)::xhatp
  real(r_kind),dimension(nuvlen),intent(in):: xhatuv
  real(r_kind),dimension(nclen),intent(in)::xhat_x,xhat_y
  real(r_kind),dimension(nclen),intent(out)::dirx_x,dirx_y
  real(r_kind),dimension(ntendlen),intent(in)::xhat_t
  real(r_kind),dimension(ntendlen),intent(out)::dirx_t
  real(r_kind),dimension(latlon1n),intent(out)::dirxt
  real(r_kind),dimension(latlon11),intent(out)::dirxp

! Declare local parameters
  integer(i_kind),parameter:: ipen = 20
  integer(i_kind),parameter:: istp_iter = 5
  integer(i_kind),parameter:: ipen_tot = ipen*12
  integer(i_kind),parameter:: ipenlin = 4
  integer(i_kind),parameter:: ioutpen = istp_iter*4

! Declare local variables
  logical:: fullfield, tracer
  integer(i_kind) i,j,k,nnn,mm1,ii
  integer(i_kind) istrong,istp_use
  real(r_quad),dimension(6,ipen):: pbc,pbcx
  real(r_quad),dimension(ipen):: bpen,cpen   
  real(r_quad),dimension(ipenlin):: pstart   
  real(r_quad) bx,cx
  real(r_kind),dimension(ipen):: stpx   
  real(r_kind),dimension(4,ipen):: pen   
  real(r_kind),dimension(2,6,ipen,npe):: pbc1,pbc2   
  real(r_kind) dels,delpen,temp
  real(r_kind) outpensave,stprat
  real(r_kind),dimension(4)::sges
  real(r_kind),dimension(0:istp_iter):: stp   
  real(r_kind),dimension(ioutpen):: outpen,outstp
  real(r_kind),dimension(latlon1n):: dirx_q,xhat_q,dirx_tsen,xhat_tsen
  real(r_kind),dimension(latlon1n):: dirx_tsen_t,xhat_tsen_t
  real(r_kind),dimension(latlon1n+latlon11):: dirx3dp,xhat_3dp
  real(r_kind),dimension(latlon11):: ps_x,ps_y,ps_t

!************************************************************************************  
! Initialize variable
  mm1=mype+1
  stp(0)=stpinout
  outpen = zero

!   Begin calculating contributions to penalty and stepsize for various terms
!   stepsize = sum(b)/sum(c)
!
!    pbc(1,*) - penalty
!    pbc(2,*) - b
!    pbc(3,*) - c
!
!    linear terms
!
!    pbc(*,1)  contribution from background
!    pbc(*,2)  contribution from bias coeff. background term
!    pbc(*,3)  contribution from precip. bias correction background term
!    pbc(*,4)  contribution from dynamic constraint term (Jc)
!
!  nonlinear terms
!
!    pbc(*,5)  contribution from wind observation term
!    pbc(*,6)  contribution from satellite radiance observation term
!    pbc(*,7)  contribution from temperature observation term
!    pbc(*,8)  contribution from precipitable water obs. term
!    pbc(*,9)  contribution from specific humidity obs.term
!    pbc(*,10) contribution from ozone obs. term
!    pbc(*,11) contribution from doppler lidar wind
!    pbc(*,12) contribution from doppler radar wind
!    pbc(*,13) contribution from radar superob wind
!    pbc(*,14) contribution from GPS local observations
!    pbc(*,15) contribution from conventional sst
!    pbc(*,16) contribution from wind speed obs. term
!    pbc(*,17) contribution from precipitation term
!    pbc(*,18) contribution from negative moisture constraint term
!    pbc(*,19) contribution from excess moisture term
!    pbc(*,20) contribution from surface pressure observation term


  pbc=zero_quad
  pstart = zero_quad

! penalty, b and c for background terms

! pbc(1,1)= dplev(xhatsave,yhatsave)
  call dplev1(xhatsave,yhatsave,mype,pstart(1))
  call dplev1(dirx,yhatsave,mype,pbc(5,1))
  pbc(5,1)=-pbc(5,1)
  call dplev1(dirx,diry,mype,pbc(6,1))

! bias correction terms

! penalty, b, c terms for satellite bias correction

  if(mype == min(1,npe-1)) then
     do i=nclen1+1,nclen2
        pstart(2)=pstart(2)+xhatsave(i)*yhatsave(i)
        pbc(5,2)=pbc(5,2)-dirx(i)*yhatsave(i)
        pbc(6,2)=pbc(6,2)+dirx(i)*diry(i)
     end do
  end if

! penalty, b, c terms for precipitation bias correction

  if(mype == min(2,npe-1)) then
     do i=nclen2+1,nclen
        pstart(3)=pstart(3)+xhatsave(i)*yhatsave(i)
        pbc(5,3)=pbc(5,3)-dirx(i)*yhatsave(i)
        pbc(6,3)=pbc(6,3)+dirx(i)*diry(i)
     end do
  end if

! Convert search direction for st/vp to u/v for stp routines
  call getuv(dirxuv(nu),dirxuv(nv),dirx(nst),dirx(nvp))

!  load dirxt and dirxp
  do i=1,latlon1n
    dirxt(i)=dirx(nt+i-1)
  end do
  do i=1,latlon11
    dirxp(i)=dirx(np+i-1)
  end do

! get 3d pressure 
  call getprs_tl(xhatp,xhatt,xhat_3dp)
  call getprs_tl(dirxp,dirxt,dirx3dp)

! convert normalized rh variable to q
  call normal_rh_to_q(dirx(nq),dirxt,dirx3dp,dirx_q)
  call normal_rh_to_q(xhat(nq),xhatt,xhat_3dp,xhat_q)

  if(switch_on_derivatives) then
!     compute derivatives
! Determine how many vertical levels each mpi task will
! handle in computing horizontal derivatives
    nnn=0
    do k=1,nsig1o
      if (levs_id(k)/=0) nnn=nnn+1
    end do
    call get_derivatives( &
       dirxuv(nu) ,dirxuv(nv) ,dirxt     ,dirxp   ,  &
       dirx_q     ,dirx  (noz),dirx  (nsst),dirx  (ncw), &
       dirx_x(nst),dirx_x(nvp),dirx_x(nt)  ,dirx_x(np),  &
       dirx_x(nq) ,dirx_x(noz),dirx_x(nsst),dirx_x(ncw), &
       dirx_y(nst),dirx_y(nvp),dirx_y(nt)  ,dirx_y(np),  &
       dirx_y(nq) ,dirx_y(noz),dirx_y(nsst),dirx_y(ncw), &
       nnn,mype,1)
  end if

  if (tendsflag) then
    tracer=.true.
    call calctends_tl( &
       dirxuv(nu) ,dirxuv(nv)  ,dirxt    ,                 &
       dirx_q     ,dirx(noz)   ,dirx(ncw)  ,               &
       dirx_x(nst),dirx_y(nst) ,dirx_x(nvp),dirx_y(nvp),   &
       dirx_x(nt) ,dirx_y(nt)  ,dirx_x(np) ,dirx_y(np),    &
       dirx_x(nq) ,dirx_y(nq)  ,dirx_x(noz),dirx_y(noz),   &
       dirx_x(ncw),dirx_y(ncw) ,     mype,          &
       dirx_t(nut),dirx_t(nvt) ,dirx_t(ntt),dirx_t(nprst), &
       dirx_t(nqt),dirx_t(nozt),dirx_t(ncwt),dirx3dp,tracer)

    if (jcdivt) call jcdivtends(dirx_t(nut),dirx_t(nvt),dirx_t(ntt),&
         dirx_t(nprst),mype,dirx_t(ndivt),dirx_t(nagvt))

    if(nvmodes_keep > 0 .and. nstrong > 0) then
      fullfield=.false.
      do istrong=1,nstrong
        do i=1,latlon11
          ps_t(i)=dirx_t(nprst+i-1)
        end do
        do i=1,latlon11+latlon1n
          dirx_t(nprst+i-1)=zero
        end do
        call strong_bal_correction(dirx_t(nut),dirx_t(nvt),dirx_t(ntt), &
             ps_t,mype,dirxuv(nu),dirxuv(nv),dirxt,dirxp,.false.,fullfield,.true.)

!    update dirx3dp to be consistent with new dirxt, dirxp
        call getprs_tl(dirxp,dirxt,dirx3dp)
        call getprs_tl(ps_t,dirx_t(ntt),dirx_t(nprst))

!    update dirx_q to be consistent with new dirxt, dirxp
        call normal_rh_to_q(dirx(nq),dirxt,dirx3dp,dirx_q)

     
        if(istrong < nstrong) then
          tracer=.true.
          call get_derivatives( &
             dirxuv(nu) ,dirxuv(nv) ,dirxt     ,dirxp ,      &
             dirx_q     ,dirx  (noz),dirx  (nsst),dirx(ncw), &
             dirx_x(nst),dirx_x(nvp),dirx_x(nt)  ,dirx_x(np),  &
             dirx_x(nq) ,dirx_x(noz),dirx_x(nsst),dirx_x(ncw), &
             dirx_y(nst),dirx_y(nvp),dirx_y(nt)  ,dirx_y(np),  &
             dirx_y(nq) ,dirx_y(noz),dirx_y(nsst),dirx_y(ncw), &
             nnn,mype,1)

          call calctends_tl( &
             dirxuv(nu) ,dirxuv(nv)  ,dirxt     ,                &
             dirx_q     ,dirx(noz)   ,dirx(ncw)  ,               &
             dirx_x(nst),dirx_y(nst) ,dirx_x(nvp),dirx_y(nvp),   &
             dirx_x(nt) ,dirx_y(nt)  ,dirx_x(np) ,dirx_y(np),    &
             dirx_x(nq) ,dirx_y(nq)  ,dirx_x(noz),dirx_y(noz),   &
             dirx_x(ncw),dirx_y(ncw) ,mype,          &
             dirx_t(nut),dirx_t(nvt) ,dirx_t(ntt),dirx_t(nprst), &
             dirx_t(nqt),dirx_t(nozt),dirx_t(ncwt),dirx3dp,tracer)
        end if
      end do
    end if
  end if

  call tv_to_tsen(dirxt,dirx_q,dirx_tsen,dirx_t(ntt),dirx_t(nqt),dirx_tsen_t)
  call tv_to_tsen(xhatt,xhat_q,xhat_tsen,xhat_t(ntt),xhat_t(nqt),xhat_tsen_t)

! step size from dynamic constraint based on time tendencies
  if (jcterm) then
    if (jcdivt) then
      call stpjc_divt(dirx_t(ndivt),dirx_t(nagvt),xhat_t(ndivt),xhat_t(nagvt),&
                 mype,pstart(4),pbc(5,4),pbc(6,4))
    else
      call stpjc(dirx_t(nut),dirx_t(nvt),dirx_t(ntt),dirx_t(nprst),&
                 xhat_t(nut),xhat_t(nvt),xhat_t(ntt),xhat_t(nprst),&
                 mype,pstart(4),pbc(5,4),pbc(6,4))
    end if
  end if


! iterate over number of stepsize iterations (istp_iter - currently set to 2
  stepsize: do ii=1,istp_iter

    dels=one_tenth ** ii
!   Do nonlinear terms
  
    sges(2)=(one-dels)*stp(ii-1)
    sges(3)= stp(ii-1)
    sges(4)=(one+dels)*stp(ii-1)
    if(ii == 1)then
      sges(1)=zero
    else
      sges(1)=0.5_r_kind*sges(2)
    end if

!$omp parallel sections
    do i=1,ipenlin
      do j=1,4
        pbc(j,i)=pstart(i)-2.0_r_quad*pbc(5,i)*sges(j)+ &
                pbc(6,i)*sges(j)*sges(j)
      end do
    end do

!$omp section
!   penalty, b, and c for winds
    call stpw(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
              pbc(1,5),sges,dirx_t(nut),dirx_t(nvt),xhat_t(nut),xhat_t(nvt))

!$omp section
!   penalty, b, and c for radiances
    call stprad(dirxt  ,dirx_q,dirx(noz),dirxuv(nu),dirxuv(nv),dirx(nsst), &
                xhatt  ,xhat_q,xhat(noz),xhatuv(nu),xhatuv(nv),xhat(nsst), &
                dirx(nclen1+1),xhat(nclen1+1), &
                pbc(1,6),sges, &
                dirx_t(ntt),dirx_t(nqt),dirx_t(nozt),dirx_t(nut),dirx_t(nvt),&
                xhat_t(ntt),xhat_t(nqt),xhat_t(nozt),xhat_t(nut),xhat_t(nvt))

!$omp section
!   penalty, b, and c for temperature
    call stpt(dirx_tsen,xhat_tsen,dirxt,xhatt,dirx_q,xhat_q, &
              dirxuv(nu),xhatuv(nu),dirxuv(nv),xhatuv(nv), &
              dirxp  ,xhatp  ,dirx(nsst),xhat(nsst), &
              pbc(1,7),sges, &
              dirx_t(ntt),xhat_t(ntt),&
              dirx_tsen_t,xhat_tsen_t,&
              dirx_t(nqt),xhat_t(nqt), &
              dirx_t(nut),xhat_t(nut),dirx_t(nvt),xhat_t(nvt), &
              dirx_t(nprst),xhat_t(nprst))

!$omp section
!   penalty, b, and c for precipitable water
    call stppw(dirx_q,xhat_q,pbc(1,8),sges,dirx_t(nqt),xhat_t(nqt))

!$omp section
!   penalty, b, and c for moisture
    call stpq(dirx_q,xhat_q,pbc(1,9),sges,dirx_t(nqt),xhat_t(nqt))

!$omp section
!   penalty, b, and c for ozone
    call stpoz(dirx(noz),xhat(noz),pbc(1,10),sges,dirx_t(nozt),xhat_t(nozt))

!$omp section
!   penalty, b, and c for wind lidar
    call stpdw(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
               pbc(1,11),sges, &
               dirx_t(nut),dirx_t(nvt),xhat_t(nut),xhat_t(nvt))

!$omp section
!   penalty, b, and c for radar
    call stprw(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
               pbc(1,12),sges, &
               dirx_t(nut),dirx_t(nvt),xhat_t(nut),xhat_t(nvt))

!$omp section
!   penalty, b, and c for radar superob wind
    call stpsrw(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
                pbc(1,13),sges, &
                dirx_t(nut),dirx_t(nvt),xhat_t(nut),xhat_t(nvt))

!$omp section
!   penalty, b, and c for GPS local observation
    call stpgps(dirxt,dirx_q,dirx3dp,xhatt,xhat_q,xhat_3dp,&
         pbc(1,14),sges, &
         dirx_t(ntt),dirx_t(nqt),dirx_t(nprst),&
         xhat_t(ntt),xhat_t(nqt),xhat_t(nprst))

!$omp section
!   penalty, b, and c for conventional sst
    call stpsst(dirx(nsst),xhat(nsst),pbc(1,15),sges)

!$omp section
!   penalty, b, and c for wind speed
    call stpspd(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
                pbc(1,16),sges, &
                dirx_t(nut),dirx_t(nvt),xhat_t(nut),xhat_t(nvt))

!$omp section
!   penalty, b, and c for precipitation
    call stppcp(dirx_tsen,dirx_q,dirxuv(nu),dirxuv(nv),dirx(ncw), &
                xhat_tsen,xhat_q,xhatuv(nu),xhatuv(nv),xhat(ncw), &
                dirx(nclen2+1),xhat(nclen2+1),                   &
                pbc(1,17),sges, &
                dirx_tsen_t,dirx_t(nqt),dirx_t(nut),dirx_t(nvt),dirx_t(ncwt),&
                xhat_tsen_t,xhat_t(nqt),xhat_t(nut),xhat_t(nvt),xhat_t(ncwt))

!$omp section
!   penalty, b, and c for moisture constraint
    call stplimq(dirx(nq),xhat(nq),dirx(ncw),xhat(ncw),sges,       &
                 pbc(1,18),pbc(1,19))
  
!$omp section
!   penalty, b, and c for surface pressure
    call stpps(dirxp,xhatp,pbc(1,20),sges,dirx_t(nprst),xhat_t(nprst))

!$omp end parallel sections

!   break up quad precision number into 2 double precision numbers for rediction
    pbc1=zero
    do j=1,ipen
      do i=1,6
        pbc1(1,i,j,mm1)=pbc(i,j)
        pbc1(2,i,j,mm1)=pbc(i,j)-pbc1(1,i,j,mm1)
      end do
    end do

    call mpi_allreduce(pbc1,pbc2,ipen_tot*npe,mpi_rtype,mpi_sum, &
               mpi_comm_world,ierror)

!  reintegrate quad precision number and sum over processors
    pbcx=zero_quad
    do k=1,npe
      do j=1,ipen
        do i=1,6
          pbcx(i,j)=pbcx(i,j)+pbc2(1,i,j,k)+pbc2(2,i,j,k)
        end do
      end do
    end do

!   penalty and sum b and sum c
    pen=zero
    bpen=zero_quad
    cpen=zero_quad
    do i=1,ipen
      pen(1,i)=pbcx(1,i)
      pen(2,i)=pbcx(2,i)
      pen(3,i)=pbcx(3,i)
      pen(4,i)=pbcx(4,i)
      bpen(i)=bpen(i)+pbcx(5,i)
      cpen(i)=cpen(i)+pbcx(6,i)
    end do
    do i=1,4
     do j=1,ipen
      outpen((ii-1)*4+i) = outpen((ii-1)*4+i)+pen(i,j)
     end do
     outstp((ii-1)*4+i) = sges(i)
    end do
    bx=zero_quad
    cx=zero_quad
    stpx=zero
    do i=1,ipen
      bx=bx+bpen(i)
      cx=cx+cpen(i)
      if(cpen(i) > 1.e-20_r_kind)stpx(i)=bpen(i)/cpen(i)
    end do

!   estimate of stepsize

    stp(ii)=stp(ii-1)
    if(cx > 1.e-20_r_kind) stp(ii)=bx/cx         ! step size estimate
!   stp(ii)=min(stp(ii),one)

    delpen = stp(ii)*(bx - 0.5_r_quad*stp(ii)*cx ) 

    penalty=outpen(1)
    stpinout=stp(ii)
    if(abs(delpen/penalty) < 1.e-17_r_kind) then
      if(mype == 0)then
        if(ii == 1)write(iout_iter,100) (pen(1,i),i=1,ipen)
        write(iout_iter,140) ii,delpen,bx,cx,stp(ii)
        write(iout_iter,101) (stpx(i),i=1,ipen)
        write(iout_iter,105) (bpen(i),i=1,ipen)
        write(iout_iter,110) (cpen(i),i=1,ipen)
      end if
      end_iter = .true.
      return
    end if

    if(cx < 1.e-20_r_kind .or. stpinout < zero) then
      stpinout=outstp(1)
      outpensave=outpen(1)
      do i=2,ii*4
        if(outpen(i) < outpensave)then
          stpinout=outstp(i)
          stp(ii)=stpinout
          outpensave=outpen(i)
        end if
      end do
      if(stpinout <= zero .and. ii /= istp_iter)then
        stp(ii)=outstp(2)
        do i=3,ii*4
          stp(ii)=min(outstp(i),stp(ii))
        end do
        stp(ii)=0.1_r_kind*stp(ii)
      end if
    end if

    if(ii == 1 .and. mype == 0) then
      write(iout_iter,100) (pen(1,i),i=1,ipen)
      write(iout_iter,101) (stpx(i),i=1,ipen)
      write(iout_iter,105) (bpen(i),i=1,ipen)
      write(iout_iter,110) (cpen(i),i=1,ipen)
    endif
100 format(' J=',3e25.18/,(3x,3e25.18))
101 format(' S=',3e25.18/,(3x,3e25.18))
105 format(' b=',3e25.18/,(3x,3e25.18))
110 format(' c=',3e25.18/,(3x,3e25.18))
130 format('***WARNING***  negative or small cx inner', &
           ' iteration terminated - probable error',i2,3e25.18)
140 format('***WARNING***  expected penalty reduction small ',/,  &
           ' inner iteration terminated - probable convergence',i2,4e25.18)
    istp_use=ii
    stprat=zero
    if(stp(ii) > zero)then
      stprat=abs((stp(ii)-stp(ii-1))/stp(ii))
    end if
!   if(mype == 0)write(6,*)' stprat ',stprat
    if(stprat < 1.e-4_r_kind) exit stepsize

  end do stepsize

  stpinout=stp(istp_use)
  if(stpinout < zero)then
    if(mype == 0)then
      write(iout_iter,130) ii,bx,cx,stp(ii)
      write(iout_iter,101) (stpx(i),i=1,ipen)
      write(iout_iter,105) (bpen(i),i=1,ipen)
      write(iout_iter,110) (cpen(i),i=1,ipen)
    end if
    end_iter = .true.
  end if
  if(mype == 0)then
    write(iout_iter,200) (stp(i),i=0,istp_use)
    write(iout_iter,201) (outstp(i),i=1,istp_use*4)
    write(iout_iter,202) (outpen(i)-outpen(1),i=1,istp_use*4)
  end if
200 format(' stepsize estimates = ',6(e24.18,1x))
201 format(' stepsize guesses = ',(8(e12.6,1x)))
202 format(' penalties        = ',(8(e12.6,1x)))


  return
end subroutine stpcalc
