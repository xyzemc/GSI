module convthin_supobs
!$$$   module documentation block
!                .      .    .                                   .
! module:  convthin_supobs
!  prgmmr: X.Su : adopted from convthin program to do super observation for 3
!  dimensions                         2014-12-22
!
! abstract:
!
! subroutines included:
!   make3grids_s
!   map3grids_s
!   del3grids_s
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: make3grids_s
  public :: map3grids_s
  public :: del3grids_s
! set passed variables to public
  public :: use_all_s

  integer(i_kind):: mlat
  integer(i_kind),allocatable,dimension(:):: mlon
  integer(i_kind),allocatable,dimension(:,:):: icount,ibest_obs,ibest_save

  real(r_kind),allocatable,dimension(:):: glat
  real(r_kind),allocatable,dimension(:,:):: glon,hll
  logical use_all_s

contains

  subroutine make3grids_s(rmesh,nlevp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    make3grids_s
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine sets up dimensions for and allocates
!            thinning grids.
!
! program history log:
!
!   input argument list:
!     rmesh - mesh size (km) of thinning grid.  If (rmesh <= one),
!             then no thinning of the data will occur.  Instead,
!             all data will be used without thinning.
!     nlevp -  vertical levels
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: rearth_equator,two,deg2rad,zero,half,one,pi
    use satthin, only:dlat_grid,dlon_grid,rlat_min,rlon_min

    implicit none

    real(r_kind)   ,intent(in   ) :: rmesh
    integer(i_kind),intent(in   ) :: nlevp

    real(r_kind),parameter:: r360 = 360.0_r_kind

    integer(i_kind) i,j,it
    integer(i_kind) mlonx,mlonj,itxmax

    real(r_kind) delonx,delat,dgv,halfpi,dx,dy
    real(r_kind) twopi
    real(r_kind) factor,delon
    real(r_kind) rkm2dg,glatm
    !   If there is to be no thinning, simply return to calling routine
    use_all_s=.false.
    if(abs(rmesh) <= one)then
       use_all_s=.true.
       itxmax=2.e6_i_kind
       return
    end if

!   Set constants
    halfpi = half*pi
    twopi  = two*pi
    rkm2dg = r360/(twopi*rearth_equator)*1.e3_r_kind

!   Set up dimensions and allocate arrays for thinning grids
!       horizontal
    if (rmesh<zero) rkm2dg=one
    dx    = rmesh*rkm2dg
    dy    = dx
    mlat  = dlat_grid/dy + half
    mlonx = dlon_grid/dx + half
    delat = dlat_grid/mlat
    delonx= dlon_grid/mlonx
    dgv  = delat*half
    mlat=max(2_i_kind,mlat);   mlonx=max(2_i_kind,mlonx)

    allocate(mlon(mlat),glat(mlat),glon(mlonx,mlat),hll(mlonx,mlat))


!   Set up thinning grid lon & lat.  The lon & lat represent the location of the
!   lower left corner of the thinning grid box.
    itxmax=0
    do j = 1,mlat
       glat(j) = rlat_min + (j-1)*delat
       glat(j) = glat(j)*deg2rad
       glatm = glat(j) + dgv*deg2rad

       factor = abs(cos(abs(glatm)))
       if (rmesh>zero) then
          mlonj   = nint(mlonx*factor)
          mlon(j) = max(2_i_kind,mlonj)
          delon = dlon_grid/mlon(j)
       else
          delon = factor*rmesh
          delon = min(delon,r360)
          mlon(j) = dlon_grid/delon
       endif

       glat(j) = min(max(-halfpi,glat(j)),halfpi)
       do i = 1,mlon(j)
          itxmax=itxmax+1
          hll(i,j)=itxmax
          glon(i,j) = rlon_min + (i-1)*delon
          glon(i,j) = glon(i,j)*deg2rad
          glon(i,j) = min(max(zero,glon(i,j)),twopi)
       enddo

    end do

!   Allocate  and initialize arrays
    allocate(icount(itxmax,nlevp))
    allocate(ibest_obs(itxmax,nlevp))
    allocate(ibest_save(itxmax,nlevp))

    do j=1,nlevp
       do i=1,itxmax
             icount(i,j) = 0
             ibest_obs(i,j)= 0
             ibest_save(i,j)= 0
       end do
    end do

    return
  end subroutine make3grids_s



  subroutine map3grids_s(flg,pflag,pcoord,nlevp,dlat_earth,dlon_earth,pob,iobs,iobsout,iin,&
            iiout,iuse,maxobs,usage,rusage,suob,dvob,spd,std_spd,std_dirct,pobb,slat,slon,&
            rlat_sup,rlon_sup,icount_obs,timdiff,rtime,uob,vob,ssuob,ssvob)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    map3grids_s
!     prgmmr:    Xiujuan Su     org: np23                date: 2014-12-22
!
! abstract:  This routine maps convential observations to a 3d thinning grid.
!
! program history log:
!   input argument list:
!     flg        - marks order of values in vertical dirction (1=increasing, -1=decreasing)
!     pflag - type of pressure-type levels; 0 : sigma level, 1 : determined by convinfo file
!     pcoord     - veritical coordinate values
!     nlevp       - number of vertical levels
!     dlat_earth - earth relative observation latitude (radians)
!     dlon_earth - earth relative observation longitude (radians)
!     pob        - observation pressure ob
!     crit1      - quality indicator for observation (smaller = better)
!     ithin      - number of obs to retain per thinning grid box
!     iin        - counter of input data
!
!   output argument list:
!     iobs  - observation counter
!     itx   - combined (i,j) index of observation on thinning grid
!     itt   - superobs thinning counter
!     iobsout- location for observation to be put
!     ip    - vertical index
!     iuse  - .true. if observation should be used
!     iiout - counter of data replaced
!     usage - data usage flag, 0 to keep, 101.0 not to keep
!      
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: one, half,two,three,zero
    implicit none
    
    logical                      ,intent(  out) :: iuse
    integer(i_kind)              ,intent(in   ) :: nlevp,pflag,flg,iin,maxobs
    integer(i_kind)              ,intent(inout) :: iobs
    integer(i_kind)              ,intent(  out) :: iobsout,iiout
    real(r_kind)                 ,intent(in   ) :: dlat_earth,dlon_earth,pob,usage,slat,slon,suob,dvob,timdiff,uob,vob
    real(r_kind),dimension(nlevp),intent(in   ) :: pcoord
    real(r_kind),dimension(maxobs),intent(inout   ) :: rusage,pobb,spd,std_spd,std_dirct,rlat_sup,rlon_sup,rtime,ssuob,ssvob 
    integer(i_kind),dimension(maxobs),intent(inout   ) :: icount_obs 
    
    integer(i_kind):: ip,itx
    integer(i_kind) ix,iy

    real(r_kind) dlat1,dlon1,pob1
    real(r_kind) dx,dy,dp,dxx,dyy,dpp

    iiout = 0

!   If using all data (no thinning), simply return to calling routine
    if(use_all_s)then
       iuse=.true.
       iobs=iobs+1
       iobsout=iobs
       rusage(iobs)=usage
       std_spd(iobs)=suob*suob
       std_dirct(iobs)=dvob*dvob
       spd(iobs)=suob
       pobb(iobs)=pob
       icount_obs(iobs)=1
       rlat_sup(iobs)=slat
       rlon_sup(iobs)=slon 
       rtime(iobs)=timdiff 
       if(suob >zero) then
          ssuob(iobs)=uob/suob
          ssvob(iobs)=vob/suob
       else
          ssuob(iobs)=uob
          ssvob(iobs)=vob
       endif
       return
    end if

!   Compute (i,j,k) indices of coarse mesh grid (grid number 1) which 
!   contains the current observation.
    dlat1=dlat_earth
    dlon1=dlon_earth
    pob1=pob

    call grdcrd(pob1,1,pcoord,nlevp,flg)
    ip=int(pob1)
    dp=pob1-ip
    ip=max(1,min(ip,nlevp))
    
    call grdcrd(dlat1,1,glat,mlat,1)
    iy=int(dlat1)
    dy=dlat1-iy
    iy=max(1,min(iy,mlat))
    
    call grdcrd(dlon1,1,glon(1,iy),mlon(iy),1)
    ix=int(dlon1)
    dx=dlon1-ix
    ix=max(1,min(ix,mlon(iy)))
    
    dxx=half-min(dx,one-dx)
    dyy=half-min(dy,one-dy)
    if( pflag == 1) then 
       dpp=half-min(dp,one-dp)
    else
       dpp=min(dp,one-dp)
    endif

    itx=hll(ix,iy)



!   Examine various cases regarding what to do with current obs.
!   Start by assuming observation will be selected.  
    iuse=.true.
!   Case:   not the first observation 
    if (icount(itx,ip) > 0 ) then
       iobs=iobs+1
       iobsout=iobs
       icount(itx,ip)=icount(itx,ip)+1
       iiout = ibest_obs(itx,ip)
       rusage(iiout)=101.0_r_kind
       ibest_save(itx,ip)=iin
       ibest_obs(itx,ip)=iobs
       rusage(iobs)=usage
       std_spd(iobs)=suob*suob+std_spd(iiout)
       std_dirct(iobs)=dvob*dvob+std_dirct(iiout)
       spd(iobs)=suob+spd(iiout)
       pobb(iobs)=pob+pobb(iiout)
       rlat_sup(iobs)=slat+rlat_sup(iiout)
       rlon_sup(iobs)=slon+rlon_sup(iiout)
       icount_obs(iobs)=icount(itx,ip)
       rtime(iobs)=timdiff+rtime(iiout)
       if(suob >zero) then
          ssuob(iobs)=uob/suob+ssuob(iiout)
          ssvob(iobs)=vob/suob+ssvob(iiout)
       else
          ssuob(iobs)=uob+ssuob(iiout)
          ssvob(iobs)=vob+ssvob(iiout)
       endif
      
!   Case:  first obs at this location, 
!     -->  keep this obs as starting point
    elseif (icount(itx,ip)==0) then
       iobs=iobs+1
       iobsout=iobs
       ibest_obs(itx,ip) = iobs
       icount(itx,ip)=icount(itx,ip)+1
       ibest_save(itx,ip) = iin 
       rusage(iobs)=usage
       std_spd(iobs)=suob*suob
       std_dirct(iobs)=dvob*dvob
       spd(iobs)=suob
       pobb(iobs)=pob
       icount_obs(iobs)=icount(itx,ip)
       rlat_sup(iobs)=slat
       rlon_sup(iobs)=slon
       rtime(iobs)=timdiff
       if(suob >zero) then
          ssuob(iobs)=uob/suob
          ssvob(iobs)=vob/suob
       else
          ssuob(iobs)=uob
          ssvob(iobs)=vob
       endif

    end if

     write(6,*) 'CONVTHIN_S:iobs,iobsout,ibest_obs,icount,ibest_save,rusage,std_spd,std_dirct,spd,dirct,&
                 pobb,icount_obs,rlat_sup,rlon_sup=',iobs,iobsout,ibest_obs(itx,ip),icount(itx,ip),&
                 ibest_save(itx,ip),rusage(iobs),std_spd(iobs),std_dirct(iobs),spd(iobs),pobb(iobs),&
                 icount_obs(iobs),rlat_sup(iobs),rlon_sup(iobs),usage,suob,dvob,slat,slon

    return
    end subroutine map3grids_s 

     subroutine del3grids_s
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    del3grids                            
!     prgmmr:    kistler     org: np23                date: 2006-01-25
!
! abstract:  This routine deallocates arrays used in 3d thinning
!
! program history log:
!   2006-01-25  kistler - original routine
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    if (.not.use_all_s) then
       deallocate(mlon,glat,glon,hll)
       deallocate(icount)
       deallocate(ibest_obs)
       deallocate(ibest_save)
    endif
  end subroutine del3grids_s

end module convthin_supobs
