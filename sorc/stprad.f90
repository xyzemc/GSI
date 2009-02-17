subroutine stprad(rt,rq,roz,ru,rv,rst,st,sq,soz,su,sv,sst, &
     rpred,spred,out,sges,drt,drq,droz,dru,drv,dst,dsq,dsoz,dsu,dsv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprad compute contribution to penalty and stepsize
!                from rad, using nonlinear qc.
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: compute contribution to penalty and stepsize from radiances.
!
! program history log:
!   1990-10-11  parrish
!   1992-07-21
!   1995-07-17  derber
!   1997-03-10  wu       
!   1998-02-02  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-07-30  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-01-20  okamoto - add wind components
!   2005-04-11  treadon - merge stprad and stprad_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!
!   input argument list:
!     rt       - search direction for temperature
!     rq       - search direction for moisture 
!     roz      - search direction for ozone
!     ru       - search direction for zonal wind
!     rv       - search direction for meridional wind
!     rst      - search direction for skin temperature
!     st       - input temperature correction field        
!     sq       - input q correction field        
!     soz      - input ozone correction field        
!     su       - input u correction field
!     sv       - input v correction field
!     sst      - input skin temp. vector 
!     rpred    - search direction for predictors
!     spred    - input predictor values
!     sges1    - step size estimates(4)
!     drt      - search direction for time derivative of temperature
!     drq      - search direction for time derivative of moisture 
!     droz     - search direction for time derivative of ozone
!     dru      - search direction for time derivative of zonal wind
!     drv      - search direction for time derivative of meridional wind
!     dst      - input time derivative of temperature correction field        
!     dsq      - input time derivative of q correction field        
!     dsoz     - input time derivative of ozone correction field        
!     dsu      - input time derivative of u correction field
!     dsv      - input time derivative of v correction field
!
!   output argument list:
!     out(1)   - penalty for radiance data sges(1)
!     out(2)   - penalty for radiance data sges(2)
!     out(3)   - penalty for radiance data sges(3)
!     out(4)   - penalty for radiance data sges(4)
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use radinfo, only: npred1,npred,jpch_rad,b_rad,pg_rad
  use obsmod, only: radptr,radhead
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad
  use gridmod, only: nsig,nsig2,nsig3,nsig4,nsig3p1,nsig3p2,nsig3p3,&
       latlon11,latlon1n,lat1,lon1
  implicit none
  
! Declare passed variables
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: rt,st,rq,sq,roz,soz,&
      ru,su,rv,sv,drt,dst,drq,dsq,droz,dsoz,dru,dsu,drv,dsv
  real(r_kind),dimension(latlon11),intent(in):: rst,sst
  real(r_kind),dimension(jpch_rad,npred),intent(in):: rpred,spred
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables
  integer(i_kind) nn,n,ic,k,nx,n_1,n_2,n_3,n_4,j1,j2,j3,j4,i
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  real(r_kind) val2,tlap2,val,tlap,w1,w2,w3,w4
  real(r_kind),dimension(nsig3p3):: tdir,rdir
  real(r_kind) cg_rad,rad0,rad1,rad2,rad3,pen1,pen3,wgross,wnotgross
  real(r_kind) term,term1,term2,term3,time_rad
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3,halfvar_rad
  real(i_kind),dimension(nsig) :: j1n,j2n,j3n,j4n

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

  radptr=>radhead
  do while(associated(radptr))
    if(radptr%luse)then
     j1=radptr%ij(1)
     j2=radptr%ij(2)
     j3=radptr%ij(3)
     j4=radptr%ij(4)
     w1=radptr%wij(1)
     w2=radptr%wij(2)
     w3=radptr%wij(3)
     w4=radptr%wij(4)
     time_rad=radptr%time
     tdir(nsig3p1)=w1* su(j1)  + w2* su(j2) + w3* su(j3)  + w4* su(j4)+ &
                  (w1*dsu(j1)  + w2*dsu(j2) + w3*dsu(j3)  + w4*dsu(j4))*time_rad
     rdir(nsig3p1)=w1* ru(j1)  + w2* ru(j2) + w3* ru(j3)  + w4* ru(j4)+ &
                  (w1*dru(j1)  + w2*dru(j2) + w3*dru(j3)  + w4*dru(j4))*time_rad
     tdir(nsig3p2)=w1* sv(j1)  + w2* sv(j2) + w3* sv(j3)  + w4* sv(j4)+ &
                  (w1*dsv(j1)  + w2*dsv(j2) + w3*dsv(j3)  + w4*dsv(j4))*time_rad
     rdir(nsig3p2)=w1* rv(j1)  + w2* rv(j2) + w3* rv(j3)  + w4* rv(j4)+ &
                  (w1*drv(j1)  + w2*drv(j2) + w3*drv(j3)  + w4*drv(j4))*time_rad
     tdir(nsig3p3)=w1*sst(j1) + w2*sst(j2)+ w3*sst(j3) + w4*sst(j4)   
     rdir(nsig3p3)=w1*rst(j1) + w2*rst(j2)+ w3*rst(j3) + w4*rst(j4)   

     n_1=nsig; n_2=nsig2
     j1n(1) = j1
     j2n(1) = j2
     j3n(1) = j3
     j4n(1) = j4
     do n=2,nsig
      j1n(n) = j1n(n-1)+latlon11
      j2n(n) = j2n(n-1)+latlon11
      j3n(n) = j3n(n-1)+latlon11
      j4n(n) = j4n(n-1)+latlon11
     enddo
!$omp parallel do private(n,j1,j2,j3,j4)
     do n=1,nsig
!        n_1=n_1+1; n_2=n_2+1
     j1 = j1n(n)
     j2 = j2n(n)
     j3 = j3n(n)
     j4 = j4n(n)

!       Input state vector
        tdir(n)=  w1* st(j1) +w2* st(j2) + w3* st(j3) +w4* st(j4)+ &
                 (w1*dst(j1) +w2*dst(j2) + w3*dst(j3) +w4*dst(j4))*time_rad
        tdir(nsig+n)=w1* sq(j1) +w2* sq(j2) + w3* sq(j3) +w4* sq(j4)+ &
                 (w1*dsq(j1) +w2*dsq(j2) + w3*dsq(j3) +w4*dsq(j4))*time_rad
        tdir(nsig2+n)=w1* soz(j1)+w2* soz(j2)+ w3* soz(j3)+w4* soz(j4)+ &
                 (w1*dsoz(j1)+w2*dsoz(j2)+ w3*dsoz(j3)+w4*dsoz(j4))*time_rad

!       Input search direction vector
        rdir(n)=  w1* rt(j1) +w2* rt(j2) + w3* rt(j3) +w4* rt(j4)+ &
                 (w1*drt(j1) +w2*drt(j2) + w3*drt(j3) +w4*drt(j4))*time_rad
        rdir(nsig+n)=w1* rq(j1) +w2* rq(j2) + w3* rq(j3) +w4* rq(j4)+ &
                 (w1*drq(j1) +w2*drq(j2) + w3*drq(j3) +w4*drq(j4))*time_rad
        rdir(nsig2+n)=w1* roz(j1)+w2* roz(j2)+ w3* roz(j3)+w4* roz(j4)+ &
                 (w1*droz(j1)+w2*droz(j2)+ w3*droz(j3)+w4*droz(j4))*time_rad

!        j1=j1+latlon11
!        j3=j3+latlon11
!        j2=j2+latlon11
!        j4=j4+latlon11
     end do
!$omp end parallel do
     pen1=zero
     pen3=zero
     do nn=1,radptr%nchan
        ic=radptr%icx(nn)

!       contribution from bias corection
        tlap=radptr%pred2(nn)
        tlap2=tlap*tlap
        val2=-radptr%res(nn)+spred(ic,npred)*tlap+spred(ic,npred1)*tlap2
        val=                 rpred(ic,npred)*tlap+rpred(ic,npred1)*tlap2

        do nx=1,npred-2
           val2=val2+spred(ic,nx)*radptr%pred1(nx)
           val =val +rpred(ic,nx)*radptr%pred1(nx)
        end do

!       contribution from atmosphere
        do k=1,nsig3p3
           val2=val2+tdir(k)*radptr%dtb_dvar(k,nn)
           val =val +rdir(k)*radptr%dtb_dvar(k,nn)
        end do

!       multiply by variance
        rad0=val2+sges(1)*val
        rad1=val2+sges(2)*val
        rad2=val2+sges(3)*val
        rad3=val2+sges(4)*val
        
        term  = radptr%err2(nn)*rad0*rad0
        term1 = radptr%err2(nn)*rad1*rad1
        term2 = radptr%err2(nn)*rad2*rad2
        term3 = radptr%err2(nn)*rad3*rad3
        
!  Modify penalty term if nonlinear QC
        if(nlnqc_iter .and. pg_rad(ic) > tiny_r_kind .and. &
                            b_rad(ic)  > tiny_r_kind)then
           cg_rad=cg_term/b_rad(ic)
           wnotgross= one-pg_rad(ic)
           wgross = pg_rad(ic)*cg_rad/wnotgross
           term  = -two*log((exp(-half*term ) + wgross)/(one+wgross))
           term1 = -two*log((exp(-half*term1) + wgross)/(one+wgross))
           term2 = -two*log((exp(-half*term2) + wgross)/(one+wgross))
           term3 = -two*log((exp(-half*term3) + wgross)/(one+wgross))
        endif

        out(1) = out(1) + term*radptr%raterr2(nn)
        out(2) = out(2) + term1*radptr%raterr2(nn)
        out(3) = out(3) + term2*radptr%raterr2(nn)
        out(4) = out(4) + term3*radptr%raterr2(nn)
        pen1   = pen1   + (term1-term2)*radptr%raterr2(nn)
        pen3   = pen3   + (term3-term2)*radptr%raterr2(nn)

     end do
     cc  = pen1+pen3
     out(5)  = out(5)+(pen1-pen3)*bcoef1+cc*bcoef2
     out(6)  = out(6)+cc*ccoef

    end if

    radptr => radptr%llpoint
  end do
  return
end subroutine stprad
