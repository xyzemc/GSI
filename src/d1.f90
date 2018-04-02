subroutine init_sf_xy_ensgrp(jcap_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_sf_xy
!   prgmmr: parrish          org: np23                date: 2009-12-17
!
! abstract: initialize horizontal spectral localization sf_xy
!
! program history log:
!   2009-12-17  parrish
!   2010-06-29  parrish, modify so localization length can be different for each vertical level.
!                 to do this, add new variable array s_ens_hv(nsig), which is read in if readin_localization=.true.
!                 Otherwise, s_ens_hv is set equal to s_ens_h.
!
!   input argument list:
!     jcap_in - maximum spectral truncation allowed
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters,only: s_ens_hv,sp_loc,grd_ens,grd_loc,sp_ens,n_ens,p_sploc2ens,grd_sploc
  use hybrid_ensemble_parameters,only: use_localization_grid
  use gridmod,only: use_sp_eqspace
  use general_specmod, only: general_init_spec_vars
  use constants, only: zero,half,one,two,three,rearth,pi
  use constants, only: rad2deg
  use mpimod, only: mype
  use general_sub2grid_mod, only: general_sub2grid_create_info
  use egrid2agrid_mod,only: g_create_egrid2agrid
  use general_sub2grid_mod, only: sub2grid_info
  use gsi_enscouplermod, only: gsi_enscoupler_localization_grid
  use gsi_io, only: verbose
  implicit none

  integer(i_kind),intent(in   ) :: jcap_in

  integer(i_kind) i,ii,j,k,l,n,jcap,kk,nsigend
  real(r_kind),allocatable::g(:),gsave(:)
  real(r_kind) factor
  real(r_kind),allocatable::rkm(:),f(:,:),f0(:,:)
  real(r_kind) ftest(grd_loc%nlat,grd_loc%nlon,grd_loc%kbegin_loc:grd_loc%kend_alloc)
  real(r_single) out1(grd_ens%nlon,grd_ens%nlat)
  real(r_single),allocatable::pn0_npole(:)
  real(r_kind) s_ens_h_min
  real(r_kind) rlats_ens_local(grd_ens%nlat)
  real(r_kind) rlons_ens_local(grd_ens%nlon)
  character(5) mapname
  logical make_test_maps
  logical,allocatable,dimension(:)::ksame
  integer(i_kind) nord_sploc2ens
  integer(i_kind) nlon_sploc0,nlon_sploc,nlat_sploc,num_fields
  logical print_verbose

  print_verbose = .false. .and. mype == 0
  if(verbose .and. mype == 0)print_verbose=.true.
  make_test_maps=.false.
  nord_sploc2ens=4

!    make sure s_ens_hv is within allowable range  ( pi*rearth*.001/jcap_in <= s_ens_hv <= 5500 )

  s_ens_h_min=pi*rearth*.001_r_kind/jcap_in
  do k=1,grd_ens%nsig
     if(s_ens_hv(k) <  s_ens_h_min) then
        if(mype == 0) write(6,*)' s_ens_hv(',k,') = ',s_ens_hv(k),' km--too small, min value = ', &
                                        s_ens_h_min,' km.'
        if(mype == 0) write(6,*)' s_ens_hv(',k,') reset to min value'
        s_ens_hv(k)=s_ens_h_min
     else if(s_ens_hv(k) >  5500._r_kind) then
        if(mype == 0) write(6,*)' s_ens_hv(',k,') = ',s_ens_hv(k),' km--too large, max value = 5500 km.'
        if(mype == 0) write(6,*)' s_ens_hv(',k,') reset to max value'
        s_ens_hv(k)=5500._r_kind
     end if
  enddo


  jcap=nint(1.2_r_kind*pi*rearth*.001_r_kind/minval(s_ens_hv))
  jcap=min(jcap,jcap_in)

!  if it is desired to have a different localization grid to apply the spectral
!  localization, do that here, with resolution based on the value of jcap:
  call gsi_enscoupler_localization_grid (rlats_ens_local,rlons_ens_local)
  if(use_localization_grid) then
     nlat_sploc=min(grd_ens%nlat-4,max(int(half*grd_ens%nlat),2*jcap+4))
     if(mod(nlat_sploc,2)/=0) nlat_sploc=nlat_sploc+1   ! make number of gaussian lats even
     nlon_sploc0=2*nlat_sploc
     call acceptable_for_essl_fft(nlon_sploc0,nlon_sploc)
  else
     nlat_sploc=grd_ens%nlat
     nlon_sploc=grd_ens%nlon
     nlon_sploc0=nlon_sploc
  end if
  if(print_verbose)then
     write(6,*)' nlat_sploc,nlon_sploc0,nlon_sploc=',nlat_sploc,nlon_sploc0,nlon_sploc
     write(6,*)' nlat_ens  ,nlon_ens              =',grd_ens%nlat,grd_ens%nlon
  end if
  num_fields=grd_ens%nsig*n_ens
  call general_sub2grid_create_info(grd_sploc,1,nlat_sploc,nlon_sploc,grd_ens%nsig,num_fields,.false.)

!  set up spectral variables for jcap

  call general_init_spec_vars(sp_loc,jcap,jcap,nlat_sploc,nlon_sploc,eqspace=use_sp_eqspace)
  if(print_verbose) then
     if( grd_ens%nlon == nlon_sploc .and. grd_ens%nlat == nlat_sploc)then
        write(6,*)' ensemble and analysis nlat,nlon are the same '
     else
        do j=1,grd_ens%nlon
           if(j.le.nlon_sploc) then
              write(6,'(" j,rlon_sploc(j),rlon_ens(j)=",i4,2f12.3)') &
                  j,rad2deg*sp_loc%rlons(j),rad2deg*sp_ens%rlons(j)
           else
              write(6,'(" j,              rlon_ens(j)=",i4,12x,f12.3)') &
                  j,rad2deg*sp_ens%rlons(j)
           end if
        enddo
        do i=1,grd_ens%nlat
           if(i.le.nlat_sploc) then
              write(6,'(" i,rlat_sploc(i),rlat_ens(i)=",i4,2f12.3)') &
                  i,rad2deg*sp_loc%rlats(i),rad2deg*sp_ens%rlats(i)
           else
              write(6,'(" i,              rlat_ens(i)=",i4,12x,f12.3)') &
                  i,rad2deg*sp_ens%rlats(i)
           end if
        enddo
     end if
  end if

!   regardless of whether or not nlat_sploc=grd_ens%nlat and nlon_sploc=grd_ens%nlon, compute
!    interpolation structure variable that will be used for interpolation from sp_loc grid to grd_ens.
!   if they are identical, then the interpolation is just an identity op.
  call g_create_egrid2agrid(grd_ens%nlat,rlats_ens_local,grd_ens%nlon,rlons_ens_local,&
                            nlat_sploc,sp_loc%rlats,nlon_sploc,sp_loc%rlons, &
                            nord_sploc2ens,p_sploc2ens,.true.,eqspace=use_sp_eqspace)

!    the following code is used to compute the desired spectrum to get a
!     gaussian localization of desired length-scale.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   how spectrum is obtained:
!
!     Correlation matrix:  C = Y*D*Ytrans
!
!       where Y is matrix of spherical harmonics, evaluated on gaussian grid, and D is a diagonal matrix
!
!     To obtain D, exploit fact that for D a function only of total wave-number n, then C is homogeneous
!       and isotropic on the sphere.
!
!     So look at the special case of a test point centered on the north pole.  The correlation function
!       is then only a function of latitude, call it c(phi), where c(pi/2) = 1.
!
!     Now we have C = P*D*Ptrans, where we have reduced the problem to 1 dimension, latitude, and in
!       spectral space, total wave number n.  P is the zonal component only of Y.
!
!     Next, form the product
!                             C*e1 =P*D*Ptrans*e1,
!
!            where e1 is a vector of all 0, except for 1 at the north pole.
!
!     Then have P*D*Ptrans*e1 = sum(n) p(n,j)*d(n)*p(n,1) = c(j,1)
!
!        where j=1 corresponds to north pole point in this formulation.
!
!     Now if we have available C(j,1), a gaussian of desired length-scale evaluated (note, doesn't have to
!            be gaussian!) on the gaussian grid, then applying the inverse transform subroutine g2s0 to
!     C yields the product
!
!             Chat(n) = d(n)*p(n,1)
!
!     So finally the desired spectrum is
!
!               d(n) = chat(n)/p(n,1)
!
!     To create the full spectral transform of the desired correlation, d(n) is copied to all non-zero
!      zonal wave numbers on lines of constant total wave number n, multiplied by 1/2 to account for
!      two degrees of freedom for non-zero zonal wave numbers.
!
!     Note that while creating this routine, a bug was discovered in s2g0 routine for evaluation of pole
!       value.  There was a missing factor of 1/sqrt(2) apparently.
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                 create reference gaussian centered on north pole with desired length-scale.

!     compute latitudes in km from north pole

  allocate(rkm(grd_sploc%nlat),f(grd_sploc%nlat,grd_sploc%nlon),f0(grd_sploc%nlat,grd_sploc%nlon))
  rkm(grd_sploc%nlat)=zero
  rkm(1)=two*asin(one)*rearth*.001_r_kind
  do i=1,(grd_sploc%nlat-2)/2
     rkm(grd_sploc%nlat-i)=(asin(one)-asin(sp_loc%slat(i)))*rearth*.001_r_kind
     rkm(1+i)=(asin(one)+asin(sp_loc%slat(i)))*rearth*.001_r_kind
  enddo
  if(print_verbose) write(6,*)' in init_sf_xy, lat,max(dlat)=', &
           rkm(1+(grd_sploc%nlat-2)/2), &
          -rkm(grd_sploc%nlat-(grd_sploc%nlat-2)/2)+rkm(1+(grd_sploc%nlat-2)/2),' km'

  allocate(spectral_filter(sp_loc%nc,grd_sploc%nsig),nscalgrp)
  allocate(sqrt_spectral_filter(sp_loc%nc,grd_sploc%nsig,nsclgrp))
  allocate(g(sp_loc%nc),gsave(sp_loc%nc))
  allocate(pn0_npole(0:sp_loc%jcap))
  allocate(ksame(grd_sploc%nsig))
  ksame=.false.
  do k=2,grd_sploc%nsig
     if(s_ens_hv(k) == s_ens_hv(k-1))ksame(k)=.true.
  enddo
  do k=1,grd_sploc%nsig
     if(ksame(k))then
        spectral_filter(:,k)=spectral_filter(:,k-1)
     else
        do i=1,grd_sploc%nlat
           f0(i,1)=exp(-half*(rkm(i)/s_ens_hv(k))**2)
        enddo

        do j=2,grd_sploc%nlon
           do i=1,grd_sploc%nlat
              f0(i,j)=f0(i,1)
           enddo
        enddo

        call general_g2s0(grd_sploc,sp_loc,g,f0)

        call general_s2g0(grd_sploc,sp_loc,g,f)

!       adjust so value at np = 1
        f=f/f(grd_sploc%nlat,1)
        f0=f
        call general_g2s0(grd_sploc,sp_loc,g,f)
        call general_s2g0(grd_sploc,sp_loc,g,f)
        if(mype == 0)then
           nsigend=k
           do kk=k+1,grd_sploc%nsig
              if(s_ens_hv(kk) /= s_ens_hv(k))exit
              nsigend=nsigend+1
           enddo
           write(6,900)k,nsigend,sp_loc%jcap,s_ens_hv(k),maxval(abs(f0-f))
  900      format(' in init_sf_xy, jcap,s_ens_hv(',i5,1x,'-',i5,'), max diff(f0-f)=', &
                                        i10,f10.2,e20.10)
        end if

!            correct spectrum by dividing by pn0_npole
        gsave=g

!       obtain pn0_npole
        do n=0,sp_loc%jcap
           g=zero
           g(2*n+1)=one
           call general_s2g0(grd_sploc,sp_loc,g,f)
           pn0_npole(n)=f(grd_sploc%nlat,1)
        enddo
   
        g=zero
        do n=0,sp_loc%jcap
           g(2*n+1)=gsave(2*n+1)/pn0_npole(n)
        enddo

!       obtain spectral_filter

        ii=0
        do l=0,sp_loc%jcap
           factor=one
           if(l >  0) factor=half
           do n=l,sp_loc%jcap
              ii=ii+1
              if(sp_loc%factsml(ii)) then
                 spectral_filter(ii,k)=zero
              else
                 spectral_filter(ii,k)=factor*g(2*n+1)
              end if
              ii=ii+1
              if(l == 0 .or. sp_loc%factsml(ii)) then
                 spectral_filter(ii,k)=zero
              else
                 spectral_filter(ii,k)=factor*g(2*n+1)
              end if
           enddo
        enddo
     end if
  enddo
  deallocate(g,gsave,pn0_npole,ksame)

  sqrt_spectral_filter=sqrt(spectral_filter)

!  assign array k_index for each processor, based on grd_loc%kbegin_loc,grd_loc%kend_loc

  allocate(k_index(grd_loc%kbegin_loc:grd_loc%kend_alloc))
  k_index=0
  do k=grd_loc%kbegin_loc,grd_loc%kend_loc
     k_index(k)=1+mod(k-1,grd_loc%nsig)
!!     write(6,*) 'k_index(',k,')=',k_index(k)
  enddo

  if(make_test_maps) then
   ftest=zero
   do k=grd_loc%kbegin_loc,grd_loc%kend_loc
      ftest(grd_ens%nlat/2,grd_ens%nlon/2,k)=one
   enddo
   call sf_xy(ftest,grd_loc%kbegin_loc,grd_loc%kend_loc)
   if(mype==0) then
      do j=1,grd_ens%nlon
        do i=1,grd_ens%nlat
           out1(j,i)=ftest(i,j,grd_loc%kbegin_loc)
        enddo
     enddo
     write(mapname,'("out_",i2.2)')1+mod(grd_loc%kbegin_loc-1,grd_ens%nsig)
     call outgrads1(out1,grd_ens%nlon,grd_ens%nlat,mapname)
   end if
  end if
  deallocate(rkm,f,f0)
  return

end subroutine init_sf_xy
