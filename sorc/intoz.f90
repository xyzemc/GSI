subroutine intoz(roz,soz,droz,dsoz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intoz       apply nonlin qc obs operator for ozone
!   prgmmr: derber           org: np23                date: 1995-07-11
!
! abstract:  This routine applies the observation operator (forward
!            model) and adjoint of this operator for ozone observations
!            with the addition of nonlinear qc.
!
! program history log:
!   1995-07-11  derber
!   1999-03-01  wu - port cray90 code to ibm-sp (mpi version)
!   2004-06-16  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intoz and intoz_qc into single routine
!   2005-06-14  wu      - add OMI total ozone
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-05-30  h.liu   - move interpolation weights w1-w4 inside k loop
!   2007-02-15  rancic - add foto
!
!   input argument list:
!     soz  - ozone increment in grid space
!     dsoz - time derivative of ozone increment in grid space
!
!   output argument list:
!     roz  - ozone results from observation operator 
!     droz - time derivative of ozone results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind,r_quad
  use ozinfo, only:b_oz,pg_oz
  use obsmod, only: ozhead,ozptr
  use qcmod, only: nlnqc_iter
  use gridmod, only: lat2,lon2,nsig,latlon11,latlon1n
  use constants, only: one,half,two,zero,tiny_r_kind,cg_term
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2*lon2,nsig),intent(in):: soz,dsoz
  real(r_kind),dimension(lat2*lon2,nsig),intent(inout):: roz,droz

! Declare local variables
  integer(i_kind) i,k,j1,j2,j3,j4,kk,iz1,iz2,kx,kkk
  real(r_kind) dz1,pob,delz,time_oz
  real(r_quad) val1,valx
  real(r_kind) cg_oz,p0,wnotgross,wgross
  real(r_kind) w1,w2,w3,w4

!
! SBUV OZONE: LAYER O3 and TOATL O3
!
! Loop over ozone observations.
  ozptr => ozhead
  do while (associated(ozptr))

!    Set location
     j1=ozptr%ij(1)
     j2=ozptr%ij(2)
     j3=ozptr%ij(3)
     j4=ozptr%ij(4)

     time_oz = ozptr%time

!    Accumulate contribution from layer observations
     dz1=nsig+1
     if ( ozptr%nloz >= 1 ) then

     do k=1,ozptr%nloz
        val1= -ozptr%res(k)
        pob = ozptr%prs(k)
        iz1=dz1
        if (iz1 > nsig) iz1=nsig
        iz2=pob
        do kk=iz2,iz1
           delz=one
           if (kk==iz1) delz=dz1-iz1
           if (kk==iz2) delz=delz-pob+iz2
           w1=ozptr%wij(1,kk)*delz
           w2=ozptr%wij(2,kk)*delz
           w3=ozptr%wij(3,kk)*delz
           w4=ozptr%wij(4,kk)*delz
           val1=val1 +  &
                w1* soz(j1,kk)+ &
                w2* soz(j2,kk)+ &
                w3* soz(j3,kk)+ &
                w4* soz(j4,kk)+ &
               (w1*dsoz(j1,kk)+ &
                w2*dsoz(j2,kk)+ &
                w3*dsoz(j3,kk)+ &
                w4*dsoz(j4,kk))*time_oz
        enddo

        valx     = val1*ozptr%err2(k)*ozptr%raterr2(k)
       
        do kk=iz2,iz1
           delz=one
           if(kk.eq.iz1)delz=dz1-iz1
           if(kk.eq.iz2)delz=delz-pob+iz2
           w1=ozptr%wij(1,kk)*delz*valx
           w2=ozptr%wij(2,kk)*delz*valx
           w3=ozptr%wij(3,kk)*delz*valx
           w4=ozptr%wij(4,kk)*delz*valx
           roz(j1,kk)  =  roz(j1,kk) + w1
           roz(j2,kk)  =  roz(j2,kk) + w2
           roz(j3,kk)  =  roz(j3,kk) + w3
           roz(j4,kk)  =  roz(j4,kk) + w4
           droz(j1,kk) = droz(j1,kk) + w1*time_oz
           droz(j2,kk) = droz(j2,kk) + w2*time_oz
           droz(j3,kk) = droz(j3,kk) + w3*time_oz
           droz(j4,kk) = droz(j4,kk) + w4*time_oz
        enddo
        dz1=pob
     end do

     end if

!    Add contribution from total column observation
     k=ozptr%nloz+1
     val1= -ozptr%res(k)
     do kk=1,nsig
        w1=ozptr%wij(1,kk)
        w2=ozptr%wij(2,kk)
        w3=ozptr%wij(3,kk)
        w4=ozptr%wij(4,kk)
        val1=val1 + ( &
             w1* soz(j1,kk)+ &
             w2* soz(j2,kk)+ &
             w3* soz(j3,kk)+ &
             w4* soz(j4,kk)+ &
            (w1*dsoz(j1,kk)+ &
             w2*dsoz(j2,kk)+ &
             w3*dsoz(j3,kk)+ &
             w4*dsoz(j4,kk))*time_oz)
     enddo

     valx     = val1*ozptr%err2(k)*ozptr%raterr2(k)

     do kk=1,nsig
        w1=ozptr%wij(1,kk)*valx
        w2=ozptr%wij(2,kk)*valx
        w3=ozptr%wij(3,kk)*valx
        w4=ozptr%wij(4,kk)*valx
        roz(j1,kk)  = roz(j1,kk) + w1
        roz(j2,kk)  = roz(j2,kk) + w2
        roz(j3,kk)  = roz(j3,kk) + w3
        roz(j4,kk)  = roz(j4,kk) + w4
        droz(j1,kk) =droz(j1,kk) + w1*time_oz
        droz(j2,kk) =droz(j2,kk) + w2*time_oz
        droz(j3,kk) =droz(j3,kk) + w3*time_oz
        droz(j4,kk) =droz(j4,kk) + w4*time_oz
     enddo

     ozptr => ozptr%llpoint

! End loop over observations
  enddo

! End of routine
  return
end subroutine intoz
