subroutine stpoz(roz,soz,out,sges,droz,dsoz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpoz       compute contribution to penalty and
!                            stepsize for ozone, using nonlinear qc
!   prgmmr: derber          org: np23                 date: 1995-07-11
!
! abstract: The routine computes the contribution to the penalty from ozone
!           observations.  The routine also computes the contribution of
!           ozone observations to the step size.  This version includes
!           nonlinear qc.
!
! program history log:
!   1995-07-11  derber
!   1999-03-01  wu - port cray90 code to ibm-sp (mpi version)
!   2004-06-16  treadon - update documenation
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpoz and stpoz_qc into single routine
!   2005-06-14  wu      - add OMI toz
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output values of b1 and b3
!   2007-02-15  rancic  - add foto
!   2007-05-30  h.liu   - move interpolation weights w1-w4 inside k loop
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!
!   input argument list:
!     roz  - search direction for ozone
!     soz  - input ozone correction field
!     droz - search direction for time derivative of ozone
!     dsoz - input time derivative of ozone correction field
!     sges - step size estimates (4)
!
!   output argument list:
!     out(1) - contribution of ozone data to penalty sges(1)
!     out(2) - contribution of ozone data to penalty sges(2)
!     out(3) - contribution of ozone data to penalty sges(3)
!     out(4) - contribution of ozone data to penalty sges(4)
!     out(5) - contribution to numerator from ozone
!     out(6) - contribution to denomonator from ozone
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: ozhead,ozptr
  use ozinfo, only: b_oz,pg_oz
  use qcmod, only: nlnqc_iter
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term,zero_quad
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(lat2*lon2,nsig),intent(in):: roz,soz,droz,dsoz
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables
  integer(i_kind) i,k,j1,j2,j3,j4,kk,iz1,iz2,kx

  real(r_kind) dz1,pob,delz
  real(r_kind) w1,w2,w3,w4,time_oz
  real(r_kind) cg_oz,oz0,oz1,oz2,oz3,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

  real(r_quad) val,val1

! Initialize output variables to zero
  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

! SBUV OZONE: LAYER O3 and TOATL O3
!
! Loop over ozone observations
  ozptr => ozhead
  do while (associated(ozptr))
    if(ozptr%luse)then

!    Get location
     j1=ozptr%ij(1)
     j2=ozptr%ij(2)
     j3=ozptr%ij(3)
     j4=ozptr%ij(4)

!    Accumulate contribution from layer observations
     dz1=nsig+1
     time_oz=ozptr%time

     if ( ozptr%nloz >= 1 ) then

     do k=1,ozptr%nloz
        val1= -ozptr%res(k)
        val = zero_quad
        pob = ozptr%prs(k)
        iz1 = dz1
        if (iz1>nsig) iz1=nsig
        iz2 = pob
        
        do kk=iz2,iz1
           delz=one
           if (kk==iz1) delz=dz1-iz1
           if (kk==iz2) delz=delz-pob+iz2
           w1=ozptr%wij(1,kk)
           w2=ozptr%wij(2,kk)
           w3=ozptr%wij(3,kk)
           w4=ozptr%wij(4,kk)
           val=val + ( &
                w1* roz(j1,kk)+ &
                w2* roz(j2,kk)+ &
                w3* roz(j3,kk)+ &
                w4* roz(j4,kk)+ &
               (w1*droz(j1,kk)+ &
                w2*droz(j2,kk)+ &
                w3*droz(j3,kk)+ &
                w4*droz(j4,kk))*time_oz )*delz
           val1=val1 + ( &
                w1* soz(j1,kk)+ &
                w2* soz(j2,kk)+ &
                w3* soz(j3,kk)+ &
                w4* soz(j4,kk)+ &
               (w1*dsoz(j1,kk)+ &
                w2*dsoz(j2,kk)+ &
                w3*dsoz(j3,kk)+ &
                w4*dsoz(j4,kk))*time_oz )*delz
        end do
        oz0=val1+sges(1)*val
        oz1=val1+sges(2)*val
        oz2=val1+sges(3)*val
        oz3=val1+sges(4)*val

        pencur = ozptr%err2(k)*oz0*oz0
        pen1   = ozptr%err2(k)*oz1*oz1
        pen2   = ozptr%err2(k)*oz2*oz2
        pen3   = ozptr%err2(k)*oz3*oz3

        cc     = (pen1+pen3-two*pen2)*ozptr%raterr2(k)
        out(1) = out(1)+pencur*ozptr%raterr2(k)
        out(2) = out(2)+pen1  *ozptr%raterr2(k)
        out(3) = out(3)+pen2  *ozptr%raterr2(k)
        out(4) = out(4)+pen3  *ozptr%raterr2(k)
        out(5) = out(5)+(pen1-pen3)*ozptr%raterr2(k)*bcoef1+cc*bcoef2
        out(6) = out(6)+cc*ccoef
        dz1=pob
     end do

     end if

!    Add contribution from total column observation
     k   = ozptr%nloz+1
     val1= -ozptr%res(k)
     val  = zero_quad
     do kk=1,nsig
        w1=ozptr%wij(1,kk)
        w2=ozptr%wij(2,kk)
        w3=ozptr%wij(3,kk)
        w4=ozptr%wij(4,kk)
        val=val+ ( &
             w1* roz(j1,kk)+ &
             w2* roz(j2,kk)+ &
             w3* roz(j3,kk)+ &
             w4* roz(j4,kk)+ &
            (w1*droz(j1,kk)+ &
             w2*droz(j2,kk)+ &
             w3*droz(j3,kk)+ &
             w4*droz(j4,kk))*time_oz )
        val1=val1 + ( &
             w1* soz(j1,kk)+ &
             w2* soz(j2,kk)+ &
             w3* soz(j3,kk)+ & 
             w4* soz(j4,kk)+ &
            (w1*dsoz(j1,kk)+ &
             w2*dsoz(j2,kk)+ &
             w3*dsoz(j3,kk)+ & 
             w4*dsoz(j4,kk))*time_oz )
     enddo
     oz0=val1+sges(1)*val
     oz1=val1+sges(2)*val
     oz2=val1+sges(3)*val
     oz3=val1+sges(4)*val

     pencur = ozptr%err2(k)*oz0*oz0
     pen1   = ozptr%err2(k)*oz1*oz1
     pen2   = ozptr%err2(k)*oz2*oz2
     pen3   = ozptr%err2(k)*oz3*oz3

     cc     = (pen1+pen3-two*pen2)*ozptr%raterr2(k)
     out(1) = out(1) +pencur*ozptr%raterr2(k)
     out(2) = out(2) +pen1  *ozptr%raterr2(k)
     out(3) = out(3) +pen2  *ozptr%raterr2(k)
     out(4) = out(4) +pen3  *ozptr%raterr2(k)
     out(5) = out(5)+(pen1-pen3)*ozptr%raterr2(k)*bcoef1+cc*bcoef2
     out(6) = out(6)+cc*ccoef
    end if

    ozptr => ozptr%llpoint

! End of loop over observations
  enddo

! End of routine.
 return
end subroutine stpoz
