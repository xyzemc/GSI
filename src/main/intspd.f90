subroutine intspd(ru,rv,su,sv,dru,drv,dsu,dsv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intspd      apply nonlin qc obs operator for wind speed
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply nonlinear observation operator and adjoint for winds
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08 parrish  - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intspd and intspd_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-02-15  rancic  - add foto
!
!   input argument list:
!     su       - u increment in grid space
!     sv       - v increment in grid space
!     dsu      - time derivative of u increment in grid space
!     dsv      - time derivative of v increment in grid space
!
!   output argument list:
!     ru       - u results from observation operator 
!     rv       - v results from observation operator 
!     dru      - time derivative of u results from observation operator 
!     drv      - time derivative of v results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: spdhead,spdptr
  use qcmod, only: nlnqc_iter
  use constants, only: zero, half, one, two,tiny_r_kind,cg_term
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: su,sv,dsu,dsv
  real(r_kind),dimension(latlon1n),intent(inout):: ru,rv,dru,drv

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4,term,time_spd
! real(r_kind) penalty
  real(r_kind) uanl,vanl,spdanl,spd,spdn,valv,valu
  real(r_kind) cg_spd,p0,wnotgross,wgross

  spdptr => spdhead
  do while (associated(spdptr))

     j1 = spdptr%ij(1)
     j2 = spdptr%ij(2)
     j3 = spdptr%ij(3)
     j4 = spdptr%ij(4)
     w1 = spdptr%wij(1)
     w2 = spdptr%wij(2)
     w3 = spdptr%wij(3)
     w4 = spdptr%wij(4)

     time_spd=spdptr%time
!    Forward model
     uanl=spdptr%uges+w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)+ &
            time_spd*(w1*dsu(j1)+w2*dsu(j2)+w3*dsu(j3)+w4*dsu(j4))
     vanl=spdptr%vges+w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)+ &
            time_spd*(w1*dsv(j1)+w2*dsv(j2)+w3*dsv(j3)+w4*dsv(j4))
     spdanl=sqrt(uanl*uanl+vanl*vanl)
     valu=zero
     valv=zero
!    spdn=zero
     spd=spdanl-spdptr%res
!    spdanl=sqrt(spdptr%err2)*spdanl

     spdn=spdptr%raterr2*spdptr%err2*spd
!    Adjoint
     if(spdanl > tiny_r_kind*100._r_kind) then
        valu=uanl/spdanl
        valv=vanl/spdanl
     else
        write(6,*) ' spd ',uanl,vanl,spdanl
        valu=zero
        valv=zero
     end if
     if (nlnqc_iter .and. spdptr%pg > tiny_r_kind .and.  &
                          spdptr%b  > tiny_r_kind) then
        cg_spd=cg_term/spdptr%b
        wnotgross= one-spdptr%pg
        wgross = spdptr%pg*cg_spd/wnotgross
        p0 = wgross/(wgross+exp(-half*spdptr%err2*spd**2))
        term = (one-p0)
        spdn = spdn*term
     endif

     valu=valu*spdn
     valv=valv*spdn
     ru(j1)=ru(j1)+w1*valu
     ru(j2)=ru(j2)+w2*valu
     ru(j3)=ru(j3)+w3*valu
     ru(j4)=ru(j4)+w4*valu
     rv(j1)=rv(j1)+w1*valv
     rv(j2)=rv(j2)+w2*valv
     rv(j3)=rv(j3)+w3*valv
     rv(j4)=rv(j4)+w4*valv

     valu=valu*time_spd
     valv=valv*time_spd
     dru(j1)=dru(j1)+w1*valu
     dru(j2)=dru(j2)+w2*valu
     dru(j3)=dru(j3)+w3*valu
     dru(j4)=dru(j4)+w4*valu
     drv(j1)=drv(j1)+w1*valv
     drv(j2)=drv(j2)+w2*valv
     drv(j3)=drv(j3)+w3*valv
     drv(j4)=drv(j4)+w4*valv

     spdptr => spdptr%llpoint

  end do
  return
end subroutine intspd
