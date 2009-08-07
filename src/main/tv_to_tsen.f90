subroutine tv_to_tsen(tv,q,tsen,tv_t,q_t,tsen_t)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tv_to_tsen  tlm for normalized virtual to sensible temp
!   prgmmr: wu               org: np20                date: 2005-03-06
!
! abstract: get sensible temperature from virtual temperature
!
! program history log:
!   2006-07-17  derber
!
!   input argument list:
!      tv     - virtual temperature
!      q      - specific humidity
!      tv_t   - virtual temperature time tendency
!      q_t    - specific humidity time tendency
!
!   output argument list:
!      tsen   - sensible temperature
!      tsen_t - sensible temperature time tendency
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use constants, only: half,zero,fv,one,one_tenth
  use guess_grids, only: ges_tv,ges_q,ges_prsl,ges_tsen,ntguessig,tropprs
  use jfunc, only: qsatg

  implicit none

  real(r_kind),intent(in):: tv(lat2,lon2,nsig)
  real(r_kind),intent(in):: q(lat2,lon2,nsig)
  real(r_kind),intent(in):: tv_t(lat2,lon2,nsig)
  real(r_kind),intent(in):: q_t(lat2,lon2,nsig)

  real(r_kind),intent(out):: tsen(lat2,lon2,nsig)
  real(r_kind),intent(out):: tsen_t(lat2,lon2,nsig)
  
! local arrays
  integer(i_kind) i,j,k
  real(r_kind):: fact

! Convert normalized tv to tsen
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
          fact = one/(one+fv*max(zero,ges_q(i,j,k,ntguessig)))
          tsen(i,j,k)=(tv(i,j,k)-fv*ges_tsen(i,j,k,ntguessig)*q(i,j,k))*fact
          tsen_t(i,j,k)=(tv_t(i,j,k)-fv*ges_tsen(i,j,k,ntguessig)*q_t(i,j,k))*fact

        end do
     end do
  end do

end subroutine tv_to_tsen

subroutine tv_to_tsen_ad(tv,q,tsen,tv_t,q_t,tsen_t)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tv_to_tsen_ad  adjoint of tv_to_tsen
!   prgmmr: wu               org: np20                date: 2005-03-06
!
! abstract: adjoint of tv_to_tsen
!
! program history log:
!   2005-03-06  wu
!   2005-03-30  treadon - reformat code (cosmetic change only)
!   2005-11-21  kleist - use 3d pressure increment for coupling
!   2005-11-21  derber modify to make qoption =1 work same as =2
!   2006-01-09  derber move sigsum calculation to compute_derived and clean up
!
!   input argument list:
!      rhnorm - normalized RH
!      t      - virtual temperature
!      p      - ln(psfc)
!
!   output argument list:
!      q      - specific humidity
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use constants, only: half,zero,fv,one,one_tenth
  use guess_grids, only: ges_tv,ges_q,ges_prsl,ges_tsen,ntguessig,tropprs
  use jfunc, only: qsatg

  implicit none

  real(r_kind),intent(inout):: tv(lat2,lon2,nsig),tv_t(lat2,lon2,nsig)
  real(r_kind),intent(inout):: q(lat2,lon2,nsig),q_t(lat2,lon2,nsig)
  real(r_kind),intent(in):: tsen(lat2,lon2,nsig),tsen_t(lat2,lon2,nsig)
  
! local variables:
  integer(i_kind) i,j,k
  real(r_kind):: fact,fact2
  
! Adjoint of convert tv to t sensible
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
          fact = one/(one+fv*max(zero,ges_q(i,j,k,ntguessig)))
          fact2 = fv*ges_tsen(i,j,k,ntguessig)*fact
          tv(i,j,k)=tv(i,j,k)+tsen(i,j,k)*fact
          q(i,j,k)=q(i,j,k)-tsen(i,j,k)*fact2

          tv_t(i,j,k)=tv_t(i,j,k)+tsen_t(i,j,k)*fact
          q_t(i,j,k)=q_t(i,j,k)-tsen_t(i,j,k)*fact2

        end do
     end do
  end do
  
end subroutine tv_to_tsen_ad
