subroutine intlimq(rq,sq)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlimq
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: limit negative q as a weak constraint
!
! program history log:
!   1996-11-19  derber
!   1998-07-10  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-03-15  kleist, d., derber, j., treadon, r., use negative q only
!   2004-06-02  kleist, add penalty for excess moisture
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2007-02-13  derber - modify to use rh rather than q
!
!   input argument list:
!     sq       - increment in grid space
!
!   output argument list:
!     rq       - results from limiting operator                 
!
! remarks: see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,one
  use gridmod, only: lat2,lon2,nsig,lat1,lon1
  use jfunc, only: factqmin,factqmax,rhgues
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sq
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: rq

! Declare local variables
  integer(i_kind) i,j,k
  real(r_kind) term,q

  if (factqmin==zero .and. factqmax==zero) return
 
  do k = 1,nsig
     do j = 2,lon1+1
        do i = 2,lat1+1
           q = rhgues(i,j,k) + sq(i,j,k)
           
!          Lower constraint limit
           if (q < zero) then
                rq(i,j,k) = rq(i,j,k) + factqmin*q

!          Upper constraint limit
           else if (q > one) then
                rq(i,j,k) = rq(i,j,k) + factqmax*(q-one)
           
           end if
        end do
     end do
  end do
  
  return
end subroutine intlimq
