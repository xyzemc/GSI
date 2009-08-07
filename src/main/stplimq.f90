subroutine stplimq(rq,sq,rc,sc,sges,outmin,outmax)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stplimq     calculate penalty and stepsize for limit of q 
!   prgmmr: derber           org: np23                date: 1996-11-19
!
! abstract: calculate stepsize contribution and penalty for limiting q
!
! program history log:
!   1996-11-19  derber
!   1998-07-10  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-03-15  kleist, d., derber, j., treadon, r., use negative q only
!   2004-06-02  kleist, add penalty for excess moisture
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-11-22  derber - modify for openMP
!   2006-09-18  derber - modify output b1 and b3
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!
!   input argument list:
!     rq       - search direction                               
!     sq       - increment in grid space
!     sges     - step size estimates (4)
!
!   output argument list:
!     outmin(1)  - current penalty for negative q sges(1)
!     outmin(2)  - current penalty for negative q sges(2)
!     outmin(3)  - current penalty for negative q sges(3)
!     outmin(4)  - current penalty for negative q sges(4)
!     outmin(5)  - contribution to numerator from negative q
!     outmin(6)  - contribution to denomenator from negative q
!     outmax(1)  - current penalty for excess q sges(1)
!     outmax(2)  - current penalty for excess q sges(2)
!     outmax(3)  - current penalty for excess q sges(3)
!     outmax(4)  - current penalty for excess q sges(4)
!     outmax(5)  - contribution to numerator from excess q
!     outmax(6)  - contribution to denomenator from excess q
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: zero,two,one,half,zero_quad
  use gridmod, only: lat1,lon1,lat2,lon2,nsig
  use jfunc, only: factqmin,factqmax,rhgues
  implicit none

! Declare passed variables
  real(r_kind),dimension(4),intent(in):: sges
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  real(r_quad),dimension(6),intent(out):: outmin,outmax
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: rq,sq,rc,sc

! Declare local variables
  integer(i_kind) i,j,k
  real(r_kind) q,q0,q1,q2,q3
  real(r_kind):: x0max,x0min,x1max,x2max,x3max,x1min,x2min,x3min
  
  outmin=zero_quad; outmax=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

  if (factqmin==zero .and. factqmax==zero) return

! Loop over interior of subdomain          
  do k = 1,nsig
    do j = 2,lon1+1
      do i = 2,lat1+1

!       Values for q using 3 stepsizes
        q  = rhgues(i,j,k) + sq(i,j,k)
        q0 = q + sges(1)*rq(i,j,k)
        q1 = q + sges(2)*rq(i,j,k)
        q2 = q + sges(3)*rq(i,j,k)
        q3 = q + sges(4)*rq(i,j,k)

         x1min=zero
         x2min=zero
         x3min=zero

        if(q0 < zero) then
         outmin(1)=outmin(1)+factqmin*(q0*q0)
        endif
        if(q1 < zero) then
          x1min=q1*q1 
          outmin(2)=outmin(2)+factqmin*x1min
        endif
        if(q2 < zero) then 
          x2min=q2*q2 
          outmin(3)=outmin(3)+factqmin*x2min
        endif
        if(q3 < zero) then
         x3min=q3*q3 
         outmin(4)=outmin(4)+factqmin*x3min
        endif

        cc=x1min+x3min-two*x2min
        outmin(5)=outmin(5)+factqmin*((x1min-x3min)*bcoef1+cc*bcoef2)
        if (cc .ne. zero) then
         outmin(6)=outmin(6)+factqmin*cc*ccoef
        endif

        x1max=zero
        x2max=zero
        x3max=zero
        if(q0 > one )then
           x0max=(q0-one)*(q0-one) 
           outmax(1)=outmax(1)+factqmax*x0max
        endif
        if(q1 > one )then
          x1max=(q1-one)*(q1-one) 
          outmax(2)=outmax(2)+factqmax*x1max
        endif
        if(q2 > one )then
         x2max=(q2-one)*(q2-one) 
         outmax(3)=outmax(3)+factqmax*x2max
        endif
        if(q3 > one )then
         x3max=(q3-one)*(q3-one) 
         outmax(4)=outmax(4)+factqmax*x3max
        endif

        cc=x1max+x3max-two*x2max
        outmax(5)=outmax(5)+factqmax*((x1max-x3max)*bcoef1+cc*bcoef2)
        if (cc .ne. zero) then
         outmax(6)=outmax(6)+factqmax*cc*ccoef
        endif
      end do
    end do
  end do

  return
end subroutine stplimq
