subroutine intps(rp,sp,drp,dsp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intps       apply nonlin qc obs operator for ps
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for ps observations
!           with nonlinear qc operator
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intps and intps_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2007-02-15  rancic - add foto
!
!   input argument list:
!     sp      - ps increment in grid space
!     dsp     - time derivative of ps increment in grid space
!
!   output argument list:
!     rp      - ps results from observation operator 
!     drp     - results from time derivative of ps observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: psptr,pshead
  use qcmod, only: nlnqc_iter,c_varqc
  use gridmod, only: latlon11
  use jfunc, only: iter,jiter,niter_no_qc,jiterstart
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon11),intent(in):: sp,dsp
  real(r_kind),dimension(latlon11),intent(inout):: rp,drp

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) cg_ps,val,p0,grad,wnotgross,wgross,ps_pg,varqc_iter
  real(r_kind) w1,w2,w3,w4,time_ps

  psptr => pshead
  do while (associated(psptr))
     j1=psptr%ij(1)
     j2=psptr%ij(2)
     j3=psptr%ij(3)
     j4=psptr%ij(4)
     w1=psptr%wij(1)
     w2=psptr%wij(2)
     w3=psptr%wij(3)
     w4=psptr%wij(4)
     
     time_ps=psptr%time
!    Forward model
     val=w1* sp(j1)+w2* sp(j2)+w3* sp(j3)+w4* sp(j4)+   &
        (w1*dsp(j1)+w2*dsp(j2)+w3*dsp(j3)+w4*dsp(j4))*time_ps-psptr%res

!    gradient of nonlinear operator
!    Gradually turn on variational qc to avoid possible convergence problems
     if(jiter == jiterstart .and. nlnqc_iter .and. psptr%pg >tiny_r_kind) then
        varqc_iter=c_varqc*(iter-niter_no_qc(1)+one)
        if(varqc_iter >=one) varqc_iter= one
        ps_pg=psptr%pg*varqc_iter
     else
        ps_pg=psptr%pg
     endif
     if (nlnqc_iter .and. psptr%pg > tiny_r_kind .and.  &
                          psptr%b  > tiny_r_kind) then
        cg_ps=cg_term/psptr%b                           ! b is d in Enderson
        wnotgross= one-ps_pg                            ! pg is A in Enderson
        wgross =ps_pg*cg_ps/wnotgross                   ! wgross is gama in Enderson
        p0=wgross/(wgross+exp(-half*psptr%err2*val**2)) ! p0 is P in Enderson
        val=val*(one-p0)                                ! term is Wqc in Enderson
     endif

     grad     = val*psptr%raterr2*psptr%err2

!    Adjoint
     rp(j1)=rp(j1)+w1*grad
     rp(j2)=rp(j2)+w2*grad
     rp(j3)=rp(j3)+w3*grad
     rp(j4)=rp(j4)+w4*grad

     grad=grad*time_ps

     drp(j1)=drp(j1)+w1*grad
     drp(j2)=drp(j2)+w2*grad
     drp(j3)=drp(j3)+w3*grad
     drp(j4)=drp(j4)+w4*grad

     psptr => psptr%llpoint
  end do
  return
end subroutine intps
