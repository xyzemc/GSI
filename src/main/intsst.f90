subroutine intsst(rsst,ssst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intsst      apply nonlin qc obs operator for conv. sst
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: apply observation operator and adjoint for conventional sst
!           observations with nonlinear qc operator
!
! program history log:
!   2004-07-20  derber
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intsst and intsst_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!
!   input argument list:
!     ssst    - increment in grid space
!
!   output argument list:
!     rsst    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: ssthead,sstptr
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon11
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon11),intent(in):: ssst
  real(r_kind),dimension(latlon11),intent(inout):: rsst

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_sst,p0,grad,wnotgross,wgross

  sstptr => ssthead
  do while (associated(sstptr))
     j1=sstptr%ij(1)
     j2=sstptr%ij(2)
     j3=sstptr%ij(3)
     j4=sstptr%ij(4)
     w1=sstptr%wij(1)
     w2=sstptr%wij(2)
     w3=sstptr%wij(3)
     w4=sstptr%wij(4)

!    Forward model
     val=w1*ssst(j1)+w2*ssst(j2)&
        +w3*ssst(j3)+w4*ssst(j4)-sstptr%res

!    gradient of nonlinear operator
     if (nlnqc_iter .and. sstptr%pg > tiny_r_kind .and. &
                          sstptr%b  > tiny_r_kind) then
        cg_sst=cg_term/sstptr%b
        wnotgross= one-sstptr%pg
        wgross = sstptr%pg*cg_sst/wnotgross
        p0   = wgross/(wgross+exp(-half*sstptr%err2*val**2))
        val = val*(one-p0)
     endif

     grad     = val*sstptr%raterr2*sstptr%err2

!    Adjoint
     rsst(j1)=rsst(j1)+w1*grad
     rsst(j2)=rsst(j2)+w2*grad
     rsst(j3)=rsst(j3)+w3*grad
     rsst(j4)=rsst(j4)+w4*grad

     sstptr => sstptr%llpoint

  end do
  return
end subroutine intsst
