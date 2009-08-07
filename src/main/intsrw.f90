subroutine intsrw(ru,rv,su,sv,dru,drv,dsu,dsv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intsrw      apply nonlin qc operator for radar superob winds
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: apply radar superob wind operator with nonlinear qc operator
!
! program history log:
!   2004-06-22  parrish, document
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intsrw and intsrw_qc into single routine
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
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: srwhead,srwptr
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: su,sv,dsu,dsv
  real(r_kind),dimension(latlon1n),intent(inout):: ru,rv,dru,drv

! Declare local variables  
  integer(i_kind) i,i1,i2,i3,i4,i5,i6,i7,i8
! real(r_kind) penalty
  real(r_kind) valu,valv,w1,w2,w3,w4,w5,w6,w7,w8,valsrw1,valsrw2
  real(r_kind) bigu11,bigu21,bigu12,bigu22,time_srw
  real(r_kind) cg_srw,p0,gradsrw1,gradsrw2,wnotgross,wgross,term

  srwptr => srwhead
  do while (associated(srwptr))
     i1=srwptr%ij(1)
     i2=srwptr%ij(2)
     i3=srwptr%ij(3)
     i4=srwptr%ij(4)
     i5=srwptr%ij(5)
     i6=srwptr%ij(6)
     i7=srwptr%ij(7)
     i8=srwptr%ij(8)
     w1=srwptr%wij(1)
     w2=srwptr%wij(2)
     w3=srwptr%wij(3)
     w4=srwptr%wij(4)
     w5=srwptr%wij(5)
     w6=srwptr%wij(6)
     w7=srwptr%wij(7)
     w8=srwptr%wij(8)

!    Forward model
     bigu11=srwptr%rsrw(1)
     bigu21=srwptr%rsrw(2)
     bigu12=srwptr%rsrw(3)
     bigu22=srwptr%rsrw(4)
     time_srw=srwptr%time
     valu=w1* su(i1)+w2* su(i2)+w3* su(i3)+w4* su(i4)+&
          w5* su(i5)+w6* su(i6)+w7* su(i7)+w8* su(i8)+&
         (w1*dsu(i1)+w2*dsu(i2)+w3*dsu(i3)+w4*dsu(i4)+&
          w5*dsu(i5)+w6*dsu(i6)+w7*dsu(i7)+w8*dsu(i8))*time_srw
     valv=w1* sv(i1)+w2* sv(i2)+w3* sv(i3)+w4* sv(i4)+&
          w5* sv(i5)+w6* sv(i6)+w7* sv(i7)+w8* sv(i8)+&
         (w1*dsv(i1)+w2*dsv(i2)+w3*dsv(i3)+w4*dsv(i4)+&
          w5*dsv(i5)+w6*dsv(i6)+w7*dsv(i7)+w8*dsv(i8))*time_srw
     valsrw1=bigu11*valu+bigu12*valv-srwptr%res1
     valsrw2=bigu21*valu+bigu22*valv-srwptr%res2

!    gradient of nonlinear operator
     if (nlnqc_iter .and. srwptr%pg > tiny_r_kind .and.  &
                          srwptr%b  > tiny_r_kind) then
        cg_srw=cg_term/srwptr%b
        wnotgross= one-srwptr%pg
        wgross = srwptr%pg*cg_srw/wnotgross
        p0   = wgross/(wgross+exp(-half*srwptr%err2*(valsrw1**2+valsrw2**2)))
        term = (one-p0)
        valsrw1=valsrw1*term
        valsrw2=valsrw2*term
     endif

     gradsrw1 = valsrw1*srwptr%raterr2*srwptr%err2
     gradsrw2 = valsrw2*srwptr%raterr2*srwptr%err2

     valu=bigu11*gradsrw1+bigu21*gradsrw2
     valv=bigu12*gradsrw1+bigu22*gradsrw2

!    Adjoint
     ru(i1)=ru(i1)+w1*valu
     ru(i2)=ru(i2)+w2*valu
     ru(i3)=ru(i3)+w3*valu
     ru(i4)=ru(i4)+w4*valu
     ru(i5)=ru(i5)+w5*valu
     ru(i6)=ru(i6)+w6*valu
     ru(i7)=ru(i7)+w7*valu
     ru(i8)=ru(i8)+w8*valu
     rv(i1)=rv(i1)+w1*valv
     rv(i2)=rv(i2)+w2*valv
     rv(i3)=rv(i3)+w3*valv
     rv(i4)=rv(i4)+w4*valv
     rv(i5)=rv(i5)+w5*valv
     rv(i6)=rv(i6)+w6*valv
     rv(i7)=rv(i7)+w7*valv
     rv(i8)=rv(i8)+w8*valv

     valu=valu*time_srw
     valv=valv*time_srw
     dru(i1)=dru(i1)+w1*valu
     dru(i2)=dru(i2)+w2*valu
     dru(i3)=dru(i3)+w3*valu
     dru(i4)=dru(i4)+w4*valu
     dru(i5)=dru(i5)+w5*valu
     dru(i6)=dru(i6)+w6*valu
     dru(i7)=dru(i7)+w7*valu
     dru(i8)=dru(i8)+w8*valu
     drv(i1)=drv(i1)+w1*valv
     drv(i2)=drv(i2)+w2*valv
     drv(i3)=drv(i3)+w3*valv
     drv(i4)=drv(i4)+w4*valv
     drv(i5)=drv(i5)+w5*valv
     drv(i6)=drv(i6)+w6*valv
     drv(i7)=drv(i7)+w7*valv
     drv(i8)=drv(i8)+w8*valv

     srwptr => srwptr%llpoint

  end do
  return
end subroutine intsrw
