subroutine intw(ru,rv,su,sv,dru,drv,dsu,dsv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intw        apply nonlin qc obs operator for winds
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for winds with
!             nonlinear qc operator
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intw and intw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-10-20  rancic  - add foto
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
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use obsmod, only: whead,wptr
  use qcmod, only: nlnqc_iter,c_varqc
  use gridmod, only: latlon1n
  use jfunc, only: iter,jiter,niter_no_qc,jiterstart
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: su,sv,dsu,dsv
  real(r_kind),dimension(latlon1n),intent(inout):: ru,rv,dru,drv

! Declare local variables
  integer(i_kind) i,i1,i2,i3,i4,i5,i6,i7,i8
! real(r_kind) penalty
  real(r_kind) valu,valv,w1,w2,w3,w4,w5,w6,w7,w8,time_w
  real(r_kind) cg_w,p0,gradu,gradv,wnotgross,wgross,term,w_pg,varqc_iter

  wptr => whead
  do while(associated(wptr))
     i1=wptr%ij(1)
     i2=wptr%ij(2)
     i3=wptr%ij(3)
     i4=wptr%ij(4)
     i5=wptr%ij(5)
     i6=wptr%ij(6)
     i7=wptr%ij(7)
     i8=wptr%ij(8)
     w1=wptr%wij(1)
     w2=wptr%wij(2)
     w3=wptr%wij(3)
     w4=wptr%wij(4)
     w5=wptr%wij(5)
     w6=wptr%wij(6)
     w7=wptr%wij(7)
     w8=wptr%wij(8)
  
     time_w=wptr%time
!    Forward model
     valu=w1* su(i1)+w2* su(i2)+w3* su(i3)+w4* su(i4)+&
          w5* su(i5)+w6* su(i6)+w7* su(i7)+w8* su(i8)-wptr%ures +&
         (w1*dsu(i1)+w2*dsu(i2)+w3*dsu(i3)+w4*dsu(i4)+&
          w5*dsu(i5)+w6*dsu(i6)+w7*dsu(i7)+w8*dsu(i8))*time_w

     valv=w1* sv(i1)+w2* sv(i2)+w3* sv(i3)+w4* sv(i4)+&
          w5* sv(i5)+w6* sv(i6)+w7* sv(i7)+w8* sv(i8)-wptr%vres +&
         (w1*dsv(i1)+w2*dsv(i2)+w3*dsv(i3)+w4*dsv(i4)+&
          w5*dsv(i5)+w6*dsv(i6)+w7*dsv(i7)+w8*dsv(i8))*time_w

!    gradient of nonlinear operator
!    Gradually turn on variational qc to avoid possible convergence problems
     if(jiter == jiterstart .and. nlnqc_iter .and. wptr%pg > tiny_r_kind) then
        varqc_iter=c_varqc*(iter-niter_no_qc(1)+one)
        if(varqc_iter >=one) varqc_iter= one
        w_pg=wptr%pg*varqc_iter
     else
        w_pg=wptr%pg
     endif

     if (nlnqc_iter .and. wptr%pg > tiny_r_kind .and.  &
                          wptr%b  > tiny_r_kind) then
        cg_w=cg_term/wptr%b
        wnotgross= one-w_pg
        wgross =w_pg*cg_w/wnotgross                ! wgross is gama in Enderson
        p0=wgross/(wgross+                      &  ! p0 is P in Enderson
         exp(-half*wptr%err2*(valu**2+valv**2))) 
        term=one-p0                                !  term is Wqc in Enderson
        valu = valu*term
        valv = valv*term
     endif

     gradu = valu*wptr%raterr2*wptr%err2
     gradv = valv*wptr%raterr2*wptr%err2

!    Adjoint
     ru(i1)=ru(i1)+w1*gradu
     ru(i2)=ru(i2)+w2*gradu
     ru(i3)=ru(i3)+w3*gradu
     ru(i4)=ru(i4)+w4*gradu
     ru(i5)=ru(i5)+w5*gradu
     ru(i6)=ru(i6)+w6*gradu
     ru(i7)=ru(i7)+w7*gradu
     ru(i8)=ru(i8)+w8*gradu

     rv(i1)=rv(i1)+w1*gradv
     rv(i2)=rv(i2)+w2*gradv
     rv(i3)=rv(i3)+w3*gradv
     rv(i4)=rv(i4)+w4*gradv
     rv(i5)=rv(i5)+w5*gradv
     rv(i6)=rv(i6)+w6*gradv
     rv(i7)=rv(i7)+w7*gradv
     rv(i8)=rv(i8)+w8*gradv
     
     gradu=gradu*time_w
     gradv=gradv*time_w
     dru(i1)=dru(i1)+w1*gradu
     dru(i2)=dru(i2)+w2*gradu
     dru(i3)=dru(i3)+w3*gradu
     dru(i4)=dru(i4)+w4*gradu
     dru(i5)=dru(i5)+w5*gradu
     dru(i6)=dru(i6)+w6*gradu
     dru(i7)=dru(i7)+w7*gradu
     dru(i8)=dru(i8)+w8*gradu

     drv(i1)=drv(i1)+w1*gradv
     drv(i2)=drv(i2)+w2*gradv
     drv(i3)=drv(i3)+w3*gradv
     drv(i4)=drv(i4)+w4*gradv
     drv(i5)=drv(i5)+w5*gradv
     drv(i6)=drv(i6)+w6*gradv
     drv(i7)=drv(i7)+w7*gradv
     drv(i8)=drv(i8)+w8*gradv

     wptr => wptr%llpoint

  end do
  return
end subroutine intw
