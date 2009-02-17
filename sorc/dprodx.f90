 subroutine dprodx(n1,nlx,dx,dy,yd,xd,out1,iter,mype,dprod)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dprodx   calculates dot product for control vector type vectors
!   prgmmr: derber           org: np23                  date: 2004-05-13
!
! abstract: calculates dot product for control vector type vectors.  Note first n1
!           elements independent n1-nlx duplicated between vectors
!
! program history log:
!   2004-05-13  derber, document
!   2004-07-15  treadon - cosmetic cleanup
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!   2004-12-03  treadon - replace mpe_iallreduce (IBM extension) with
!                         standard mpi_allreduce
!   2005-05-27  derber  - modified to reflect changes in minimization
!   2005-06-06  treadon - comment out computation of dx*dx since not used
!   2005-11-20  derber  - modify to improve reproducibility
!
!   input argument list:
!     n1       - length of independent control vector
!     nlx      - length of control vector
!     dx       - input vector 1
!     dy       - input vector 2
!     yd       - input vector 3
!     xd       - input vector 4
!     iter     - iteration number
!
!   output argument list
!     out1     - array containing 2 dot product
!              - out(1) = dx*dy
!              - out(2) = yd*dy
!              - out(3) = dx*dx
!     dprod    - out(2)/out(1)from previous iteration
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,r_double,i_kind,r_quad
  use constants, only: zero,izero

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: n1,nlx,iter,mype  	
  real(r_kind),dimension(nlx),intent(in)::dx,dy,yd,xd
  real(r_kind),dimension(3),intent(out):: out1
  real(r_kind),intent(out):: dprod  	

! Declare local variables
  integer(i_kind) i,j,k

  real(r_kind):: out1save
  real(r_quad):: b1,b2,t1,t2

! Zero solution
  out1save = out1(1)
  out1(1) = zero
  out1(2) = zero
  out1(3) = zero
  dprod = zero
  if(nlx <= izero)return

! Code for independent part of vector
  if(n1 > izero ) then
     call dplev(dx,dy,mype,t1)
     call dplev(yd,dy,mype,t2)

  end if


! Dot product for duplicated part of vector.  Done on all processors so no
! communication necessary
  b1=zero
  b2=zero
  if(nlx-n1 > izero ) then
     do i = n1+1,nlx
        b1 = b1 + dx(i)*dy(i)
        b2 = b2 + yd(i)*dy(i)
!       out1(3) = out1(3) + dx(i)*dx(i)
     end do
  end if
! Sum independent and duplicated parts

  out1(1)=t1+b1
  out1(2)=t2+b2
! if(mype == 0)write(6,*)' t1,t2,b1,b2',t1,t2,b1,b2

  if(out1save > 1.e-16 .and. iter > 0)dprod = (t2+b2)/out1save

  return
end subroutine dprodx

 subroutine dplev(dx,dy,mype,dprod)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dplev   calculates dot product for data on subdomain
!   prgmmr: derber           org: np23                date: 2004-05-13
!
! abstract: calculates dot product for data on subdomain.  Note loops over
!           streamfunction, velocity potential, temperature, etc. Also, only 
!           over interior of subdomain.
!
! program history log:
!   2004-05-13  derber, document
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!
!   input argument list:
!     dx       - input vector 1
!     dy       - input vector 2
!     mype     - processor id
!
!   output argument list
!     dprod    - dot product
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  
  use kinds, only: r_kind,r_double,i_kind,r_quad
  use jfunc, only:  nval_levs
  use gridmod, only:  nlat,nlon,lat2,lon2,lat1,lon1,&
     ltosi,ltosj,iglobal,itotsub,ijn,displs_g
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use constants, only:  zero
  implicit none

! Declar passed variables
  real(r_kind),dimension(lat2,lon2,nval_levs),intent(in)::dx,dy
  integer(i_kind),intent(in)::mype
  real(r_quad),intent(out):: dprod

! Declare local variables
  real(r_kind),dimension(iglobal):: work1
  real(r_kind),dimension(lat1,lon1):: sum

  integer(i_kind) i,j,k,mm1,kk,jp
  
  mm1=mype+1
  sum=zero
  do k=1,nval_levs
     do j=1,lon1
        jp=j+1
        do i=1,lat1
           sum(i,j)=sum(i,j)+dx(i+1,jp,k)*dy(i+1,jp,k)
        end do
     end do
  end do

  call mpi_allgatherv(sum,ijn(mm1),mpi_rtype,&
     work1,ijn,displs_g,mpi_rtype,&
     mpi_comm_world,ierror)

  dprod=0.0_r_kind
  do k=1,iglobal
    dprod=dprod+work1(k)
  end do
    
  return
  end subroutine dplev
 subroutine dplev1(dx,dy,mype,dprod)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dplev   calculates dot product for data on subdomain
!   prgmmr: derber           org: np23                date: 2004-05-13
!
! abstract: calculates dot product for data on subdomain.  Note loops over
!           streamfunction, velocity potential, temperature, etc. Also, only 
!           over interior of subdomain.
!
! program history log:
!   2004-05-13  derber, document
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!
!   input argument list:
!     dx       - input vector 1
!     dy       - input vector 2
!     mype     - processor id
!
!   output argument list
!     dprod    - dot product
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  
  use kinds, only: r_kind,r_double,i_kind,r_quad
  use jfunc, only:  nval_levs
  use gridmod, only:  nlat,nlon,lat2,lon2,lat1,lon1,&
     ltosi,ltosj,iglobal,itotsub,ijn,displs_g
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use constants, only:  zero,izero,zero_quad
  implicit none

! Declar passed variables
  real(r_kind),dimension(lat2,lon2,nval_levs),intent(in)::dx,dy
  integer(i_kind),intent(in)::mype
  real(r_quad),intent(out):: dprod

! Declare local variables
  real(r_kind),dimension(iglobal):: work1
  real(r_kind),dimension(lat1,lon1):: sum

  integer(i_kind) i,j,k,mm1,kk,jp
  
  mm1=mype+1
  sum=zero
  do k=1,nval_levs
     do j=1,lon1
        jp=j+1
        do i=1,lat1
           sum(i,j)=sum(i,j)+dx(i+1,jp,k)*dy(i+1,jp,k)
        end do
     end do
  end do

  call mpi_gatherv(sum,ijn(mm1),mpi_rtype,&
     work1,ijn,displs_g,mpi_rtype,izero,&
     mpi_comm_world,ierror)

  dprod=zero_quad
  if(mype == izero)then
    do k=1,iglobal
      dprod=dprod+work1(k)
    end do
  end if
    
  return
end subroutine dplev1
  subroutine fplev(dx,mype,dprod)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fplev   calculates dot product for data on subdomain
!   prgmmr: derber           org: np23                date: 2004-05-13
!
! abstract: calculates dot product for data on subdomain.  Note loops over
!           streamfunction, velocity potential, temperature, etc. Also, only 
!           over interior of subdomain.
!
! program history log:
!   2004-05-13  derber, document
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!
!   input argument list:
!     dx       - input vector 1
!     dy       - input vector 2
!
!   output argument list
!     dprod    - dot product
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  
  use kinds, only: r_kind,r_double,i_kind
  use jfunc, only:  nval_levs
  use gridmod, only:  nlat,nlon,lat2,lon2,lat1,lon1,&
     ltosi,ltosj,iglobal,itotsub,ijn,displs_g
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype,strip
  use constants, only:  zero
  implicit none

! Declar passed variables
  real(r_kind),dimension(lat2,lon2),intent(in)::dx
  real(r_kind),intent(out)::dprod
  integer(i_kind),intent(in)::mype

! Declare local variables
  real(r_kind),dimension(lat1*lon1):: zsm
  real(r_kind),dimension(iglobal):: work1
  real(r_kind),dimension(lat2,lon2):: sum
  real(r_kind),dimension(nlat,nlon):: sumall

  integer(i_kind) i,j,k,mm1,kk
  
  mm1=mype+1
  dprod=zero
  do j=1,lon1*lat1
    zsm(j)=zero
  end do

  call strip(dx,zsm,1)

  call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
     work1,ijn,displs_g,mpi_rtype,&
     mpi_comm_world,ierror)

  do k=1,iglobal
    i=ltosi(k) ; j=ltosj(k)
    sumall(i,j)=work1(k)
  end do
! if(mype == 0)then
! do j=1,nlon
!   do i=1,nlat
!     write(500,*)i,j,sumall(i,j)
!   end do
! end do
! end if
  do j=1,nlon
    do i=1,nlat
      dprod=dprod+sumall(i,j)*sumall(i,j)
    end do
  end do
    
  return
end subroutine fplev
