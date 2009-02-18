subroutine combine_radobs(mype,mype_sub,mype_root,&
     npe_sub,mpi_comm_sub,nele,itxmax,nread,ndata,&
     data_all,data_crit,idata_itx)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    combine_radobs        merge input from multiple tasks
!   prgmmr: treadon          org: np23                date: 2006-06-19
!
! abstract:  This routine combines observation from multile tasks
!            into a single data array.
!
! program history log:
!   2006-06-19  treadon
!   2008-06-21  derber - introduce new algorithm for merging data
!
!   input argument list:
!     mype     - mpi task id for mpi_comm_world
!     mype_sub - mpi task id for mpi_comm_sub
!     mype_root - mpi task id for task which combines data
!     npe_sub   - number of tasks in mpi_comm_sub
!     mpi_comm_sub  - sub-communicator
!     nele     - total number of data elements
!     itxmax   - maximum number of observations
!     data_all - observation data array
!     data_crit- array containing observation "best scores"
!     idata_itx- array containing thinning grid location of obs
!     nread    - task specific number of obesrvations read from data file
!     ndata    - task specific number of observations keep for assimilation
!
!   output argument list:
!     nread    - total number of observations read from data file (mype_root)
!     ndata    - total number of observations keep for assimilation (mype_root)
!     data_all - merged observation data array (mype_root)
!     data_crit- merged array containing observation "best scores" (mype_root)
!     idata_itx- merged array containing thinning grid location of obs (mype_root)
!     
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,izero
  use mpimod, only: ierror,mpi_rtype,mpi_itype,mpi_sum
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype_sub,mype
  integer(i_kind),intent(in):: mype_root
  integer(i_kind),intent(in):: npe_sub,itxmax
  integer(i_kind),intent(in):: nele
  integer(i_kind),intent(in):: mpi_comm_sub
  integer(i_kind),intent(inout):: nread,ndata
  integer(i_kind),dimension(itxmax),intent(inout):: idata_itx
  real(r_kind),dimension(itxmax),intent(inout):: data_crit
  real(r_kind),dimension(nele,itxmax),intent(inout):: data_all

! Declare local variables
  integer(i_kind):: i,j,k,kk,l,n
  integer(i_kind):: itx_sub,itx,ndata1
  integer(i_kind),dimension(npe_sub+2):: ncounts,ncounts1
  integer(i_kind),dimension(npe_sub):: recvcounts,displs,displs2
  integer(i_kind),dimension(itxmax):: itxsave

  real(r_kind):: crit_sub
  real(r_kind),dimension(nele+2,ndata) :: data_sub
  real(r_kind),allocatable,dimension(:,:):: data_all_sub

! Determine total number of data read and retained.
  do i=1,npe_sub
     ncounts(i)=izero
  end do
  ncounts(mype_sub+1)=ndata
  ncounts(npe_sub+1)=nread
  ncounts(npe_sub+2)=ndata
  call mpi_allreduce(ncounts,ncounts1,npe_sub+2,mpi_itype,mpi_sum,mpi_comm_sub,ierror)

! Set total number of observations summed over all tasks and
! construct starting location of subset in reduction array

  nread=0
  ndata1=ncounts1(npe_sub+2)
  if (mype_sub==mype_root) nread = ncounts1(npe_sub+1)
  if (ndata1 == 0)return

  recvcounts(1)=ncounts1(1)*(nele+2)
  displs(1)=0
  displs2(1)=0
  do i=2,npe_sub
    recvcounts(i)=ncounts1(i)*(nele+2)
    displs(i)=displs(i-1)+ncounts1(i-1)*(nele+2)
    displs2(i)=displs2(i-1)+ncounts1(i-1)
  end do

  do i=1,ndata
    do j=1,nele
      data_sub(j,i)=data_all(j,i)
    end do
    data_sub(nele+1,i)=data_crit(i)
    data_sub(nele+2,i)=idata_itx(i) + 0.01_r_kind
  end do

! Allocate arrays to hold data
  if(mype_sub==mype_root)allocate(data_all_sub(nele+2,ndata1))

! gather arrays over all tasks in mpi_comm_sub.  Reduction result
! is only needed on task mype_root
  call mpi_gatherv(data_sub,(nele+2)*ndata,mpi_rtype,data_all_sub,recvcounts,displs, &
       mpi_rtype,mype_root,mpi_comm_sub,ierror)

! Reset counters
  
! A single task, mype_root, merges the all the data together
  if (mype_sub==mype_root) then

     nread = ncounts1(npe_sub+1)

     do k=1,itxmax
        itxsave(k)=0
        data_crit(k)=999999._r_kind
     end do

!    Loop over task specific obs.  Retain "best"
!    observations from each task
     data_loop: do kk=1,ndata1

!       Loop over contents of scratch file just read in.
!       Retain best observation in each thinning grid box

           crit_sub=data_all_sub(nele+1,kk)
           itx_sub=data_all_sub(nele+2,kk)
           
           if(itxsave(itx_sub) == 0 .or. data_crit(itx_sub) > crit_sub)then
              itxsave(itx_sub)=kk
              data_crit(itx_sub)=crit_sub
           end if 
              
     end do data_loop
     ndata=0
     do i=1,itxmax
       if(itxsave(i) > 0)then
         itx=itxsave(i)
         ndata=ndata+1
         do l=1,nele
            data_all(l,ndata)=data_all_sub(l,itx)
         end do
       end if
     end do

!    Deallocate arrays
     deallocate(data_all_sub)

! End of block for mype_root task
  endif


! End of routine
  return
end subroutine combine_radobs
