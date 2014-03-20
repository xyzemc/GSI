module converr_q
!$$$   module documentation block
!                .      .    .                                       .
! module:    converr_q
!   prgmmr: su          org: np2                date: 2007-03-15
! abstract:  This module contains variables and routines related
!            to the assimilation of conventional observations error
!
! program history log:
!   2007-03-15  su  - original code - move reading observation error table 
!                                     from read_prepbufr to here so all the 
!                                     processor can have the new error information 
!
! Subroutines Included:
!   sub converr_q_read      - allocate arrays for and read in conventional error table 
!   sub converr_q_destroy   - destroy conventional error arrays
!
! Variable Definitions:
!   def etabl_q             -  the array to hold the error table
!   def ptabl_q             -  the array to have vertical pressure values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only:r_kind,i_kind,r_single
use constants, only: zero
use obsmod, only : oberrflg 
implicit none

! set default as private
  private
! set subroutines as public
  public :: converr_q_read
  public :: converr_q_destroy
! set passed variables as public
  public :: etabl_q,ptabl_q

  integer(i_kind),save:: ietabl_q,itypex,lcount,iflag,k,m
  real(r_single),save,allocatable,dimension(:,:,:) :: etabl_q
  real(r_kind),save,allocatable,dimension(:)  :: ptabl_q

contains


  subroutine converr_q_read(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convinfo_err      read conventional information file
!
!     prgmmr:    su    org: np2                date: 2007-03-15
!
! abstract:  This routine reads the conventional error table file
!
! program history log:
!   2008-06-04  safford -- add subprogram doc block
!   2013-05-14  guo     -- add status and iostat in open, to correctly
!                          handle the error case of "obs error table not
!                          available to 3dvar".
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
     use constants, only: half
     implicit none

     integer(i_kind),intent(in   ) :: mype

     integer(i_kind):: ier

     allocate(etabl_q(100,33,6))

     etabl_q=1.e9_r_kind
      
     ietabl_q=19
     open(ietabl_q,file='errtable_q',form='formatted',status='old',iostat=ier)
     if(ier/=0) then
        write(6,*)'CONVERR:  ***WARNING*** obs error table ("errtable") not available to 3dvar.'
        lcount=0
        oberrflg=.false.
        return
     endif

     rewind ietabl_q
     etabl_q=1.e9_r_kind
     lcount=0
     loopd : do 
        read(ietabl_q,100,IOSTAT=iflag,end=120) itypey
        if( iflag /= 0 ) exit loopd
100     format(1x,i3,2x,i3)
        lcount=lcount+1
        itypex=itypey-99
        do k=1,33
           read(ietabl_q,110)(etabl_q(itypex,k,m),m=1,6)
110        format(1x,6e12.5)
        end do
     end do   loopd
120  continue

     if(lcount<=0 .and. mype==0) then
        write(6,*)'CONVERR:  ***WARNING*** obs error table not available to 3dvar.'
        oberrflg=.false.
     else
        if(mype == 0) write(6,*)'CONVERR:  using observation errors from user provided table'
        allocate(ptabl_q(34))
        ptabl_q=zero
        ptabl_q(1)=etabl_q(20,1,1)
        do k=2,33
           ptabl_q(k)=half*(etabl_q(20,k-1,1)+etabl_q(20,k,1))
        enddo
        ptabl_q(34)=etabl_q(20,33,1)
     endif

     close(ietabl_q)

     return
  end subroutine converr_q_read


subroutine converr_q_destroy
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    converr_q_destroy      destroy conventional information file
!     prgmmr:    su    org: np2                date: 2007-03-15
!
! abstract:  This routine destroys arrays from converr_q file
!
! program history log:
!   2007-03-15  su 
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
     implicit none

     deallocate(etabl_q,ptabl_q)
     return
  end subroutine converr_q_destroy

end module converr_q



