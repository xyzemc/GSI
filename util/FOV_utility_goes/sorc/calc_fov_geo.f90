 module calc_fov_geo
!
!Module that preserves precomputed values across calls to FOV_Ellipse
!
! Thomas J. Kleespies  NOAA/NESDIS  15 September 2004
!                      Revised       8 April 2005
!                      Adapted for conical 10 July 2008

 use kinds, only : r_kind

 implicit none
 
 real(r_kind)         ::  along_track_angle 
 real(r_kind)         ::  cross_track_angle 
 real(r_kind)         ::  along_track_fov_size 
 real(r_kind)         ::  cross_track_fov_size 
 real(r_kind)         ::  rmax 
 real(r_kind)         ::  rmin 
 real(r_kind)         ::  eccen 

 end module calc_fov_geo
