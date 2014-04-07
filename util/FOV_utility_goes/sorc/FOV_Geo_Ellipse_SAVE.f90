MODULE FOV_Geo_Ellipse_SAVE

!
!Module that preserves precomputed values across calls to FOV_Ellipse
!
! Thomas J. Kleespies  NOAA/NESDIS  15 September 2004
!                      Revised       8 April 2005
!                      Adapted for conical 10 July 2008
 Implicit None
 
 Real  AlongTrackAngle 
 Real  CrossTrackAngle 
 Real  AlongTrackFOVSize 
 Real  CrossTrackFOVSize 
 Real  rmax 
 Real  rmin 
 Real  eccen 


End MODULE FOV_Geo_Ellipse_SAVE