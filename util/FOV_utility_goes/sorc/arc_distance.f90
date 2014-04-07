 real(r_kind) function arc_distance(lat1,lon1,lat2,lon2,arc_angle)
!$$$  subprogram documentation block
!
! function:   arc_distance                 compute arc distance
!   prgmmr: gayno            org: np23           date: 2012-04-03
!	
! abstract:  This function computes the arc distance and angular distance 
!            between two points on the earth.
!
! program history log:
!   2008-07-25   kleespies - initial version
!   2014-04-03   gayno - bring up to GSI coding standards

! input argument list:
!    lat1/lon1  - latitude and longitude of point 1
!    lat2/lon2  - latitude and longitude of point 2
!
! output argument list:
!    arc_angle    - arc angle in radians
!    arc_distance - arc distance in km (returned from function)
!
! attributes:
!   language: f90
!   machine:  NCEP WCOSS
!
!$$$ end documentation block

 use constants, only : deg2rad
 use kinds, only : r_kind

 implicit none

 real(r_kind), intent(in)      :: lat1,lon1,lat2,lon2
 real(r_kind), intent(out)     :: arc_angle
 real(r_kind), parameter       :: rearth=6371._r_kind
 real(r_kind)                  :: colat1,colat2,dellon,s

 colat1 = (90._r_kind - lat1)*deg2rad
 colat2 = (90._r_kind - lat2)*deg2rad
 dellon = abs(lon2 - lon1)*deg2rad

 s = cos(colat1)*cos(colat2) + sin(colat1)*sin(colat2)*cos(dellon)

 arc_angle = acos(s)

 arc_distance = rearth * arc_angle

 return

 end function arc_distance
