 subroutine inside_fov_geo(Instr, Ichan, SubLat, SubLon, &
                                              lat, lon, testlat, testlon,   &
                                              expansion, inside)
! Determines if a point is inside or outside of an Geo FOV ellipse
!
!
! Consider the earth to be locally flat around the field of view
!
!
! Thomas J. Kleespies  NOAA/NESDIS  Adapted 24 July 2007
!                                                  
!
! Externals: 
!
!    Module     FOV_Geo_Ellipse_SAVE
!    Subroutine FOVGeoAngle_Sizes
!
!
! Input:
!
!  Integer Instr    - Instrument number - SEE RESTRICTIONS
!		  int   Instr 	Instrument number
!		  int   Instr 	Instrument number
!               41      GOES Imager
!               42      GOES Sounder
!cggg  other routines use 31 and 32
!               31      GOES Imager
!               32      GOES Sounder
!  Integer Ichan    - Channel number
!  Real    Sublat   - Subsatellite latitude 
!  Real    Sublon   - Subsatellite longitude 
!  Real    Lat      - Latitude of FOV center
!  Real    Lon      - Longitude of FOV center
!  Real    TestLat  - Latitude of point to be tested
!  Real    TestLon  - Longitude of point to be tested
!  Real    Expansion- expansion factor.  Must be 1.0 for accurate rendering, 
!                     > 1.0 makes bigger ellipses, < 1.0 makes smaller ellipses.
!  
! Output:
!  Real    Inside     = 1 if [TestLat,TestLon] is inside the FOV, = 0 if outside
!
! Restrictions:
!
!!
!
!  There are several engineering checks to handle things like arcsin( x>1.0 or x<-1.0)
!  These things can happen because POES navigation uses an oblate spheroid earth, while here we are
!  using a spherical earth, so there is a small inconsistency in computing arc angles.
!
!  No provisions are made for spacecraft roll-pitch-yaw errors, which are presumed to be small 
!  (SC attitude is usually held to within 0.1 deg)
!

 use constants, only : deg2rad, rad2deg, pi
 use kinds, only : r_kind, i_kind
 use calc_fov_geo

 implicit none

 real(r_kind), external          :: arc_distance

 integer(i_kind), intent(in)     :: instr     ! instrument number
 integer(i_kind), intent(in)     :: ichan     ! channel number
 real(r_kind), intent(in)        :: sublat    ! subsatellite latitude
 real(r_kind), intent(in)        :: sublon    ! subsatellite longitude
 real(r_kind), intent(in)        :: lat       ! fov center latitude
 real(r_kind), intent(in)        :: lon       ! fov center longitude
 real(r_kind), intent(in)        :: testlat   ! latitude to be tested
 real(r_kind), intent(in)        :: testlon   ! longitude to be tested
 real(r_kind), intent(in)        :: expansion ! fov expansion factor, normally = 1.0
 real(r_kind), intent(out)       :: inside    ! return value = 1.0 if test location inside fov

! move these to a module?
 real(r_kind), parameter         :: earth = 6371.22_r_kind   ! nominal earth radius
 real(r_kind), parameter         :: height = 35786.0_r_kind  ! geosynchronous altitude 
 real(r_kind), parameter         :: geosynch = earth+height
 real(r_kind), parameter         :: r1 = 1.0_r_kind   ! Equatorial radius. Work in angular distance, 
                                                      ! not km (otherwise r1=6371)
 real(r_kind), parameter         :: r2 = r1  ! assume spherical earth (otherwise r2 = polar radius)

! local variables
 integer(i_kind), save           :: ninstr 
 real(r_kind)                    :: dellon ! latitude difference from fov center to test location
 real(r_kind)                    :: dellat ! longitude difference from fov center to test location
 real(r_kind)                    :: d     ! angular distance from fov center to test location
 real(r_kind)                    :: r     ! angular distance from fov center to ellipse along d
 real(r_kind)                    :: psi   ! angle from 3:00 on fov to line from 
                                          ! fov center to test location
 real(r_kind)                    :: psip  ! angle from latitude parallel through 
                                          ! fov center to line from fov center 
                                          ! to test location
 real(r_kind)                    :: distance_north ! north angular distance from 
                                                   ! fov center to test location.
                                                   ! for flat earth.
 real(r_kind)                    :: distance_east  ! east angular distance from
                                                   ! fov center to test location.
                                                   ! for flat earth.
 real(r_kind)                    :: bearing_to_test ! same as psip. for flat earth.
 real(r_kind)                    :: bearing_to_test_deg ! in degrees. for flat earth.
 real(r_kind)                    :: ata, cta, atf, ctf ! temp variables for fov sizes
 real(r_kind)                    :: satellite_azimuth  ! satellite azimuth, 
                                                       ! used in computing psi
 real(r_kind)                    :: ratio  ! local ratio for eccentricity calculation
 real(r_kind)                    :: nadir_angle
 real(r_kind)                    :: adist, cc, a, s, coss, sinalpha, arc_angle

 ninstr = instr

 if(lat == sublat .and. lon == sublon) then
   nadir_angle = 0.0
 else
   adist = arc_distance(sublat,sublon,lat,lon,arc_angle)
   a = sqrt( geosynch*geosynch + earth*earth - 2.0_r_kind*geosynch*earth*cos(arc_angle) )
   cc = acos( (a*a + geosynch*geosynch - earth*earth) / (2.0_r_kind*a*geosynch) )
   nadir_angle = cc*rad2deg  ! this is our scan angle
 endif

 call fov_geo_angles_sizes(ninstr,ichan,nadir_angle,ata,cta,atf,ctf)
 along_track_angle   = ata
 cross_track_angle   = cta
 along_track_fov_size = atf
 cross_track_fov_size = ctf

 rmax = 0.5_r_kind*cross_track_angle* expansion ! remember, these are semiaxes
 rmin = 0.5_r_kind*along_track_angle* expansion

 ratio = rmin**2/rmax**2
 if(ratio > 1.0_r_kind ) Ratio = 1.0_r_kind  !  this takes care of some precision issues

 eccen = sqrt(1.0_r_kind - ratio)
  
 if( (lat == sublat) .and. (lon == sublon) ) then
   satellite_azimuth = 0.0_r_kind
 else
   dellon = Lon - Sublon

   coss = cos((90.0_r_kind-lat)/rad2deg)*cos((90.0_r_kind-sublat)/rad2deg) &
        + sin((90.0_r_kind-lat)/rad2deg)*sin((90.0_r_kind-sublat)/rad2deg)*cos((dellon/rad2deg))

   s = acos(coss)

   sinalpha = sin((90.0_r_kind - sublat)/rad2deg)*sin(dellon/rad2deg)/sin(s)

   if(sinalpha >  1.0_r_kind) sinalpha =  1.0_r_kind
   if(sinalpha < -1.0_r_kind) sinalpha = -1.0_r_kind

   satellite_azimuth = asin(sinalpha)*rad2deg
   if (lat < 0.0_r_kind) satellite_azimuth = sign(180.0_r_kind,dellon) - satellite_azimuth
 endif

! get satellite az where we want it.
! 1st, convert +- to 0-360
 if(satellite_azimuth < 0.0_r_kind) satellite_azimuth = 360.0 + satellite_azimuth

! 2nd, Shift rotation direction
 satellite_azimuth = mod((450.0_r_kind-satellite_azimuth),360.0_r_kind)

 dellat = (testlat - lat)*deg2rad
 dellon = (testlon - lon)*deg2rad

! distance north and east in degrees
 distance_north =  r1*dellat
 distance_east  =  r2*cos(lat*deg2rad)*dellon

! angle to the test point
 bearing_to_test = mod(atan2(distance_north,distance_east),2.0_r_kind*pi)
 bearing_to_test_deg = bearing_to_test*rad2deg ! convert to degrees

! this is the arc distance to the test point
 d=2.0_r_kind*asin(sqrt((sind((testlat-lat)/2.0_r_kind))**2 +     & 
              cosd(testlat)*cosd(lat)*(sind((testlon-lon)/2.0_r_kind))**2))
 d = d*rad2deg  ! convert to degrees

 psip = bearing_to_test_deg

 psi = psip  - satellite_azimuth
 psi = psi*deg2rad ! convert to radians

! r is the angular distance from the fov center to the edge of the ellipse in degrees
 r = rmax * sqrt( (1.0_r_kind - eccen**2.0_r_kind)/(1.0_r_kind - eccen**2 *cos(psi)**2) )

 inside = 0.0_r_kind
 
 if (d<r) inside = 1.0_r_kind

 end subroutine inside_fov_geo
