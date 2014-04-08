 module calc_fov_geo
!
!Module that preserves precomputed values across calls to FOV_Ellipse
!
! Thomas J. Kleespies  NOAA/NESDIS  15 September 2004
!                      Revised       8 April 2005
!                      Adapted for conical 10 July 2008

 use kinds, only : r_kind

 implicit none
 
 private

 real(r_kind)         ::  along_track_angle 
 real(r_kind)         ::  cross_track_angle 
 real(r_kind)         ::  along_track_fov_size 
 real(r_kind)         ::  cross_track_fov_size 
 real(r_kind)         ::  rmax 
 real(r_kind)         ::  rmin 
 real(r_kind)         ::  eccen 

 public inside_fov_geo
 public fov_ellipse_geo

 contains

 subroutine inside_fov_geo(instr, ichan, sublat, sublon, &
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

 implicit none

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
!
 subroutine fov_ellipse_geo (instr, ichan, sublat, sublon,       &
                             lat, lon, expansion, elats, elons )
! Input satellite azimuth

! Computes FOV ellipses in latitude/longitude coordinates of a geosynchronous instrument
! Note that we model the FOV as circles. This is fine for the GOES sounder, but the 
! imager has square fovs.
!
! Thomas J. Kleespies  NOAA/NESDIS  
!
! 22 July 2008  Adapted from FOV_ELLIPSE
!                     
!
!
! Externals: 
!
!    Module     FOV_Ellipse_Geo_SAVE
!    Subroutine FOVGeoAngle_Sizes
!
! Input:
!
!  Integer Instr    - Instrument number - SEE RESTRICTIONS
!		  int   Instr 	Instrument number
!cggg  other routines use 31/32.  be consistent
!               41      GOES Imager
!               42      GOES Sounder
!
!  Integer Ichan    - channel number
!  Real    Sublat   - Subsatellite latitude 
!  Real    Sublon   - Subsatellite longitude 
!  Real    Lat      - Latitude 
!  Real    Lon      - Longitude
!  Real    Expansion- expansion factor.  Must be 1.0 for accurate renderine, 
!                     > 1.0 makes bigger ellipses, < 1.0 makes smaller ellipses.
!  
! Output:
!  Real    Elats    - ellipse latitudes  centered about lat,lon
!  Real    Elons    - ellipse longitudes centered about lat,lon
!
! Restrictions:
!
!!
!  Set Npoly for the order of the polygon that represents the ellipse 
!
!  Elats and Elon must be dimensioned to npoly in the calling routine
!
!  There are several engineering checks to handle things like arcsin( x>1.0 or x<-1.0)
!  These things can happen because POES navigation uses an oblate spheroid earth, while here we are
!  using a spherical earth, so there is a small inconsistency in computing arc angles.
!
!  No provisions are made for spacecraft roll-pitch-yaw errors, which are presumed to be small 
!  (SC attitude is usually held to within 0.1 deg)
!

 use constants, only : rad2deg, pi
 use kinds, only : i_kind, r_kind

 implicit none

 integer(i_kind), parameter     :: npoly = 120  ! set this to be the number of vertices in the polygon
 real(r_kind), parameter        :: earth = 6371.22   ! nominal earth radius
 real(r_kind), parameter        :: height = 35786.0  ! geosynchronous altitude 
 real(r_kind), parameter        :: geosynch = earth+height

 integer(i_kind), intent(in)    :: instr
 integer(i_kind), intent(in)    :: ichan
 real(r_kind), intent(in)       :: expansion
 real(r_kind), intent(in)       :: lat, lon
 real(r_kind), intent(in)       :: sublat, sublon 
 real(r_kind), intent(out)      :: elats(npoly), elons(npoly)

 integer(i_kind)                :: i
 integer(i_kind), save          :: ninstr ! Hang on to instrument number statically
 real(r_kind)                   :: dellon ! longitude difference from spot to subpoint
 real(r_kind)                   :: dellat ! latitude difference  from spot to subpoint
 real(r_kind)                   :: pos_ang ! rotation angle of the ellipse
 real(r_kind)                   :: coss, s, sinalpha 
 real(r_kind), dimension(npoly) :: psi, psip, r, cosc, c, sinb, b
 real(r_kind)                   :: ata, cta, atf, ctf     
 real(r_kind)                   :: a , cc , adist, arc_angle
 real(r_kind)                   :: nadir_angle, satellite_azimuth
 real(r_kind)                   :: ratio ! local ratio for eccentricity calculation

 ninstr = instr
  
 do i = 1 , npoly
   psi(i) = 2.0_r_kind*pi*(i-1)/(npoly-1)  ! Will connect Npoly points
 enddo

 if(lat == sublat .and. lon == sublon) then
   nadir_angle = 0.0_r_kind
 else
   adist = arc_distance(sublat,sublon,lat,lon,arc_angle)
   a = sqrt( geosynch*geosynch + earth*earth - 2.*geosynch*earth*cos(arc_angle) )
   cc = acos( (a*a + geosynch*geosynch - earth*earth) / (2.0_r_kind*a*geosynch) )
   nadir_angle = cc*rad2deg  ! this is our scan angle
 endIf

 call fov_geo_angles_sizes(ninstr,ichan,nadir_angle,ata,cta,atf,ctf)

 along_track_angle   = ata
 cross_track_angle   = cta
 along_track_fov_Size = atf
 cross_track_fov_Size = ctf

 rmax = 0.5_r_kind*cross_track_angle*expansion ! remember, these are semiaxes
 rmin = 0.5_r_kind*along_track_angle*expansion

 ratio = rmin**2/rmax**2
 if(ratio > 1.0_r_kind ) ratio = 1.0_r_kind  !  this takes care of some precision issues
 eccen = sqrt(1.0_r_kind - ratio)
 
 if( (lat == sublat) .and. (lon == sublon) ) then
    satellite_azimuth = 0.0_r_kind
 else
   dellon = lon - sublon

   coss = cos((90.0_r_kind-lat)/rad2deg)*cos((90.0_r_kind-sublat)/rad2deg) &
         + sin((90.0_r_kind-lat)/rad2deg)*sin((90.0_r_kind-sublat)/rad2deg)*cos(dellon/rad2deg) 

   s = acos(coss)

   sinalpha = sin((90.0_r_kind-sublat)/rad2deg)*sin(dellon/rad2deg)/sin(s)

   if(sinalpha >  1.0_r_kind) sinalpha =  1.0_r_kind
   if(sinalpha < -1.0_r_kind) sinalpha = -1.0_r_kind

   satellite_azimuth = asin(sinalpha)*rad2deg
   if (lat < 0.0_r_kind) satellite_azimuth = sign(180.0_r_kind,dellon) - satellite_azimuth
 endif

 pos_ang = satellite_azimuth 

 psip = psi + pos_ang/rad2deg
 r = rmax * sqrt( (1.0_r_kind - eccen**2)/(1.0_r_kind - eccen**2 * cos(psi)**2) ) 
 cosc = cos((90.0_r_kind-lat)/rad2deg)*cos(r/rad2deg) + sin((90.0_r_kind-lat)/rad2deg)*sin(r/rad2deg)*cos(psip)
 c = acos(cosc)*rad2deg

 elats(1:npoly) = 90.0_r_kind - c
 sinb = sin(r/rad2deg)*sin(psip)/sin(c/rad2deg)

 do i = 1 , npoly
   if(sinb(i) >  1.0_r_kind) sinb(i) =  1.0_r_kind
   if(sinb(i) < -1.0_r_kind) sinb(i) = -1.0_r_kind
 enddo

 b = asin(sinb)*rad2deg
 elons(1:npoly) = lon + b
 
 end subroutine fov_ellipse_geo

 subroutine fov_geo_angles_sizes(instr, chan, nadir_angle, along_track_angle, &
                                 cross_track_angle, along_track_fov_size, cross_track_fov_size)
! computes the cross track and along track angles of a geosynchronous instrument FOV as viewed
! from the center of the earth, and cross track and along track FOV size in km.
! presumes a spherical earth
!
! Thomas J. Kleespies  NOAA/NESDIS  17 February 2004
!									15 September 2004
!                                   Convert from IDL to FORTRAN90
!                      Revised       8 April 2005
!                      Adapted from cross track  25 July 2008
! Reference:
!
!
! Input:
!		  int   Instr 	Instrument number
!
!                31      GOES Imager
!                32      GOES Sounder
!               
!         int   Chan     Channel number
!
!         Real   Nadir_Angle  Angle from subsatellite point to observation point
!
! Output:
!         real   along_track_angle
!         real   cross_track_angle
!         real   along_track_fov_size
!         real   cross_track_fov_size
! Internal Variables
!
!	fovangle	angular dimension of the FOV
!	rtd			radians to degree conversion factor
!	radius		nominal earth radius
!   height      nominal geosynchronous altitude
!

 use constants, only : rad2deg, pi
 use kinds, only : i_kind, r_kind

 implicit none

 integer(i_kind), intent(in)             :: instr
 integer(i_kind), intent(in)             :: chan
 real(r_kind), intent(in)                :: nadir_angle
 real(r_kind), intent(out)               :: along_track_angle
 real(r_kind), intent(out)               :: cross_track_angle
 real(r_kind), intent(out)               :: along_track_fov_size
 real(r_kind), intent(out)               :: cross_track_fov_size

 real(r_kind), parameter                 :: radius = 6371.22
 real(r_kind), parameter                 :: height = 35786.0  ! geosynchronous altitude 

 real(r_kind), dimension(5), target      :: fovangle_31 =  &
  (/1.604282E-03, 6.417127E-03, 1.283425E-02, 6.417127E-03, 6.417127E-03/)
 real(r_kind), dimension(19), target     :: fovangle_32 =  &
  (/1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02,   &
    1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02,   &
    1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02,   &
    1.386558E-02, 1.386558E-02, 1.386558E-02, 1.386558E-02/)
 real(r_kind), dimension(:), pointer     :: fovangle

 real(r_kind)                            :: comp_za_center
 real(r_kind)                            :: comp_za_p
 real(r_kind)                            :: comp_za_m
 real(r_kind)                            :: distance_to_fov
 real(r_kind)                            :: nadir_angle_m
 real(r_kind)                            :: nadir_angle_p
!cggg  get rid of these?
 real(r_kind) :: prod1, prod2, prod3

! initialize to bad value
 along_track_angle = -999.0_r_kind
 cross_track_angle = -999.0_r_kind

! test for Instr in range
 nullify(fovangle)
 select case (instr)
   case (31)
     fovangle=>fovangle_31
   case (32)
     fovangle=>fovangle_32
   case default
     write(6,*) "Instr: ",Instr," OUT OF RANGE."
     stop 3
 end select

!Nadir angles of center and crosstrack extremes of fov

 nadir_angle_m = nadir_angle - fovangle(chan)*0.5_r_kind
 nadir_angle_p = nadir_angle + fovangle(chan)*0.5_r_kind

 prod1=(radius+height)/radius

!Complement of zenith angle for center and crosstrack extremes of fov
 prod2=sin(nadir_angle/rad2deg)
 prod3=prod1*prod2
 if (prod3 > 1.0_r_kind) then
   print*,'too far ', prod3
   stop
 else
   comp_za_center = 180.0_r_kind-asin(prod3)*rad2deg
 endif

 prod2=sin(nadir_angle_m/rad2deg)
 prod3=prod1*prod2
 if (prod3 > 1.0_r_kind) then
   print*,'too far ', prod3
   stop
 else
   comp_za_m = 180.0_r_kind-asin(prod3)*rad2deg
 end if

 prod2=sin(nadir_angle_p/rad2deg)
 prod3=prod1*prod2
 if (prod3 > 1.0_r_kind) then
   print*,'too far ', prod3
   stop
 else
   comp_za_p = 180.0_r_kind-asin(prod3)*rad2deg
 end if

! Cross track angle of the fov as viewed from center of the earth.
 cross_track_angle = abs(nadir_angle_p + comp_za_p - nadir_angle_m - comp_za_m)

! Cross track fov size in km
 cross_track_fov_size = abs(cross_track_angle*2.0_r_kind*pi*radius/360.0_r_kind)

! Distance from satellite to the center of the fov in km.
 distance_to_fov = (radius+height)*sin( (180.0_r_kind-nadir_angle-comp_za_center)/rad2deg)/sin((comp_za_center)/rad2deg)
 if(distance_to_fov <= 0.0_r_kind) distance_to_fov = height ! for nadir fov
  
! Along track fov size in km.  
! the following is an approximation, but it is close.  It underestimates the FOV by a smidge
 along_track_fov_size = 2.0_r_kind*distance_to_fov*tan(fovangle(chan)*0.5_r_kind/rad2deg)

! Along track angle of the fov as viewed from center of the earth.
 along_track_angle = 360.0_r_kind*along_track_fov_size/(2.0_r_kind*pi*radius)

 nullify(fovangle)

 end subroutine fov_geo_angles_sizes

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
 end module calc_fov_geo
