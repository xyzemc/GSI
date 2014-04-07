Subroutine Inside_FOV_Ellipse_Flat_Earth_Geo(                              &
                              Instr, Ichan,                                &
                              SubLat,SubLon,                               &
                              Lat, Lon, TestLat,TestLon,                   &
						      Expansion, Inside                            )


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

 USE FOV_Geo_Ellipse_SAVE

 Implicit None

! input variables
 Integer :: Instr             ! instrument number
 Integer :: Ichan             ! channel number
 Real    :: Sublat            ! subsatellite latitude
 Real    :: Sublon            ! subsatellite longitude
 Real    :: Lat               ! fov center latitude
 Real    :: Lon               ! fov center longitude
 Real    :: TestLat           ! latitude to be tested
 Real    :: TestLon           ! longitude to be tested
 Real    :: Expansion         ! fov expansion factor, normally = 1.0

! output variable
 Real    :: Inside            ! return value = 1.0 if test location inside fov


! local variables

! number of instruments
 Integer , Parameter :: maxinstr = 2
 Integer , Parameter :: MaxImagerChan   = 5
 Integer , Parameter :: MaxSounderChan  = 19

!cggg put in module definitions? Integer , Parameter :: InstrumentRange(2) = (/41 , 42/)


 Integer IFOV,i ! loop counters

 Real  :: dellon ! longitude difference from fov center to test location
 Real  :: dellat ! latitude  difference from fov center to test location

!  These two are what gets compared to determine if test location is within fov
 Real  :: d      ! angular distance from fov center to test location
 Real  :: r      ! angular distance from fov center to ellipse along d

 Real  :: psi    ! angle from 3:00 on fov to line from fov center to test location
 Real  :: psip   ! angle from latitude parallel through fov center to line from fov center to test location
 
 ! These are the flat earth variables
 Real  :: distance_north  ! north angular distance from fov center to test location
 Real  :: distance_east   ! east  angular distance from fov center to test location
 Real  :: Bearing_to_test ! same as psip
 Real  :: Bearing_to_test_deg ! in degrees

 Real ATA,CTA,ATF,CTF     ! temp variables for fov sizes

 Real  :: rtd      = 57.29577951       ! radians to degrees
 Real  :: dtr      = 0.017453293       ! degrees to radians
 Real  :: Pi       = 3.141526535       ! pi
 Real,parameter  :: earth      = 6371.22         ! nominal earth radius
 Real,parameter  :: height   = 35786.0         ! geosynchronous altitude 
 real, parameter :: geosynch = earth+height

 Real, Parameter  :: R1 = 1.  ! Equatorial radius. Work in angular distance, not km (otherwise r1=6371)
 Real, Parameter  :: R2 = R1  ! assume spherical earth (otherwise r2 = polar radius)

 Real  :: Satellite_Azimuth   ! satellite azimuth, used in computing psi
 Real  :: SolarAz             ! solar azimuth, used to get satellite azimuth from relative azimuth
 Real  :: SolarZA             ! solar zenith, not used.

 Real  :: Ratio ! local ratio for eccentricity calculation

 Integer, SAVE :: Ninstr ! Hang on to instrument number statically

 Real NadirAngle
 Real Adist , Cc, a, s, coss, sinalpha, arc_angle

 Real Arc_Distance

  Ninstr = Instr
  

  If(Lat == SubLat .and. Lon == SubLon) Then
   NadirAngle = 0.0
  Else
   ADist = Arc_Distance(SubLat,SubLon,Lat,Lon,Arc_Angle)
   a = sqrt( geosynch*geosynch + earth*earth - 2.*geosynch*earth*cos(arc_angle) )
   Cc = acos( (a*a + geosynch*geosynch - earth*earth) / (2.*a*geosynch) )
   NadirAngle = Cc*rtd  ! this is our scan angle
  EndIf

  Call FOVGeoAnglesSizes( Ninstr,Ichan,NadirAngle,ATA,CTA,ATF,CTF)
  AlongTrackAngle   = ATA
  CrossTrackAngle   = CTA
  AlongTrackFOVSize = ATF
  CrossTrackFOVSize = CTF

  rmax = .5*CrossTrackAngle* expansion ! remember, these are semiaxes
  rmin = .5*AlongTrackAngle* expansion

  Ratio = rmin**2./rmax**2.
  If(Ratio > 1.0 ) Ratio = 1.0  !  this takes care of some precision issues

  eccen = sqrt(1. - Ratio)
  
  If( (Lat == Sublat) .and. (Lon == Sublon) ) Then
   Satellite_Azimuth = 0.0
  Else

   dellon = Lon - Sublon

   coss = cos((90.d0-Lat)/rtd)*cos((90.d0-Sublat)/rtd) &
        + sin((90.d0-Lat)/rtd)*sin((90.d0-Sublat)/rtd)*cos(dble(Dellon/rtd)) ! watch the double

   s = acos(coss)

   sinalpha = sin((90.d0- Sublat  )/rtd)*sin(dble(dellon)/rtd)/sin(s)

! handle numeric imprecision 
   if(sinalpha >  1.0) sinalpha =  1.0
   if(sinalpha < -1.0) sinalpha = -1.0

   Satellite_Azimuth = asin(sinalpha)*rtd
   if (lat < 0.0) satellite_azimuth = sign(180.0,dellon) - satellite_azimuth

  EndIf

! get satellite az where we want it.
! 1st, convert +- to 0-360
  If(Satellite_Azimuth < 0.0) Satellite_Azimuth = 360.0 + Satellite_Azimuth
! 2nd, Shift rotation direction
  Satellite_Azimuth = Mod((450.0-Satellite_Azimuth),360.0)

! delta lat and lon
  dellat = (TestLat - Lat)*DTR
  dellon = (TestLon - Lon)*DTR

! distance north and east in degrees
  Distance_North =  R1*dellat
  Distance_East  =  R2*cos(lat*DTR)*Dellon

! angle to the test point
  Bearing_to_Test = mod(atan2(distance_north,distance_east),2*Pi )
  Bearing_to_Test_deg = Bearing_to_Test*RTD ! convert to degrees

! this is the arc distance to the test point
  d=2.*asin(sqrt((sind((testlat-lat)/2))**2 +     & 
                 cosd(testlat)*cosd(lat)*(sind((testlon-lon)/2))**2))
  d = d*RTD  ! convert to degrees

  psip = bearing_to_test_deg

  psi = (psip  - Satellite_Azimuth)
  psi = psi*DTR ! convert to radians

! r is the angular distance from the fov center to the edge of the ellipse in degrees

  r = rmax * sqrt( (1. - eccen**2.)/(1. - eccen**2. *cos(psi)**2.) )

  Inside = 0.0
!
  if(d<r) Inside = 1.  

 Return
 End
