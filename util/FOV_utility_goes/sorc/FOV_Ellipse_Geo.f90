Subroutine FOV_Ellipse_Geo (Instr, Ichan,                     &
                        SubLat,SubLon,                        &
                        Lat, Lon,                             &
						Expansion,Elats, Elons )

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

 USE FOV_Geo_Ellipse_SAVE

 Implicit None

 Integer , Parameter :: Npoly = 120  ! set this to be the number of vertices in the polygon

 Integer Instr
 Integer Ichan
 Real Expansion
 Real Relative_Azimuth
 Real Lat
 Real Lon
 Real Slat  
 Real Slon  
 Real Elats(Npoly)
 Real Elons(Npoly)
 Integer , Parameter :: maxinstr = 2
 Integer , Parameter :: MaxImagerChan   = 5
 Integer , Parameter :: MaxSounderChan  = 19

!cggg not used here Integer , Parameter :: InstrumentRange(2) = (/41 , 42/)


 Integer i ! loop counters

 Real dellon ! longitude differencd from spot to subpoint
 Real dellat ! latitude difference  from spot to subpoint
 Real pos_ang ! rotation angle of the ellipse

 Real coss,s,sinalpha ! local variables
! more local variables
 Real , Dimension(Npoly) :: psi
 Real , Dimension(Npoly) :: psip
 Real , Dimension(Npoly) :: r
 Real , Dimension(Npoly) :: cosc
 Real , Dimension(Npoly) :: c
 Real , Dimension(Npoly) :: sinB
 Real , Dimension(Npoly) :: B
 Real ATA,CTA,ATF,CTF     

 Real a , cc , rr, ADist, Arc_Angle

 Real Arc_Distance

 Real  :: rtd      = 57.29577951   ! radians to degrees
 Real  :: PI	   = 3.141592653589793  ! YEAH, YEAH, I know this is more precision than necessary, but so what?

 Real,parameter  :: earth      = 6371.22           ! nominal earth radius
 Real,parameter  :: height   = 35786.0           ! geosynchronous altitude 
 real, parameter :: geosynch = earth+height

 Real SubLat,SubLon  ! subsatellite position

 Real NadirAngle

 Real Satellite_Azimuth

 Real Ratio ! local ratio for eccentricity calculation

 Integer, SAVE :: Ninstr ! Hang on to instrument number statically


  Ninstr = Instr

  
  Do i = 1 , Npoly
   psi(i) = 2.*pi*(i-1)/(Npoly-1)  ! Will connect Npoly points
  EndDo


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

  pos_ang = Satellite_Azimuth 

  psip = psi + pos_ang/rtd
  r = rmax * sqrt( (1 - eccen**2.)/(1 - eccen**2. *cos(psi)**2.) ) 
  cosc = cos((90.-lat)/rtd)*cos(r/rtd) + sin((90.-lat)/rtd)*sin(r/rtd)*cos(psip)
  c = acos(cosc)*rtd

  Elats(1:Npoly) = 90. - c
  sinB = sin(r/rtd)*sin(psip)/sin(c/rtd)

! handle numeric imprecision 
  Do i = 1 , Npoly
   If(sinB(i) >  1.0) sinB(i) =  1.0
   If(sinB(i) < -1.0) sinB(i) = -1.0
  EndDo

  B = asin(sinB)*rtd
  Elons(1:Npoly) = lon + B
 
 End Subroutine FOV_Ellipse_Geo
