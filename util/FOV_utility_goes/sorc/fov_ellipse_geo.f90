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
 use calc_fov_geo

 implicit none

 real(r_kind), external         :: arc_distance

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
