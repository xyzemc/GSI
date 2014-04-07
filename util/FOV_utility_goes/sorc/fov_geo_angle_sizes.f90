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
