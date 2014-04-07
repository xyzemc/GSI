	Real Function Arc_Distance(lat1,lon1,lat2,lon2,Arc_Angle)
!	
!	Name: Arc_Distance
!
!	Date: 25 March 1997
!         25 July 2008 - convert to Fortran 90
!
!	Purpose: Computes the arc distance and angular distance between two points on the earth
!	
!	Input:
!		Real Lat1,Lon1,Lat2,Lon2
!		     location of two points on sfc of earth
!		     Lat -90 + 90, lon -180 + 180
!
!	Output: Real Arc_Distance in km (function return)
!			Real Arc_Angle    in radians
!
!	Author:	Thomas J. Kleespies
!		Sensor Physics Branch
!       Center for Satellite Applications and Research
!		NOAA/NESDIS
!		301-763-8136
!
!	Mailing Address:
!		701 WWB E/RA-2
!		NOAA/NESDIS
!       5200 Auth Road
!		Camp Springs MD 20746
!
!	Email:
!		Thomas.J.Kleespies@noaa.gov
!

	Implicit None

	Real Lat1,Lon1,Lat2,Lon2,ss
	Real Colat1,Colat2,DelLon
	Real s
	Real DTR
	Real Arc_Angle


	DTR = ACOS(-1.)/180.

	Colat1 = (90. - Lat1)*DTR
	Colat2 = (90. - Lat2)*DTR
	DelLon = Abs(Lon2 - Lon1)*DTR 

	S = Cos(Colat1)*Cos(Colat2) + Sin(Colat1)*Sin(Colat2)*Cos(DelLon)

	Arc_Angle = Acos(S)

	Arc_Distance = 6371. * Arc_Angle

	Return

End Function Arc_Distance
