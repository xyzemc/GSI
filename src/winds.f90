!C-----------------------------------------------------------------------
!C  PROGRAM TO COMPUTE WIND SPEED AND DIRECTION FROM U AND V COMPONENTS
!C-----------------------------------------------------------------------
      SUBROUTINE UV2DS(U,V)

 use kinds, only: r_kind
 use constants, only: deg2rad,pi,zero,rad2deg
 real(r_kind) u,v,du,dv,dd,ds
 
!C-----------------------------------------------------------------------
!C-----------------------------------------------------------------------
 
!C  RETURN THE SPEED AND DIRECTION IN THE WIND COMPONENT VARAIBLES
!C  --------------------------------------------------------------
 
      DU = U
      DV = V
 
      IF(DU == zero .AND. DV == zero) THEN
         DD = 360.0_r_kind
      ELSE IF(DU ==zero) THEN
         IF(DV>zero) DD = 180.0_r_kind
         IF(DV<zero) DD = 360.0_r_kind
         ELSE IF(DV == zero) THEN
         IF( DU>zero ) DD = 270.0_r_kind
         IF( DU<zero ) DD = 90.0_r_kind
      ELSE
         DD = ATAN2(DU,DV) * rad2deg + 180.0_r_kind
         DD = dMOD(DD,360.0_r_kind)
      ENDIF
      DS = SQRT(DU**2 + DV**2)
 
      U = DD
      V = DS
 
      RETURN
      END
