! module igrf_utilities

!    public 

!    contains 

!*==dmddec.f90 processed by SPAG 8.01MH 15:55 30 Jan 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!
SUBROUTINE dmddec(I,M,X)
   IMPLICIT NONE
!*** Start of declarations inserted by SPAG
   DOUBLE PRECISION de , em , X
   INTEGER I , M
!*** End of declarations inserted by SPAG
   de = I
   em = M
   IF ( I<0 ) em = -em
   X = de + em/60.0
END SUBROUTINE dmddec
!*==ddecdm.f90 processed by SPAG 8.01MH 15:55 30 Jan 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!
SUBROUTINE ddecdm(X,I,M)
   IMPLICIT NONE
!*** Start of declarations inserted by SPAG
   DOUBLE PRECISION dr , sig , t , X
   INTEGER I , isig , M
!*** End of declarations inserted by SPAG
   sig = sign(1.1D0,X)
   dr = abs(X)
   I = int(dr)
   t = I
   M = nint(60.*(dr-t))
   IF ( M==60 ) THEN
      M = 0
      I = I + 1
   ENDIF
   isig = int(sig)
   IF ( I/=0 ) THEN
      I = I*isig
   ELSE
      IF ( M/=0 ) M = M*isig
   ENDIF
END SUBROUTINE ddecdm

! end module igrf_utilities