!*==igrf12.f90 processed by SPAG 8.01MH 15:55 30 Jan 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
PROGRAM igrf12
!
!     This is a program for synthesising geomagnetic field values from the
!     International Geomagnetic Reference Field series of models as agreed
!     in December 2014 by IAGA Working Group V-MOD.
!     It is the 12th generation IGRF, ie the 11th revision.
!     The main-field models for 1900.0, 1905.0,..1940.0 and 2015.0 are
!     non-definitive, those for 1945.0, 1950.0,...2010.0 are definitive and
!     the secular-variation model for 2015.0 to 2020.0 is non-definitive.
!
!     Main-field models are to degree and order 10 (ie 120 coefficients)
!     for 1900.0-1995.0 and to 13 (ie 195 coefficients) for 2000.0 onwards.
!     The predictive secular-variation model is to degree and order 8 (ie 80
!     coefficients).
!
!     Options include values at different locations at different
!     times (spot), values at same location at one year intervals
!     (time series), grid of values at one time (grid); geodetic or
!     geocentric coordinates, latitude & longitude entered as decimal
!     degrees or degrees & minutes (not in grid), choice of main field
!     or secular variation or both (grid only).
! Recent history of code:
!     Aug 2003:
!     Adapted from 8th generation version to include new maximum degree for
!     main-field models for 2000.0 and onwards and use WGS84 spheroid instead
!     of International Astronomical Union 1966 spheroid as recommended by IAGA
!     in July 2003. Reference radius remains as 6371.2 km - it is NOT the mean
!     radius (= 6371.0 km) but 6371.2 km is what is used in determining the
!     coefficients.
!     Dec 2004:
!     Adapted for 10th generation
!     Jul 2005:
!     1995.0 coefficients as published in igrf9coeffs.xls and igrf10coeffs.xls
!     now used in code - (Kimmo Korhonen spotted 1 nT difference in 11 coefficients)
!     Dec 2009:
!     Adapted for 11th generation
!     Dec 2014:
!     Adapted for 12th generation
!
   IMPLICIT NONE
!*** Start of declarations inserted by SPAG
   DOUBLE PRECISION alt , clt , d , date , dd , df , dh , ds , dtmn , dtmx , dx , dy , dz , f , f1 , fact , h , s , x , xln
   DOUBLE PRECISION xlnd , xlnf , xlni , xlt , xltd , xltf , xlti , y , z
   INTEGER i , idd , idec , idecm , idf , idh , idm , ids , idx , idy , idz , ifl , ih , imx , inc , incm , iopt , itype , iu , ix
   INTEGER iy , iz , ln , lnd , lnf , lni , lnm , lt , ltd , ltf , lti , ltm , ncount , nf
!*** End of declarations inserted by SPAG
   CHARACTER*1 ia
   CHARACTER*11 type
   CHARACTER*20 name
   CHARACTER*30 fnm
   INTEGER :: spag_nextblock_1
   DATA dtmn , dtmx/1900.0 , 2025.0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         WRITE (6,*)
         WRITE (6,*) '******************************************************'
         WRITE (6,*) '*              IGRF SYNTHESIS PROGRAM                *'
         WRITE (6,*) '*                                                    *'
         WRITE (6,*) '* A program for the computation of geomagnetic       *'
         WRITE (6,*) '* field elements from the International Geomagnetic  *'
         WRITE (6,*) '* Reference Field (12th generation) as revised in    *'
         WRITE (6,*) '* December 2014 by the IAGA Working Group V-MOD.     *'
         WRITE (6,*) '*                                                    *'
         WRITE (6,*) '* It is valid for dates from 1900.0 to 2020.0,       *'
         WRITE (6,*) '* values up to 2025.0 will be computed but with      *'
         WRITE (6,*) '* reduced accuracy. Values for dates before 1945.0   *'
         WRITE (6,*) '* and after 2010.0 are non-definitive, otherwise the *'
         WRITE (6,*) '* values are definitive.                             *'
         WRITE (6,*) '*                                                    *'
         WRITE (6,*) '* Susan Macmillan          British Geological Survey *'
         WRITE (6,*) '*                           IAGA Working Group V-MOD *'
         WRITE (6,*) '******************************************************'
         WRITE (6,*)
         WRITE (6,*) 'Enter name of output file (30 characters maximum)'
         WRITE (6,*) 'or press "Return" for output to screen'
         READ (5,99001) fnm
99001    FORMAT (A30)
         IF ( ichar(fnm(1:1))==32 ) THEN
            iu = 6
         ELSE
            iu = 2
            OPEN (UNIT=iu,FILE=fnm,STATUS='NEW')
         ENDIF
         fact = 180.0/3.141592654
         ncount = 0
         SPAG_Loop_1_2: DO
!
            WRITE (6,*) 'Enter value for coordinate system:'
            WRITE (6,*) '1 - geodetic (shape of Earth is approximated by a spheroid)'
            WRITE (6,*) '2 - geocentric (shape of Earth is approximated by a sphere)'
            READ (5,*) itype
            IF ( itype>=1 .AND. itype<=2 ) THEN
               IF ( itype==1 ) type = ' geodetic  '
               IF ( itype==2 ) type = ' geocentric'
               DO
!
                  WRITE (6,*) 'Choose an option:'
                  WRITE (6,*) '1 - values at one or more locations & dates'
                  WRITE (6,*) '2 - values at yearly intervals at one location'
                  WRITE (6,*) '3 - values on a latitude/longitude grid at one date'
                  READ (5,*) iopt
                  IF ( iopt>=1 .AND. iopt<=3 ) THEN
                     IF ( iopt==3 ) THEN
                        DO
!
!     GRID OF VALUES...
!
                           WRITE (6,*) 'Enter value for MF/SV flag:'
                           WRITE (6,*) '0 for main field (MF)'
                           WRITE (6,*) '1 for secular variation (SV)'
                           WRITE (6,*) '2 for both'
                           WRITE (6,*) '9 to quit'
                           READ (5,*) ifl
                           IF ( ifl==9 ) STOP
                           IF ( ifl==0 .OR. ifl==1 .OR. ifl==2 ) THEN
!
                              WRITE (6,*) 'Enter initial value, final value & increment or'
                              WRITE (6,*) 'decrement of latitude, in degrees & decimals'
                              READ (5,*) xlti , xltf , xltd
                              lti = nint(1000.0*xlti)
                              ltf = nint(1000.0*xltf)
                              ltd = nint(1000.0*xltd)
                              WRITE (6,*) 'Enter initial value, final value & increment or'
                              WRITE (6,*) 'decrement of longitude, in degrees & decimals'
                              READ (5,*) xlni , xlnf , xlnd
                              lni = nint(1000.0*xlni)
                              lnf = nint(1000.0*xlnf)
                              lnd = nint(1000.0*xlnd)
                              IF ( lti<-90000 .OR. lti>90000 ) THEN
                                 spag_nextblock_1 = 10
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              IF ( ltf<-90000 .OR. ltf>90000 ) THEN
                                 spag_nextblock_1 = 10
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              IF ( lni<-360000 .OR. lni>360000 ) THEN
                                 spag_nextblock_1 = 11
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              IF ( lnf<-360000 .OR. lnf>360000 ) THEN
                                 spag_nextblock_1 = 11
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              WRITE (6,*) 'Enter date in years A.D.'
                              READ (5,*) date
                              IF ( date<dtmn .OR. date>dtmx ) THEN
                                 spag_nextblock_1 = 5
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              IF ( itype==1 ) THEN
                                 WRITE (6,*) 'Enter altitude in km'
                              ELSE
                                 WRITE (6,*) 'Enter radial distance in km (>3485 km)'
                              ENDIF
                              READ (5,*) alt
                              IF ( itype==2 .AND. alt<=3485.0 ) THEN
                                 spag_nextblock_1 = 6
                                 CYCLE SPAG_DispatchLoop_1
                              ENDIF
                              WRITE (iu,99002) date , alt , type
99002                         FORMAT (' Date =',F9.3,5X,'Altitude =',F10.3,' km',5X,A11//'      Lat     Long',7X,'D',7X,'I',7X,'H',&
                                    & 7X,'X',7X,'Y',7X,'Z',7X,'F')
!
                              lt = lti
                              DO
                                 xlt = lt
                                 xlt = 0.001*xlt
                                 clt = 90.0 - xlt
                                 IF ( clt<-0.001 .OR. clt>180.001 ) THEN
                                    spag_nextblock_1 = 7
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                                 ln = lni
                                 SPAG_Loop_5_1: DO
                                    xln = ln
                                    xln = 0.001*xln
                                    IF ( xln<=-360.0 ) xln = xln + 360.0
                                    IF ( xln>=360.0 ) xln = xln - 360.0
                                    CALL igrf12syn(0,date,itype,alt,clt,xln,x,y,z,f)
                                    d = fact*atan2(y,x)
                                    h = sqrt(x*x+y*y)
                                    s = fact*atan2(z,h)
                                    ih = nint(h)
                                    ix = nint(x)
                                    iy = nint(y)
                                    iz = nint(z)
                                    nf = nint(f)
                                    IF ( ifl/=0 ) THEN
                                       CALL igrf12syn(1,date,itype,alt,clt,xln,dx,dy,dz,f1)
                                       idx = nint(dx)
                                       idy = nint(dy)
                                       idz = nint(dz)
                                       dd = (60.0*fact*(x*dy-y*dx))/(h*h)
                                       idd = nint(dd)
                                       dh = (x*dx+y*dy)/h
                                       idh = nint(dh)
                                       ds = (60.0*fact*(h*dz-z*dh))/(f*f)
                                       ids = nint(ds)
                                       df = (h*dh+z*dz)/f
                                       idf = nint(df)
                                    ENDIF
!
                                    IF ( ifl==0 ) WRITE (iu,99003) xlt , xln , d , s , ih , ix , iy , iz , nf
                                    IF ( ifl==1 ) WRITE (iu,99004) xlt , xln , idd , ids , idh , idx , idy , idz , idf
99004                               FORMAT (2F9.3,7I8)
                                    IF ( ifl==2 ) THEN
                                       WRITE (iu,99003) xlt , xln , d , s , ih , ix , iy , iz , nf
                                       WRITE (iu,99005) idd , ids , idh , idx , idy , idz , idf
99005                                  FORMAT (14X,'SV: ',7I8)
                                    ENDIF
!
                                    ln = ln + lnd
                                    IF ( lnd<0 ) THEN
                                       IF ( ln<lnf ) EXIT SPAG_Loop_5_1
                                    ELSEIF ( ln>lnf ) THEN
                                       EXIT SPAG_Loop_5_1
                                    ENDIF
                                 ENDDO SPAG_Loop_5_1
                                 lt = lt + ltd
                                 IF ( ltd<0 ) THEN
                                    IF ( lt<ltf ) THEN
                                       spag_nextblock_1 = 4
                                       CYCLE SPAG_DispatchLoop_1
                                    ENDIF
                                 ELSEIF ( lt>ltf ) THEN
                                    spag_nextblock_1 = 4
                                    CYCLE SPAG_DispatchLoop_1
                                 ENDIF
                              ENDDO
                           ENDIF
                        ENDDO
                     ELSE
                        DO
!
                           WRITE (6,*) 'Enter value for format of latitudes and longitudes:'
                           WRITE (6,*) '1 - in degrees & minutes'
                           WRITE (6,*) '2 - in decimal degrees'
                           READ (5,*) idm
                           IF ( idm>=1 .AND. idm<=2 ) THEN
                              IF ( ncount/=0 ) EXIT SPAG_Loop_1_2
                              spag_nextblock_1 = 3
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_3: DO
!
            WRITE (6,*) 'Do you want values for another date & position? (y/n)'
            READ (5,'(A1)') ia
            IF ( ia=='Y' .OR. ia=='y' .OR. ia=='N' .OR. ia=='n' ) THEN
               IF ( ia=='N' .OR. ia=='n' ) THEN
                  WRITE (iu,99006)
99006             FORMAT (' D is declination (+ve east)'/' I is inclination (+ve down)'/' H is horizontal intensity'/              &
                         &' X is north component'/' Y is east component'/' Z is vertical component (+ve down)'/                    &
                         &' F is total intensity')
                  WRITE (iu,99007)
99007             FORMAT (/' SV is secular variation (annual rate of change)')
                  IF ( itype==2 ) THEN
                     WRITE (iu,*) 'These elements are relative to the geocentric coordinate system'
                  ELSE
                     WRITE (iu,*)
                  ENDIF
                  STOP
               ENDIF
               EXIT SPAG_Loop_1_3
            ENDIF
         ENDDO SPAG_Loop_1_3
         spag_nextblock_1 = 3
      CASE (3)
!
         ncount = 1
         IF ( iopt/=2 ) THEN
            WRITE (6,*) 'Enter date in years A.D.'
            READ (5,*) date
            IF ( date<dtmn .OR. date>dtmx ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
 
         IF ( itype==1 ) THEN
            WRITE (6,*) 'Enter altitude in km'
         ELSE
            WRITE (6,*) 'Enter radial distance in km (>3485 km)'
         ENDIF
         READ (5,*) alt
         IF ( itype==2 .AND. alt<=3485.0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         IF ( idm==1 ) THEN
            WRITE (6,*) 'Enter latitude & longitude in degrees & minutes'
            WRITE (6,*) '(if either latitude or longitude is between -1'
            WRITE (6,*) 'and 0 degrees, enter the minutes as negative).'
            WRITE (6,*) 'Enter 4 integers'
            READ (5,*) ltd , ltm , lnd , lnm
            IF ( ltd<-90 .OR. ltd>90 .OR. ltm<=-60 .OR. ltm>=60 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( lnd<-360 .OR. lnd>360 .OR. lnm<=-60 .OR. lnm>=60 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ltm<0 .AND. ltd/=0 ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( lnm<0 .AND. lnd/=0 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL dmddec(ltd,ltm,xlt)
            CALL dmddec(lnd,lnm,xln)
         ELSE
            WRITE (6,*) 'Enter latitude & longitude in decimal degrees'
            READ (5,*) xlt , xln
            IF ( xlt<-90.0 .OR. xlt>90.0 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( xln<-360.0 .OR. xln>360.0 ) THEN
!
               WRITE (6,99008) xln
99008          FORMAT (' ***** Error *****'/' XLN =',F10.3,' - out of range')
               STOP
            ENDIF
         ENDIF
!
         WRITE (*,*) 'Enter place name (20 characters maximum)'
         READ (*,'(A)') name
         clt = 90.0 - xlt
         IF ( clt<0.0 .OR. clt>180.0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( xln<=-360.0 .OR. xln>=360.0 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iopt==2 ) THEN
!
!
!     SERIES OF VALUES AT ONE LOCATION...
!
            IF ( idm==1 ) THEN
               WRITE (iu,99009) ltd , ltm , type , lnd , lnm , alt , name
99009          FORMAT ('Lat',2I4,A11,'  Long ',2I4,F10.3,' km ',A20)
            ELSE
               WRITE (iu,99010) xlt , type , xln , alt , name
99010          FORMAT ('Lat',F8.3,A11,'  Long ',F8.3,F10.3,' km ',A20)
            ENDIF
            WRITE (iu,99011)
99011       FORMAT (3X,'DATE',7X,'D',3X,'SV',6X,'I',2X,'SV',6X,'H',4X,'SV',7X,'X',4X,'SV',7X,'Y',4X,'SV',7X,'Z',4X,'SV',6X,'F',4X, &
                   &'SV')
            imx = dtmx - dtmn - 5
            DO i = 1 , imx
               date = dtmn - 0.5 + i
               CALL igrf12syn(0,date,itype,alt,clt,xln,x,y,z,f)
               d = fact*atan2(y,x)
               h = sqrt(x*x+y*y)
               s = fact*atan2(z,h)
               ih = nint(h)
               ix = nint(x)
               iy = nint(y)
               iz = nint(z)
               nf = nint(f)
!
               CALL igrf12syn(1,date,itype,alt,clt,xln,dx,dy,dz,f1)
               dd = (60.0*fact*(x*dy-y*dx))/(h*h)
               dh = (x*dx+y*dy)/h
               ds = (60.0*fact*(h*dz-z*dh))/(f*f)
               df = (h*dh+z*dz)/f
               idd = nint(dd)
               idh = nint(dh)
               ids = nint(ds)
               idx = nint(dx)
               idy = nint(dy)
               idz = nint(dz)
               idf = nint(df)
!
               WRITE (iu,99012) date , d , idd , s , ids , ih , idh , ix , idx , iy , idy , iz , idz , nf , idf
99012          FORMAT (1X,F6.1,F8.2,I5,F7.2,I4,I7,I6,3(I8,I6),I7,I6)
            ENDDO
            ifl = 2
         ELSE
!
            CALL igrf12syn(0,date,itype,alt,clt,xln,x,y,z,f)
            d = fact*atan2(y,x)
            h = sqrt(x*x+y*y)
            s = fact*atan2(z,h)
            CALL ddecdm(d,idec,idecm)
            CALL ddecdm(s,inc,incm)
!
            CALL igrf12syn(1,date,itype,alt,clt,xln,dx,dy,dz,f1)
            dd = (60.0*fact*(x*dy-y*dx))/(h*h)
            dh = (x*dx+y*dy)/h
            ds = (60.0*fact*(h*dz-z*dh))/(f*f)
            df = (h*dh+z*dz)/f
!
            IF ( idm==1 ) THEN
               WRITE (iu,99013) date , ltd , ltm , type , lnd , lnm , alt , name
99013          FORMAT (1X,F8.3,' Lat',2I4,A11,' Long ',2I4,F10.3,' km ',A20)
            ELSE
               WRITE (iu,99014) date , xlt , type , xln , alt , name
99014          FORMAT (1X,F8.3,' Lat',F8.3,A11,' Long ',F8.3,F10.3,' km ',A20)
            ENDIF
!
            idd = nint(dd)
            WRITE (iu,99015) idec , idecm , idd
99015       FORMAT (15X,'D =',I5,' deg',I4,' min',4X,'SV =',I8,' min/yr')
!
            ids = nint(ds)
            WRITE (iu,99016) inc , incm , ids
99016       FORMAT (15X,'I =',I5,' deg',I4,' min',4X,'SV =',I8,' min/yr')
!
            ih = nint(h)
            idh = nint(dh)
            WRITE (iu,99017) ih , idh
99017       FORMAT (15X,'H =',I8,' nT     ',5X,'SV =',I8,' nT/yr')
!
            ix = nint(x)
            idx = nint(dx)
            WRITE (iu,99018) ix , idx
99018       FORMAT (15X,'X =',I8,' nT     ',5X,'SV =',I8,' nT/yr')
!
            iy = nint(y)
            idy = nint(dy)
            WRITE (iu,99019) iy , idy
99019       FORMAT (15X,'Y =',I8,' nT     ',5X,'SV =',I8,' nT/yr')
!
            iz = nint(z)
            idz = nint(dz)
            WRITE (iu,99020) iz , idz
99020       FORMAT (15X,'Z =',I8,' nT     ',5X,'SV =',I8,' nT/yr')
!
            nf = nint(f)
            idf = nint(df)
            WRITE (iu,99021) nf , idf
!
99021       FORMAT (15X,'F =',I8,' nT     ',5X,'SV =',I8,' nT/yr'/)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         IF ( ifl==0 .OR. ifl==2 ) THEN
            WRITE (iu,99022)
99022       FORMAT (/' D is declination in degrees (+ve east)'/' I is inclination in degrees (+ve down)'/                          &
                   &' H is horizontal intensity in nT'/' X is north component in nT'/' Y is east component in nT'/                 &
                   &' Z is vertical component in nT (+ve down)'/' F is total intensity in nT')
            IF ( ifl/=0 ) WRITE (iu,99023)
99023       FORMAT (' SV is secular variation (annual rate of change)'/' Units for SV: minutes/yr (D & I); nT/yr (H,X,Y,Z & F)')
            IF ( itype==2 ) WRITE (iu,*) 'These elements are relative to the geocentric coordinate system'
         ELSE
            WRITE (iu,99024)
99024       FORMAT (/' D is SV in declination in minutes/yr (+ve east)'/' I is SV in inclination in minutes/yr (+ve down)'/        &
                   &' H is SV in horizontal intensity in nT/yr'/' X is SV in north component in nT/yr'/                            &
                   &' Y is SV in east component in nT/yr'/' Z is SV in vertical component in nT/yr (+ve down)'/                    &
                   &' F is SV in total intensity in nT/yr')
            IF ( itype==2 ) WRITE (iu,*) 'These elements are relative to the geocentric coordinate system'
         ENDIF
         STOP
      CASE (5)
!
         WRITE (6,99025) date
99025    FORMAT (' ***** Error *****'/' DATE =',F9.3,' - out of range')
         STOP
      CASE (6)
!
         WRITE (6,99026) alt , itype
99026    FORMAT (' ***** Error *****'/' A value of ALT =',F10.3,' is not allowed when ITYPE =',I2)
         STOP
      CASE (7)
!
         WRITE (6,99027) xlt
99027    FORMAT (' ***** Error *****'/' XLT =',F9.3,' - out of range')
         STOP
      CASE (8)
!
         WRITE (6,99028) ltd , ltm
99028    FORMAT (' ***** Error *****'/' Latitude out of range',' - LTD =',I6,5X,'LTM =',I4)
         STOP
      CASE (9)
!
         WRITE (6,99029) lnd , lnm
99029    FORMAT (' ***** Error *****'/' Longitude out of range',' - LND =',I8,5X,'LNM =',I4)
         STOP
      CASE (10)
!
         WRITE (6,99030) lti , ltf
99030    FORMAT (' ***** Error *****'/' Latitude limits of table out of range - LTI =',I6,5X,' LTF =',I6)
         STOP
      CASE (11)
!
         WRITE (6,99031) lni , lnf
99031    FORMAT (' ***** Error *****'/' Longitude limits of table out of range - LNI =',I8,5X,' LNF =',I8)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99003 FORMAT (2F9.3,2F8.2,5I8)
!
END PROGRAM igrf12
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
!*==igrf12syn.f90 processed by SPAG 8.01MH 15:55 30 Jan 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE igrf12syn(Isv,Date,Itype,Alt,Colat,Elong,X,Y,Z,F)
!
!     This is a synthesis routine for the 12th generation IGRF as agreed
!     in December 2014 by IAGA Working Group V-MOD. It is valid 1900.0 to
!     2020.0 inclusive. Values for dates from 1945.0 to 2010.0 inclusive are
!     definitive, otherwise they are non-definitive.
!   INPUT
!     isv   = 0 if main-field values are required
!     isv   = 1 if secular variation values are required
!     date  = year A.D. Must be greater than or equal to 1900.0 and
!             less than or equal to 2025.0. Warning message is given
!             for dates greater than 2020.0. Must be double precision.
!     itype = 1 if geodetic (spheroid)
!     itype = 2 if geocentric (sphere)
!     alt   = height in km above sea level if itype = 1
!           = distance from centre of Earth in km if itype = 2 (>3485 km)
!     colat = colatitude (0-180)
!     elong = east-longitude (0-360)
!     alt, colat and elong must be double precision.
!   OUTPUT
!     x     = north component (nT) if isv = 0, nT/year if isv = 1
!     y     = east component (nT) if isv = 0, nT/year if isv = 1
!     z     = vertical component (nT) if isv = 0, nT/year if isv = 1
!     f     = total intensity (nT) if isv = 0, rubbish if isv = 1
!
!     To get the other geomagnetic elements (D, I, H and secular
!     variations dD, dH, dI and dF) use routines ptoc and ptocsv.
!
!     Adapted from 8th generation version to include new maximum degree for
!     main-field models for 2000.0 and onwards and use WGS84 spheroid instead
!     of International Astronomical Union 1966 spheroid as recommended by IAGA
!     in July 2003. Reference radius remains as 6371.2 km - it is NOT the mean
!     radius (= 6371.0 km) but 6371.2 km is what is used in determining the
!     coefficients. Adaptation by Susan Macmillan, August 2003 (for
!     9th generation), December 2004, December 2009 & December 2014.
!
!     Coefficients at 1995.0 incorrectly rounded (rounded up instead of
!     to even) included as these are the coefficients published in Excel
!     spreadsheet July 2005.
!
   IMPLICIT NONE
!*** Start of declarations inserted by SPAG
   DOUBLE PRECISION a2 , Alt , b2 , cd , cl , Colat , ct , Date , Elong , F , fm , fn , g0 , g1 , g2 , g3 , g4 , g5 , g6 , g7
   DOUBLE PRECISION g8 , g9 , ga , gb , gc , gd , ge , gf , gg , gh , gi , gj , gk , gl , gm , gmm , gn , gp , gq , gr
   INTEGER i , Isv , Itype , j , k , kmx , l , ll , lm , m , n , nc , nmx
   DOUBLE PRECISION one , p , q , r , ratio , rho , rr , sd , sl , st , t , tc , three , two , X , Y , Z
!*** End of declarations inserted by SPAG
   DIMENSION gh(3451) , g0(120) , g1(120) , g2(120) , g3(120) , g4(120) , g5(120) , g6(120) , g7(120) , g8(120) , g9(120) , ga(120)&
           & , gb(120) , gc(120) , gd(120) , ge(120) , gf(120) , gg(120) , gi(120) , gj(120) , gk(195) , gl(195) , gm(195) ,       &
           & gp(195) , gq(195) , gr(195) , p(105) , q(105) , cl(13) , sl(13)
   EQUIVALENCE (g0,gh(1)) , (g1,gh(121)) , (g2,gh(241)) , (g3,gh(361)) , (g4,gh(481)) , (g5,gh(601)) , (g6,gh(721)) , (g7,gh(841)) &
    & , (g8,gh(961)) , (g9,gh(1081)) , (ga,gh(1201)) , (gb,gh(1321)) , (gc,gh(1441)) , (gd,gh(1561)) , (ge,gh(1681)) ,             &
    & (gf,gh(1801)) , (gg,gh(1921)) , (gi,gh(2041)) , (gj,gh(2161)) , (gk,gh(2281)) , (gl,gh(2476)) , (gm,gh(2671)) , (gp,gh(2866))&
    & , (gq,gh(3061)) , (gr,gh(3256))
!
   DATA g0/ - 31543. , -2298. , 5922. , -677. , 2905. , -1061. , 924. , 1121. , 1022. , -1469. , -330. , 1256. , 3. , 572. , 523. ,&
      & 876. , 628. , 195. , 660. , -69. , -361. , -210. , 134. , -75. , -184. , 328. , -210. , 264. , 53. , 5. , -33. , -86. ,    &
      & -124. , -16. , 3. , 63. , 61. , -9. , -11. , 83. , -217. , 2. , -58. , -35. , 59. , 36. , -90. , -69. , 70. , -55. , -45. ,&
      & 0. , -13. , 34. , -10. , -41. , -1. , -21. , 28. , 18. , -12. , 6. , -22. , 11. , 8. , 8. , -4. , -14. , -9. , 7. , 1. ,   &
      & -13. , 2. , 5. , -9. , 16. , 5. , -5. , 8. , -18. , 8. , 10. , -20. , 1. , 14. , -11. , 5. , 12. , -3. , 1. , -2. , -2. ,  &
      & 8. , 2. , 10. , -1. , -2. , -1. , 2. , -3. , -4. , 2. , 2. , 1. , -5. , 2. , -2. , 6. , 6. , -4. , 4. , 0. , 0. , -2. ,    &
      & 2. , 4. , 2. , 0. , 0. , -6./
   DATA g1/ - 31464. , -2298. , 5909. , -728. , 2928. , -1086. , 1041. , 1065. , 1037. , -1494. , -357. , 1239. , 34. , 635. ,     &
      & 480. , 880. , 643. , 203. , 653. , -77. , -380. , -201. , 146. , -65. , -192. , 328. , -193. , 259. , 56. , -1. , -32. ,   &
      & -93. , -125. , -26. , 11. , 62. , 60. , -7. , -11. , 86. , -221. , 4. , -57. , -32. , 57. , 32. , -92. , -67. , 70. ,      &
      & -54. , -46. , 0. , -14. , 33. , -11. , -41. , 0. , -20. , 28. , 18. , -12. , 6. , -22. , 11. , 8. , 8. , -4. , -15. , -9. ,&
      & 7. , 1. , -13. , 2. , 5. , -8. , 16. , 5. , -5. , 8. , -18. , 8. , 10. , -20. , 1. , 14. , -11. , 5. , 12. , -3. , 1. ,    &
      & -2. , -2. , 8. , 2. , 10. , 0. , -2. , -1. , 2. , -3. , -4. , 2. , 2. , 1. , -5. , 2. , -2. , 6. , 6. , -4. , 4. , 0. ,    &
      & 0. , -2. , 2. , 4. , 2. , 0. , 0. , -6./
   DATA g2/ - 31354. , -2297. , 5898. , -769. , 2948. , -1128. , 1176. , 1000. , 1058. , -1524. , -389. , 1223. , 62. , 705. ,     &
      & 425. , 884. , 660. , 211. , 644. , -90. , -400. , -189. , 160. , -55. , -201. , 327. , -172. , 253. , 57. , -9. , -33. ,   &
      & -102. , -126. , -38. , 21. , 62. , 58. , -5. , -11. , 89. , -224. , 5. , -54. , -29. , 54. , 28. , -95. , -65. , 71. ,     &
      & -54. , -47. , 1. , -14. , 32. , -12. , -40. , 1. , -19. , 28. , 18. , -13. , 6. , -22. , 11. , 8. , 8. , -4. , -15. , -9. ,&
      & 6. , 1. , -13. , 2. , 5. , -8. , 16. , 5. , -5. , 8. , -18. , 8. , 10. , -20. , 1. , 14. , -11. , 5. , 12. , -3. , 1. ,    &
      & -2. , -2. , 8. , 2. , 10. , 0. , -2. , -1. , 2. , -3. , -4. , 2. , 2. , 1. , -5. , 2. , -2. , 6. , 6. , -4. , 4. , 0. ,    &
      & 0. , -2. , 2. , 4. , 2. , 0. , 0. , -6./
   DATA g3/ - 31212. , -2306. , 5875. , -802. , 2956. , -1191. , 1309. , 917. , 1084. , -1559. , -421. , 1212. , 84. , 778. ,      &
      & 360. , 887. , 678. , 218. , 631. , -109. , -416. , -173. , 178. , -51. , -211. , 327. , -148. , 245. , 58. , -16. , -34. , &
      & -111. , -126. , -51. , 32. , 61. , 57. , -2. , -10. , 93. , -228. , 8. , -51. , -26. , 49. , 23. , -98. , -62. , 72. ,     &
      & -54. , -48. , 2. , -14. , 31. , -12. , -38. , 2. , -18. , 28. , 19. , -15. , 6. , -22. , 11. , 8. , 8. , -4. , -15. , -9. ,&
      & 6. , 2. , -13. , 3. , 5. , -8. , 16. , 6. , -5. , 8. , -18. , 8. , 10. , -20. , 1. , 14. , -11. , 5. , 12. , -3. , 1. ,    &
      & -2. , -2. , 8. , 2. , 10. , 0. , -2. , -1. , 2. , -3. , -4. , 2. , 2. , 1. , -5. , 2. , -2. , 6. , 6. , -4. , 4. , 0. ,    &
      & 0. , -2. , 1. , 4. , 2. , 0. , 0. , -6./
   DATA g4/ - 31060. , -2317. , 5845. , -839. , 2959. , -1259. , 1407. , 823. , 1111. , -1600. , -445. , 1205. , 103. , 839. ,     &
      & 293. , 889. , 695. , 220. , 616. , -134. , -424. , -153. , 199. , -57. , -221. , 326. , -122. , 236. , 58. , -23. , -38. , &
      & -119. , -125. , -62. , 43. , 61. , 55. , 0. , -10. , 96. , -233. , 11. , -46. , -22. , 44. , 18. , -101. , -57. , 73. ,    &
      & -54. , -49. , 2. , -14. , 29. , -13. , -37. , 4. , -16. , 28. , 19. , -16. , 6. , -22. , 11. , 7. , 8. , -3. , -15. , -9. ,&
      & 6. , 2. , -14. , 4. , 5. , -7. , 17. , 6. , -5. , 8. , -19. , 8. , 10. , -20. , 1. , 14. , -11. , 5. , 12. , -3. , 1. ,    &
      & -2. , -2. , 9. , 2. , 10. , 0. , -2. , -1. , 2. , -3. , -4. , 2. , 2. , 1. , -5. , 2. , -2. , 6. , 6. , -4. , 4. , 0. ,    &
      & 0. , -2. , 1. , 4. , 3. , 0. , 0. , -6./
   DATA g5/ - 30926. , -2318. , 5817. , -893. , 2969. , -1334. , 1471. , 728. , 1140. , -1645. , -462. , 1202. , 119. , 881. ,     &
      & 229. , 891. , 711. , 216. , 601. , -163. , -426. , -130. , 217. , -70. , -230. , 326. , -96. , 226. , 58. , -28. , -44. ,  &
      & -125. , -122. , -69. , 51. , 61. , 54. , 3. , -9. , 99. , -238. , 14. , -40. , -18. , 39. , 13. , -103. , -52. , 73. ,     &
      & -54. , -50. , 3. , -14. , 27. , -14. , -35. , 5. , -14. , 29. , 19. , -17. , 6. , -21. , 11. , 7. , 8. , -3. , -15. , -9. ,&
      & 6. , 2. , -14. , 4. , 5. , -7. , 17. , 7. , -5. , 8. , -19. , 8. , 10. , -20. , 1. , 14. , -11. , 5. , 12. , -3. , 1. ,    &
      & -2. , -2. , 9. , 2. , 10. , 0. , -2. , -1. , 2. , -3. , -4. , 2. , 2. , 1. , -5. , 2. , -2. , 6. , 6. , -4. , 4. , 0. ,    &
      & 0. , -2. , 1. , 4. , 3. , 0. , 0. , -6./
   DATA g6/ - 30805. , -2316. , 5808. , -951. , 2980. , -1424. , 1517. , 644. , 1172. , -1692. , -480. , 1205. , 133. , 907. ,     &
      & 166. , 896. , 727. , 205. , 584. , -195. , -422. , -109. , 234. , -90. , -237. , 327. , -72. , 218. , 60. , -32. , -53. ,  &
      & -131. , -118. , -74. , 58. , 60. , 53. , 4. , -9. , 102. , -242. , 19. , -32. , -16. , 32. , 8. , -104. , -46. , 74. ,     &
      & -54. , -51. , 4. , -15. , 25. , -14. , -34. , 6. , -12. , 29. , 18. , -18. , 6. , -20. , 11. , 7. , 8. , -3. , -15. , -9. ,&
      & 5. , 2. , -14. , 5. , 5. , -6. , 18. , 8. , -5. , 8. , -19. , 8. , 10. , -20. , 1. , 14. , -12. , 5. , 12. , -3. , 1. ,    &
      & -2. , -2. , 9. , 3. , 10. , 0. , -2. , -2. , 2. , -3. , -4. , 2. , 2. , 1. , -5. , 2. , -2. , 6. , 6. , -4. , 4. , 0. ,    &
      & 0. , -2. , 1. , 4. , 3. , 0. , 0. , -6./
   DATA g7/ - 30715. , -2306. , 5812. , -1018. , 2984. , -1520. , 1550. , 586. , 1206. , -1740. , -494. , 1215. , 146. , 918. ,    &
      & 101. , 903. , 744. , 188. , 565. , -226. , -415. , -90. , 249. , -114. , -241. , 329. , -51. , 211. , 64. , -33. , -64. ,  &
      & -136. , -115. , -76. , 64. , 59. , 53. , 4. , -8. , 104. , -246. , 25. , -25. , -15. , 25. , 4. , -106. , -40. , 74. ,     &
      & -53. , -52. , 4. , -17. , 23. , -14. , -33. , 7. , -11. , 29. , 18. , -19. , 6. , -19. , 11. , 7. , 8. , -3. , -15. , -9. ,&
      & 5. , 1. , -15. , 6. , 5. , -6. , 18. , 8. , -5. , 7. , -19. , 8. , 10. , -20. , 1. , 15. , -12. , 5. , 11. , -3. , 1. ,    &
      & -3. , -2. , 9. , 3. , 11. , 0. , -2. , -2. , 2. , -3. , -4. , 2. , 2. , 1. , -5. , 2. , -2. , 6. , 6. , -4. , 4. , 0. ,    &
      & 0. , -1. , 2. , 4. , 3. , 0. , 0. , -6./
   DATA g8/ - 30654. , -2292. , 5821. , -1106. , 2981. , -1614. , 1566. , 528. , 1240. , -1790. , -499. , 1232. , 163. , 916. ,    &
      & 43. , 914. , 762. , 169. , 550. , -252. , -405. , -72. , 265. , -141. , -241. , 334. , -33. , 208. , 71. , -33. , -75. ,   &
      & -141. , -113. , -76. , 69. , 57. , 54. , 4. , -7. , 105. , -249. , 33. , -18. , -15. , 18. , 0. , -107. , -33. , 74. ,     &
      & -53. , -52. , 4. , -18. , 20. , -14. , -31. , 7. , -9. , 29. , 17. , -20. , 5. , -19. , 11. , 7. , 8. , -3. , -14. , -10. ,&
      & 5. , 1. , -15. , 6. , 5. , -5. , 19. , 9. , -5. , 7. , -19. , 8. , 10. , -21. , 1. , 15. , -12. , 5. , 11. , -3. , 1. ,    &
      & -3. , -2. , 9. , 3. , 11. , 1. , -2. , -2. , 2. , -3. , -4. , 2. , 2. , 1. , -5. , 2. , -2. , 6. , 6. , -4. , 4. , 0. ,    &
      & 0. , -1. , 2. , 4. , 3. , 0. , 0. , -6./
   DATA g9/ - 30594. , -2285. , 5810. , -1244. , 2990. , -1702. , 1578. , 477. , 1282. , -1834. , -499. , 1255. , 186. , 913. ,    &
      & -11. , 944. , 776. , 144. , 544. , -276. , -421. , -55. , 304. , -178. , -253. , 346. , -12. , 194. , 95. , -20. , -67. ,  &
      & -142. , -119. , -82. , 82. , 59. , 57. , 6. , 6. , 100. , -246. , 16. , -25. , -9. , 21. , -16. , -104. , -39. , 70. ,     &
      & -40. , -45. , 0. , -18. , 0. , 2. , -29. , 6. , -10. , 28. , 15. , -17. , 29. , -22. , 13. , 7. , 12. , -8. , -21. , -5. , &
      & -12. , 9. , -7. , 7. , 2. , -10. , 18. , 7. , 3. , 2. , -11. , 5. , -21. , -27. , 1. , 17. , -11. , 29. , 3. , -9. , 16. , &
      & 4. , -3. , 9. , -4. , 6. , -3. , 1. , -4. , 8. , -3. , 11. , 5. , 1. , 1. , 2. , -20. , -5. , -1. , -1. , -6. , 8. , 6. ,  &
      & -1. , -4. , -3. , -2. , 5. , 0. , -2. , -2./
   DATA ga/ - 30554. , -2250. , 5815. , -1341. , 2998. , -1810. , 1576. , 381. , 1297. , -1889. , -476. , 1274. , 206. , 896. ,    &
      & -46. , 954. , 792. , 136. , 528. , -278. , -408. , -37. , 303. , -210. , -240. , 349. , 3. , 211. , 103. , -20. , -87. ,   &
      & -147. , -122. , -76. , 80. , 54. , 57. , -1. , 4. , 99. , -247. , 33. , -16. , -12. , 12. , -12. , -105. , -30. , 65. ,    &
      & -55. , -35. , 2. , -17. , 1. , 0. , -40. , 10. , -7. , 36. , 5. , -18. , 19. , -16. , 22. , 15. , 5. , -4. , -22. , -1. ,  &
      & 0. , 11. , -21. , 15. , -8. , -13. , 17. , 5. , -4. , -1. , -17. , 3. , -7. , -24. , -1. , 19. , -25. , 12. , 10. , 2. ,   &
      & 5. , 2. , -5. , 8. , -2. , 8. , 3. , -11. , 8. , -7. , -8. , 4. , 13. , -1. , -2. , 13. , -10. , -4. , 2. , 4. , -3. ,     &
      & 12. , 6. , 3. , -3. , 2. , 6. , 10. , 11. , 3. , 8./
   DATA gb/ - 30500. , -2215. , 5820. , -1440. , 3003. , -1898. , 1581. , 291. , 1302. , -1944. , -462. , 1288. , 216. , 882. ,    &
      & -83. , 958. , 796. , 133. , 510. , -274. , -397. , -23. , 290. , -230. , -229. , 360. , 15. , 230. , 110. , -23. , -98. ,  &
      & -152. , -121. , -69. , 78. , 47. , 57. , -9. , 3. , 96. , -247. , 48. , -8. , -16. , 7. , -12. , -107. , -24. , 65. ,      &
      & -56. , -50. , 2. , -24. , 10. , -4. , -32. , 8. , -11. , 28. , 9. , -20. , 18. , -18. , 11. , 9. , 10. , -6. , -15. ,      &
      & -14. , 5. , 6. , -23. , 10. , 3. , -7. , 23. , 6. , -4. , 9. , -13. , 4. , 9. , -11. , -4. , 12. , -5. , 7. , 2. , 6. ,    &
      & 4. , -2. , 1. , 10. , 2. , 7. , 2. , -6. , 5. , 5. , -3. , -5. , -4. , -1. , 0. , 2. , -8. , -3. , -2. , 7. , -4. , 4. ,   &
      & 1. , -2. , -3. , 6. , 7. , -2. , -1. , 0. , -3./
   DATA gc/ - 30421. , -2169. , 5791. , -1555. , 3002. , -1967. , 1590. , 206. , 1302. , -1992. , -414. , 1289. , 224. , 878. ,    &
      & -130. , 957. , 800. , 135. , 504. , -278. , -394. , 3. , 269. , -255. , -222. , 362. , 16. , 242. , 125. , -26. , -117. ,  &
      & -156. , -114. , -63. , 81. , 46. , 58. , -10. , 1. , 99. , -237. , 60. , -1. , -20. , -2. , -11. , -113. , -17. , 67. ,    &
      & -56. , -55. , 5. , -28. , 15. , -6. , -32. , 7. , -7. , 23. , 17. , -18. , 8. , -17. , 15. , 6. , 11. , -4. , -14. , -11. ,&
      & 7. , 2. , -18. , 10. , 4. , -5. , 23. , 10. , 1. , 8. , -20. , 4. , 6. , -18. , 0. , 12. , -9. , 2. , 1. , 0. , 4. , -3. , &
      & -1. , 9. , -2. , 8. , 3. , 0. , -1. , 5. , 1. , -3. , 4. , 4. , 1. , 0. , 0. , -1. , 2. , 4. , -5. , 6. , 1. , 1. , -1. ,  &
      & -1. , 6. , 2. , 0. , 0. , -7./
   DATA gd/ - 30334. , -2119. , 5776. , -1662. , 2997. , -2016. , 1594. , 114. , 1297. , -2038. , -404. , 1292. , 240. , 856. ,    &
      & -165. , 957. , 804. , 148. , 479. , -269. , -390. , 13. , 252. , -269. , -219. , 358. , 19. , 254. , 128. , -31. , -126. , &
      & -157. , -97. , -62. , 81. , 45. , 61. , -11. , 8. , 100. , -228. , 68. , 4. , -32. , 1. , -8. , -111. , -7. , 75. , -57. , &
      & -61. , 4. , -27. , 13. , -2. , -26. , 6. , -6. , 26. , 13. , -23. , 1. , -12. , 13. , 5. , 7. , -4. , -12. , -14. , 9. ,   &
      & 0. , -16. , 8. , 4. , -1. , 24. , 11. , -3. , 4. , -17. , 8. , 10. , -22. , 2. , 15. , -13. , 7. , 10. , -4. , -1. , -5. , &
      & -1. , 10. , 5. , 10. , 1. , -4. , -2. , 1. , -2. , -3. , 2. , 2. , 1. , -5. , 2. , -2. , 6. , 4. , -4. , 4. , 0. , 0. ,    &
      & -2. , 2. , 3. , 2. , 0. , 0. , -6./
   DATA ge/ - 30220. , -2068. , 5737. , -1781. , 3000. , -2047. , 1611. , 25. , 1287. , -2091. , -366. , 1278. , 251. , 838. ,     &
      & -196. , 952. , 800. , 167. , 461. , -266. , -395. , 26. , 234. , -279. , -216. , 359. , 26. , 262. , 139. , -42. , -139. , &
      & -160. , -91. , -56. , 83. , 43. , 64. , -12. , 15. , 100. , -212. , 72. , 2. , -37. , 3. , -6. , -112. , 1. , 72. , -57. , &
      & -70. , 1. , -27. , 14. , -4. , -22. , 8. , -2. , 23. , 13. , -23. , -2. , -11. , 14. , 6. , 7. , -2. , -15. , -13. , 6. ,  &
      & -3. , -17. , 5. , 6. , 0. , 21. , 11. , -6. , 3. , -16. , 8. , 10. , -21. , 2. , 16. , -12. , 6. , 10. , -4. , -1. , -5. , &
      & 0. , 10. , 3. , 11. , 1. , -2. , -1. , 1. , -3. , -3. , 1. , 2. , 1. , -5. , 3. , -1. , 4. , 6. , -4. , 4. , 0. , 1. ,     &
      & -1. , 0. , 3. , 3. , 1. , -1. , -4./
   DATA gf/ - 30100. , -2013. , 5675. , -1902. , 3010. , -2067. , 1632. , -68. , 1276. , -2144. , -333. , 1260. , 262. , 830. ,    &
      & -223. , 946. , 791. , 191. , 438. , -265. , -405. , 39. , 216. , -288. , -218. , 356. , 31. , 264. , 148. , -59. , -152. , &
      & -159. , -83. , -49. , 88. , 45. , 66. , -13. , 28. , 99. , -198. , 75. , 1. , -41. , 6. , -4. , -111. , 11. , 71. , -56. , &
      & -77. , 1. , -26. , 16. , -5. , -14. , 10. , 0. , 22. , 12. , -23. , -5. , -12. , 14. , 6. , 6. , -1. , -16. , -12. , 4. ,  &
      & -8. , -19. , 4. , 6. , 0. , 18. , 10. , -10. , 1. , -17. , 7. , 10. , -21. , 2. , 16. , -12. , 7. , 10. , -4. , -1. , -5. ,&
      & -1. , 10. , 4. , 11. , 1. , -3. , -2. , 1. , -3. , -3. , 1. , 2. , 1. , -5. , 3. , -2. , 4. , 5. , -4. , 4. , -1. , 1. ,   &
      & -1. , 0. , 3. , 3. , 1. , -1. , -5./
   DATA gg/ - 29992. , -1956. , 5604. , -1997. , 3027. , -2129. , 1663. , -200. , 1281. , -2180. , -336. , 1251. , 271. , 833. ,   &
      & -252. , 938. , 782. , 212. , 398. , -257. , -419. , 53. , 199. , -297. , -218. , 357. , 46. , 261. , 150. , -74. , -151. , &
      & -162. , -78. , -48. , 92. , 48. , 66. , -15. , 42. , 93. , -192. , 71. , 4. , -43. , 14. , -2. , -108. , 17. , 72. , -59. ,&
      & -82. , 2. , -27. , 21. , -5. , -12. , 16. , 1. , 18. , 11. , -23. , -2. , -10. , 18. , 6. , 7. , 0. , -18. , -11. , 4. ,   &
      & -7. , -22. , 4. , 9. , 3. , 16. , 6. , -13. , -1. , -15. , 5. , 10. , -21. , 1. , 16. , -12. , 9. , 9. , -5. , -3. , -6. , &
      & -1. , 9. , 7. , 10. , 2. , -6. , -5. , 2. , -4. , -4. , 1. , 2. , 0. , -5. , 3. , -2. , 6. , 5. , -4. , 3. , 0. , 1. ,     &
      & -1. , 2. , 4. , 3. , 0. , 0. , -6./
   DATA gi/ - 29873. , -1905. , 5500. , -2072. , 3044. , -2197. , 1687. , -306. , 1296. , -2208. , -310. , 1247. , 284. , 829. ,   &
      & -297. , 936. , 780. , 232. , 361. , -249. , -424. , 69. , 170. , -297. , -214. , 355. , 47. , 253. , 150. , -93. , -154. , &
      & -164. , -75. , -46. , 95. , 53. , 65. , -16. , 51. , 88. , -185. , 69. , 4. , -48. , 16. , -1. , -102. , 21. , 74. , -62. ,&
      & -83. , 3. , -27. , 24. , -2. , -6. , 20. , 4. , 17. , 10. , -23. , 0. , -7. , 21. , 6. , 8. , 0. , -19. , -11. , 5. , -9. ,&
      & -23. , 4. , 11. , 4. , 14. , 4. , -15. , -4. , -11. , 5. , 10. , -21. , 1. , 15. , -12. , 9. , 9. , -6. , -3. , -6. , -1. ,&
      & 9. , 7. , 9. , 1. , -7. , -5. , 2. , -4. , -4. , 1. , 3. , 0. , -5. , 3. , -2. , 6. , 5. , -4. , 3. , 0. , 1. , -1. , 2. , &
      & 4. , 3. , 0. , 0. , -6./
   DATA gj/ - 29775. , -1848. , 5406. , -2131. , 3059. , -2279. , 1686. , -373. , 1314. , -2239. , -284. , 1248. , 293. , 802. ,   &
      & -352. , 939. , 780. , 247. , 325. , -240. , -423. , 84. , 141. , -299. , -214. , 353. , 46. , 245. , 154. , -109. , -153. ,&
      & -165. , -69. , -36. , 97. , 61. , 65. , -16. , 59. , 82. , -178. , 69. , 3. , -52. , 18. , 1. , -96. , 24. , 77. , -64. ,  &
      & -80. , 2. , -26. , 26. , 0. , -1. , 21. , 5. , 17. , 9. , -23. , 0. , -4. , 23. , 5. , 10. , -1. , -19. , -10. , 6. ,      &
      & -12. , -22. , 3. , 12. , 4. , 12. , 2. , -16. , -6. , -10. , 4. , 9. , -20. , 1. , 15. , -12. , 11. , 9. , -7. , -4. ,     &
      & -7. , -2. , 9. , 7. , 8. , 1. , -7. , -6. , 2. , -3. , -4. , 2. , 2. , 1. , -5. , 3. , -2. , 6. , 4. , -4. , 3. , 0. , 1. ,&
      & -2. , 3. , 3. , 3. , -1. , 0. , -6./
   DATA gk/ - 29692. , -1784. , 5306. , -2200. , 3070. , -2366. , 1681. , -413. , 1335. , -2267. , -262. , 1249. , 302. , 759. ,   &
      & -427. , 940. , 780. , 262. , 290. , -236. , -418. , 97. , 122. , -306. , -214. , 352. , 46. , 235. , 165. , -118. , -143. ,&
      & -166. , -55. , -17. , 107. , 68. , 67. , -17. , 68. , 72. , -170. , 67. , -1. , -58. , 19. , 1. , -93. , 36. , 77. , -72. ,&
      & -69. , 1. , -25. , 28. , 4. , 5. , 24. , 4. , 17. , 8. , -24. , -2. , -6. , 25. , 6. , 11. , -6. , -21. , -9. , 8. , -14. ,&
      & -23. , 9. , 15. , 6. , 11. , -5. , -16. , -7. , -4. , 4. , 9. , -20. , 3. , 15. , -10. , 12. , 8. , -6. , -8. , -8. , -1. ,&
      & 8. , 10. , 5. , -2. , -8. , -8. , 3. , -3. , -6. , 1. , 2. , 0. , -4. , 4. , -1. , 5. , 4. , -5. , 2. , -1. , 2. , -2. ,   &
      & 5. , 1. , 1. , -2. , 0. , -7. , 75*0./
   DATA gl/ - 29619.4 , -1728.2 , 5186.1 , -2267.7 , 3068.4 , -2481.6 , 1670.9 , -458.0 , 1339.6 , -2288.0 , -227.6 , 1252.1 ,     &
      & 293.4 , 714.5 , -491.1 , 932.3 , 786.8 , 272.6 , 250.0 , -231.9 , -403.0 , 119.8 , 111.3 , -303.8 , -218.8 , 351.4 , 43.8 ,&
      & 222.3 , 171.9 , -130.4 , -133.1 , -168.6 , -39.3 , -12.9 , 106.3 , 72.3 , 68.2 , -17.4 , 74.2 , 63.7 , -160.9 , 65.1 ,     &
      & -5.9 , -61.2 , 16.9 , 0.7 , -90.4 , 43.8 , 79.0 , -74.0 , -64.6 , 0.0 , -24.2 , 33.3 , 6.2 , 9.1 , 24.0 , 6.9 , 14.8 ,     &
      & 7.3 , -25.4 , -1.2 , -5.8 , 24.4 , 6.6 , 11.9 , -9.2 , -21.5 , -7.9 , 8.5 , -16.6 , -21.5 , 9.1 , 15.5 , 7.0 , 8.9 , -7.9 ,&
      & -14.9 , -7.0 , -2.1 , 5.0 , 9.4 , -19.7 , 3.0 , 13.4 , -8.4 , 12.5 , 6.3 , -6.2 , -8.9 , -8.4 , -1.5 , 8.4 , 9.3 , 3.8 ,   &
      & -4.3 , -8.2 , -8.2 , 4.8 , -2.6 , -6.0 , 1.7 , 1.7 , 0.0 , -3.1 , 4.0 , -0.5 , 4.9 , 3.7 , -5.9 , 1.0 , -1.2 , 2.0 , -2.9 ,&
      & 4.2 , 0.2 , 0.3 , -2.2 , -1.1 , -7.4 , 2.7 , -1.7 , 0.1 , -1.9 , 1.3 , 1.5 , -0.9 , -0.1 , -2.6 , 0.1 , 0.9 , -0.7 , -0.7 ,&
      & 0.7 , -2.8 , 1.7 , -0.9 , 0.1 , -1.2 , 1.2 , -1.9 , 4.0 , -0.9 , -2.2 , -0.3 , -0.4 , 0.2 , 0.3 , 0.9 , 2.5 , -0.2 , -2.6 ,&
      & 0.9 , 0.7 , -0.5 , 0.3 , 0.3 , 0.0 , -0.3 , 0.0 , -0.4 , 0.3 , -0.1 , -0.9 , -0.2 , -0.4 , -0.4 , 0.8 , -0.2 , -0.9 ,      &
      & -0.9 , 0.3 , 0.2 , 0.1 , 1.8 , -0.4 , -0.4 , 1.3 , -1.0 , -0.4 , -0.1 , 0.7 , 0.7 , -0.4 , 0.3 , 0.3 , 0.6 , -0.1 , 0.3 ,  &
      & 0.4 , -0.2 , 0.0 , -0.5 , 0.1 , -0.9/
   DATA gm/ - 29554.63 , -1669.05 , 5077.99 , -2337.24 , 3047.69 , -2594.50 , 1657.76 , -515.43 , 1336.30 , -2305.83 , -198.86 ,   &
      & 1246.39 , 269.72 , 672.51 , -524.72 , 920.55 , 797.96 , 282.07 , 210.65 , -225.23 , -379.86 , 145.15 , 100.00 , -305.36 ,  &
      & -227.00 , 354.41 , 42.72 , 208.95 , 180.25 , -136.54 , -123.45 , -168.05 , -19.57 , -13.55 , 103.85 , 73.60 , 69.56 ,      &
      & -20.33 , 76.74 , 54.75 , -151.34 , 63.63 , -14.58 , -63.53 , 14.58 , 0.24 , -86.36 , 50.94 , 79.88 , -74.46 , -61.14 ,     &
      & -1.65 , -22.57 , 38.73 , 6.82 , 12.30 , 25.35 , 9.37 , 10.93 , 5.42 , -26.32 , 1.94 , -4.64 , 24.80 , 7.62 , 11.20 ,       &
      & -11.73 , -20.88 , -6.88 , 9.83 , -18.11 , -19.71 , 10.17 , 16.22 , 9.36 , 7.61 , -11.25 , -12.76 , -4.87 , -0.06 , 5.58 ,  &
      & 9.76 , -20.11 , 3.58 , 12.69 , -6.94 , 12.67 , 5.01 , -6.72 , -10.76 , -8.16 , -1.25 , 8.10 , 8.76 , 2.92 , -6.66 , -7.73 ,&
      & -9.22 , 6.01 , -2.17 , -6.12 , 2.19 , 1.42 , 0.10 , -2.35 , 4.46 , -0.15 , 4.76 , 3.06 , -6.58 , 0.29 , -1.01 , 2.06 ,     &
      & -3.47 , 3.77 , -0.86 , -0.21 , -2.31 , -2.09 , -7.93 , 2.95 , -1.60 , 0.26 , -1.88 , 1.44 , 1.44 , -0.77 , -0.31 , -2.27 , &
      & 0.29 , 0.90 , -0.79 , -0.58 , 0.53 , -2.69 , 1.80 , -1.08 , 0.16 , -1.58 , 0.96 , -1.90 , 3.99 , -1.39 , -2.15 , -0.29 ,   &
      & -0.55 , 0.21 , 0.23 , 0.89 , 2.38 , -0.38 , -2.63 , 0.96 , 0.61 , -0.30 , 0.40 , 0.46 , 0.01 , -0.35 , 0.02 , -0.36 ,      &
      & 0.28 , 0.08 , -0.87 , -0.49 , -0.34 , -0.08 , 0.88 , -0.16 , -0.88 , -0.76 , 0.30 , 0.33 , 0.28 , 1.72 , -0.43 , -0.54 ,   &
      & 1.18 , -1.07 , -0.37 , -0.04 , 0.75 , 0.63 , -0.26 , 0.21 , 0.35 , 0.53 , -0.05 , 0.38 , 0.41 , -0.22 , -0.10 , -0.57 ,    &
      & -0.18 , -0.82/
   DATA gp/ - 29496.57 , -1586.42 , 4944.26 , -2396.06 , 3026.34 , -2708.54 , 1668.17 , -575.73 , 1339.85 , -2326.54 , -160.40 ,   &
      & 1232.10 , 251.75 , 633.73 , -537.03 , 912.66 , 808.97 , 286.48 , 166.58 , -211.03 , -356.83 , 164.46 , 89.40 , -309.72 ,   &
      & -230.87 , 357.29 , 44.58 , 200.26 , 189.01 , -141.05 , -118.06 , -163.17 , -0.01 , -8.03 , 101.04 , 72.78 , 68.69 ,        &
      & -20.90 , 75.92 , 44.18 , -141.40 , 61.54 , -22.83 , -66.26 , 13.10 , 3.02 , -78.09 , 55.40 , 80.44 , -75.00 , -57.80 ,     &
      & -4.55 , -21.20 , 45.24 , 6.54 , 14.00 , 24.96 , 10.46 , 7.03 , 1.64 , -27.61 , 4.92 , -3.28 , 24.41 , 8.21 , 10.84 ,       &
      & -14.50 , -20.03 , -5.59 , 11.83 , -19.34 , -17.41 , 11.61 , 16.71 , 10.85 , 6.96 , -14.05 , -10.74 , -3.54 , 1.64 , 5.50 , &
      & 9.45 , -20.54 , 3.45 , 11.51 , -5.27 , 12.75 , 3.13 , -7.14 , -12.38 , -7.42 , -0.76 , 7.97 , 8.43 , 2.14 , -8.42 , -6.08 ,&
      & -10.08 , 7.01 , -1.94 , -6.24 , 2.73 , 0.89 , -0.10 , -1.07 , 4.71 , -0.16 , 4.44 , 2.45 , -7.22 , -0.33 , -0.96 , 2.13 ,  &
      & -3.95 , 3.09 , -1.99 , -1.03 , -1.97 , -2.80 , -8.31 , 3.05 , -1.48 , 0.13 , -2.03 , 1.67 , 1.65 , -0.66 , -0.51 , -1.76 , &
      & 0.54 , 0.85 , -0.79 , -0.39 , 0.37 , -2.51 , 1.79 , -1.27 , 0.12 , -2.11 , 0.75 , -1.94 , 3.75 , -1.86 , -2.12 , -0.21 ,   &
      & -0.87 , 0.30 , 0.27 , 1.04 , 2.13 , -0.63 , -2.49 , 0.95 , 0.49 , -0.11 , 0.59 , 0.52 , 0.00 , -0.39 , 0.13 , -0.37 ,      &
      & 0.27 , 0.21 , -0.86 , -0.77 , -0.23 , 0.04 , 0.87 , -0.09 , -0.89 , -0.87 , 0.31 , 0.30 , 0.42 , 1.66 , -0.45 , -0.59 ,    &
      & 1.08 , -1.14 , -0.31 , -0.07 , 0.78 , 0.54 , -0.18 , 0.10 , 0.38 , 0.49 , 0.02 , 0.44 , 0.42 , -0.25 , -0.26 , -0.53 ,     &
      & -0.26 , -0.79/
   DATA gq/ - 29442.0 , -1501.0 , 4797.1 , -2445.1 , 3012.9 , -2845.6 , 1676.7 , -641.9 , 1350.7 , -2352.3 , -115.3 , 1225.6 ,     &
      & 244.9 , 582.0 , -538.4 , 907.6 , 813.7 , 283.3 , 120.4 , -188.7 , -334.9 , 180.9 , 70.4 , -329.5 , -232.6 , 360.1 , 47.3 , &
      & 192.4 , 197.0 , -140.9 , -119.3 , -157.5 , 16.0 , 4.1 , 100.2 , 70.0 , 67.7 , -20.8 , 72.7 , 33.2 , -129.9 , 58.9 , -28.9 ,&
      & -66.7 , 13.2 , 7.3 , -70.9 , 62.6 , 81.6 , -76.1 , -54.1 , -6.8 , -19.5 , 51.8 , 5.7 , 15.0 , 24.4 , 9.4 , 3.4 , -2.8 ,    &
      & -27.4 , 6.8 , -2.2 , 24.2 , 8.8 , 10.1 , -16.9 , -18.3 , -3.2 , 13.3 , -20.6 , -14.6 , 13.4 , 16.2 , 11.7 , 5.7 , -15.9 ,  &
      & -9.1 , -2.0 , 2.1 , 5.4 , 8.8 , -21.6 , 3.1 , 10.8 , -3.3 , 11.8 , 0.7 , -6.8 , -13.3 , -6.9 , -0.1 , 7.8 , 8.7 , 1.0 ,    &
      & -9.1 , -4.0 , -10.5 , 8.4 , -1.9 , -6.3 , 3.2 , 0.1 , -0.4 , 0.5 , 4.6 , -0.5 , 4.4 , 1.8 , -7.9 , -0.7 , -0.6 , 2.1 ,     &
      & -4.2 , 2.4 , -2.8 , -1.8 , -1.2 , -3.6 , -8.7 , 3.1 , -1.5 , -0.1 , -2.3 , 2.0 , 2.0 , -0.7 , -0.8 , -1.1 , 0.6 , 0.8 ,    &
      & -0.7 , -0.2 , 0.2 , -2.2 , 1.7 , -1.4 , -0.2 , -2.5 , 0.4 , -2.0 , 3.5 , -2.4 , -1.9 , -0.2 , -1.1 , 0.4 , 0.4 , 1.2 ,     &
      & 1.9 , -0.8 , -2.2 , 0.9 , 0.3 , 0.1 , 0.7 , 0.5 , -0.1 , -0.3 , 0.3 , -0.4 , 0.2 , 0.2 , -0.9 , -0.9 , -0.1 , 0.0 , 0.7 ,  &
      & 0.0 , -0.9 , -0.9 , 0.4 , 0.4 , 0.5 , 1.6 , -0.5 , -0.5 , 1.0 , -1.2 , -0.2 , -0.1 , 0.8 , 0.4 , -0.1 , -0.1 , 0.3 , 0.4 , &
      & 0.1 , 0.5 , 0.5 , -0.3 , -0.4 , -0.4 , -0.3 , -0.8/
   DATA gr/10.3 , 18.1 , -26.6 , -8.7 , -3.3 , -27.4 , 2.1 , -14.1 , 3.4 , -5.5 , 8.2 , -0.7 , -0.4 , -10.1 , 1.8 , -0.7 , 0.2 ,   &
      & -1.3 , -9.1 , 5.3 , 4.1 , 2.9 , -4.3 , -5.2 , -0.2 , 0.5 , 0.6 , -1.3 , 1.7 , -0.1 , -1.2 , 1.4 , 3.4 , 3.9 , 0.0 , -0.3 , &
      & -0.1 , 0.0 , -0.7 , -2.1 , 2.1 , -0.7 , -1.2 , 0.2 , 0.3 , 0.9 , 1.6 , 1.0 , 0.3 , -0.2 , 0.8 , -0.5 , 0.4 , 1.3 , -0.2 ,  &
      & 0.1 , -0.3 , -0.6 , -0.6 , -0.8 , 0.1 , 0.2 , -0.2 , 0.2 , 0.0 , -0.3 , -0.6 , 0.3 , 0.5 , 0.1 , -0.2 , 0.5 , 0.4 , -0.2 , &
      & 0.1 , -0.3 , -0.4 , 0.3 , 0.3 , 0.0 , 115*0.0/
 
!
!     set initial values
!
   X = 0.0
   Y = 0.0
   Z = 0.0
   IF ( Date<1900.0 .OR. Date>2025.0 ) THEN
!
!     error return if date out of bounds
!
      F = 1.0D8
      WRITE (6,99001) Date
99001 FORMAT (/' This subroutine will not work with a date of',f9.3,'.  Date must be in the range 1900.0.ge.date',                 &
             &'.le.2025.0. On return f = 1.0d8., x = y = z = 0.')
      RETURN
   ELSE
      IF ( Date>2020.0 ) WRITE (6,99002) Date
99002 FORMAT (/' This version of the IGRF is intended for use up',' to 2020.0.'/' values for',f9.3,' will be computed',            &
             &' but may be of reduced accuracy'/)
      IF ( Date>=2015.0 ) THEN
!
         t = Date - 2015.0
         tc = 1.0
         IF ( Isv==1 ) THEN
            t = 1.0
            tc = 0.0
         ENDIF
!
!     pointer for last coefficient in pen-ultimate set of MF coefficients...
!
         ll = 3060
         nmx = 13
         nc = nmx*(nmx+2)
         kmx = (nmx+1)*(nmx+2)/2
      ELSE
         t = 0.2*(Date-1900.0)
         ll = t
         one = ll
         t = t - one
!
!     SH models before 1995.0 are only to degree 10
!
         IF ( Date<1995.0 ) THEN
            nmx = 10
            nc = nmx*(nmx+2)
            ll = nc*ll
            kmx = (nmx+1)*(nmx+2)/2
         ELSE
            nmx = 13
            nc = nmx*(nmx+2)
            ll = 0.2*(Date-1995.0)
!
!     19 is the number of SH models that extend to degree 10
!
            ll = 120*19 + nc*ll
            kmx = (nmx+1)*(nmx+2)/2
         ENDIF
         tc = 1.0 - t
         IF ( Isv==1 ) THEN
            tc = -0.2
            t = 0.2
         ENDIF
      ENDIF
      r = Alt
      one = Colat*0.017453292
      ct = cos(one)
      st = sin(one)
      one = Elong*0.017453292
      cl(1) = cos(one)
      sl(1) = sin(one)
      cd = 1.0
      sd = 0.0
      l = 1
      m = 1
      n = 0
      IF ( Itype/=2 ) THEN
!
!     conversion from geodetic to geocentric coordinates
!     (using the WGS84 spheroid)
!
         a2 = 40680631.6
         b2 = 40408296.0
         one = a2*st*st
         two = b2*ct*ct
         three = one + two
         rho = sqrt(three)
         r = sqrt(Alt*(Alt+2.0*rho)+(a2*one+b2*two)/three)
         cd = (Alt+rho)/r
         sd = (a2-b2)/rho*ct*st/r
         one = ct
         ct = ct*cd - st*sd
         st = st*cd + one*sd
      ENDIF
   ENDIF
!
   ratio = 6371.2/r
   rr = ratio*ratio
!
!     computation of Schmidt quasi-normal coefficients p and x(=q)
!
   p(1) = 1.0
   p(3) = st
   q(1) = 0.0
   q(3) = ct
   DO k = 2 , kmx
      IF ( n<m ) THEN
         m = 0
         n = n + 1
         rr = rr*ratio
         fn = n
         gn = n - 1
      ENDIF
      fm = m
      IF ( m/=n ) THEN
         gmm = m*m
         one = sqrt(fn*fn-gmm)
         two = sqrt(gn*gn-gmm)/one
         three = (fn+gn)/one
         i = k - n
         j = i - n + 1
         p(k) = three*ct*p(i) - two*p(j)
         q(k) = three*(ct*q(i)-st*p(i)) - two*q(j)
      ELSEIF ( k/=3 ) THEN
         one = sqrt(1.0-0.5/fm)
         j = k - n - 1
         p(k) = one*st*p(j)
         q(k) = one*(st*q(j)+ct*p(j))
         cl(m) = cl(m-1)*cl(1) - sl(m-1)*sl(1)
         sl(m) = sl(m-1)*cl(1) + cl(m-1)*sl(1)
      ENDIF
!
!     synthesis of x, y and z in geocentric coordinates
!
      lm = ll + l
      one = (tc*gh(lm)+t*gh(lm+nc))*rr
      IF ( m==0 ) THEN
         X = X + one*q(k)
         Z = Z - (fn+1.0)*one*p(k)
         l = l + 1
      ELSE
         two = (tc*gh(lm+1)+t*gh(lm+nc+1))*rr
         three = one*cl(m) + two*sl(m)
         X = X + three*q(k)
         Z = Z - (fn+1.0)*three*p(k)
         IF ( st==0.0 ) THEN
            Y = Y + (one*sl(m)-two*cl(m))*q(k)*ct
         ELSE
            Y = Y + (one*sl(m)-two*cl(m))*fm*p(k)/st
         ENDIF
         l = l + 2
      ENDIF
      m = m + 1
   ENDDO
!
!     conversion to coordinate system specified by itype
!
   one = X
   X = X*cd + Z*sd
   Z = Z*cd - one*sd
   F = sqrt(X*X+Y*Y+Z*Z)
!
   RETURN
END SUBROUTINE igrf12syn
