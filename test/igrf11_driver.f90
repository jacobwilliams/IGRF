!*==igrf11.f90 processed by SPAG 8.01MH 15:55 30 Jan 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
PROGRAM igrf11
!
!     This is a program for synthesising geomagnetic field values from the
!     International Geomagnetic Reference Field series of models as agreed
!     in December 2009 by IAGA Working Group V-MOD.
!     It is the 11th generation IGRF, ie the 10th revision.
!     The main-field models for 1900.0, 1905.0,..1940.0 and 2010.0 are
!     non-definitive, those for 1945.0, 1950.0,...2005.0 are definitive and
!     the secular-variation model for 2010.0 to 2015.0 is non-definitive.
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
   DATA dtmn , dtmx/1900.0 , 2020.0/
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
         WRITE (6,*) '* Reference Field (11th generation) as revised in    *'
         WRITE (6,*) '* December 2009 by the IAGA Working Group V-MOD.     *'
         WRITE (6,*) '*                                                    *'
         WRITE (6,*) '* It is valid for dates from 1900.0 to 2015.0,       *'
         WRITE (6,*) '* values up to 2020.0 will be computed but with      *'
         WRITE (6,*) '* reduced accuracy. Values for dates before 1945.0   *'
         WRITE (6,*) '* and after 2005.0 are non-definitive, otherwise the *'
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
                                    CALL igrf11syn(0,date,itype,alt,clt,xln,x,y,z,f)
                                    d = fact*atan2(y,x)
                                    h = sqrt(x*x+y*y)
                                    s = fact*atan2(z,h)
                                    ih = nint(h)
                                    ix = nint(x)
                                    iy = nint(y)
                                    iz = nint(z)
                                    nf = nint(f)
                                    IF ( ifl/=0 ) THEN
                                       CALL igrf11syn(1,date,itype,alt,clt,xln,dx,dy,dz,f1)
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
               CALL igrf11syn(0,date,itype,alt,clt,xln,x,y,z,f)
               d = fact*atan2(y,x)
               h = sqrt(x*x+y*y)
               s = fact*atan2(z,h)
               ih = nint(h)
               ix = nint(x)
               iy = nint(y)
               iz = nint(z)
               nf = nint(f)
!
               CALL igrf11syn(1,date,itype,alt,clt,xln,dx,dy,dz,f1)
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
            CALL igrf11syn(0,date,itype,alt,clt,xln,x,y,z,f)
            d = fact*atan2(y,x)
            h = sqrt(x*x+y*y)
            s = fact*atan2(z,h)
            CALL ddecdm(d,idec,idecm)
            CALL ddecdm(s,inc,incm)
!
            CALL igrf11syn(1,date,itype,alt,clt,xln,dx,dy,dz,f1)
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
END PROGRAM igrf11
