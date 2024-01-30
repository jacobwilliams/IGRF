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
!*==igrf11syn.f90 processed by SPAG 8.01MH 15:55 30 Jan 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE igrf11syn(Isv,Date,Itype,Alt,Colat,Elong,X,Y,Z,F)
!
!     This is a synthesis routine for the 11th generation IGRF as agreed
!     in December 2009 by IAGA Working Group V-MOD. It is valid 1900.0 to
!     2015.0 inclusive. Values for dates from 1945.0 to 2005.0 inclusive are
!     definitive, otherwise they are non-definitive.
!   INPUT
!     isv   = 0 if main-field values are required
!     isv   = 1 if secular variation values are required
!     date  = year A.D. Must be greater than or equal to 1900.0 and
!             less than or equal to 2020.0. Warning message is given
!             for dates greater than 2015.0. Must be double precision.
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
!     9th generation), December 2004 & December 2009.
!
!     Coefficients at 1995.0 incorrectly rounded (rounded up instead of
!     to even) included as these are the coefficients published in Excel
!     spreadsheet July 2005.
!
   IMPLICIT NONE
!*** Start of declarations inserted by SPAG
   DOUBLE PRECISION a2 , Alt , b2 , cd , cl , Colat , ct , Date , Elong , F , fm , fn , g0 , g1 , g2 , g3 , g4 , g5 , g6 , g7
   DOUBLE PRECISION g8 , g9 , ga , gb , gc , gd , ge , gf , gg , gh , gi , gj , gk , gl , gm , gmm , gn , gp , gq , one
   DOUBLE PRECISION p , q , r , ratio , rho , rr , sd , sl , st , t , tc , three , two , X , Y , Z
   INTEGER i , Isv , Itype , j , k , kmx , l , ll , lm , m , n , nc , nmx
!*** End of declarations inserted by SPAG
   DIMENSION gh(3256) , g0(120) , g1(120) , g2(120) , g3(120) , g4(120) , g5(120) , g6(120) , g7(120) , g8(120) , g9(120) , ga(120)&
           & , gb(120) , gc(120) , gd(120) , ge(120) , gf(120) , gg(120) , gi(120) , gj(120) , gk(195) , gl(195) , gm(195) ,       &
           & gp(195) , gq(195) , p(105) , q(105) , cl(13) , sl(13)
   EQUIVALENCE (g0,gh(1)) , (g1,gh(121)) , (g2,gh(241)) , (g3,gh(361)) , (g4,gh(481)) , (g5,gh(601)) , (g6,gh(721)) , (g7,gh(841)) &
    & , (g8,gh(961)) , (g9,gh(1081)) , (ga,gh(1201)) , (gb,gh(1321)) , (gc,gh(1441)) , (gd,gh(1561)) , (ge,gh(1681)) ,             &
    & (gf,gh(1801)) , (gg,gh(1921)) , (gi,gh(2041)) , (gj,gh(2161)) , (gk,gh(2281)) , (gl,gh(2476)) , (gm,gh(2671)) , (gp,gh(2866))&
    & , (gq,gh(3061))
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
   DATA gp/ - 29496.5 , -1585.9 , 4945.1 , -2396.6 , 3026.0 , -2707.7 , 1668.6 , -575.4 , 1339.7 , -2326.3 , -160.5 , 1231.7 ,     &
      & 251.7 , 634.2 , -536.8 , 912.6 , 809.0 , 286.4 , 166.6 , -211.2 , -357.1 , 164.4 , 89.7 , -309.2 , -231.1 , 357.2 , 44.7 , &
      & 200.3 , 188.9 , -141.2 , -118.1 , -163.1 , 0.1 , -7.7 , 100.9 , 72.8 , 68.6 , -20.8 , 76.0 , 44.2 , -141.4 , 61.5 , -22.9 ,&
      & -66.3 , 13.1 , 3.1 , -77.9 , 54.9 , 80.4 , -75.0 , -57.8 , -4.7 , -21.2 , 45.3 , 6.6 , 14.0 , 24.9 , 10.4 , 7.0 , 1.6 ,    &
      & -27.7 , 4.9 , -3.4 , 24.3 , 8.2 , 10.9 , -14.5 , -20.0 , -5.7 , 11.9 , -19.3 , -17.4 , 11.6 , 16.7 , 10.9 , 7.1 , -14.1 ,  &
      & -10.8 , -3.7 , 1.7 , 5.4 , 9.4 , -20.5 , 3.4 , 11.6 , -5.3 , 12.8 , 3.1 , -7.2 , -12.4 , -7.4 , -0.8 , 8.0 , 8.4 , 2.2 ,   &
      & -8.4 , -6.1 , -10.1 , 7.0 , -2.0 , -6.3 , 2.8 , 0.9 , -0.1 , -1.1 , 4.7 , -0.2 , 4.4 , 2.5 , -7.2 , -0.3 , -1.0 , 2.2 ,    &
      & -4.0 , 3.1 , -2.0 , -1.0 , -2.0 , -2.8 , -8.3 , 3.0 , -1.5 , 0.1 , -2.1 , 1.7 , 1.6 , -0.6 , -0.5 , -1.8 , 0.5 , 0.9 ,     &
      & -0.8 , -0.4 , 0.4 , -2.5 , 1.8 , -1.3 , 0.2 , -2.1 , 0.8 , -1.9 , 3.8 , -1.8 , -2.1 , -0.2 , -0.8 , 0.3 , 0.3 , 1.0 , 2.2 ,&
      & -0.7 , -2.5 , 0.9 , 0.5 , -0.1 , 0.6 , 0.5 , 0.0 , -0.4 , 0.1 , -0.4 , 0.3 , 0.2 , -0.9 , -0.8 , -0.2 , 0.0 , 0.8 , -0.2 , &
      & -0.9 , -0.8 , 0.3 , 0.3 , 0.4 , 1.7 , -0.4 , -0.6 , 1.1 , -1.2 , -0.3 , -0.1 , 0.8 , 0.5 , -0.2 , 0.1 , 0.4 , 0.5 , 0.0 ,  &
      & 0.4 , 0.4 , -0.2 , -0.3 , -0.5 , -0.3 , -0.8/
   DATA gq/11.4 , 16.7 , -28.8 , -11.3 , -3.9 , -23.0 , 2.7 , -12.9 , 1.3 , -3.9 , 8.6 , -2.9 , -2.9 , -8.1 , -2.1 , -1.4 , 2.0 ,  &
      & 0.4 , -8.9 , 3.2 , 4.4 , 3.6 , -2.3 , -0.8 , -0.5 , 0.5 , 0.5 , -1.5 , 1.5 , -0.7 , 0.9 , 1.3 , 3.7 , 1.4 , -0.6 , -0.3 ,  &
      & -0.3 , -0.1 , -0.3 , -2.1 , 1.9 , -0.4 , -1.6 , -0.5 , -0.2 , 0.8 , 1.8 , 0.5 , 0.2 , -0.1 , 0.6 , -0.6 , 0.3 , 1.4 ,      &
      & -0.2 , 0.3 , -0.1 , 0.1 , -0.8 , -0.8 , -0.3 , 0.4 , 0.2 , -0.1 , 0.1 , 0.0 , -0.5 , 0.2 , 0.3 , 0.5 , -0.3 , 0.4 , 0.3 ,  &
      & 0.1 , 0.2 , -0.1 , -0.5 , 0.4 , 0.2 , 0.4 , 115*0.0/
!
!     set initial values
!
   X = 0.0
   Y = 0.0
   Z = 0.0
   IF ( Date<1900.0 .OR. Date>2020.0 ) THEN
!
!     error return if date out of bounds
!
      F = 1.0D8
      WRITE (6,99001) Date
99001 FORMAT (/' This subroutine will not work with a date of',f9.3,'.  Date must be in the range 1900.0.ge.date',                 &
             &'.le.2020.0. On return f = 1.0d8., x = y = z = 0.')
      RETURN
   ELSE
      IF ( Date>2015.0 ) WRITE (6,99002) Date
99002 FORMAT (/' This version of the IGRF is intended for use up',' to 2015.0.'/' values for',f9.3,' will be computed',            &
             &' but may be of reduced accuracy'/)
      IF ( Date>=2010.0 ) THEN
!
         t = Date - 2010.0
         tc = 1.0
         IF ( Isv==1 ) THEN
            t = 1.0
            tc = 0.0
         ENDIF
!
!     pointer for last coefficient in pen-ultimate set of MF coefficients...
!
         ll = 2865
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
END SUBROUTINE igrf11syn
 
