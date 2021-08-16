FUNCTION UV_Absorption(WN)
! *** the function NAME is the same for any species ***
!#JPL 2011
!#lambda[nm] sigma [1e20 cm2]
CHARACTER UV_abs_name*100
!PARAMETER (NP=98,BD1=121.,BD2=198.)
!DIMENSION S(NP),BDA(NP)
PARAMETER (NPmax=24000)
REAL*4 BD1, BD2
REAL*4 S(NPmax), BDA(NPmax)

OPEN (57, FILE='Initial_UV.dat')
455 FORMAT (A100)
READ (57, 455) UV_abs_name
CLOSE(57)

OPEN (58, FILE=UV_abs_name)
READ (58, *)
READ (58, *) NP
DO I=1,NP
READ(58,*)BDA(I),S(I)
END DO
CLOSE(58)

!DATA BDA,S/&
!  121.0000,  121.5000,  121.5670,  122.0000,  122.5000,  123.0000,  123.5000,  124.0000,  124.5000,  125.0000,&
!  125.5000,  126.0000,  126.5000,  127.0000,  127.5000,  128.0000,  128.5000,  129.0000,  129.5000,  130.0000,&
!  130.5000,  131.0000,  131.5000,  132.0000,  132.5000,  133.0000,  133.5000,  134.0000,  134.5000,  135.0000,&
!  135.5000,  136.0000,  136.5000,  137.0000,  137.5000,  138.0000,  138.5000,  139.0000,  139.5000,  140.0000,&
!  141.0000,  142.0000,  143.0000,  144.0000,  145.0000,  146.0000,  147.0000,  148.0000,  149.0000,  150.0000,&
!  151.0000,  152.0000,  153.0000,  154.0000,  155.0000,  156.0000,  157.0000,  158.0000,  159.0000,  160.0000,&
!  161.0000,  162.0000,  163.0000,  164.0000,  165.0000,  166.0000,  167.0000,  168.0000,  169.0000,  170.0000,&
!  171.0000,  172.0000,  173.0000,  174.0000,  175.0000,  176.0000,  177.0000,  178.0000,  179.0000,  180.0000,&
!  181.0000,  182.0000,  183.0000,  184.0000,  185.0000,  186.0000,  187.0000,  188.0000,  199.0000,  190.0000,&
!  191.0000,  192.0000,  193.0000,  194.0000,  195.0000,  196.0000,  197.0000,  198.0000,&
!0.6240E+03,0.1276E+04,0.1480E+04,0.1689E+04,0.8260E+03,0.2830E+03,0.5890E+03,0.1332E+04,0.6630E+03,0.6190E+03,&
!0.6930E+03,0.7060E+03,0.7680E+03,0.8000E+03,0.7800E+03,0.8540E+03,0.8200E+03,0.7770E+03,0.8190E+03,0.7180E+03,&
!0.7330E+03,0.6990E+03,0.6010E+03,0.6670E+03,0.5380E+03,0.4940E+03,0.5130E+03,0.4240E+03,0.3820E+03,0.3670E+03,&
!0.3100E+03,0.2510E+03,0.2570E+03,0.2040E+03,0.1950E+03,0.1770E+03,0.1290E+03,0.1260E+03,0.1250E+03,0.1000E+03,&
!0.8230E+02,0.6410E+02,0.5710E+02,0.5640E+02,0.5800E+02,0.6580E+02,0.7520E+02,0.8490E+02,0.1010E+03,0.1200E+03,&
!0.1410E+03,0.1650E+03,0.1970E+03,0.2110E+03,0.2360E+03,0.2660E+03,0.2950E+03,0.3270E+03,0.3540E+03,0.3850E+03,&
!0.4130E+03,0.4340E+03,0.4560E+03,0.4800E+03,0.4990E+03,0.5090E+03,0.5100E+03,0.5080E+03,0.5020E+03,0.4920E+03,&
!0.4700E+03,0.4350E+03,0.3940E+03,0.3530E+03,0.3190E+03,0.2840E+03,0.2400E+03,0.1930E+03,0.1400E+03,0.9000E+02,&
!0.5460E+02,0.3290E+02,0.1690E+02,0.1210E+02,0.6780E+01,0.4390E+01,0.2710E+01,0.1770E+01,0.1080E+01,0.6720E+00,&
!0.4640000,0.3000000,0.2100000,0.1600000,0.1300000,0.1100000,0.1000000,0.09/

UV_Absorption=0.
BD=1E7/WN
BD1 = BDA(1)
BD2 = BDA(NP)
IF(BD<BD1.OR.BD>BD2)RETURN

DO I=2,NP
IF(BDA(I)>=BD)EXIT
END DO
C_2=(BDA(I)-BD)/(BDA(I)-BDA(I-1))
  C_1=1.-C_2
UV_Absorption=(C_1*S(I)+C_2*S(I-1))
END



