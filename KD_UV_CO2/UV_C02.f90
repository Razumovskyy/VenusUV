!### open(7,file='CO2.TEST')
!### do w=50000.0,90000.0,10.
!### !do BD=120.0,200.0,0.1
!### BD=1E7/W  ! W=1E7/BD
!### s=UV_ABSORPTION(W)
!### write(7,*)BD,S
!### end do
!### end

FUNCTION UV_ABSORPTION(WN)
!***CO2_Parkinson(2003)_295K_163-200nm.txt
!*** 23870
!#lambda[nm] sigma [cm2] 
CHARACTER AAA*5
PARAMETER (NP=23870,BD1=163.37281,BD2=199.98835)
DIMENSION S(NP),BDA(NP)
DATA NBEG/0/
IF(NBEG==0)THEN
NBEG=1
OPEN(8452,FILE='CO2_Parkinson2003.dat')
3 FORMAT(A5); READ(8452,3)AAA ; READ(8452,3)AAA 
DO I=1,NP
READ(8452,*)BDA(I),S(I)
END DO
END IF
UV_ABSORPTION=0.
BD=1E7/WN
IF(BD>BD2)RETURN
UV_ABSORPTION= S(1)
IF(BD<=BD1)RETURN

DO I=2,NP
IF(BDA(I)>=BD)EXIT
END DO
C_2=(BDA(I)-BD)/(BDA(I)-BDA(I-1))
  C_1=1.-C_2
UV_ABSORPTION=(C_1*S(I)+C_2*S(I-1))
END



