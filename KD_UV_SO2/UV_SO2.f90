!### open(7,file='SO2.TEST') 
!### do w=20000.0,90000.0,10.
!###  do BD=106.0,403.0,0.1
!###   W=1E7/BD
 !### s=UV_ABSORPTION(W)
!### write(7,*)w,BD,S
 !###  end do
 !###  end

FUNCTION UV_ABSORPTION(WN)
!#SO2_ManattLane(1993)_293K_106.1-403.7nm.txt  2977
!#lambda[nm] sigma [cm2] 
CHARACTER AAA*5
PARAMETER (NP=2977,BD1=106.1,BD2=403.7)
DIMENSION S(NP),BDA(NP)
DATA NBEG/0/
IF(NBEG==0)THEN
NBEG=1
OPEN(8452,FILE='SO2_ManattLane1993.dat')
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


