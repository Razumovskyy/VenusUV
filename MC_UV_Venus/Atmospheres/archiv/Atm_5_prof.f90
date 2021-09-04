!  ***   21 Aug. 2021***
CHARACTER*3 AAA(5)
REAL*4 RO(5)
DATA AAA/'CO2','H2O','OCS','HCL','SO2'/
OPEN(11,FILE='Venus_5gases')
OPEN(21,FILE='CO2_gas_profile.dat')
OPEN(22,FILE='H2O_gas_profile.dat')
OPEN(23,FILE='OCS_gas_profile.dat')
OPEN(24,FILE='HCL_gas_profile.dat')
OPEN(25,FILE='SO2_gas_profile.dat')

WRITE(11,*)' *** Haus2015 (21.08.2017) ***'
WRITE(11,*)'  5    66 '
DO I=1,5 ; WRITE(11,1)AAA(I) ; END DO
1 FORMAT(A3)
DO I=21,25 
READ(I,1)AAA(1) 
READ(I,1)AAA(2) 
READ(I,1)AAA(3)
END DO

DO J=1,66
DO I=21,25
READ(I,*)Z,P,T,RO(I-20)
END DO
WRITE(11,2)Z,P,T,RO
END DO
2 FORMAT(F7.2,2X,E12.5,2X,F7.2,5E14.5)
END        