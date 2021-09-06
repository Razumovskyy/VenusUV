! *** NO any FluxUP calculations ets. Only CROSS-SECTIONS calculations ***!
CHARACTER ATM_PATH*100,RES*20, ATM*3   
INTEGER*4  JMAX,NAGL 
REAL*4  V1,V2,ALBEDO
PARAMETER (NTH=1001, DELT=10.0, H=DELT/(NTH-1.0), NAGL=2,J200=200,CP_CONST=3.963) !  *** CP_CONST for  Venus = 3.963 (see Heating Rates calc.)***'
! ***  DELT=100.0 means the given spectral region (V1,V2) in calculations is dIvided by (V1,V1+100.0), (V1+100.0,V1+200.0),...,(V2-100.0,V2 intervals). (All in cm-1).
! *** in each interval are NTH=1001points, distance between points H=DELT/(NTH-1.0)   (0.1 cm-1 for the given example).
! *** J200 - maximal number of the horizontal levels in the atmosphere.Really it is  JMAX  (JMAX should be < or = j200 now 200).
! *** NAGL- maximal number of the solar zenith angles (now NAGL=2).

REAL*4 Z(J200),TAUMA(J200),TAUMA_EF(J200,NAGL),P(J200),ANGLE(NAGL),RO(J200),CANGLE(NAGL), &    
        FDO(J200,NAGL),FLUXDO(J200,NAGL),SUMRO(J200)
		
! *** 
 CALL  ATM_PROF_READING(V1,V2,ALBEDO,NAGL,J200,JMAX,Z,P,ANGLE,RO,CANGLE,NAG,SUMRO,ATM_PATH,RES)
!*** In this subroutine will be defined:
!*** V1,V2 - Spectral Region (cm-1) ; ALBEDO  - Albedo (usually 1.0);  NAGL- Maximal Number of Zenith Angles;  NAG- Real Number of Zenith Angles (NAG<=NAGL)
!*** Z- Array of Altitudes km Z(1,2,...,JMAX)  ; P- Array  of Pressures atm P(1,2,...,JMAX)  ; 
!*** RO and SUMRO -  Concentrations in mol/(cm*cm*km) and Number of Molecules along the Solar Ray in  mol/(cm*cm) at each atmospheric level (1,2,...,JMAX);
!*** ANGLE and CANGLE - Arrays (1,...,NAG) for Zenith Angles (degrees) and their COS;  
!*** ATM_PATH - Path to Atmospheric Model

WRITE(*,*)ATM_PATH
!###ATM(1:3)=ATM_PATH(18:20)
!###RES='Result_'//ATM ;
OPEN(11,FILE=RES)  ! *** Filename with calc
FLUXDO=0.
JMAX1=JMAX-1
 sum=0.0
DO KKK=0,99999   !***  Loop over DELT-intervals  (of 100 cm-1)
FDO=0.  !*** Arrays for Fluxes at each Altitude Level  
VA=V1+DELT*KKK 
VB=VA+DELT
                                        
!================================!
DO I=1,NTH  ! ### V-Loop (over each of NTH Wavenamber Points)

! --------------------------------!
WES=H/3.    ! ### see Simpthon's Rule for Wavenumber Integration   
IF(I>1.AND.I<NTH)THEN
 IF(I/2*2==I)THEN 
 WES=WES*4.
 ELSE 
 WES=WES*2.
 END IF
END IF
! --------------------------------!

V=VA+H*(I-1) ! Wavenumber Point
SOL=SUN(V) ! In SUN * 1.93 ! Solar Irradience (R_Earth/R_Venus)**2 = (150E6/108E6 km)**2
! --- Taking into accaunt Sun-Venus distance ----- 
sum=sum+sol*h     ! Primitiv calculation of the Solar Flux in (V1,V2)                               
A_COEF=UV_Absorption(V)  ! ***   Absorption (see FUNCTION UV_H2O,FUNCTION UV_CO2,etc.)

! *** TAU-matrix *** !
TAUMA=0.  
DO J=JMAX1,1,-1
TAUMA(J)=TAUMA(J+1)+A_COEF*0.5*(RO(J+1)+RO(J))*(Z(J+1)-Z(J)) !### Spectral Optical Depth
END DO

! *** DOWNWARD & UPWARD FLUXES *** !
DO N=1,NAGL
   SO=SOL*CANGLE(N) 
   DO J=1,JMAX   ! *** DOWNWARD
FDO(J,N)=FDO(J,N)+WES*SO*EXP(-TAUMA(J)/CANGLE(N)) ! DONWARD  FLUXES at the V-point
!### Attention: sum with WES for the wavenumber integration! 
   END DO
END DO
END DO  ! V-Loop
!================================!
FLUXDO=FLUXDO+FDO  ! To obtain fluxes at the whole (V1,V2).
IF(VB+1.0>V2)EXIT
WRITE(*,*)VA,VB
END DO   ! K- Loop
WRITE(*,*)'*** Finish !!! ***' 

write(11,*)sum  ! *** ~ Solar Flux in (V1,V2) (for control)
DO J=JMAX,1,-1
WRITE(11,3)Z(J),FLUXDO(J,:)     ! Printing in 'Result_'//ATM Z and Fluxes
END DO
3 FORMAT(F7.1,10E12.4)

! =================== PART II - Cross Sections Profiles from   FLUXDO(J,:) ===== ! 

! *** total TAUMA *** !
DO J=1,JMAX
  DO N=1,NAGL
  ddtt=FLUXDO(J,N)/FLUXDO(JMAX,N)
  if(ddtt<1e-20)ddtt=1e-20
!### TAUMA_EF(J,N)=-ALOG((FLUXDO(J,N)/FLUXDO(JMAX,N)))*CANGLE(N) !###  'Effective' Optical Depth
 TAUMA_EF(J,N)=-ALOG(ddtt)*CANGLE(N)
  END DO 
END DO 

! *** Final Printing *** !

!! ----------------- FINAL PART OF THE PROGRAM --------------------------- !
!*** All values are defined at the (Z(j-1)+Z(j))/2.0 , (j=2,3,...,JMAX).

! *** Calc. for the first Zenith Angle ***
OPEN(21,FILE='K_FDO_.00deg') 
! *** printed values in file: 
! Zj km, SUMROj mol/cm**2 , DONWARD  Fluxe wt/m**2 ('Exact' only')
!*** 
N=1
DO J=2,JMAX
ROS=(SUMRO(J-1)-SUMRO(J))/CANGLE(N)
SIGMA=((TAUMA_EF(J-1,N)-TAUMA_EF(J,N))/CANGLE(N))/ROS
WRITE(21,4)(Z(J)+Z(J-1))/2.,(SUMRO(J)+SUMRO(J-1))/2./CANGLE(N),SIGMA, (FLUXDO(J,N)+FLUXDO(J-1,N))/2. 
END DO
CLOSE(21)

! *** Calc. for the second Zenith Angle ***
OPEN(21,FILE='K_FDO_.75deg')
! *** printed values as for the first zenith angle ***
N=2
DO J=2,JMAX
ROS=(SUMRO(J-1)-SUMRO(J))/CANGLE(N)
SIGMA=((TAUMA_EF(J-1,N)-TAUMA_EF(J,N))/CANGLE(N))/ROS
WRITE(21,4)(Z(J)+Z(J-1))/2.,(SUMRO(J)+SUMRO(J-1))/2./CANGLE(N),SIGMA, (FLUXDO(J,N)+FLUXDO(J-1,N))/2.
END DO
CLOSE(21)
4 FORMAT(F7.1,2X,E12.4,3X,E15.5,3E12.4)

!*** Fluxes and Heating Rates printing *** !
OPEN(21,FILE='FDO_.A&B')
DO J=1,JMAX
WRITE(21,5)Z(J),FLUXDO(J,1),FLUXDO(J,2)
!*** 
END DO
5 FORMAT(F6.1,3E12.4,2E11.3,3E12.4,2E11.3) 

CLOSE(21)

!*** Plotting !!! ***!
 OPEN(21,FILE='PLOT')
 WRITE(21,*)' 2',JMAX
DO J=1,JMAX
 WRITE(21,4)Z(J),FLUXdo(J,1),FLUXdo(J,2)
 END DO
 CLOSE(21)
 result=RUNQQ('ToDraw.exe','PLOT')
END

  
 SUBROUTINE  ATM_PROF_READING(V1,V2,ALBEDO,NAGL,J200,JMAX,Z,P,ANGLE,RO,CANGLE,NAG,SUMRO,ATM_PATH,RES)
CHARACTER ATM_PATH*100,At_Name*21, GAZ*4, RES*20
REAL*4 Z(J200),P(J200),ANGLE(NAGL),RO(J200),CANGLE(NAGL),SUMRO(J200)
 
 ! --- Reeding of Initial Data in FILE='Initial.dat'--- !
        OPEN(66,FILE='Check.inform') ! for Control
	OPEN(99,FILE='Initial.dat')
	READ(99,*)V1,V2
	READ(99,*)ALBEDO
	READ(99,98)ATM_PATH  ! *** File with Atmospheric Model *** !
98 FORMAT(A50)
		WRITE(66,*)' The atmospheric conditions from file = ',ATM_PATH
                READ(99,*)NAG 
				IF(NAG>NAGL)THEN
   WRITE(*,*)'*** Check NAMBER of Angles => ', NAGL,NAG ; STOP             
				END IF
READ(99,*)ANGLE
DO N=1,NAG
CANGLE(N)=COS(ANGLE(N)*3.1416/180.0)
END DO
READ(99,399)RES
399 FORMAT(A20)
		CLOSE(99)
!---------------------------------- ! 

! --- Reeding information about Atmosphere in FILE=ATM_PATH --- !
	OPEN(55,FILE=ATM_PATH)

455 format(A21) ; READ(55,455)At_Name
	READ(55,*)ngazov,JMAX ! <-- Number of levels in Atmospheric Model. 
456 format(A4) ; read(55,456)GAZ
write(*,*)gaz,At_Name
	WRITE(66,*)gaz,'The number of levels (JMAX) : ',JMAX
	DO J=1,JMAX
	READ(55,*)Z(J),P(J),ttt,RO(J)  ! Altitude, Pressure and Concentration.
	WRITE(66,*)j,Z(J),P(J),ttt,RO(J)
	END DO
	
!---- Calculation of Molecules along the Solar Ray in  mol/(cm*cm) --- ! 	
	SUMRO(JMAX)=0.
	DO J=JMAX,2,-1
	SUMRO(J-1)=SUMRO(J)+0.5*(RO(J)+RO(J-1))*(Z(J)-Z(J-1))
	END DO 
		CLOSE(55)
                CLOSE(66)
  END 
  