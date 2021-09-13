CHARACTER ATM_PATH*100,RES*50, ATM*3
INTEGER*4  JMAX,NAGL
REAL*4  V1,V2,ALBEDO
PARAMETER (NTH=1001, DELT=100.0, H=DELT/(NTH-1.0), NAGL=2,J200=200,CP_CONST=3.963) !  *** CP_CONST for  Venus = 3.963 (see Heating Rates calc.)***'
! ***  DELT=100.0 means the given spectral region (V1,V2) in calculations is dIvided by (V1,V1+100.0), (V1+100.0,V1+200.0),...,(V2-100.0,V2 intervals). (All in cm-1).
! *** in each interval are NTH=1001points, distance between points H=DELT/(NTH-1.0)   (0.1 cm-1 for the given example).
! *** J200 - maximal number of the horizontal levels in the atmosphere.Really it is  JMAX  (JMAX should be < or = j200 now 200).
! *** NAGL- maximal number of the solar zenith angles (now NAGL=2).

REAL*4 Z(J200),TAUMA(J200),TAUMA_EF(J200,NAGL),P(J200),ANGLE(NAGL),RO(J200),CANGLE(NAGL), &
        FUP(J200,NAGL),FDO(J200,NAGL),FLUXUP(J200,NAGL),FLUXDO(J200,NAGL),SUMRO(J200) &
		,Qex(J200,NAGL),Qap(J200,NAGL)

! ***
CALL  ATM_PROF_READING(V1,V2,ALBEDO,NAGL,J200,JMAX,Z,P,ANGLE,RO,CANGLE,NAG,SUMRO,ATM_PATH,RES)
!*** In this subroutine will be defined:
!*** V1,V2 - Spectral Region (cm-1) ; ALBEDO  - Albedo (usually 1.0);  NAGL- Maximal Number of Zenith Angles;  NAG- Real Number of Zenith Angles (NAG<=NAGL)
!*** Z- Array of Altitudes km Z(1,2,...,JMAX)  ; P- Array  of Pressures atm P(1,2,...,JMAX)  ;
!*** RO and SUMRO -  Concentrations in mol/(cm*cm*km) and Number of Molecules along the Solar Ray in  mol/(cm*cm) at each atmospheric level (1,2,...,JMAX);
!*** ANGLE and CANGLE - Arrays (1,...,NAG) for Zenith Angles (degrees) and their COS;
!*** ATM_PATH - Path to Atmospheric Model

WRITE(*,*)ATM_PATH
OPEN(11,FILE=RES)  ! *** Filename with calc

FLUXUP=0. ; FLUXDO=0.
JMAX1=JMAX-1
sum=0.0

DO KKK=0,99999   !***  Loop over DELT-intervals  (of 100 cm-1)

    FUP=0. ; FDO=0.  !*** Arrays for Fluxes at each Altitude Level
    VA=V1+DELT*KKK
    VB=VA+DELT

!================================!
    DO I=1,NTH  ! *** V-Loop (over each of NTH Wavenamber Points)

! --------------------------------!
        WES=H/3.    ! *** see Simpthon's Rule for Wavenumber Integration
        IF(I>1.AND.I<NTH)THEN
            IF(I/2*2==I)THEN
                WES=WES*4.
            ELSE
                WES=WES*2.
            END IF
        END IF
! --------------------------------!

        V=VA+H*(I-1) ! *** Wavenumber Point
        SOL=SUN(V)   ! *** Solar Irradiance (R_Earth/R_Venus)**2 = (150E6/108E6 km)**2
        sum=sum+sol*h     ! *** Primitive calculation of the Solar Flux in (V1,V2)
        A_COEF=UV_Absorption(V)  ! ***   Absorption (see FUNCTION UV_H2O,FUNCTION UV_CO2,etc.)

! *** TAU-matrix *** !
        TAUMA=0.
        DO J=JMAX1,1,-1
            TAUMA(J)=TAUMA(J+1)+A_COEF*0.5*(RO(J+1)+RO(J))*(Z(J+1)-Z(J)) !*** Spectral Optical Depth
        END DO

! *** DOWNWARD & UPWARD FLUXES *** !
        DO N=1,NAGL
            SO=SOL*CANGLE(N)
            DO J=1,JMAX
                FDO(J,N)=FDO(J,N)+WES*SO*EXP(-TAUMA(J)/CANGLE(N))          ! *** DONWARD  FLUXES at the V-point. Attention: sum with WES for the wavenumber integration!
            END DO

            F0=WES*SO*EXP(-TAUMA(1)/CANGLE(N))                             !*** Donward Intensity at the atmospheric bottom
            DO J=1,JMAX
                FUP(J,N)=FUP(J,N)+ALBEDO*F0*EXP(-1.66*(TAUMA(1)-TAUMA(J))) ! *** UPWARD  FLUXES at the V-point
            END DO
        END DO

    END DO  ! V-Loop
!================================!

    FLUXUP=FLUXUP+FUP ; FLUXDO=FLUXDO+FDO  ! To obtain fluxes at the whole (V1,V2).

    IF(VB+1.0>V2)EXIT
    WRITE(*,*)VA,VB

END DO   ! *** End of the loop over whole (V1, V2) interval. V1=50000, V2=80000 cm^-1
WRITE(*,*)'*** Finish !!! ***'

WRITE(11,*)sum  ! *** Printing to RES=FDO_ANGLES_FUP approximate Solar Flux in (V1,V2) (for control)
DO J=JMAX,1,-1
    WRITE(11,3)Z(J),FLUXDO(J,:),FLUXUP(J,:) ! *** Printing to RES=FDO_ANGLES_FUP results for exact fluxes profiles!
END DO
3 FORMAT(F7.1,10E12.4)

! =================== PART II - Cross Sections Profiles from   FLUXDO(J,:) ===== !

! *** total TAUMA *** ! ---- calculation of effective optical depth over whole interval
DO J=1,JMAX
  DO N=1,NAGL
  ddtt=FLUXDO(J,N)/FLUXDO(JMAX,N)
  if(ddtt<1e-20)ddtt=1e-20
!### TAUMA_EF(J,N)=-ALOG((FLUXDO(J,N)/FLUXDO(JMAX,N)))*CANGLE(N) !###  'Effective' Optical Depth
 TAUMA_EF(J,N)=-ALOG(ddtt)*CANGLE(N)
  END DO
END DO

! *** Final Printing *** !

! *** KD Fup *** !
DO J=1,JMAX
FUP(J,:)=ALBEDO*FLUXDO(1,:)*EXP(-1.66*(TAUMA_EF(1,:)-TAUMA_EF(J,:)))
!*** Approximate UPWARD Flux ***!
END DO

! ----------------- FINAL PART OF THE PROGRAM --------------------------- !
!*** All values are defined at the (Z(j-1)+Z(j))/2.0 , (j=2,3,...,JMAX).

! *** Calculation for the first zenith Angle ***
OPEN(21,FILE='MOL_SIGMA_FDO_FUP_FUPap_1ANGLE.dat')
! *** printed values in file:
! Zj km, SUMROj mol/cm**2 , DONWARD and UPWARD Fluxes wt/m**2 ('Exact' and 'Approximate')
!***
N=1
DO J=2,JMAX
    ROS=(SUMRO(J-1)-SUMRO(J))/CANGLE(N)
    SIGMA=((TAUMA_EF(J-1,N)-TAUMA_EF(J,N))/CANGLE(N))/ROS
    WRITE(21,4)(Z(J)+Z(J-1))/2.,(SUMRO(J)+SUMRO(J-1))/2./CANGLE(N),SIGMA, &
    (FLUXDO(J,N)+FLUXDO(J-1,N))/2.,(FLUXUP(J,N)+FLUXUP(J-1,N))/2.,(FUP(J,N)+FUP(J-1,N))/2.
END DO
CLOSE(21)

! *** Calculation for the second zenith Angle ***
OPEN(21,FILE='MOL_SIGMA_FDO_FUP_FUPap_2ANGLE.dat')
N=2
DO J=2,JMAX
    ROS=(SUMRO(J-1)-SUMRO(J))/CANGLE(N)
    SIGMA=((TAUMA_EF(J-1,N)-TAUMA_EF(J,N))/CANGLE(N))/ROS
    WRITE(21,4)(Z(J)+Z(J-1))/2.,(SUMRO(J)+SUMRO(J-1))/2./CANGLE(N),SIGMA, &
        (FLUXDO(J,N)+FLUXDO(J-1,N))/2.,(FLUXUP(J,N)+FLUXUP(J-1,N))/2.,(FUP(J,N)+FUP(J-1,N))/2.
END DO
CLOSE(21)

4 FORMAT(F7.1,2X,E12.4,3X,E15.5,3E12.4)

!*** Heating Rates calculations *** !
DO N=1,NAGL
    DO J=2,JMAX
        EF1=FLUXDO(J,N)-FLUXUP(J,N) ; EF2=FLUXDO(J-1,N)-FLUXUP(J-1,N)
        Qex(J,N)=(EF2-EF1)/(P(J)-P(J-1))*CP_CONST/1013.25

        EF1=FLUXDO(J,N)-FUP(J,N) ; EF2=FLUXDO(J-1,N)-FUP(J-1,N)

        Qap(J,N)=(EF2-EF1)/(P(J)-P(J-1))*CP_CONST/1013.25
    END DO
    Qex(1,N)=Qex(2,N) ; Qap(1,N)=Qap(2,N)
END DO

!*** Fluxes and Heating Rates printing *** !
OPEN(21,FILE='FDO_FUP_FUPap_Q_Qap_ANGLES.dat')

DO J=1,JMAX
    WRITE(21,5)Z(J),FLUXDO(J,1),FLUXUP(J,1),FUP(J,1),Qex(J,1),Qap(J,1) &
        ,FLUXDO(J,2),FLUXUP(J,2),FUP(J,2),Qex(J,2),Qap(J,2)
END DO

5 FORMAT(F6.1,3E12.4,2E11.3,3E12.4,2E11.3)

CLOSE(21)

!*** Plotting !!! ***!
! OPEN(21,FILE='PLOT')
! WRITE(21,*)' 4 ',JMAX
!DO J=1,JMAX
! WRITE(21,4)Z(J),FLUXUP(J,1),FUP(J,1),FLUXUP(J,2),FUP(J,2)
! if(z(j)<110.0)jplot=j
!END DO
!CLOSE(21)
!result=RUNQQ('ToDraw.exe','PLOT')

 !*** Plotting2 !!! ***!
! OPEN(21,FILE='PLOT2')
! WRITE(21,*)' 4 ',jplot
! DO J=1,JMAX
! WRITE(21,4)Z(J),Qex(J,1),Qap(J,1),Qex(J,2),Qap(J,2)
! END DO
! CLOSE(21)
! result=RUNQQ('ToDraw.exe','PLOT2')

END

SUBROUTINE  ATM_PROF_READING(V1,V2,ALBEDO,NAGL,J200,JMAX,Z,P,ANGLE,RO,CANGLE,NAG,SUMRO,ATM_PATH,RES)
CHARACTER ATM_PATH*100, ATM_NAME*120, GAS_NAME*4, RES*50
REAL*4 Z(J200), P(J200),ANGLE(NAGL),RO(J200),CANGLE(NAGL),SUMRO(J200), T(J200)

 ! --- Reading input data from file Initial.dat--- !

    !OPEN(66,FILE='Check-inform.dat') ! *** here would be printed gas profile from ATM_PATH - for control *** !
	OPEN(99,FILE='Initial.dat')
	READ(99,*)V1,V2
	READ(99,*)ALBEDO

	READ(99,98)ATM_PATH  ! *** extract path to gas profile *** !
    98 FORMAT(A50)
    !WRITE(66,*)' The atmospheric conditions from file = ',ATM_PATH

    READ(99,*)NAG

	IF(NAG>NAGL)THEN
    WRITE(*,*)'*** Check NUMBER of Angles => ', NAGL,NAG ; STOP ! ***exception for the case if number of input zenith angles is more than NAGL*** !
	END IF

    READ(99,*)ANGLE

    DO N=1,NAG
        CANGLE(N)=COS(ANGLE(N)*3.1416/180.0) ! *** creating array with cos of set zenith angles
    END DO

    READ(99,947)RES
    947 FORMAT(A50)

	CLOSE(99) ! *** closing file Initial.dat
!--------------------------------------------------!

! --- Reading information about Atmosphere in FILE=ATM_PATH --- !

	OPEN(55,FILE=ATM_PATH)

    455 FORMAT(A120)
    READ(55,455)ATM_NAME

	READ(55,*)ngazov,JMAX ! *** extract number of gases and number of levels in gas profile ***

    456 format(A4)
    READ(55,456)GAS_NAME

    WRITE(*,*)GAS_NAME ! *** printing gas name (CO2) before calculations - for control ***
    WRITE(*,*)ATM_NAME ! *** printing source of mixing ratio profile and VIRA atmospheric file before calculations- for control

    !WRITE(66,*)GAS_NAME,'The number of levels (JMAX) : ',JMAX ! *** start optional printing to Check-inform.dat
	DO J=1,JMAX
	READ(55,*)Z(J),P(J),T(J),RO(J)  ! Altitude, Pressure (total), Temperature and Concentration of gas
	!WRITE(66,*)J,Z(J),P(J),T(J),RO(J)
	END DO                                                    ! *** end reading gas profile ***

    CLOSE(55) ! *** closing file with gas profile
    !CLOSE(66) ! *** closing Check-inform file

!---- Calculation of Molecules along the Solar Ray in  mol/(cm*cm) --- for 0 deg zenith angle!

	SUMRO(JMAX)=0.
	DO J=JMAX,2,-1
        SUMRO(J-1)=SUMRO(J)+0.5*(RO(J)+RO(J-1))*(Z(J)-Z(J-1))
	END DO

!-----------------------------------------------------------------------!

  END
