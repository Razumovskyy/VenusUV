! <<< State of the Art -> 2006 year ! >>>
MODULE M_C
 REAL*4 X0_,Y0_,Z0_,P1_0,P2_0,P3_0,P4_0 !Coordinates and Stokes parameters of initial ray.
 REAL*8 A0_,B0_,C0_ &  !  COS of initial ray.
        ,X_RAND        ! For the random data
 INTEGER*4 N_POINTS ! The real number of scattering points in the trajectory.
 PARAMETER (IMAX_MC=998,IMAX_M_=IMAX_MC+2) !The maximal number of scattering points in the trajectory (cut off).
 REAL*4 X_(IMAX_M_),Y_(IMAX_M_),Z_(IMAX_M_),COS1_(IMAX_M_),COS2_(IMAX_M_),COS3_(IMAX_M_) &
 ,P1_(IMAX_M_),P2_(IMAX_M_),P3_(IMAX_M_),P4_(IMAX_M_) ! Trajectory points.
  ALLOCATABLE ALB(:),QQ(:),TAUMA(:,:),CL0(:),CL1(:),DIST(:,:) &
,PIST(:,:),AERRAB(:),LMIN(:),DL0(:),DL1(:),CL1I(:),CL0I(:),LMINV(:) &
,CL0V(:),CL1V(:),DL0V(:),DL1V(:)
 END MODULE M_C
      FUNCTION RAND()
	  USE M_C
      REAL*8 dm37
      DATA dm37 /137438953472./
          X_RAND=X_RAND*3125.
          X_RAND=MOD(X_RAND,dm37)
          RAND=X_RAND/dm37
      END  FUNCTION RAND

PROGRAM UV_ETALON_Monte_Carlo ! 21 August,2021
USE INITIAL_ShW_CLOUD 
USE A_MOD_SHW
USE M_C
USE MESH
!***************************************************************
!*   This program calculates shortwave fluxes & heating rates  *
!*   by Monte-Carlo method                                     *
!* "Universal" wavenumber grid  (McW+IR)                       *
!* Define Path to the spectral database !!!          *
!***************************************************************
CHARACTER*70 FIA,FLE_AL,FIT,FIR,HITRAN,LINE_COUPLING*3    
! ------ VERY IMPORTANT -------------------------------------------- !
! Maximal number points in the WaveNumber grid.
! Please, optimize this parameter, which depends on the number of 
! atmospheric levels and the computer memory (in the past NTmax=20481).
! ------------------------------------------------------------------ !

 ALLOCATABLE FUP(:,:),FDO(:,:),XUP(:,:),XDO(:,:), QT(:,:), FLE_AL(:),ALBED(:) &
  ,PUP(:,:),PDO(:,:),FLUXUP(:,:),FLUXDO(:,:),PLUXUP(:,:),PLUXDO(:,:),FIR(:)
REAL*8 VF,VSTART,VFINISH,VEND,FUP,FDO,FLUXUP,FLUXDO,PLUXUP,PLUXDO &
        ,START,                                        PUP,PDO,h_minSTANDARD
 PARAMETER(IOUT=147,HITRAN='.\HITRAN\',NTmax=2561, &  ! 20481

! ------ VERY IMPORTANT -------------------------------------------- !
! Maximal number points in the WaveNumber grid.
! Please, optimize this parameter, which depends on the number of 
! atmospheric levels and the computer memory (in the past NTmax=20481).
! ------------------------------------------------------------------ !
  DELTAMAX=50.D0, &
      LINE_COUPLING='NO ') ! <<< *** WithOUT Line Coupling >>> *** 

	  COMMON/R_A/VSTART,VFINISH 
	  COMMON/W_N/W_NUM4
COMMON/CUT_OFF/EPS
!* ---------------------------------------------------------------- *
          OPEN(IOUT,FILE='CONTROL_FILE.ALB')
! ***************************** Example ************************************** !
!   500.D0   520.D0                                    Spectral Region [cm^-1]
!   30.0                                               SZA 
!   200                Number of photons in 10 cm^-1
! c:\databases\ATMOSPHERES\Standard_33\MLS4_300.b95 'File with atmospheric model'
! 2                                                  Number of Surfaces
! c:\databases\Surfaces\SNOW.TXT                 ' Surface Albedo ' 
! c:\databases\Surfaces\0.2                      ' Surface Albedo ' 
! Flux&CoollingRate.MLS_SNOW    'Calculated FLUX [W/m^2] and COOLLING RATE  [K/day]'
! Flux&CoollingRate.MLS_02      'Calculated FLUX [W/m^2] and COOLLING RATE  [K/day]'
! Extraterrestrial Solar Flux  (if 0.0 => STANDARD)
! **************************************************************************** !
 READ(IOUT,*)VSTART, VEND
 write(*,*)VSTART, VEND

 READ(IOUT,*)ANGLE
 write(*,*)ANGLE


 READ(IOUT,*)NGAME 
 write(*,*)NGAME 


 READ(IOUT,100)FIA
 write(*,*)FIA

LA=1 !### READ(IOUT,*)LA ; 
ALLOCATE (FLE_AL(LA),ALBED(LA),FIR(LA)) 
  DO L=1,LA 
   READ(IOUT,100)FLE_AL(L)
   write(*,*)FLE_AL(L)
   END DO 
    DO L=1,LA 
	READ(IOUT,100)FIR(L) 
	write(*,*)FIR(L) 
	 END DO
 100    FORMAT(A70)
 S_Insol=0.0 
! ############################################## !
 READ(IOUT,*)S_Insol ! Extraterrestrial Solar Flux
! ############################################## ! 
          CLOSE(IOUT)
	
	    OPEN(IOUT,FILE='k_coef.in')
        WRITE(IOUT,*)FIA
		WRITE(IOUT,100)HITRAN
		CLOSE(IOUT)  


  X_RAND=130000000001.


    CALL ATM_PROF_READING
   ALLOCATE (FUP(LA,JMAX),FDO(LA,JMAX), QT(LA,JMAX), &
               FLUXUP(LA,JMAX),FLUXDO(LA,JMAX), &
   PUP(LA,JMAX),PDO(LA,JMAX),PLUXUP(LA,JMAX),PLUXDO(LA,JMAX),STAT=IERR)
         IF(IERR/=0)THEN
          WRITE(*,*)' Allocaion is wrong (main) !!!'
          STOP
         END IF
 
    CALL FILES_CREATING   

	FUP=0.D0 ; FDO=0.D0 ; PUP=0.D0 ; PDO=0.D0
		DO WHILE (VSTART<VEND) ! Loop over WAVENUMBER SUBINTERVALS
  H_MIN=VSTART*4.29E-7*SQRT(200./18.) ! H2O Doppler Halfwidth, T=200.
  H_MIN=(H_MIN+0.05*P1(JMAX)) ! *(1./5.)    ! Typical Pressure broadaning; Accuracy coefficient=1/5.
     h_minSTANDARD=1.d0/4096.0d0 ; if(h_min > h_minSTANDARD) h_min=h_minSTANDARD
Nper1cm=256 ; H_MIN=1.D0/Nper1cm         

    DELTA=H_MIN*(NTmax-1)
	IF(DELTA>DELTAMAX)DELTA=DELTAMAX 
              VF=VSTART+DELTA
CALL GRIDS(NTmax,H_MIN,VSTART,VF)      
              DELTA=VF-VSTART 

! ############################################

			FLUXUP=0.D0  ; FLUXDO=0.D0 ; PLUXUP=0.D0  ; PLUXDO=0.D0

CALL K_COEF_H_U(LINE_COUPLING) !

! #######################
!*****		CALL K_coef_ShW(H_MIN)   ! Absorption coefficients
                W_NUM4=(VSTART+VFINISH)*0.5
				write(*,*)W_NUM4,vend
    DO L=1,LA ; ALBED(L)=ALBEDO_N(FLE_AL,L,LA) ; END DO
	 VEND4=VEND
                    Z0_=Z(JMAX)
                    C0_=-COS(Pi*ANGLE/180.D0)
                    A0_=-DSQRT(1.D0-C0_**2)
                    B0_=0.D0
  CALL FLUX_SH_AER(FLUXUP,FLUXDO,PLUXUP,PLUXDO,ALBED,H_MIN,NGAME,LA) ! flux/influx
!               *** Direct Fluxes ***

			FUP=FUP+FLUXUP; FDO=FDO+FLUXDO
            PUP=PUP+PLUXUP; PDO=PDO+PLUXDO
		VSTART=VFINISH
CALL GRIDS_END 
       	END DO
!* ---------------------------------- *

!*			Writing			*
!*   'LAYER' Heating rates  
IF(S_Insol>0.0)THEN 
   cs=S_Insol/FDO(la,jmax) ! Correction on the solar distance etc.
fup=fup*cs ; fdo=fdo*cs ; pup=pup*cs ; pdo=pdo*cs
END IF
       
        DO L=1,LA 
        QT(L,JMAX)=0.0 
                 DO J=2,JMAX
      QT(L,J)=((FUP(L,J)-FUP(L,J-1))-(FDO(L,J)-FDO(L,J-1))) &
	           /(P1(J)-P1(J-1))*8.442/1013.25
                 END DO
        QT(L,1)=QT(L,2)  
         END DO
!*******  Writing ******
              DO M=1,LA 
			  IOT=8432        
       OPEN(IOT,FILE=FIR(M))
!            WRITE(IOUT,*)START,' - ',VSTART,' cm**(-1)'
QT(:,JMAX)=0.
QT(:,1)=0. ! *** ATTENTION !!! ***
			DO J=JMAX,1,-1
fdoj=FDO(M,J) ; FUPJ=FUP(M,J)
	WRITE(IOT,1)J,Z(j),P1(J),T1(J),FDOJ,FUPJ,QT(M,J) ! Here ZI = ZJ is used
 1		FORMAT(I4,F10.5,E15.7,F8.3,2E12.5,3x,E12.4)

			END DO

!###            DO J=JMAX,1,-1
 !     WRITE(IOT,1)Z(J),FUP(M,J),FDO(M,J),FDO(M,J)-PDO(M,J),QT(M,J)
 ! 1         FORMAT(F7.3,F20.5,D20.12,F16.3)

!###a1=FUP(M,J); a2= FDO(M,J) ; a3=PDO(M,J) ; a4= QT(M,J)
!###WRITE(IOT,1)Z(J),a1,a2,a3,a4
!###   1         FORMAT(F7.3,3F11.5,F10.3)
  2         FORMAT(F7.2,'  km ')
!###              END DO
       CLOSE(IOT)
            END DO
!* ---------------------------------- *

       END
         FUNCTION ALBEDO_N(FLE_AL,L,LA) ! 30 May,2005 (20 Jan.,2005).
! At present 3 surfaces only.
         CHARACTER*70 FLE_AL(LA)
         ALLOCATABLE X1(:),Y1(:),X2(:),Y2(:),X3(:),Y3(:)
         COMMON/W_N/WAVE_NB
         SAVE X1,Y1,X2,Y2,X3,Y3
         DATA IBG/0/
  IF(IBG==0)THEN
         IBG=1
! 1-surface
          OPEN(98765,FILE=FLE_AL(1))
         DO IE1=1,1000000
         READ(98765,*,END=1)XY
         END DO
 1    REWIND (98765)
         IE1=IE1-1
         ALLOCATE (X1(IE1),Y1(IE1))
         DO I=1,IE1
         READ(98765,*)X1(I),Y1(I)
         END DO
      CLOSE(98765)
         XA1=X1(1) ; XB1=X1(IE1)
          IF(XA1 > XB1)THEN
          XY=XB1 ; XB1=XA1 ; XA1=XY
          I2=IE1/2
          DO I=1,I2
  XY=X1(IE1-I+1) ; X1(IE1-I+1)=X1(I) ; X1(I)=XY
  XY=Y1(IE1-I+1) ; Y1(IE1-I+1)=Y1(I) ; Y1(I)=XY
          END DO
          END IF 

	  IF(LA == 1) GOTO 10
! 2-surface
          OPEN(98765,FILE=FLE_AL(2))
         DO IE2=1,1000000
         READ(98765,*,END=2)XY
         END DO
 2    REWIND (98765)
         IE2=IE2-1
         ALLOCATE (X2(IE2),Y2(IE2))
         DO I=1,IE2
         READ(98765,*)X2(I),Y2(I)
         END DO
      CLOSE(98765)
         XA2=X2(1) ; XB2=X2(IE2)
          IF(XA2 > XB2)THEN
          XY=XB2 ; XB2=XA2 ; XA2=XY
          I2=IE2/2
          DO I=1,I2
  XY=X2(IE2-I+1) ; X2(IE2-I+1)=X2(I) ; X2(I)=XY
  XY=Y2(IE2-I+1) ; Y2(IE2-I+1)=Y2(I) ; Y2(I)=XY
          END DO
          END IF 

	  IF(LA == 2) GOTO 10
! 3-surface
          OPEN(98765,FILE=FLE_AL(3))
         DO IE3=1,1000000
         READ(98765,*,END=3)XY
         END DO
 3    REWIND (98765)
         IE3=IE3-1
         ALLOCATE (X3(IE3),Y3(IE3))
         DO I=1,IE3
         READ(98765,*)X3(I),Y3(I)
         END DO
      CLOSE(98765)
         XA3=X3(1) ; XB3=X3(IE3)
          IF(XA3 > XB3)THEN
          XY=XB3 ; XB3=XA3 ; XA3=XY
          I2=IE3/2
          DO I=1,I2
  XY=X3(IE3-I+1) ; X3(IE3-I+1)=X3(I) ; X3(I)=XY
  XY=Y3(IE3-I+1) ; Y3(IE3-I+1)=Y3(I) ; Y3(I)=XY
          END DO
          END IF 
	  IF(LA > 3) THEN
	  WRITE(*,*)' *** NOT more then 3 surfaces ! ***   ', LA ; STOP 
	  END IF
10 CONTINUE

    END IF
 IF(L==1)THEN
         IF(WAVE_NB <=XA1) THEN
         ALBEDO_N=Y1(1)
         ELSE
          IF(WAVE_NB >=XB1) THEN
          ALBEDO_N=Y1(IE1)
          ELSE 
           DO I=1,IE1
           IF(WAVE_NB >=X1(I).AND.WAVE_NB <= X1(I+1))EXIT
           END DO
           C2=(WAVE_NB-X1(I))/(X1(I+1)-X1(I))
           ALBEDO_N=(1.-C2)*Y1(I)+C2*Y1(I+1)
          END IF
         END IF
 ELSE
   IF(L==2)THEN
          IF(WAVE_NB <=XA2) THEN
         ALBEDO_N=Y2(1)
         ELSE
          IF(WAVE_NB >=XB2) THEN
          ALBEDO_N=Y2(IE2)
          ELSE 
           DO I=1,IE2
           IF(WAVE_NB >=X2(I).AND.WAVE_NB <= X2(I+1))EXIT
           END DO
           C2=(WAVE_NB-X2(I))/(X2(I+1)-X2(I))
           ALBEDO_N=(1.-C2)*Y2(I)+C2*Y2(I+1)
          END IF
         END IF
      ELSE
         IF(WAVE_NB <=XA3) THEN
         ALBEDO_N=Y3(1)
         ELSE
          IF(WAVE_NB >=XB3) THEN
          ALBEDO_N=Y3(IE3)
          ELSE 
           DO I=1,IE3
           IF(WAVE_NB >=X3(I).AND.WAVE_NB <= X3(I+1))EXIT
           END DO
           C2=(WAVE_NB-X3(I))/(X3(I+1)-X3(I))
           ALBEDO_N=(1.-C2)*Y3(I)+C2*Y3(I+1)
          END IF
         END IF

   END IF
 END IF

         END

