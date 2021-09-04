          PROGRAM KD_Monte_Carlo_MAIN ! 25 Dec.,2003.
USE INITIAL_SHW_KD
USE A_MOD
USE M_C
!***************************************************************
!*   This program calculates shortwave fluxes & heating rates  *
!*   by Monte-Carlo method using KD-approximation              *
!***************************************************************

      PARAMETER(IOUT=147,LA=1)
        CHARACTER fi*40,fif*40,FIFI2*40 ! FIFI2*13 #####
        CHARACTER METKA(LA)*25 
        REAL*4 ALBED(LA)
      DIMENSION NGAME(NT)
	  COMMON/W_N/W_NUM4
!###      DATA ALBED/0.1/, &
!###      DATA METKA/'KD-'/ ! Albedo
	

!* ---------------------------------------------------------------- *
OPEN(IOUT,FILE='CONTROL_KD.INP')
  READ(IOUT,*)ANGLE
  READ(IOUT,*)ALBED_1 ; ALBED=ALBED_1
  READ(IOUT,*)NGAME_1 ; NGAME= NGAME_1
  READ(IOUT,765)FIFI2
  READ(IOUT,567)METKA(1)
765 FORMAT(A40)  
567 FORMAT(A25)
CLOSE(IOUT)

   FI=FIFI2   !### 'KD_'
 OPEN(IOUT,FILE='k_coef.in')
  WRITE(IOUT,765)FIFI2 ! 
 CLOSE(IOUT)

! --------------------------------------------- !
 !###       NGAME=4000 !*16! Number of trajectories per DIAP (10 cm**-1)
         write(*,*)ANGLE,ngame(1),' SZA and trajectories per channel '
!###    ANGLE=30.
  X_RAND=130000000001.


    CALL ATM_PROF_READING(LA)
                    Z0_=Z(JMAX)
                    C0_=-COS(Pi*ANGLE/180.D0)
                    A0_=-DSQRT(1.D0-C0_**2)
                    B0_=0.D0
   C_LOG=DLOG(-C0_)
RO_INT_CO2=RO_INT_CO2-C_LOG
RO_INT_H2O=RO_INT_H2O-C_LOG
RO_INT_HCL=RO_INT_HCL-C_LOG
RO_INT_OCS=RO_INT_OCS-C_LOG
RO_INT_SO2=RO_INT_SO2-C_LOG

    CALL FILES_CREATING   

!* ---------------------------------------------------------------- *
                CALL K_COEF_KD ! Absorption coefficients

  CALL SH_AER_KD(ALBED,NGAME,LA) ! flux/influx


!* ---------------------------------- *
!*   'LAYER' Heating rates   
          
        DO L=1,LA 
        QT(L,JMAX)=0.0 
                 DO J=2,JMAX
      QT(L,J)=((FUP(L,J)-FUP(L,J-1))-(FDO(L,J)-FDO(L,J-1))) &
	           /(P(J)-P(J-1))*8.442/1013.25
                 END DO
        QT(L,1)=QT(L,2)  
         END DO
!*******  Writing ******
              DO M=1,LA         
       OPEN(IOUT,FILE=METKA(M))

QT(:,JMAX)=0.
QT(:,1)=0.  ! *** ATTENTION !!! ***
			DO J=JMAX,1,-1
fdoj=FDO(M,J) ; FUPJ=FUP(M,J)
	WRITE(IOUT,1)Z(j),P(J),T(J),FDOJ,FUPJ,QT(M,J) ! Here ZI = ZJ is used
 1		FORMAT(F10.5,E15.7,F8.3,2E12.5,3X,E12.4)

			END DO

!###            DO J=JMAX,1,-1
!###      WRITE(IOUT,1)Z(J),FUP(M,J),FDO(M,J),QT(M,J)
!###  1         FORMAT(F7.3,F20.5,D20.12,F16.3)

  2         FORMAT(F7.2,'  km ')
!###              END DO


       CLOSE(IOUT)
 
 
 
            END DO
!* ---------------------------------- *
!-------------- Plotting a graph  --------------- !
 ! Upward fluxes
                   open(98765,file='plot')  
				   jmaxplot=jmax
  write(98765,*) LA,jmaxplot 
  do j=1,jmaxplot  
   write(98765,*)z(j),(fup(L,j),L=1,LA)
  end do
                   close(98765)  
result=RUNQQ('ToDraw.exe','plot')

 ! Downward fluxes
                   open(98765,file='plot')  
  write(98765,*) LA,jmaxplot  
  do j=1,jmax 
   write(98765,*)z(j),(fdo(L,j),L=1,LA)
  end do
                   close(98765)  
result=RUNQQ('ToDraw.exe','plot')
 
 ! Heating rates
                   open(98765,file='plot')  
  write(98765,*)LA,jmaxplot  
  do j=1,jmax 
   write(98765,*)z(j),(QT(L,j),L=1,LA) ! PUP(J)+PDO(J)
  end do
                   close(98765)  
result=RUNQQ('ToDraw.exe','plot')



       END

SUBROUTINE SH_AER_KD(A_BEDO, NROZZ,LA) ! 2.02.2004
!*********************************************************************
!*  To calculate solar fluxes and influxes in the molecular and      *
!*  aerosol PLANE-PARALLEL atmosphere - KD approximation             *
!*********************************************************************
USE INITIAL_SHW_KD
USE M_C
	  COMMON/W_N/V

      REAL*8 A0,B0,C0

!* ---------------------------------------------------------------*
  DIMENSION A_BEDO(LA),NROZZ(NT),V_CHAN(NT),SUN(NT)
       EXTERNAL NEW_Z,NEW_DIRECTION,LAMBERT

!### DATA V_CHAN/65000./ & ! fictiv channel wavenumbers
!### , SUN/ 0.1694E+00/
	   DATA IBG/0/
	   IF(IBG.EQ.0)THEN
	   IBG=1
	   open(987654,file='SUN&WN_CLOUD-UV')
	   read(987654,*)SUN(1)
       read(987654,*)V_CHAN(1)
	   close (987654)
       IMAX=JMAX
 ALLOCATE (ALB(LA),QQ(LA),CL0(JMAX),CL1(JMAX),TAUMA(JMAX)     &
  ,DIST(LA,IMAX),PIST(LA,IMAX),AERRAB(JMAX),LMIN(IMAX),DL0(IMAX) &
  ,DL1(IMAX),CL1I(IMAX),CL0I(IMAX),LMINV(IMAX_M_),CL0V(IMAX_M_)  &
  ,CL1V(IMAX_M_),DL0V(IMAX_M_),DL1V(IMAX_M_),STAT=IERR)

         IF(IERR/=0)THEN
          WRITE(*,*)' Allocaion is wrong (SH_AER) !!!'
          STOP
         END IF
         COSIN=-C0_
!******************* CL0,CL1 for TAUMA - matrix  ****************
      CL0(1) = 0.
      CL1(1) = 0.
      DO 1 J = 2, JMAX
      Z1=Z(J-1)
      Z2=Z(J)
      PSIL=Z1
      PSIL1=Z2
      GA=Z2-Z1
      GB=0.5*(Z2+Z1)*GA
      CC=PSIL-PSIL1
      CL1(J)=(PSIL*GA-GB)/CC
      CL0(J)=(GB-PSIL1*GA)/CC
1     CONTINUE
	   END IF  
!* --------------------------------------------------------- *


!***********************************************************************
!  ***  I- MESH : LMIN, DLO(I), DL1(I)  ***
 
      DO 12 I=1,IMAX
      ZZZ=Z(I)
      LMIN(I)=JMAX-1
      DO 13 J=2,JMAX
      IF(ZZZ.LT.Z(J))GOTO 14
   13 CONTINUE
      GOTO 12
   14 LMIN(I)=J-1
   12 CONTINUE
      DO 16 I=1,IMAX
      J=LMIN(I)
      Z1=Z(J)
      Z2=Z(J+1)
      PSIL=Z1
      PSIL1=Z2
      ZZ=Z(I)
      GA=ZZ-Z1
      GB=0.5*(ZZ+Z1)*GA
      CC=PSIL-PSIL1
      PSIZ=ZZ
      CL0I(I)=(GB-PSIL1*GA)/CC
      CL1I(I)=(PSIL*GA-GB)/CC
      DL0(I)=(PSIZ-PSIL1)/CC
      DL1(I)=(PSIL-PSIZ)/CC
   16 CONTINUE


! ---------------------------------------------------------
!* Fluxes and influxes *
 FUP=0. ; FDO=0.
    DO I=1,NT
 write(*,*)i,'  channel'
 
 FLUXUP=0.D0 ; FLUXDO=0.D0
!*  ' I ' point *
     V=V_CHAN(I)
        SOLAR=SUN(I)*COSIN
     CALL LAYER_PROPERTY ! Setting of aerosol opt. prop. at given Wave-Number
!*  ***  TAU - MATRIX ***
    TAUMA(1)=0.
      DO   J = 2, JMAX
      CCLL0=CL0(J)
      CCLL1=CL1(J)
 TAUMA(J)=TAUMA(J-1)+RABMA(I,J-1)*CCLL0+RABMA(I,J)*CCLL1
        END DO
!   ***  TAKING INTO ACCOUNT OF AEROSOL ABSORPTION ***
         SUCTAU=0.
         DO J=2,JMAX
         ZUKA=(Z(J)+Z(J-1))*0.5
         CALL SC_AB_A(ZUKA,S_SCAT,SUCA,LNUMB)
         AERRAB(J)=SUCA
         SUCTAU=SUCTAU+(Z(J)-Z(J-1))*SUCA
           TAUMA(J)=TAUMA(J)+SUCTAU
         END DO
          AERRAB(1)=AERRAB(2)
! --------  CHAIN - CYCLE ---------------------------------
!  ***  Z_ - MESH ***
      NROZ=NROZZ(I)
      ROZ=1./NROZ
      DO 128 NMONTE=1,NROZ
      CALL MONTE_CARLO(NEW_Z,NEW_DIRECTION,LAMBERT)

                                             Z_(N_POINTS)=Z(JMAX)

         DO L=1,LA
         QQ(L)=1.*SOLAR ! To take into account of solar irradience
         END DO
!* -----------------------------------------------------------
!  *** PARAMETERS FOR IV - MESH ***
!  *** LMINV( ),CL0V( ),CL1V( ),DL0V( ),DL1V( ) ***
      DO 322 IV=1,N_POINTS
      ZZZ=Z_(IV)
      LMINV(IV)=JMAX-1
      DO 323 J=2,JMAX
      IF(ZZZ.LT.Z(J))GOTO 324
  323 CONTINUE
      GOTO 322
  324 LMINV(IV)=J-1
  322 CONTINUE
      DO 316 IH=1,N_POINTS
      J=LMINV(IH)
      Z1=Z(J)
      Z2=Z(J+1)
      ZZ=Z_(IH)
      CC=Z2-Z1
      CCZ=ZZ-Z1
      CCZZ=(ZZ+Z1)*CCZ*0.5
      CL0V(IH)=(Z2*CCZ-CCZZ)/CC
      CL1V(IH)=(CCZZ-Z1*CCZ)/CC
      DL0V(IH)=(Z2-ZZ)/CC
      DL1V(IH)=(ZZ-Z1)/CC
  316 CONTINUE
!  ***********************************
!  ***  ABSORBTIONS ONLY FOR WEITS ***
!
      DO 21 IV=2,N_POINTS
      J1=LMINV(IV-1)
      J2=LMINV(IV)
             ZUKA=0.5*(Z(J1)+Z(J1+1))
      CALL SC_AB_A(ZUKA,S_SCAT,SUCA1,LNUMB)
             ZUKA=0.5*(Z(J2)+Z(J2+1))
      CALL SC_AB_A(ZUKA,S_SCAT,SUCA2,LNUMB)
             TUKA1=(Z_(IV-1)-Z(J1))*SUCA1
             TUKA2=(Z_(IV)-Z(J2))*SUCA2
      CCL00=CL0V(IV-1)
      CCL10=CL1V(IV-1)
      CCL01=CL0V(IV)
      CCL11=CL1V(IV)
             ABSTE=ABS(COS3_(IV-1))
                   IF(ABSTE.LT.1E-9) THEN
                      TETCOS=1E9
                      IF(COS3_(IV-1).LT.0.)TETCOS=-TETCOS
                   ELSE
      TETCOS=1./COS3_(IV-1)
                   END IF

            IF(ABSTE.LT.1.0E-04)THEN
                                     ABSTE = 0.0
                                ELSE
                                     ABSTE = 1.0/ABSTE
                                ENDIF

      ZZZ=Z_(IV-1)
      ZZZZ=Z_(IV)
      IF=0
      IB=0
      DO IIII=1,IMAX
          DO L=1,LA
      DIST(L,IIII)=0.
      PIST(L,IIII)=0.
          END DO
      FFF=(ZZZ-Z(IIII))*(ZZZZ-Z(IIII))
      IF(FFF.LE.0..AND.IB.EQ.0)IB=IIII
      IF(FFF.LE.0.)IF=IIII
      END DO
          DO L=1,LA
      ALB(L)=1.
      IF(Z_(IV).EQ.Z(1))ALB(L)=A_BEDO(L)
          END DO
!*  ----------------------------------------

      TAU1=TAUMA(J1)+RABMA(I,J1)*CCL00+RABMA(I,J1+1)*CCL10
      TAU2=TAUMA(J2)+RABMA(I,J2)*CCL01+RABMA(I,J2+1)*CCL11
                  TAU1=TAU1+TUKA1
                  TAU2=TAU2+TUKA2
!   ***  Calculation at ZI - LEVELS ***
      IF(IB.EQ.0)GOTO 1325
      DO 3222 II=IB,IF
      JJ=LMIN(II)
      TAU=TAUMA(JJ)+RABMA(I,JJ)*CL0I(II)+RABMA(I,JJ+1)*CL1I(II)
             TAU=TAU+AERRAB(JJ+1)*(Z(II)-Z(JJ))
      RAB=RABMA(I,JJ)*DL0(II)+RABMA(I,JJ+1)*DL1(II)+AERRAB(JJ+1)
          DO L=1,LA
      WES=QQ(L)*EXP((TAU1-TAU)*TETCOS)
!      PIST(L,II)=PIST(L,II)+WES*RAB*ABSTE
      DIST(L,II)=DIST(L,II)+WES
          END DO
 3222 CONTINUE
 1325 CONTINUE
      YL=(TAU1-TAU2)*TETCOS
      QQYL=EXP(YL)
      DO L =1,LA
      QQ(L)=QQ(L)*QQYL*ALB(L)
      END DO
      IF(IB.EQ.0)GOTO 21
      DO II=IB,IF
      IF(TETCOS.GT.0.)THEN
          DO L=1,LA
       FLUXUP(L,II)=FLUXUP(L,II)+DIST(L,II)*ROZ
!       PLUXUP(L,II)=PLUXUP(L,II)+PIST(L,II)*ROZ
          END DO
                      ELSE
          DO L=1,LA
       FLUXDO(L,II)=FLUXDO(L,II)+DIST(L,II)*ROZ
!       PLUXDO(L,II)=PLUXDO(L,II)+PIST(L,II)*ROZ
          END DO
                      END IF
      END DO

   21 CONTINUE
  128 CONTINUE
     FUP=FUP+FLUXUP ; FDO=FDO+FLUXDO
     END DO
      END
