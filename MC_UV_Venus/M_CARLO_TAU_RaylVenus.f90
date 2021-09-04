! **** Modification of 10 May, 2011. TAU(1)>TAU(2)>...TAU(Jmax)=0.0 *** !
SUBROUTINE FLUX_SH_AER(FLUXUP, FLUXDO, PLUXUP,PLUXDO,A_BEDO,HMIN2,NROZ,LA) !26.12.2003
!*********************************************************************
!*  To calculate solar fluxes and influxes in the molecular and      *
!*  aerosol PLANE-PARALLEL atmosphere                                *
!*********************************************************************
USE INITIAL_SHW_CLOUD
USE M_C
USE MESH
      REAL*8 VSTART,FLUXUP,FLUXDO,PLUXUP,PLUXDO
      REAL*8 A0,B0,C0
!*  JMAX   - maximal number (for DIMENSION) of the ZJ-grid.
!*  LA     - number of albedoes
!*  NT     -  ... of the wavenumber grid (per 10 inverse cm).
!* ---------------------------------------------------------------*
  DIMENSION FLUXUP(LA,JMAX),FLUXDO(LA,JMAX),PLUXUP(LA,JMAX),PLUXDO(LA,JMAX) &
  ,A_BEDO(LA)
       COMMON/R_A/VSTART
       EXTERNAL NEW_Z,NEW_DIRECTION,LAMBERT
	   DATA IBG/0/
DIAP2=(NT-1)*HMIN2
	   IF(IBG.EQ.0)THEN
	   IBG=1
 ALLOCATE (ALB(LA),QQ(LA),TAUMA(NT,JMAX),CL0(JMAX),CL1(JMAX)     &
  ,DIST(LA,JMAX),PIST(LA,JMAX),AERRAB(JMAX),LMIN(JMAX),DL0(JMAX) &
  ,DL1(JMAX),CL1I(JMAX),CL0I(JMAX),LMINV(IMAX_M_),CL0V(IMAX_M_)  &
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
    CALL LAYER_PROPERTY ! Setting of aerosol opt. prop. at given Wave-Number
!*  ***  TAU - MATRIX ***
      TAUMA(:,JMAX)=0.  
JZM_1=JMAX-1 
DO  32 J = JZM_1,1,-1   
TAUMA(:,J)=TAUMA(:,J+1)+RABMA(:,J)*CL0(J+1)+RABMA(:,J+1)*CL1(J+1) 
32    CONTINUE

!***********************************************************************
!  ***  I- MESH : LMIN, DLO(I), DL1(I)  ***
      DO 12 I=1,JMAX
      ZZZ=Z(I)
      LMIN(I)=JMAX-1
      DO 13 J=2,JMAX
      IF(ZZZ.LT.Z(J))GOTO 14
   13 CONTINUE
      GOTO 12
   14 LMIN(I)=J-1
   12 CONTINUE
      DO 16 I=1,JMAX
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
!   ***  TAKING INTO ACCOUNT OF AEROSOL ABSORPTION ***
         SUCTAU=0.
 DO J = JZM_1,1,-1  
         ZUKA=(Z(J)+Z(J+1))*0.5
         CALL SC_AB_A(ZUKA,S_SCAT,SUCA,LNUMB)
         AERRAB(J+1)=SUCA
  SUCTAU=SUCTAU+(Z(J+1)-Z(J))*SUCA  
  TAUMA(:,J)=TAUMA(:,J)+SUCTAU      
END DO
   AERRAB(1)=AERRAB(2)
! --------  CHAIN - CYCLE ---------------------------------
!  ***  Z_ - MESH ***
      ROZ=1./NROZ*DIAP2
      DO 128 NMONTE=1,NROZ
      CALL MONTE_CARLO(NEW_Z,NEW_DIRECTION,LAMBERT)
                                             Z_(N_POINTS)=Z(JMAX)
!*   *** DEFINING  ' I ' POINT ***
                IMO=RAND()*NT
                V=VSTART+IMO*H
        SOLAR=SUN(V)*COSIN

                IF(IMO<NT) IMO=IMO+1
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
      DO 316 I=1,N_POINTS
      J=LMINV(I)
      Z1=Z(J)
      Z2=Z(J+1)
      ZZ=Z_(I)
      CC=Z2-Z1
      CCZ=ZZ-Z1
      CCZZ=(ZZ+Z1)*CCZ*0.5
      CL0V(I)=(Z2*CCZ-CCZZ)/CC
      CL1V(I)=(CCZZ-Z1*CCZ)/CC
      DL0V(I)=(Z2-ZZ)/CC
      DL1V(I)=(ZZ-Z1)/CC
  316 CONTINUE
!  ***********************************
!  ***  ABSORBTIONS ONLY FOR WEIGHTS ***
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
      DO IIII=1,JMAX
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
             I=IMO
    TAU1=TAUMA(I,J1)-(RABMA(I,J1)*CCL00+RABMA(I,J1+1)*CCL10)   ! *** NEW !
    TAU2=TAUMA(I,J2)-(RABMA(I,J2)*CCL01+RABMA(I,J2+1)*CCL11)   ! *** NEW !
                 TAU1=TAU1-TUKA1   ! *** NEW !
                 TAU2=TAU2-TUKA2   ! *** NEW !
!   ***  Calculation at ZI - LEVELS ***
      IF(IB.EQ.0)GOTO 1325
      DO 3222 II=IB,IF
      JJ=LMIN(II)
      TAU=TAUMA(I,JJ)-(RABMA(I,JJ)*CL0I(II)+RABMA(I,JJ+1)*CL1I(II))  ! *** NEW !
             TAU=TAU-(AERRAB(JJ+1)*(Z(II)-Z(JJ)))  ! *** NEW !
      RAB=RABMA(I,JJ)*DL0(II)+RABMA(I,JJ+1)*DL1(II)+AERRAB(JJ+1)  
          DO L=1,LA
      WES=QQ(L)*EXP(-(TAU1-TAU)*TETCOS)   !### OLD     WES=QQ(L)*EXP((TAU1-TAU)*TETCOS)
      PIST(L,II)=PIST(L,II)+WES*RAB*ABSTE
      DIST(L,II)=DIST(L,II)+WES
          END DO
 3222 CONTINUE
 1325 CONTINUE
      YL=(TAU2-TAU1)*TETCOS  
IF(YL>0.)THEN 
QQYL=0.
ELSE
      QQYL=EXP(YL)
END IF
      DO L =1,LA
      QQ(L)=QQ(L)*QQYL*ALB(L)
      END DO
      IF(IB.EQ.0)GOTO 21
      DO II=IB,IF
      IF(TETCOS.GT.0.)THEN
          DO L=1,LA
       FLUXUP(L,II)=FLUXUP(L,II)+DIST(L,II)*ROZ
          END DO
                      ELSE
          DO L=1,LA
       FLUXDO(L,II)=FLUXDO(L,II)+DIST(L,II)*ROZ
 IF(IV>2)PLUXDO(L,II)=PLUXDO(L,II)+DIST(L,II)*ROZ   ! Direct Radiation
          END DO
                      END IF
      END DO

   21 CONTINUE
  128 CONTINUE

      RETURN
      END

! 11 Dec., 2003.
      SUBROUTINE MONTE_CARLO(NEW_XYZ,NEWDIR,REFLECT) 
!*------------------------------------------------------------------------*
!* Subroutine to obtain photon's trajectory by Monte-Carlo technique      *
!* See in MODULE M_C                                                      *
!* X_(i) is x- coordinate of i-th scattering points, (i=1,2,...,N_POINTS),*
!* Y_(i),Z_(i) are y,z- coordinates, respectively.                        *
!* COS1_(i),COS2_(i),COS3_(i) and P1_(i),...,P4_(i) are cosines of the    *
!*    photon's direction and its Stokes parameters after i-th scattering. *
!* Now it's considered 4-Stokes param. only (for spherical particl. etc.),*
!*  but their number may be easily increased (see COMMON/RAY_ST/ ...).    *
!* NEW_XYZ -  subroutine to define coordinates of the next photon scatter.*
!* NEWDIR  -  ... angles and Stokes parameters after the scattering.      *
!* REFLECT -  ... angles and St. par. after the reflection at the surface.*
!*------------------------------------------------------------------------*
USE M_C
USE INITIAL_SHW_CLOUD 
  REAL*8 A,B,C,CHECK
  DATA K/0/
         IF(K == 0)THEN
          K=1
         CHECK=A0_**2+B0_**2+C0_**2
          IF(DABS(1.D0-CHECK).GT.1D-10)THEN
      WRITE(*,*)' Sum of the initial cosines = ',CHECK,A0_,B0_,C0_
      WRITE(*,*)' Pause (only once) ! '
         PAUSE
          END IF
         END IF

! --------- Start of a new trajectory ------------ !
          I=1
  X_I=X0_ ;  Y_I=Y0_ ;  Z_I=Z0_ ;  X_(I)=X_I ;  Y_(I)=Y_I ; Z_(I)=Z_I
   A=A0_ ;   B=B0_ ;   C=C0_ ;  COS1_(I)=A ;  COS2_(I)=B ;  COS3_(I)=C
Q1=P1_0; Q2=P2_0; Q3=P3_0; Q4=P4_0; P1_(I)=Q1; P2_(I)=Q2; P3_(I)=Q3; P4_(I)=Q4

! ---------  After i-th scatterring --------------- !
 1    I=I+1 
       CALL NEW_XYZ(X_I,Y_I,Z_I,A,B,C,IBOUND) ! New x,y,z . If 
! <<< IBOUND=-1,0,+1, if a new point is ON a SURFACE, INSIDE and OUTSIDE ATMOSPHERE >>>
   X_(I)=X_I ;  Y_(I)=Y_I ;  Z_(I)=Z_I
      N_POINTS=I
      IF(IBOUND.EQ.1)RETURN ! Photon outside atmosphere - End of this trajjectory.
 
     IF(IBOUND.EQ.0)THEN 
      CALL NEWDIR(X_I,Y_I,Z_I,A,B,C,Q1,Q2,Q3,Q4)! Scattering inside atmosphere - New direction.
                      ELSE
					  Z_I=Z(1) ; Z_(I)=Z_I
      CALL REFLECT(X_I,Y_I,Z_I,A,B,C,Q1,Q2,Q3,Q4)! Reflectance at the surface - New direction. 
                      END IF
    COS1_(I)=A ;  COS2_(I)=B ;  COS3_(I)=C
    P1_(I)=Q1  ;  P2_(I)=Q2  ;  P3_(I)=Q3 ;  P4_(I)=Q4
      IF(I.GE.IMAX_MC)THEN ! End of trajectory (artificial).
          COS1_(I)=0.D0
          COS2_(I)=0.D0
          COS3_(I)=0.9999999
          COS1_(I+1)=0.D0
          COS2_(I+1)=0.D0
          COS3_(I+1)=0.9999999
          Z_(I+1)=Z(JMAX)
          X_(I+1)=X_(I)
          Y_(I+1)=Y_(I)
          N_POINTS=I+1
          RETURN
       END IF
                   GO TO 1
        END


  SUBROUTINE NEW_DIRECTION(X_I,Y_I,Z_I,A,B,C,Q1,Q2,Q3,Q4)
! New Photon's direction (A,B,C (old) -> A,B,C (new) ) Aerosol+Molecular scattering.
  USE A_MOD_ShW
  REAL*8 A,B,C,W1,W2,W3,W4,W5,CUG
  COMMON/SCAT_POINT/SSMSST,LNUMB 
! ----------------------------- *
!  New direction of the photon (Without polarization) *
  1       W1=1.D0-2.D0*RAND()  ! COS & SIN : see Marchuk p.10 .
          W2=1.D0-2.D0*RAND()
          W3=W1**2 +W2**2
          IF(W3.GT.1D0.OR.W3.EQ.0.d0)GOTO 1
             IND=(IM4-1)*RAND()
		        IF(RAND().GT.SSMSST) THEN
      	         CUG=PROBFUN(LNUMB,IND) ! Aerosol scattering            
     	        ELSE
                 CUG=PROBMOL(IND) ! Molecular scattering
                END IF
          W3=DSQRT((1.D0-CUG**2)/W3)
          W4=W1*W3
          W5=W2*W3
          W1=A*W4-B*W5
!  New cosines *
          W3=(CUG-W1/(1.D0+DABS(C)))
          A=A*W3+W4
          B=B*W3-W5
          C=C*CUG-W1*SIGN(1.D0,C)
  END
 
      SUBROUTINE LAMBERT(X_I,Y_I,Z_I,A,B,C,P1,P2,P3,P4)
! Photon's reflection at the surface - Lambert's low. 
! New direct.and polar.(A,B,C,P1,...,P4 old > A,B,C,P1,...,P4 new).
! X_I,Y_I,Z_I- for future applications.
        REAL*8 A,B,C,W1,W2,W3
      P1=1.
      P2=1.
      P3=0.
      P4=0.
      W1=RAND()
      W2=RAND()
      C=MAX(W1,W2)  ! modelling P(X)=2X (see Ermakov p.25)
!                                  a=sqrt(1.-c**2)
!                                  b=0.
!                                  return
 1        W1=1.D0-2.D0*RAND()
          W2=1.D0-2.D0*RAND()
          W3=W1**2 +W2**2
          IF(W3.GT.1D0)GOTO 1
          W3=DSQRT((1.D0-C**2)/W3)
          A=W1*W3
          B=W2*W3
       END SUBROUTINE LAMBERT

 SUBROUTINE NEW_Z(X,Y,Z_Z,A,B,C0,IBOUND) ! Version of 31 Nov.,2005.
 USE INITIAL_SHW_CLOUD
 !!! USE A_MOD_SHW
! ------------------------------------------------------------ *
! To calculate Z-coordinate for the plane-parallel atmosphere. *
! C0 - cosine between Z-axis and a photon's direction.         *
! IBOUND = (-1,0,+1) if Z at the SURFACE and IN or OUT atmosph.*
! X,Y,A,B are not essential here (other angles and coordinates)*
! SSMSST - Smol/(Smol+Saeros), LNUMB - number of aerosol layer.*
! ------------------------------------------------------------ *
      REAL*8 A,B,C0
      COMMON/W_N/WAVE ! Wavenumber
      COMMON/SCAT_POINT/SSMSST,LNUMB 
	  SAVE N_LOY_J, TAU_POINT
      DATA WOLD/-1./
!* ----------------------------------------- *
     IF(WOLD.NE.WAVE)THEN ! New wavenumber
       WOLD=WAVE
	   RAY_FICT=SC_M(Z(1)) ! To obtain S_RLGH
	   D_UA=0.
        DO J=JMAX,2,-1
        ZZZ=(Z(J-1)+Z(J))*0.5
     CALL SC_AB_A(ZZZ,S_SCAT,S_ABS,LNUMB)
	    D_UA=D_UA+S_SCAT*(Z(J)-Z(J-1))
		S_RLGH(J-1)=S_RLGH(J-1)+D_UA
NUMBL(J-1)=LNUMB ; SIG_S_J(J-1)=S_SCAT 
        END DO
     END IF
! ---------------------------------------------------------------- *
      IF(Z_Z==Z(JMAX)) THEN 
	  TAU_POINT=0. ; N_LOY_J=JMAX-1
	  END IF
! ---
      DTAU=-ALOG(RAND())
      DTAU=DTAU*C0 ! Taking into account of the cosine.
 TAU_POINT=TAU_POINT-DTAU ! New TAU-point
! --- 
IF(C0 > 0.D0)THEN
  IF(TAU_POINT <=0.) THEN
! Photon flew away to the cosmos.
       IBOUND =1
       Z_Z=1000.
       RETURN
  END IF
! Upward photon.
			DO J=N_LOY_J,JMAX
            IF(TAU_POINT >= S_RLGH(J+1)) EXIT
			END DO
ELSE
	IF(TAU_POINT >= S_RLGH(1)) THEN
! Photon droped down on the surface.
          IBOUND=-1
          Z_Z=Z(1)
         SSMSST=0.  ; LNUMB=1 ; N_LOY_J=1 
         TAU_POINT = S_RLGH(1)
          RETURN
     END IF
! Downward photon.
			DO J=N_LOY_J,1,-1
            IF(TAU_POINT <=S_RLGH(J)) EXIT
			END DO
 END IF
			IBOUND=0 ; N_LOY_J=J ; LNUMB=NUMBL(J)

DSR=(S_RLGH(J+1)-S_RLGH(J))
IF(DSR==0.)THEN  ! ### 01.06/2021
Z_Z=(Z(J+1)+Z(J))/2.
ELSE
Z_Z=(TAU_POINT*(Z(J+1)-Z(J))+S_RLGH(J+1)*Z(J) -S_RLGH(J)*Z(J+1)) & 
     /(S_RLGH(J+1)-S_RLGH(J))
END IF

 C1=(Z(J+1)-Z_Z)/(Z(J+1)-Z(J)) ; C2=1.-C1
   SC_M_=RLGH(J)*C1+RLGH(J+1)*C2
   SC_TOT=SIG_S_J(J)+SC_M_
   SSMSST=SC_M_/SC_TOT
 END
 
 
FUNCTION SC_M(Z_I)  ! Venus (26 August, 2021.) 
  USE INITIAL_SHW_CLOUD
  REAL*4 PRO(300)
  COMMON/W_N/V
  SAVE R
   DATA VIBG/0./
 IF(VIBG /= V)THEN

 IF(VIBG==0.) THEN
PRO(1)=DERIV(Z(1),Z(2),Z(3),P1(1),P1(2),P1(3),Z(1))
JMAX_1=JMAX-1
DO J=2,JMAX_1
PRO(J)=DERIV(Z(J-1),Z(J),Z(J+1),P1(J-1),P1(J),P1(J+1),Z(J))
END DO
PRO(JMAX)=DERIV(Z(JMAX-2),Z(JMAX_1),Z(JMAX),P1(JMAX-2),P1(JMAX_1),P1(JMAX),Z(JMAX))
END IF

 VIBG=V
        BDA=10000./V
BDA2=1./BDA**2 
BDA4=BDA2**2
UR0=1.492*BDA4*(1.+0.013*BDA2)
UR0P=UR0/P1(1)
DO J=1,JMAX
RLGH(J)=UR0P*PRO(J)
END DO

S_RLGH(JMAX)=0.
DO J=JMAX_1,1,-1
S_RLGH(J)=S_RLGH(J+1)+(Z(J+1)-Z(J))*(RLGH(J)+RLGH(J+1))*0.5
END DO
END IF

SC_M=0. 
DO J=2,JMAX
IF(Z(J) >= Z_I)EXIT 
END DO
 C1=(Z(J)-Z_I)/(Z(J)-Z(J-1)) ; C2=1.-C1
   SC_M=RLGH(J-1)*C1+RLGH(J)*C2
END

FUNCTION DERIV(X1,X2,X3,Y1,Y2,Y3,X)
YY1=Y1/((X1-X2)*(X1-X3))
YY2=Y2/((X2-X1)*(X2-X3))
YY3=Y3/((X3-X1)*(X3-X2))
DERIV=-(2.*(YY1+YY2+YY3)*X-((X2+X3)*YY1+(X1+X3)*YY2+(X1+X2)*YY3))

END