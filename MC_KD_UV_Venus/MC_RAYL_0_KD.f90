 MODULE M_C
 REAL*4 X0_,Y0_,Z0_,P1_0,P2_0,P3_0,P4_0 !Coordinates and Stokes parameters of initial ray.
 REAL*8 A0_,B0_,C0_ &  !  COS of initial ray.
        ,X_RAND        ! For the random data
 INTEGER*4 N_POINTS ! The real number of scattering points in the trajectory.
 PARAMETER (IMAX_MC=998,IMAX_M_=IMAX_MC+2) !The maximal number of scattering points in the trajectory (cut off).
 REAL*4 X_(IMAX_M_),Y_(IMAX_M_),Z_(IMAX_M_),COS1_(IMAX_M_),COS2_(IMAX_M_),COS3_(IMAX_M_) &
 ,P1_(IMAX_M_),P2_(IMAX_M_),P3_(IMAX_M_),P4_(IMAX_M_) ! Trajectory points.
  ALLOCATABLE ALB(:),QQ(:),TAUMA(:),CL0(:),CL1(:),DIST(:,:) &
,PIST(:,:),AERRAB(:),LMIN(:),DL0(:),DL1(:),CL1I(:),CL0I(:),LMINV(:) &
,CL0V(:),CL1V(:),DL0V(:),DL1V(:)

 CONTAINS
      FUNCTION RAND()
      REAL*8 dm37
      DATA dm37 /137438953472./
          X_RAND=X_RAND*3125.
          X_RAND=MOD(X_RAND,dm37)
          RAND=X_RAND/dm37
      END  FUNCTION RAND

 END MODULE M_C

!PROGRAM START
!use initial_
!USE M_C
!use a_mod
!      COMMON/W_N/WAVE ! Wavenumber 123456789

! EXTERNAL NEW_Z,NEW_DIRECTION,LAMBERT
 
! X_RAND=130000000001.D0

! call sta
! call FILES_CREATING 
! WAVE=18181.81818
!CALL LAYER_PROPERTY(WAVE)
!DO JK=1,100
! X0_=0. ; Y0_=0. ; Z0_=100.D0
! A0_=0.D0 ; B0_=DSQRT(3.D0)/2.D0 ; C0_=-0.5D0
!  CALL MONTE_CARLO(NEW_Z,NEW_DIRECTION,LAMBERT)
! 
!  DO I=1,N_POINTS
!  WRITE(*,*)I,X_(I),Y_(I),Z_(I)
!  WRITE(*,*)COS1_(I),COS2_(I),COS3_(I)
!  PAUSE
!  END DO
!END DO
!END        





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
USE INITIAL_SHW_KD
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
  USE A_MOD
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

 SUBROUTINE NEW_Z(X,Y,Z_Z,A,B,C0,IBOUND) ! 23 Dec.,2003.
 USE INITIAL_SHW_KD
 USE A_MOD
! ------------------------------------------------------------ *
! To calculate Z-coordinate for the plane-parallel atmosphere. *
! C0 - cosine between Z-axis and a photon's direction.         *
! IBOUND = (-1,0,+1) if Z at the SURFACE and IN or OUT atmosph.*
! X,Y,A,B are not essential here (other angles and coordinates)*
! SSMSST - Smol/(Smol+Saeros), LNUMB - number of aerosol layer.*
! Atmosphere up to HMAX km will be divided by a set of the     *
! HOMOGENEOUS layers with the thikness of SZ km (see below)    *        
! ------------------------------------------------------------ *
      PARAMETER (SZ=0.2,HMAX=150.,N_LOYR=HMAX/SZ +200) !
! Mol. scat. up to 50 km, number of grid points can contain 2*100 aeros. boundaries. 	     
      REAL*8 A,B,C0
   DIMENSION QZZ(N_LOYR) & ! Molecular + Aerosol scattering coeff.
   ,ZB(0:N_LOYR) & ! Boundaries
   ,SMST(N_LOYR),NUMBL(N_LOYR) ! Smol/(Smol+Saeros) and number of aerosol layer.
      COMMON/W_N/WAVE ! Wavenumber
      COMMON/SCAT_POINT/SSMSST,LNUMB 
	  save n_lo, GAS_CLOUD,zb,zc,jga,N_LOM
      DATA WOLD/-1./
!* ----------------------------------------- *
 IF(WOLD.EQ.-1.)THEN ! Altitude grid.
      WOLD=0.
      N_LO=HMAX/SZ+1
      DO I=0,N_LO
      ZB(I)=Z(1)+SZ*I
      END DO 
! ---- Inserting boundaries of aerosol layers ------- !
             GAS_CLOUD=0.001 ! [KM] (If /Z_CL-Z/ < GAS_CLOUD levels are the same) 
  
! Lower boundaries 
  DO ICL=1,NLAY
		ZC=ZDOWN(ICL)
		 DO JGA=1,N_LO
		 JGAM=JGA-1
		 IF(ZB(JGAM)<=ZC.AND.ZC<ZB(JGA)) EXIT 
         END DO
         IF(JGA > N_LO) THEN
         WRITE(*,*)' Aerosol layers inseting is wrong !!!' ; PAUSE ; STOP
         END IF
! ZB(J-1)=ZC
   IF(ABS(ZB(JGAM)-ZC)<GAS_CLOUD) THEN
		  ZB(JGAM)=ZC
	
    ELSE
! Z(J)=ZC
           IF(ABS(ZB(JGA)-ZC)<GAS_CLOUD) THEN ! ###### Attention ! ##### 
		   ZB(JGA)=ZC

		   ELSE
! Z(J-1)< ZC < Z(J) - Inserting
                   ZB(JGA)=ZC
                   JGA=JGA+1
                 DO N_LO=JGA,100000
                   ZB(N_LO)=ZB(N_LO-1)+SZ
                 IF(ZB(N_LO) > HMAX) EXIT
                 END DO
            END IF 
     END IF
        END DO			 
! Upper Boundaries 
  DO ICL=1,NLAY
		ZC=ZUP(ICL)
		 DO JGA=1,N_LO
		 JGAM=JGA-1
		 IF(ZB(JGAM)<=ZC.AND.ZC<ZB(JGA)) EXIT 
         END DO
         IF(JGA > N_LO) THEN
         WRITE(*,*)' Aerosol layers inseting is wrong !!!' ; PAUSE ; STOP
         END IF
! ZB(J-1)=ZC
   IF(ABS(ZB(JGAM)-ZC)<GAS_CLOUD) THEN
		  ZB(JGAM)=ZC
	
   ELSE
! Z(J)=ZC
           IF(ABS(Z(JGA)-ZC)<GAS_CLOUD) THEN
		   ZB(JGA)=ZC

		   ELSE
! Z(J-1)< ZC < Z(J) - Inserting
                   ZB(JGA)=ZC
                   JGA=JGA+1
                 DO N_LO=JGA,100000
                   ZB(N_LO)=ZB(N_LO-1)+SZ
                 IF(ZB(N_LO) > HMAX) EXIT
                 END DO
		   END IF
    END IF
  END DO	
		N_LOM=N_LO		 

 END IF ! end of grid obtaining

      IF(WOLD.NE.WAVE)THEN ! New wavenumber
       WOLD=WAVE
!  Obtaining QZZ(), etc.  *

        DO I=1,N_LOM
        ZZZ=(ZB(I-1)+ZB(I))*0.5
     CALL SC_AB_A(ZZZ,S_SCAT,S_ABS,LNUMB)
           SSM=SC_M(ZZZ)
          SC_TOT=S_SCAT+SSM
	  QZZ(I)=SC_TOT
	 IF(SSM>0.00001) THEN  !### 02.06.2021
          SMST(I)=SSM/SC_TOT
	  ELSE !### 02.06.2021
          SMST(I)=0. !### 02.06.2021
	  END IF !### 02.06.2021
          NUMBL(I)=LNUMB
         END DO
!#### 2.06.2021         ZMX=ZB(N_LOM)-0.001 ! km-the altitude of the starting point of the trajec.
ZMX=Z(JMAX)-0.001   !#### 2.06.2021 
 do nnn=N_LOM,1,-1  !#### 2.06.2021 
 if(zb(nnn)<zmx)exit !#### 2.06.2021 
 end do              !#### 2.06.2021 
il_max=nnn           !#### 2.06.2021  
     END IF
! ---------------------------------------------------------------- *
      IF(Z_Z.GE.ZMX) THEN
!####         IL=N_LOM -1 !#### 2.06.2021  IL=N_LOM 
 !####        ZIL=ZB(IL)    !#### 2.06.2021 HMAX
 il=il_max ; zil=zb(il_max)
       END IF
! ---
      DTAU=-ALOG(RAND())
      DTAU=DTAU*ABS(C0) ! Taking into account of the cosine.
 
     IF(C0.LE.0.D0)THEN ! Photon is falling down *
 10   CONTINUE
         ZIL=ZB(IL-1)
         DELTAZ=Z_Z-ZIL
          DT=QZZ(IL)*DELTAZ
         DTAU=DTAU-DT
        IF(DTAU.LE.0.)THEN
         Z_Z=ZIL-DTAU/QZZ(IL)
         IBOUND=0
   SSMSST=SMST(IL)  ; LNUMB=NUMBL(IL)       
         RETURN
        END IF

        IF(IL.EQ.1)THEN  ! Photon has fallen down on the surface *
          IBOUND=-1
          Z_Z=Z(1)
         SSMSST=0.  ; LNUMB=1  
          RETURN
          END IF
        Z_Z=ZIL
        IL=IL-1
        GOTO 10
 
     ELSE ! Photon is flying up
20     CONTINUE
       ZIL=ZB(IL)
       DELTAZ=ZIL-Z_Z
       DT=DELTAZ*QZZ(IL)
       DTAU=DTAU-DT
       IF(DTAU.LE.0.)THEN
       Z_Z=ZIL+DTAU/QZZ(IL)
       IBOUND=0
      SSMSST=SMST(IL)  ; LNUMB=NUMBL(IL)  
       RETURN
       END IF
       IF(IL.EQ.N_LOM) THEN ! Photon is in the cosmos .
       IBOUND =1
       Z_Z=1000.
       RETURN
       END IF
        IL=IL+1
        Z_Z=ZIL
        GOTO 20
        END IF
       END

   FUNCTION SC_M(Z_I) ! 12 March, 1999 - 29 December, 2003.
!***********************************************************************
!     CALCULATES MOLECULAR RAYLEIGH SCATTERING COEFFICIENT             *
! USES APPROXIMATION by Fisher J.,1996 (APPL OPT,v35,n12,pp.1931-1941) *
!     WITH THE various DEPOLARIZATIONS                                 *
!     V = WAVENUMBER(CM-1)                                             *
!     SC_M = MOLECULAR SCATTERING COEFFICIENT (KM-1)                   *
!  The atmspheric models is the same that for LBL calculations         *  
!***********************************************************************
 USE INITIAL_SHW_KD
 PARAMETER (D_U=0.25,JMD=1000) ! Step in uniform grid for Molec. Scatt. Calc.
 DIMENSION P_U(0:JMD),T_U(0:JMD)
  COMMON/W_N/V
   DATA VIBG/0./
 IF(VIBG /= V)THEN

     IF(VIBG == 0) THEN ! Uniform Z - grid.
        I=0
         P_U(I)=P(1)/1013.25
         T_U(I)=T(1)
         DO J=2,JMAX
        ZA=Z(J-1)
        PA=P(J-1)/1013.25
        TA=T(J-1)
        ZB=Z(J)
        PB=P(J)/1013.25
        TB=T(J)
                  DO II=1,10000
                  ZZ=Z(1)+D_U*(I+1)
                  IF(ZZ.GT.ZB)GOTO 1
                  I=I+1
                  C2=(ZZ-ZA)/(ZB-ZA)
                  C1=1.-C2
         P_U(I)=PA*C1+PB*C2
         T_U(I)=TA*C1+TB*C2
                  IF(I.EQ.JMD)GOTO 2
                  END DO
   1    CONTINUE
         END DO
   2    CONTINUE
     END IF

 VIBG=V
        BDA=10000./V
!        IF(BDA.GT.4.)THEN
!        R=10.21675E-4/BDA**4*288.15
!        ELSE  
!                IF(BDA.LE.0.5) THEN
!      R=7.68246E-4/BDA**(3.55212+1.35579*BDA+0.11563/BDA)*288.15
!                ELSE
!      R=10.21675E-4/BDA**(3.99668+1.10298E-3*BDA+0.0271393/BDA)*288.15          
!                END IF
!        END IF

		 R=0.0  ! ### without Molecular Scattering

! ### 		 R=63.592
! ###     if(v.lt.4400.) r=0.0045725
 ! ###      if(v.lt.8200..and.v.ge.4400.)r=0.059823
 ! ###        if(v.lt.14280..and.v.ge.8200.)r=0.53820
  ! ###         if(v.lt.31000..and.v.ge.14280.)r=6.4063
 ! ###           if(v.lt.33000..and.v.ge.31000.)r=34.482

 END IF
 I=(Z_I-Z(1))/D_U
 ZZ=Z(1)+D_U*I
 C2=(Z_I-ZZ)/D_U ; C1=1.-C2
 PPP=P_U(I)*C1+P_U(I+1)*C2
 TTT=T_U(I)*C1+T_U(I+1)*C2
   SC_M=R*PPP/TTT
   END

