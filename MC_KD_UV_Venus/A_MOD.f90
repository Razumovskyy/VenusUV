 


  MODULE A_MOD ! 27 Nov.,2003
   REAL*8 Pi,ST_ANG
   PARAMETER  (IHOL=80,Pi=3.14159265359 &
   ,IM4=1000 & ! DIMENSION of the Arrays for ANGLE scattering simulation. 
   ,I_FCTR=1) ! I_FCTR=4 for other FORTRAN. 
  ALLOCATABLE :: E55(:),ZDOWN(:),ZUP(:) &! Extinction and boundaries in each layer
 ,PH_F_I(:),ST_ANG(:),WFRAC(:,:) & ! Array over STandard ANGle grid, weights of the fractions. 
 ,EFT(:,:),RHT(:,:),EFT_BD(:),RHT_BD(:),SMES(:), PROBFUN(:,:),SCATT(:),ABSS(:),EXTT(:) &
 ,SCEFT(:),ABEFT(:),SCRHT(:),ABRHT(:),PH_LAER(:),PROBMOL(:)
! Cheeks (NomA,IMEN_PHF)x2 ; boundaries(NomA)x2, smes(0:IM4), probfun(NLAY,0:IM4),s(NLAY),a(NLAY)
!                                                             probmol(0:im4)
  CHARACTER*IHOL, ALLOCATABLE :: ALIST(:),BLIST(:) ! Aerosol's Name-Path in temporary database.
                              ! Files with Sigma and Int.Phase Functon. 
  ALLOCATABLE :: LIST(:),NUFRAC(:), &! Name-pathes catalog, Number of fractions in each layer.
  IEFT_BD(:),IRHT_BD(:) ! REC numbers (NomA)
  INTEGER*4 NLAY, IMEN_PHF, IDIF
 ! Number of Layers with different Phase Functions, DIM for STANDARD grid, Number of Aerosols    
  REAL*8 WN55 ! Given wavenumber cm^-1 (usually 18181.818... = 0.55 mkm)

  CONTAINS

   SUBROUTINE FILES_CREATING   ! 10 Nov.,2003
! Creating and reading FILES with Atmospheric Optical Properties for Monte-Carlo !
    REAL*8 COP,ANG,C
  CHARACTER*IHOL AZL,NAME_W,NAME_W2
  CHARACTER*2 HVOST(99)
  CHARACTER*IHOL, ALLOCATABLE :: A_ZOL(:,:) ! Aerosol Name-Path 
  ALLOCATABLE  COP(:),PHF(:)  ! Aerosol Angle grid and Phase function in permanent DATABASE. 

! 1 --------------------------------------- ! 

      IM_PHF=1 ; ALLOCATE(COP(0:IM_PHF),PHF(0:IM_PHF))
    OPEN(10,FILE='ATM_AEROSOL.MODEL') ! input File - AEROSOL (CLOUD) in Atmosphere
    OPEN(9,FILE='AER_MOD.PROTOCOL')   ! output File for control

! --- Standard ANGLE grid --- !
       OPEN(8,FILE='./M-C_OPT_BASE/STAND_ANGLE.GRID')
      DO IMEN_PHF=0,1000000
       READ(8,*)ANG
	   IF(ANG > 179.99) EXIT
      END DO
   ALLOCATE (PH_F_I(0:IMEN_PHF),ST_ANG(0:IMEN_PHF))
        REWIND(8)
        DO I=0,IMEN_PHF
        READ(8,*)ANG
       ST_ANG(I)=DCOS(Pi*ANG/180.D0)
        END DO
        CLOSE(8)
! *****  Reading information about  AEROSOL (CLOUD) in Atmosphere ***** !
    READ(10,*) NLAYER ; WRITE(9,*)NLAYER ! Number of Layers with different Phase Functions
   NLAY=ABS(NLAYER) 
   DO NL=1,NLAY
   HVOST(NL)='  '
   IF(NL < 10 )THEN
   WRITE(HVOST(NL)(1:1),'(I1)')NL
                  ELSE
   WRITE(HVOST(NL)(1:2),'(I2)')NL
   END IF
   END DO
 ALLOCATE(E55(NLAY),ZDOWN(NLAY),ZUP(NLAY),NUFRAC(NLAY)) ! Ext.(0.55 mkm), Boundaries (KM), Frac.Numb.

! This part to DEFINE MAXimal number of fractions in complex aerosol (for dimension) !
   READ(10,*)WN55 ; WN55=-WN55
   READ(10,*)E555  
      ILM=0
      DO I=1,NLAY
	  READ(10,*)N_TEC
      READ(10,*)IL 
      IF(IL > ILM) ILM=IL
      READ(10,*) WWW
        DO J=1,IL
        READ(10,8)AZL 
        END DO
 8     FORMAT(A80)
      END DO

    ALLOCATE (A_ZOL(NLAY,ILM),WFRAC(NLAY,ILM)) ! Names of aerosol fractions and weights
    REWIND(10)
    READ(10,*) NLAYER             ! Number of Layers 
   READ(10,*)WN55 ; WRITE(9,*)WN55, ' WaveNumber (cm^-1)' ; WN55=-WN55
    READ(10,*)E55 ; WRITE(9,*)E55 ! Extinctions at 0.55 mkm in each layer
      DO I=1,NLAY
	  READ(10,*)N_TEC,ZDOWN(I),ZUP(I)      ! Boundaries in km
	  WRITE(9,*)N_TEC,ZDOWN(I),ZUP(I)
      READ(10,*)NFRAC ; WRITE(9,*)NFRAC        ! Number of fractions
	  NUFRAC(I)=NFRAC
      READ(10,*) (WFRAC(I,J),J=1,NFRAC)        ! their Weights
      WRITE(9,*) (WFRAC(I,J),J=1,NFRAC)
	
                SW=0. ! Sum of weights control
	        DO J=1,NFRAC
			SW=SW+WFRAC(I,J)
	        END DO
			WRITE(9,*)' SUM Weights = ',SW
			IF(SW < 0.9999 .OR. SW > 1.0001) THEN
			WRITE(*,*)I,'-th fraction! ATTENTION: SUM of WEIGHTS = ', SW
			PAUSE
			END IF

        DO J=1,NFRAC
        READ(10,8)A_ZOL(I,J) ; WRITE(9,8)A_ZOL(I,J) ! path to aerosol model in database
        END DO
       END DO
    CLOSE (10)
    CLOSE(9) 
! 2 ***** Information about AEROSOL (CLOUD) in Atmosphere has been written ***** ! 

! 2 **** Files with CROSS-SECTIONS and INTEGRATED PHASE-FUNCTIONS for aerosols in each layer **** !
       DO NL=1,NLAY
	   NFRAC=NUFRAC(NL)
	    DO IFRAC=1,NFRAC
       AZL=A_ZOL(NL,IFRAC)
	   NAME_W='./M-C_OPT_BASE/S_'
      DO J=1,IHOL
      IF(AZL(J:J) == ' ')EXIT 
      IF(AZL(J:J) == '\' .OR. AZL(J:J) == '/')JMIN=J
      END DO
      JMIN=JMIN+1 ; JMAX=J-1 ; LNG=JMAX-JMIN+1 ; LNG=LNG+18
	  NAME_W(18:LNG)=AZL(JMIN:JMAX) ! file-Name definition (with the cross-sections)

! ------------------------------------------------------------------------------------ ! 
  ! File structure :
  ! 1. (Number wavenumber points. 
  ! 2. (Wavenumber, Scattering coeff., Absorption coeff.)
  ! 3. (Wavenumber, Scattering coeff., Absorption coeff.)
  ! ....   etc.
   IRECLSS=3*1*I_FCTR ! 3 values, odinary precision, FORTRAN
   OPEN(2, ACCESS='DIRECT', FORM='UNFORMATTED',RECL = IRECLSS, FILE=NAME_W,ERR=5)
! ------------------------------------------------------------------------------------ ! 
      A_ZOL(NL,IFRAC)=NAME_W
      READ(2,REC=1,ERR=5)N_ZAP
	  WRITE(*,*)' "DIRECT ACCESS" file EXISTS !!!'
	  GOTO 3
 5     WRITE(*,*)' "DIRECT ACCESS" File should be created !!! '
!       PAUSE 3	
! --- Creating files with aerosol optical properties for M-C calculations  !
  NAME_W2=NAME_W ; WRITE(NAME_W2(16:16),'(A1)')'I'
   WRITE(*,*)NAME_W2  !; PAUSE 100 
! ------------------------------------------------------------------------------------ ! 
!   File structure :
! 1. - - - - - - - - -  empty - - - - - -
! 2. (Itegrated Phase Function over the "standard COS(angle)-grid,IMEN_PHF+1 points)
! 3. (Itegrated Phase Function over the "standard COS(angle)-grid,IMEN_PHF+1 points) 
  ! ....   etc.
! Each record corresponds the wavenumber point.
   IRECLII=(IMEN_PHF+1)*1*I_FCTR ! number of values, odinary precision, FORTRAN
   OPEN(3, ACCESS='DIRECT', FORM='UNFORMATTED',RECL = IRECLII, FILE=NAME_W2,ERR=5)
! ------------------------------------------------------------------------------------ ! 
       OPEN(11,FILE=AZL)
	    DO NHJ=2,1000000
       READ(11,*,END=101)BDA,RN_PH,SS,SE,SA
       WRITE(2,REC=NHJ)10000./BDA,SS,SA
       N_PH=RN_PH+0.1
!  PH_F_I(0:IMEN_PHF) 
        IF(N_PH > IM_PHF ) THEN
		DEALLOCATE (COP,PHF)
		IM_PHF=N_PH
        ALLOCATE(COP(0:IM_PHF),PHF(0:IM_PHF))
		END IF
        DO IANG=0,N_PH
        READ(11,*)ANG,STO1,STO2 ! Stokes paramters from ASCII-file
		ANG=DCOS(ANG*Pi/180.D0)
		COP(IANG)=ANG
		PHF(IANG)=0.5*(STO1+STO2)
        END DO


! --- Non-Integrated Phase Function over the standard angle grid --- !
   CALL GRID_GRID(N_PH+1,PHF,COP,IMEN_PHF+1,PH_F_I,ST_ANG)

!------ Phase Function Integration and Normalizing -----!
       P1B=PH_F_I(IMEN_PHF)
       PH_F_I(IMEN_PHF)=0.D0
       DO N=IMEN_PHF,1,-1
       P1A=PH_F_I(N-1)
       C=0.5D0*(ST_ANG(N-1)-ST_ANG(N))
       PH_F_I(N-1)=PH_F_I(N)+C*(P1A+P1B)
       P1B=P1A
       END DO 
       DO N=IMEN_PHF,0,-1
       PH_F_I(N)=PH_F_I(N)/PH_F_I(0) ! Normalizing
       END DO
!---------------------------------------!


  WRITE(3,REC=NHJ)PH_F_I ! Writting the Integrated Phase Function, STANDARD angle grid.
	
       END DO
 101   CLOSE(11)
       NHJ=NHJ-1
	   WRITE(2,REC=1)NHJ
 3    CONTINUE
      CLOSE (2)
	  CLOSE (3)
 	    END DO
       END DO
       CLOSE(10)
! **** File with CROSS-SECTIONS for each aerosol fractions has been created **** !
       DEALLOCATE (COP,PHF)

! 3 ---- Calculations of different aerosol species ---- !
       IAT=0 
  DO I=1,NLAY
     NFRAC=NUFRAC(I)
          DO J=1,NFRAC
          IAT=IAT+1
          END DO
  END DO
  ALLOCATE (ALIST(IAT),LIST(IAT),BLIST(IAT))
     NFRAC=NUFRAC(1)
     IT=0
          DO J=1,NFRAC
          IT=IT+1
         ALIST(IT)=A_ZOL(1,J) ! Different aerosol in the first layer
          LIST(IT)=IT
          END DO
 IDIF=IT
  IF(NLAY > 1) THEN
  DO I=2,NLAY
     NFRAC=NUFRAC(I)
          DO J=1,NFRAC
          IT=IT+1
          AZL=A_ZOL(I,J)
 
           NCA=0
           DO JK=1,IDIF
           IF(AZL == ALIST(JK))THEN
           NCA=JK
		   EXIT
           END IF
           END DO
 
          IF(NCA > 0) THEN
		  LIST(IT)=NCA
		  ELSE
		  IDIF=IDIF+1
          LIST(IT)=IDIF
          ALIST(IDIF)=AZL
		  END IF
          END DO
  END DO
  END IF
!!!!!  IRECLSS=3*2*I_FCTR ! 3 values, double precision, FORTRAN
!!!!!  IRECLII=(IMEN_PHF+1)*2*I_FCTR ! number of values, double precision, FORTRAN
  IRECLSS=3*I_FCTR ! 3 values, ODINARY precision, FORTRAN
  IRECLII=(IMEN_PHF+1)*I_FCTR ! number of values, ODINARY precision, FORTRAN
  DO I=1,IDIF
  BLIST(I)=ALIST(I)
  WRITE(BLIST(I)(16:16),'(A1)')'I'
  NU_FS=200+I ; NU_FI=300+I
  OPEN(NU_FS, ACCESS='DIRECT', FORM='UNFORMATTED',RECL = IRECLSS, FILE=ALIST(I),ERR=200)
  OPEN(NU_FI, ACCESS='DIRECT', FORM='UNFORMATTED',RECL = IRECLII, FILE=BLIST(I),ERR=300)
  END DO
! ------------------------------- !
  ALLOCATE (EFT(IDIF,0:IMEN_PHF),RHT(IDIF,0:IMEN_PHF),EFT_BD(IDIF),RHT_BD(IDIF), &
  SMES(0:IM4), PROBFUN(NLAY,0:IM4),SCATT(NLAY),ABSS(NLAY),EXTT(NLAY),PH_LAER(0:IMEN_PHF), &
  IEFT_BD(IDIF),IRHT_BD(IDIF),SCEFT(IDIF),ABEFT(IDIF),SCRHT(IDIF),ABRHT(IDIF), &
  PROBMOL(0:IM4))

! 4 -------- The first position of the "cheeks" ------- !
  IEFT_BD=2
  IRHT_BD=3 
  DO I=1,IDIF
  NU_FS=200+I ; NU_FI=NU_FS+100
  READ(NU_FS,REC=IEFT_BD(I))EFT_BD(I),SCEFT(I),ABEFT(I)
  READ(NU_FS,REC=IRHT_BD(I))RHT_BD(I),SCRHT(I),ABRHT(I)
  READ(NU_FI,REC=IEFT_BD(I))(EFT(I,J),J=0,IMEN_PHF)
  READ(NU_FI,REC=IRHT_BD(I))(RHT(I,J),J=0,IMEN_PHF)
  END DO

! 5 --------- Probability function for the molecular scattering ------------ !
      DELTA=0.0279 ! Depolarization factor.
                  PPP=((1.+DELTA)/(1.-DELTA))**3
                  QQQ1=(4.+2.*DELTA)/(1.-DELTA)
                  QQQ2=4.*(2.+DELTA)/(1.-DELTA)
        DO I=0,IM4
        FFF=(1.D0/IM4)*I
        QQQ=-(QQQ1-QQQ2*FFF)*0.5
        QQQQQQ=PPP+QQQ**2
        QQQQQQ=SQRT(QQQQQQ)
        YYY=QQQ+QQQQQQ
          IF(YYY.GE.0.D0)THEN
           YYY=YYY**(1./3.)
          ELSE
           YYY=-(-YYY)**(1./3.)
          END IF
        ZZZ=QQQ-QQQQQQ
          IF(ZZZ.GE.0.D0)THEN
           ZZZ=ZZZ**(1.D0/3.)
          ELSE
           ZZZ=-(-ZZZ)**(1./3.)
          END IF
        PROBMOL(I)=YYY+ZZZ
        END DO
        PROBMOL(IM4)=1.0
        PROBMOL(0)=-1.0
! 6 -----------------------------------------------------------------------!

  RETURN

200 WRITE(*,*)' OPEN error in ',NU_FS ; PAUSE ; STOP  
300 WRITE(*,*)' OPEN error in ',NU_FI ; PAUSE ; STOP  

  END SUBROUTINE FILES_CREATING 
 
 SUBROUTINE INTEGR_PH_F(CUGOL,F1,NP,U1,N1) ! 24 Nov.,2003.
 REAL*8 CUGOL, A1,A2
!* ------------------------------------------------------- *
!   U1(0:N1)-Array for PHASE FUNCTION simulation           *
!  F1(0:NP)-INTEGRATED PHASE FUNCTION, CUGOL(0:NP)- COSINE *
!* --------------------------------------------------------*
          DIMENSION CUGOL(0:NP),F1(0:NP),U1(0:N1)
! ----------------------------- !
          I1=NP
          I2=NP-1
          A1=CUGOL(I1)
          A2=CUGOL(I2)
          F1O=F1(I1)
          F1N=F1(I2)
          U1(0)=-1.D0
          D=F1(0)/N1
        DO N=1,N1-1
          C=D*N
          IF(C.GT.F1N)THEN
  1       I1=I1-1
          I2=I2-1
          A1=CUGOL(I1)
          A2=CUGOL(I2)
          F1O=F1(I1)
          F1N=F1(I2)
          IF(C.GT.F1N)GOTO 1
          END IF 
          DA=(C-F1O)*(A2-A1)/(F1N-F1O)
          U1(N)=A1+DA
        END DO 
          U1(N1)=1.0
        END SUBROUTINE INTEGR_PH_F
 SUBROUTINE GRID_GRID(N1,Y1,X1,N2,Y2,X2)
 REAL*8 X1,X2,A,B,C,D
 DIMENSION Y1(N1),Y2(N2),X1(N1),X2(N2)
! ----------------------------------------------- !
! 25 November,2003.                               !
! Y1-values over X1-grid (N1 points) to           !
! Y2-values over X2-grid (N2 points).             !
! Attention !!! X1(1)=X2(1)  and X1(N1)=X2(N2)    !                
! LINEAR interpolation is used.                   !
! ----------------------------------------------- !
 
 A=X1(1) ; YA=Y1(1) ; J=2 ; B=X1(J) ; YB=Y1(J)
 D=B-A; Y2(1)=YA 
 DO I=2,N2
 X=X2(I) 
1 CONTINUE
  IF(X > A .AND. X > B .OR. X < A .AND. X < B) THEN
 A=B; YA=YB
    J=J+1 ; J=MIN0(J,N1)
    B=X1(J) ; YB=Y1(J)
    D=B-A
  END IF
 IF(X > A .AND. X > B .OR. X < A .AND. X < B) GOTO 1
  C=(X-A)/D
 Y2(I)=YA*(1.-C)+YB*C
 END DO
 Y2(N2)=Y1(N1)
 END SUBROUTINE GRID_GRID
 END MODULE A_MOD

  SUBROUTINE LAYER_PROPERTY
! **** Creating Aerosol Optical Properties for the given WaveNumber. **** !
  USE A_MOD
	  COMMON/W_N/WN
! ------ "Cheek" setting ------- !
 1 CONTINUE
   IF(WN55 < 0.) THEN
   W=-WN55
                 ELSE
   W=WN
   END IF
   DO I=1,IDIF
   W1=EFT_BD(I) ; W2=RHT_BD(I) ; I1=IEFT_BD(I); I2=IRHT_BD(I)
   IF(W1 > W2 ) THEN
     IF(W > W1) THEN
     NU_FS=200+I 
     IB=IEFT_BD(I)-1
	  DO II=IB,2,-1
	  READ(NU_FS,REC=II)EFT_BD(I),SCEFT(I),ABEFT(I)	
	  IF(EFT_BD(I) >= W)EXIT 
	  END DO
                     IF(II <= 2) II=2
      I1=II ; I2=I1+1
	  IEFT_BD(I)=I1 ; IRHT_BD(I)=I2
	   READ(NU_FS,REC=I2)RHT_BD(I),SCRHT(I),ABRHT(I)
       NU_FI=NU_FS+100
  READ(NU_FI,REC=I1)(EFT(I,J),J=0,IMEN_PHF)
  READ(NU_FI,REC=I2)(RHT(I,J),J=0,IMEN_PHF)
      ELSE 
	    IF(W < W2) THEN
     NU_FS=200+I 
     IA=IRHT_BD(I)+1
       DO II=IA,10000
	   READ(NU_FS,REC=II, ERR=2110)RHT_BD(I),SCRHT(I),ABRHT(I)
	              IF(RHT_BD(I) <= W)EXIT
       END DO
	   GOTO 2111
 2110  II=II-2
 2111  I2=II ; I1=I2-1
	   IEFT_BD(I)=I1 ; IRHT_BD(I)=I2
	   READ(NU_FS,REC=I1)EFT_BD(I),SCEFT(I),ABEFT(I)
       NU_FI=NU_FS+100
  READ(NU_FI,REC=I1)(EFT(I,J),J=0,IMEN_PHF)
  READ(NU_FI,REC=I2)(RHT(I,J),J=0,IMEN_PHF)

	   END IF   
      END IF     

   ELSE ! W1 < W2
	 IF(W > W2) THEN
     NU_FS=200+I 
     IA=IRHT_BD(I)+1
       DO II=IA,10000
	   READ(NU_FS,REC=II, ERR=2100)RHT_BD(I),SCRHT(I),ABRHT(I)
         	   IF(RHT_BD(I) >= W)EXIT
       END DO
	   GOTO 2101
 2100  II=II-1
 2101  I2=II ; I1=I2-1
	   IEFT_BD(I)=I1 ; IRHT_BD(I)=I2
	   READ(NU_FS,REC=I1)EFT_BD(I),SCEFT(I),ABEFT(I) ! 
       NU_FI=NU_FS+100
  READ(NU_FI,REC=I1)(EFT(I,J),J=0,IMEN_PHF)
  READ(NU_FI,REC=I2)(RHT(I,J),J=0,IMEN_PHF)
	   
 	 ELSE 
	       IF(W < W1) THEN
     NU_FS=200+I 
     IB=IEFT_BD(I)-1
	  DO II=IB,2,-1
	  READ(NU_FS,REC=II)EFT_BD(I),SCEFT(I),ABEFT(I)	
	              IF(EFT_BD(I) <= W)EXIT  
	  END DO
      IF(II <= 2) II=2
      I1=II ; I2=I1+1
	  IEFT_BD(I)=I1 ; IRHT_BD(I)=I2
	   READ(NU_FS,REC=I2)RHT_BD(I),SCRHT(I),ABRHT(I)
       NU_FI=NU_FS+100
  READ(NU_FI,REC=I1)(EFT(I,J),J=0,IMEN_PHF)
  READ(NU_FI,REC=I2)(RHT(I,J),J=0,IMEN_PHF)
               END IF
	  END IF           
 END IF	

   END DO
! ---------------------------------------- !
! **************** Optical properties of the layers ************* !
       IAT=0 
  DO NL=1,NLAY
     NFRAC=NUFRAC(NL)
	      SUM_A=0.D0 ; SUM_S=0.D0  
     PH_LAER=0.D0
          DO NF=1,NFRAC
          IAT=IAT+1
		  IA=LIST(IAT)
! ----- Cross  Sections at given Wave-Number ---------- !
          C2=(W-EFT_BD(IA))/(RHT_BD(IA)-EFT_BD(IA)) ; C1=1.D0-C2
		  T_S=(C1*SCEFT(IA)+C2*SCRHT(IA))*WFRAC(NL,NF)
		  T_A=(C1*ABEFT(IA)+C2*ABRHT(IA))*WFRAC(NL,NF)
		  T_E=T_S+T_A
		  SUM_A=SUM_A+T_A
          SUM_S=SUM_S+T_S
     PH_LAER=PH_LAER+T_S*(C1*EFT(IA,:)+C2*RHT(IA,:)) ! Phase Function in each layer.
          END DO
     IF(WN55 > 0.) THEN 
	 SCATT(NL)=SUM_S * E55(NL)   ! Result - Volume scattering coefficient in ech layer
	 ABSS(NL)= SUM_A * E55(NL)   ! Result - Volume absorption coefficient in ech layer
	 EXTT(NL)=SCATT(NL)+ABSS(NL) ! Result - Volume extinction coefficient in ech layer
     PH_LAER=PH_LAER/SUM_S
     CALL INTEGR_PH_F(ST_ANG,PH_LAER,IMEN_PHF,SMES,IM4) 
     PROBFUN(NL,:)=SMES ! Result - Array for angle scattering drawing 
	 ELSE
	 E55(NL)=E55(NL)/(SUM_A+SUM_S) ! Factor to get given extinction coefficient at given wavwnumber
	 END IF
! ----------------------------------------------------- !
  END DO
! ************************************************************** !
  IF(WN55 < 0.0) THEN
   WN55=-WN55
   GO TO 1
  END IF
  END

 SUBROUTINE SC_AB_A(Z_I,SC_A,AB_A,L_NUMB)
! Subroutine to calculate aerosol scatt. absorp. coeffitients and layer number.
 USE A_MOD
 DATA ZI_OLD,L_OLD/-1000.0,1/
  SC_A=0.0 ; AB_A=0.0
 IF(Z_I < ZDOWN(1).OR.Z_I > ZUP(NLAY))RETURN
  IF(Z_I <= ZI_OLD)THEN
        DO L=L_OLD,1,-1
        IF(Z_I <= ZUP(L) .AND. ZDOWN(L) <= Z_I ) EXIT
        END DO
  IF(L < 1) RETURN
  ELSE
        DO L=L_OLD,NLAY
        IF(Z_I <= ZUP(L) .AND. ZDOWN(L) <= Z_I) EXIT
        END DO
  IF(L > NLAY) RETURN
 END IF
  SC_A=SCATT(L) ; AB_A=ABSS(L) 
  ZI_OLD=Z_I ; L_OLD=L ; L_NUMB=L
 END
