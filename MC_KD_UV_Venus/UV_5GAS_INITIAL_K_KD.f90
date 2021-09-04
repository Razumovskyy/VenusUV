! May, August,2021.
 MODULE INITIAL_SHW_KD ! Information from atmospheric profile
      IMPLICIT REAL*8 (A-H,O-Z)
       PARAMETER (NCOMP = 1,NT=1) !  <=== H2O-ONLY-NT; 1 - total number of channels for H2O
	CHARACTER*80 TITLE
	CHARACTER    MOLECULE(NCOMP)*5,MLC*7
   INTEGER*4 :: NGAS,JMAX,IH2O=0,ICO2=0,IHCL=0,IOCS=0, ISO2=0
   
   REAL*8, ALLOCATABLE :: Z(:),P(:),T(:),RO(:,:), &
   FLUXUP(:,:),FLUXDO(:,:),QT(:,:)
   REAL*4, ALLOCATABLE :: RO_INT_HCL(:),RO_INT_OCS(:),RO_INT_CO2(:),RO_INT_H2O(:),RO_INT_SO2(:)  &
           ,FUP(:,:),FDO(:,:),Q(:,:),RABMA(:,:)

  CONTAINS

   SUBROUTINE ATM_PROF_READING(LA)
      IMPLICIT REAL*8 (A-H,O-Z)
	CHARACTER*80 ATM_PATH
   CHARACTER*1 A_ZO
   DIMENSION Z_CL(99) ! MAXimal namber of layers in cloud

	  DATA IBG/0/

    OPEN(110,FILE='ATM_AEROSOL.MODEL') ! input File - AEROSOL (CLOUD) model 
    READ(110,*) NLAYER  ! Number of Layers with different Phase Functions
   NLAY=ABS(NLAYER)   
   READ(110,*)WN5 ! not essential
   READ(110,*)E55 ! not essential
	  READ(110,*)N_TEC,ZDOW,ZU      ! Boundaries in km
    N_CL=2 ; Z_CL(1)=ZDOW ; Z_CL(2)=ZU
 IF(NLAY > 1)THEN
      READ(110,*)NFRAC     ! Number of fractions
      READ(110,*)WFRA ! not essential       ! their Weights
        DO J=1,NFRAC
        READ(110,*)A_ZO ! not essential ! path to aerosol model in database
        END DO
        DO I=2,NLAY
	  READ(110,*)N_TEC,ZDOW,ZU      ! Boundaries in km
      READ(110,*)NFRAC  ! Number of fractions
      READ(110,*)WFRA ! not essential       ! their Weights
        DO J=1,NFRAC
        READ(110,*)A_ZO ! not essential ! path to aerosol model in database
        END DO
   IF(ZDOW /= Z_CL(N_CL)) THEN
           N_CL=N_CL+1 ; Z_CL(N_CL)=ZDOW
   END IF
           N_CL=N_CL+1 ; Z_CL(N_CL)=ZU
       END DO
 END IF
    CLOSE (110)

       IF(N_CL.LT.2.OR.N_CL.GT.99) THEN
       WRITE(*,*)' CHECK CLOUD/AEROSOL MODEL:',NLAY,ZDOW,ZU
	   STOP
       END IF
                OPEN(66,FILE='k_coef.chk')
	OPEN(99,FILE='k_coef.in')
	READ(99,'(A)')ATM_PATH
!		WRITE(66,*)' The atmospheric conditions from file = ',ATM_PATH
	CLOSE(99)
	OPEN(55,FILE=ATM_PATH)
!*
		READ(55,'(A)')TITLE
!	WRITE(*,*)TITLE
!	WRITE(66,*)TITLE
	READ(55,*)NGAS,JMAX
!	WRITE(*,*)'The number of gases (NGAS) : ',NGAS
!	WRITE(*,*)'The number of levels (JMAX) : ',JMAX
!	WRITE(66,*)'The number of gases (NGAS) : ',NGAS
!	WRITE(66,*)'The number of levels (JMAX) : ',JMAX
JMAC=JMAX+N_CL
		DO I=1,NGAS
		READ(55,'(A)')MOLECULE(I)
		END DO
!	WRITE(66,*)'Account atmosphere gases		: ',	&
!				(MOLECULE(I),I=1,NGAS)
     IF(IBG.EQ.0)THEN
	 IBG=1
        ALLOCATE (Z(JMAC),P(JMAC),T(JMAC),RABMA(NT,JMAC) &
,FLUXUP(LA,JMAC),FLUXDO(LA,JMAC),QT(LA,JMAC),FUP(LA,JMAC),FDO(LA,JMAC),Q(LA,JMAC))
        ALLOCATE (RO(NCOMP,JMAC),RO_INT_HCL(JMAC),RO_INT_OCS(JMAC), &
       RO_INT_CO2(JMAC),RO_INT_H2O(JMAC),RO_INT_SO2(JMAC),STAT=IERR)
         IF(IERR/=0)THEN
          WRITE(*,*)' Allocaion is wrong !!!'
          STOP
         END IF
      END IF
      RABMA=0.
	DO J=1,JMAX
	READ(55,*)Z(J),P(J),T(J),( RO(I,J), I = 1, NGAS )
!	WRITE(66,*)Z(J),P(J),T(J),( RO(I,J), I = 1, NGAS )
	P(J)=P(J)  ! #### not *1013.25 -> ATMOSPHERES Units ONLY!
	END DO
	WRITE(*,*) ' ##################   Pressure in mBAR ############# '
		CLOSE(55)

! Inserting CLOUD into atmospheric model
! *************************************************
             GAS_CLOUD=0.001 ! [KM] (If /Z_CL-Z/ < GAS_CLOUD levels are the same) 
			 ICL_MAX=-1 ; ICL_MIN=100000
        DO ICL=1,N_CL
		ZC=Z_CL(ICL)
		 DO JGA=2,JMAX
		 JGAM=JGA-1
		 IF(Z(JGAM)<=ZC.AND.ZC<Z(JGA)) EXIT 
         END DO
! Z(J-1)=ZC
          IF(ABS(Z(JGAM)-ZC)<GAS_CLOUD) THEN
		  Z(JGAM)=Z_CL(ICL)
		   IF(JGA<ICL_MIN)ICL_MIN=JGAM
           IF(JGA>ICL_MAX)ICL_MAX=JGAM
		  ELSE
! Z(J)=ZC
           IF(ABS(Z(JGA)-ZC)<GAS_CLOUD) THEN
		   Z(JGA)=Z_CL(ICL)
		   IF(JGA<ICL_MIN)ICL_MIN=JGA
           IF(JGA>ICL_MAX)ICL_MAX=JGA
		   ELSE
! Z(J-1)< ZC < Z(J) - RAZDVIDGKA
     		   DO JJ=JMAX,JGA,-1
           Z(JJ+1)=Z(JJ)
           P(JJ+1)=P(JJ)
		   T(JJ+1)=T(JJ)
		       DO III=1,NGAS
		   RO(III,JJ+1)=RO(III,JJ)
		       END DO
		   END DO
		   Z(JGA)=ZC
           C1=(Z(JGA+1)-ZC)/(Z(JGA+1)-Z(JGAM))
		   C2=1.-C1
		   P(JGA)=P(JGAM)*C1+P(JGA+1)*C2
		   T(JGA)=T(JGAM)*C1+T(JGA+1)*C2
		       DO III=1,NGAS
		   RO(III,JGA)=RO(III,JGAM)*C1+RO(III,JGA+1)*C2
		       END DO
			   continue
              JMAX=JMAX+1
		   IF(JGA<ICL_MIN)ICL_MIN=JGA
           IF(JGA>ICL_MAX)ICL_MAX=JGA
		   END IF
          END IF

		END DO
	WRITE(*,*)'The number of gases (NGAS) : ',NGAS
	WRITE(*,*)'The number of levels (JMAX) : ',JMAX
	WRITE(66,*)'The number of gases (NGAS) : ',NGAS
	WRITE(66,*)'The number of levels (JMAX) : ',JMAX

 WRITE(66,*)'CLOUD between: ',ICL_MIN,ICL_MAX,Z(ICL_MIN),Z(ICL_MAX)

 open(555,file='ATM&CLOUD.Prof')
	write(555,'(A)')ATM_PATH
	write(555,*)NGAS,JMAX
do I=1,NGAS
		write(555,'(A)')MOLECULE(I)
end do
	DO J=1,JMAX
	WRITE(66,*)J,Z(J),P(J),T(J), (RO(I,J), I = 1, NGAS )
	write(555,*)Z(J),P(J),T(J), (RO(I,J), I = 1, NGAS )
	END DO
close(555)
                CLOSE(66)


 IH2O=0 ; ICO2=0 ; IHCL=0 ; IOCS=0 ; ISO2=0
                  DO I=1,NGAS
                  IF(MOLECULE(I).EQ.'CO2')ICO2=I
                  IF(MOLECULE(I).EQ.'H2O')IH2O=I
                  IF(MOLECULE(I).EQ.'HCL')IHCL=I
                  IF(MOLECULE(I).EQ.'OCS')IOCS=I
                  IF(MOLECULE(I).EQ.'SO2')ISO2=I
                  END DO
     JMX1=JMAX-1
RO_INT_CO2=0. ; RO_INT_H2O=0. ; RO_INT_HCL=0. ; RO_INT_OCS=0. ; RO_INT_SO2=0. 
! Integration of SO2's profile 
   IF(ISO2 > 0)THEN
      DO J=JMAX,2,-1
     RO_INT_SO2(J-1)=RO_INT_SO2(J)+(RO(ISO2,J)+RO(ISO2,J-1))/2.0*(Z(J)-Z(J-1))
      END DO
         DO J=1,JMX1
         RO_INT_SO2(J)=ALOG(RO_INT_SO2(J))
         END DO
   END IF
! Integration of HCL's profile 
   IF(IHCL > 0)THEN
      DO J=JMAX,2,-1
     RO_INT_HCL(J-1)=RO_INT_HCL(J)+(RO(IHCL,J)+RO(IHCL,J-1))/2.0*(Z(J)-Z(J-1))
      END DO
         DO J=1,JMX1
         RO_INT_HCL(J)=ALOG(RO_INT_HCL(J))
         END DO
   END IF
! Integration of OCS's profile 
   IF(IOCS > 0)THEN
      DO J=JMAX,2,-1
     RO_INT_OCS(J-1)=RO_INT_OCS(J)+(RO(IOCS,J)+RO(IOCS,J-1))/2.0*(Z(J)-Z(J-1))
      END DO
         DO J=1,JMX1
         RO_INT_OCS(J)=ALOG(RO_INT_OCS(J))
         END DO
   END IF
! Integration of H2O's profile 
   IF(IH2O > 0)THEN
      DO J=JMAX,2,-1
     RO_INT_H2O(J-1)=RO_INT_H2O(J)+(RO(IH2O,J)+RO(IH2O,J-1))/2.0*(Z(J)-Z(J-1))
      END DO
         DO J=1,JMX1
         RO_INT_H2O(J)=ALOG(RO_INT_H2O(J))
         END DO
   END IF
! Integration of CO2's profile 
   IF(ICO2 > 0)THEN
      DO J=JMAX,2,-1
     RO_INT_CO2(J-1)=RO_INT_CO2(J)+(RO(ICO2,J)+RO(ICO2,J-1))/2.0*(Z(J)-Z(J-1))
      END DO
         DO J=1,JMX1
         RO_INT_CO2(J)=ALOG(RO_INT_CO2(J))
         END DO
   END IF

  END SUBROUTINE ATM_PROF_READING
  END MODULE INITIAL_SHW_KD


!* --------------------------------------------------------------------- *
SUBROUTINE K_COEF_KD
! USE INITIAL_ShW_KD
! 1) BAND CO2 
     I=0
 		DO NCH=1,1
        I=I+1
        CALL UV_CO2(I,NCH)
         END DO
! 2) BAND H2O 
     I=0
 		DO NCH=1,1
        I=I+1
        CALL UV_H2O(I,NCH)
         END DO
! 3) BAND HCL 
     I=0
 		DO NCH=1,1
        I=I+1
        CALL UV_HCL(I,NCH)
         END DO
! 4) BAND OCS 
     I=0
 		DO NCH=1,1
        I=I+1
        CALL UV_OCS(I,NCH)
         END DO
! 5) BAND SO2 
     I=0
 		DO NCH=1,1
        I=I+1
        CALL UV_SO2(I,NCH)
         END DO

END 

