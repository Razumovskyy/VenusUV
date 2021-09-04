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
	  DATA IBG/0/
!        OPEN(66,FILE='k_coef.chk')
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
		DO I=1,NGAS
		READ(55,'(A)')MOLECULE(I)
		END DO
!	WRITE(66,*)'Account atmosphere gases		: ',	&
!				(MOLECULE(I),I=1,NGAS)
     IF(IBG.EQ.0)THEN
	 IBG=1
        ALLOCATE (Z(JMAX),P(JMAX),T(JMAX),RABMA(NT,JMAX) &
,FLUXUP(LA,JMAX),FLUXDO(LA,JMAX),QT(LA,JMAX),FUP(LA,JMAX),FDO(LA,JMAX),Q(LA,JMAX))
        ALLOCATE (RO(NCOMP,JMAX),RO_INT_HCL(JMAX),RO_INT_OCS(JMAX), &
       RO_INT_CO2(JMAX),RO_INT_H2O(JMAX),RO_INT_SO2(JMAX),STAT=IERR)
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
! Water vapor ammount
 !                  sh2o=0.
  !                do j=2,jmax
!				  sh2o=sh2o+0.5*(ro(ih2o,j-1)+ro(ih2o,j))*(z(j)-z(j-1))
!				  end do
!				  sa=sh2o*18./6e23
!				  saa=sh2o
!	write(*,*)' Water vapor ammount ',sa,' [g/cm^2] ',saa,' [mol/cm^2]'
!	PAUSE
!	write(66,*)' Water vapor ammount ',sa,' [g/cm^2] ',saa,' [mol/cm^2]'
!                CLOSE(66)


  END SUBROUTINE ATM_PROF_READING
  END MODULE INITIAL_SHW_KD


!* --------------------------------------------------------------------- *
SUBROUTINE K_COEF_KD
USE INITIAL_ShW_KD
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

