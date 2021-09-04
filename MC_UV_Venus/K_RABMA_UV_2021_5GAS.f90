! --------------------------------------------------------------------- !
	FUNCTION VAN_VLE(T,V)
!* the radiation factor
	PARAMETER (PLANCK=6.626075E-27,BOLTZ=1.380658E-16,	&
	CLIGHT=2.99792458E10,RADCN2=PLANCK*CLIGHT/BOLTZ)
!*
VAN_VLE=1.0 ! 1.0 Attention: it will be taken into account in K_coef_UNIV_H.f90
			END

! 22 September,2007.
	SUBROUTINE K_COEF_H_U(LINE_COPLING)
USE MESH 
USE INITIAL_ShW_CLOUD 
USE A_MOD_SHW

!* --------------------------------------------------------------------- *
!* H = CONST "UNIVERSAL" (Depends on DOPPLER Half-Width), MT_CKD-1.0 etc.*
!* --------------------------------------------------------------------- *
	REAL*8 VSTART,VFINISH,VS
	CHARACTER COMPONENT*5, LINE_COPLING*3, FNAME*80 
	COMMON/R_A/VSTART,VFINISH
	COMMON/GASES/ COMPONENT(NCOMP)
	COMMON/AMOL/ AMOLI(NCOMP)
	COMMON/PHIPAR/ T, P, RO
	COMMON/FILES/FNAME(42), NZ(42)  ! <*** correction 12May,2011
	DIMENSION ICODE(NCOMP) 
	DATA ISTART/0/
!*

!--------------	START ------------!

		VS=VSTART
		VFINISH=VS+DLT8
!WRITE(*,*)'<',VS,'cm**(-1)>'
 
 
 DO JLEV= 1,JMAX ! *** global JLEV - loop ! *** !
	P	= P1(JLEV)
	T	= T1(JLEV)
			 RKG=0.; RKGL=0. ;RKGP=0. ; RK=0. 
N2_FLAG=0 
IO2_FLAG=0
		DO 4 IG=1,NGAS
	RO=RO1(IG,JLEV) 
IF(MOLECULE(IG) == 'CO2') CALL CRS_CO2(VS)
 IF(MOLECULE(IG) == 'H2O') CALL CRS_H2O(VS)
  IF(MOLECULE(IG) == 'OCS') CALL CRS_OCS(VS)
  IF(MOLECULE(IG) == 'HCL') CALL CRS_HCL(VS)
  IF(MOLECULE(IG) == 'SO2') CALL CRS_SO2(VS)
!!! IF(MOLECULE(IG) == 'XXX') CALL CRS_XXX(VS)


 4	CONTINUE

! ------------------------ !
!*	cross sections *
            VR4=VS
!####					CALL HALOCARBONS(VR4)
!****************************************************************
!*						*** SUMMARISING		***
DO N=NGR,2,-1
   DO J=1,NTG(N)
   I=J*2-1
   RKGP(I,N-1)=RKGP(I,N-1)+RKGP(J,N)
   A=RKGP(J,N)*0.375+RKG(J,N)*0.75-RKGL(J,N)*0.125
   IF(A<RKG(J,N))THEN
 IF(RKG(J,N)<RKGP(J,N).AND.RKG(J,N)<RKGL(J,N))THEN
 ELSE
 A=0.5*(RKGP(J,N)+RKG(J,N))
 END IF
  END IF
			RKG(I,N-1) =RKG(I,N-1)+A
			RKGL(I,N-1)=RKGL(I,N-1)+RKG(J,N)
			M=I+1
			RKGP(M,N-1)=RKGP(M,N-1)+RKG(J,N)
            A=RKGL(J,N)*0.375+RKG(J,N)*0.75-RKGP(J,N)*0.125
IF(A<RKG(J,N))THEN
 IF(RKG(J,N)<RKGP(J,N).AND.RKG(J,N)<RKGL(J,N))THEN
 ELSE
 A=0.5*(RKGL(J,N)+RKG(J,N))
 END IF
END IF			
			RKG(M,N-1) =RKG(M,N-1)+A
			RKGL(M,N-1)=RKGL(M,N-1)+RKGL(J,N)
	END DO


END DO
!*
!*
			I=1
			DO J = 1,NTG(1)
			I=I+1
A=(RKGP(J,1)*0.375+RKG(J,1)*0.75-RKGL(J,1)*0.125)
IF(A<RKG(J,1))THEN
! IF(RKG(J,1)<RKGP(J,1).AND.RKG(J,1)<RKGL(J,1))THEN
! ELSE
 A=0.5*(RKGP(J,1)+RKG(J,1))
! END IF
END IF
	RK(I) =RK(I)+A
			I=I+1
	RK(I)=RK(I)+RKG(J,1)
			I=I+1
A=(RKGL(J,1)*0.375+RKG(J,1)*0.75-RKGP(J,1)*0.125)
IF(A<RKG(J,1))THEN
! IF(RKG(J,1)<RKGP(J,1).AND.RKG(J,1)<RKGL(J,1))THEN
! ELSE
 A=0.5*(RKGL(J,1)+RKG(J,1))
! END IF
END IF
	RK(I) =RK(I)+A
			I=I+1
	RK(I)=RK(I)+RKGL(J,1)
			END DO
!********************* ********************************************
!### NO!!!! VW_H factor 
!### NO!*		van Vleck-Weisskopf-  Huber factor !!!	*

!### NO!  Fi(V)={S(Tref)*[S(T)/S(Tref)] * [(1+exp(-Vi*b/Tref)/(1-exp(-Vi*b/Tref))]/Vi } x ! see LBL2003     
!### NO!       x [(1-exp(-V*b/T)/(1+exp(-V*b/T))]*V        ! see below 

!### NOIF(VSTART>=2000.) THEN
				JM1=0 ;	DO J =1,NT
!### NO                                   FACTV=VR4+H*JM1
!### NO                                    RK(J)=RK(J)*FACTV 
!### NO					if(rk(j) < 0.)rk(j)=0.
                                    RABMA(J,JLEV)=RK(J)
!### NO					JM1=JM1+1
!### NO					END DO
!### NO ELSE
!### NO				JM1=0 ;	DO J =1,NT
!### NO                                        VIVI=VSTART+H*JM1
!### NO EVV_=VIVI*1.438786/T ! ! ### 1
!### NO IF(VIVI<0.1)THEN
!### NO FACTV=VIVI*EVV_/(2.-EVV_)
!### NO ELSE
!### NO EVV=EXP(-EVV_)
!### NO FACTV=VIVI *(1.-EVV)/(1.+EVV)
!### NO END IF
!### NO                                    RK(J)=RK(J)*FACTV 
!### NO					if(rk(j) < 0.)rk(j)=0.
!### NO                                    RABMA(J,JLEV)=RK(J)
!### NO					JM1=JM1+1
				END DO
!### NOEND IF
END DO ! JLEV - loop
 100 FORMAT(/80(1H-))
!******************************************************************
!### RABMA-plot!
!###     open(432,file='rabma')
!###  write(432,*)' 3 ',nt
 !###    do i=1,nt
 !###	write(432,*)i,rabma(i,1),rabma(i,21),rabma(i,51)
 !###	end do
 !###	close(432)
 !### result=RUNQQ('ToDraw.exe','rabma')
  !### stop
	END
