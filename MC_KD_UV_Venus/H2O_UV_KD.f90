!  *** UV-H2O KD    (28 May,2021). *** !
      SUBROUTINE UV_H2O(I,NCH)
 USE INITIAL_SHW_KD
 REAL*8 H2O_1_1,H2O1  !### ,H2O_2_1,H2O_3_1
	write(*,*)' *********** H2O NOT Ready ********** '; RETURN
 
         DO J=1,JMAX
       H2O1=0.
       IF(IH2O > 0)THEN
       H2O1=RO(IH2O,J) 
       END IF 
        A1=0. 
     IF(NCH == 1)THEN
!   O=18.9606005208126 
A1=H2O_1_1(H2O1,J)
     ELSE
       IF(NCH == 2)THEN
write(*,*)' in UV is 1-channel only ' ;  pause 100 ; stop  !   ### 
!   O=13.1639587850482               
!   ###  A1=H2O_2_1(H2O1,J)
!   ###  A2=0.949E-24*(P(J)/1013.)**0.609*CO21
       ELSE 
!   O= 18.3479913512714               
!### A1=H2O_3_1(H2O1,J)
!### A2=0.364E-23*(P(J)/1013.)**0.799*CO21
       END IF
     END IF 
         RABMA(I,J)=A1    !### +A2 
         END DO
 END
            FUNCTION H2O_1_1(RP,J_LAYER) !
        USE INITIAL_SHW_KD  		    
        IMPLICIT REAL*8 (A-H,O-Z) 
        REAL*4 RO_I,ROI
  DIMENSION AB(200),RO_I(200)   !###          DIMENSION AB(N_TAB),RO_I(N_TAB)

DATA IBG/0/
IF(IBG.EQ.0)THEN
          IBG=1   
OPEN(1007,FILE='Cr-Sect.75deg')
READ(1007,*)N_TAB
          DO J=1,N_TAB
         READ(1007,*)RO_I(J),AB(J)
         RO_I(J)=ALOG(RO_I(J))
         END DO
	ROMAX=RO_I(1)  
        CLOSE(1007)
END IF 
          ROI=RO_INT_H2O(J_LAYER)
	IF(ROI.GE.ROMAX)THEN
      H2O_1_1=AB(1)*RP
	RETURN
	END IF
 
                         IF(ROI.LE.RO_I(N_TAB)) THEN
                        H2O_1_1=AB(N_TAB)*RP
                         ELSE 
                     DO J=2,N_TAB
                   IF(ROI.LE.RO_I(J-1).AND.ROI.GE.RO_I(J)) EXIT
                   END DO
        C1=(ROI-RO_I(J))/(RO_I(J-1)-RO_I(J)) ; C2=1.-C1
      H2O_1_1=(AB(J-1)*C1+AB(J)*C2)*RP ! 
                         END IF 
	      END

     
