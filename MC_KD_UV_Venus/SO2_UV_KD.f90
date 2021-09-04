!  *** UV-SO2 KD    (28 Aug,2021). *** !
      SUBROUTINE UV_SO2(I,NCH)
 USE INITIAL_SHW_KD
 REAL*8 SO2_1_1,SO21  !### ,H2O_2_1,H2O_3_1
  DO J=1,JMAX
       SO21=0.
       IF(ISO2 > 0)THEN
       SO21=RO(ISO2,J) 
       END IF 
        A1=0. 
     IF(NCH == 1)THEN
!   O=18.9606005208126 
A1=SO2_1_1(SO21,J)
     ELSE
       IF(NCH == 2)THEN
write(*,*)' in UV is 1-channel only ' ;  pause 100 ; stop  !   ### 
       END IF
     END IF 
         RABMA(I,J)=RABMA(I,J)+A1    !### ??? +A2 
  END DO
 END
            FUNCTION SO2_1_1(RP,J_LAYER) !
        USE INITIAL_SHW_KD  		    
        IMPLICIT REAL*8 (A-H,O-Z) 
        REAL*4 RO_I,ROI
  DIMENSION AB(200),RO_I(200)   !###          DIMENSION AB(N_TAB),RO_I(N_TAB)

DATA IBG/0/
IF(IBG.EQ.0)THEN
          IBG=1   
OPEN(1007,FILE='./K-Terms/SO2.CrS')
READ(1007,*)N_TAB
          DO J=1,N_TAB
         READ(1007,*)RO_I(J),AB(J)
         RO_I(J)=ALOG(RO_I(J))
         END DO
	ROMAX=RO_I(1)  
        CLOSE(1007)
END IF 
          ROI=RO_INT_SO2(J_LAYER)
	IF(ROI.GE.ROMAX)THEN
      SO2_1_1=AB(1)*RP
	RETURN
	END IF
 
                         IF(ROI.LE.RO_I(N_TAB)) THEN
                        SO2_1_1=AB(N_TAB)*RP
                         ELSE 
                     DO J=2,N_TAB
                   IF(ROI.LE.RO_I(J-1).AND.ROI.GE.RO_I(J)) EXIT
                   END DO
        C1=(ROI-RO_I(J))/(RO_I(J-1)-RO_I(J)) ; C2=1.-C1
      SO2_1_1=(AB(J-1)*C1+AB(J)*C2)*RP ! 
                         END IF 
	      END

     
