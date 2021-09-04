!  *** UV-H2O KD    (28 May,2021). *** !
      SUBROUTINE UV_H2O(I,NCH)
 USE INITIAL_SHW_KD
 REAL*8 H2O_1_1,H2O1  !### ,H2O_2_1,H2O_3_1
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
        PARAMETER (N_TAB=21) ! Number of points in internal RO (integral)-grid.
        DIMENSION AB(N_TAB),RO_I(N_TAB)
   DATA RO_I,AB/ &
  0.2494E+21,  0.1412E+21,  0.6328E+20,  0.1704E+20,  0.2721E+19,  0.1847E+19,  0.1244E+19,  0.8306E+18,  0.5489E+18,  0.3586E+18, &
  0.2307E+18,  0.1456E+18,  0.8984E+17,  0.5425E+17,  0.3218E+17,  0.1885E+17,  0.1094E+17,  0.6325E+16,  0.3658E+16,  0.2134E+16, &
  0.1262E+16, &
  0.1241E-19,  0.1374E-19,  0.1483E-19,  0.1963E-19,  0.4051E-19,  0.5418E-19,  0.7769E-19,  0.1192E-18,  0.1861E-18,  0.2764E-18, &
  0.3774E-18,  0.4731E-18,  0.5523E-18,  0.6113E-18,  0.6515E-18,  0.6775E-18,  0.6930E-18,  0.7023E-18,  0.7085E-18,  0.7114E-18, &
  0.7125E-18/

IF(IBG.EQ.0)THEN
          IBG=1   
          DO J=1,N_TAB
         RO_I(J)=ALOG(RO_I(J))
         END DO
	ROMAX=RO_I(1)  
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

     
